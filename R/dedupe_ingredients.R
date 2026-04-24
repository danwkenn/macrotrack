#' Preview the effect of deduplicating ingredients
#'
#' Returns a data frame showing, for each affected meal, the current
#' ingredient rows that will be consolidated and the single row that
#' will replace them. Does not modify the database.
#'
#' The portion_size invariant: grams consumed in each meal must be
#' unchanged by the dedupe. The returned grams_before_total and
#' grams_after values should be equal (within floating-point tolerance).
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param keep_id Integer. The ingredient_id to keep.
#' @param drop_ids Integer vector. The ingredient_ids to remove.
#'
#' @return A data frame with one row per affected meal. Columns:
#'   meal_id, meal_name, before (character, multi-line description of
#'   existing rows), after (character, description of the consolidated
#'   row), grams_before_total, grams_after.
#'
#' @export
preview_dedupe_ingredients <- function(con, keep_id, drop_ids) {

  keep_id  <- as.integer(keep_id)
  drop_ids <- as.integer(drop_ids)

  # Keeper's portion_size and name
  keeper <- DBI::dbGetQuery(
    con,
    "SELECT name, portion_size FROM ingredients WHERE ingredient_id = $1",
    params = list(keep_id)
  )
  if (nrow(keeper) == 0) {
    stop("Keeper ingredient_id not found.", call. = FALSE)
  }
  keep_name         <- keeper$name
  keep_portion_size <- keeper$portion_size

  # All meal_ingredients rows affected: duplicates, plus any existing
  # keeper row in the same meals. We fetch both so the preview shows
  # the full set of rows that will be replaced.
  affected_ids <- unique(c(keep_id, drop_ids))

  # RPostgres doesn't support array parameters directly. We build
  # comma-separated placeholder lists like "$1, $2, $3" for IN clauses
  # and pass each element as its own scalar parameter.
  affected_ph <- paste0("$", seq_along(affected_ids), collapse = ", ")
  drop_ph     <- paste0("$", length(affected_ids) + seq_along(drop_ids),
                        collapse = ", ")

  sql <- sprintf("
    SELECT
      mi.meal_ingredient_id,
      mi.meal_id,
      m.name         AS meal_name,
      mi.ingredient_id,
      i.name         AS ingredient_name,
      i.portion_size AS ingredient_portion_size,
      mi.quantity
    FROM meal_ingredients mi
    JOIN ingredients i ON mi.ingredient_id = i.ingredient_id
    JOIN meals       m ON mi.meal_id       = m.meal_id
    WHERE mi.ingredient_id IN (%s)
      AND mi.meal_id IN (
        SELECT DISTINCT meal_id FROM meal_ingredients
        WHERE ingredient_id IN (%s)
      )
    ORDER BY mi.meal_id, mi.ingredient_id
  ", affected_ph, drop_ph)

  # Only include meals that actually contain at least one duplicate;
  # a meal that has only the keeper should be untouched.
  rows <- DBI::dbGetQuery(
    con,
    sql,
    params = as.list(c(affected_ids, drop_ids))
  )

  if (nrow(rows) == 0) {
    return(data.frame(
      meal_id            = integer(),
      meal_name          = character(),
      before             = character(),
      after              = character(),
      grams_before_total = numeric(),
      grams_after        = numeric(),
      stringsAsFactors   = FALSE
    ))
  }

  # Grams per row using that row's own ingredient portion size
  rows$grams <- rows$quantity * rows$ingredient_portion_size

  # Build the summary, one row per meal
  meal_ids <- unique(rows$meal_id)

  preview <- do.call(rbind, lapply(meal_ids, function(mid) {
    sub <- rows[rows$meal_id == mid, , drop = FALSE]

    before_lines <- paste0(
      sub$ingredient_name, " \u00d7 ",
      round(sub$quantity, 3), " (= ",
      round(sub$grams, 1), "g)"
    )
    before_str <- paste(before_lines, collapse = "\n")

    grams_total <- sum(sub$grams)
    new_quantity <- grams_total / keep_portion_size

    after_str <- paste0(
      keep_name, " \u00d7 ",
      round(new_quantity, 3), " (= ",
      round(new_quantity * keep_portion_size, 1), "g)"
    )

    data.frame(
      meal_id            = mid,
      meal_name          = sub$meal_name[1],
      before             = before_str,
      after              = after_str,
      grams_before_total = round(grams_total, 1),
      grams_after        = round(new_quantity * keep_portion_size, 1),
      stringsAsFactors   = FALSE
    )
  }))

  preview
}


#' Deduplicate ingredients
#'
#' Consolidates duplicate ingredients by rewriting all meal_ingredients
#' rows that reference a duplicate to instead reference the keeper,
#' with quantities converted so that total grams consumed in each meal
#' is preserved. If a meal contains both keeper and duplicate rows, they
#' are collapsed into a single row. The duplicate ingredients are then
#' deleted from the ingredients table.
#'
#' The entire operation is atomic: if any step fails, nothing is
#' committed.
#'
#' Conversion: new_quantity = (old_quantity * old_portion_size) / keep_portion_size
#'
#' Note: this function does not check that the duplicates share per-100g
#' macros with the keeper. The caller is responsible for confirming the
#' ingredients are truly duplicates.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param keep_id Integer. The ingredient_id to keep.
#' @param drop_ids Integer vector. The ingredient_ids to remove.
#'   Must not contain keep_id.
#'
#' @return An integer vector of length 2: the number of meal_ingredients
#'   rows rewritten and the number of ingredient rows deleted,
#'   returned invisibly.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   dedupe_ingredients(con, keep_id = 1, drop_ids = c(5, 9))
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
dedupe_ingredients <- function(con, keep_id, drop_ids) {

  keep_id  <- as.integer(keep_id)
  drop_ids <- as.integer(drop_ids)

  if (length(drop_ids) == 0) {
    stop("drop_ids must contain at least one ingredient_id.", call. = FALSE)
  }
  if (keep_id %in% drop_ids) {
    stop("keep_id must not appear in drop_ids.", call. = FALSE)
  }

  # Confirm keeper exists
  keeper <- DBI::dbGetQuery(
    con,
    "SELECT portion_size FROM ingredients WHERE ingredient_id = $1",
    params = list(keep_id)
  )
  if (nrow(keeper) == 0) {
    stop("Keeper ingredient_id not found.", call. = FALSE)
  }

  DBI::dbWithTransaction(con, {

    # RPostgres doesn't support array parameters directly, so we build
    # comma-separated placeholder lists (e.g. "$1, $2, $3") and pass
    # each id as its own scalar parameter.
    affected_ids <- c(keep_id, drop_ids)

    # Step 1: build a temp table of consolidated rows, one per affected
    # meal, with the summed quantity expressed in keeper portions.
    # "Affected" meals are those containing at least one duplicate row;
    # keeper rows in meals without any duplicate must be left alone.
    #
    # Placeholder layout:
    #   $1                          = keep_id
    #   $2                          = keeper$portion_size
    #   $3 .. $(2 + N_affected)     = affected_ids (keeper + drops)
    #   next N_drop                 = drop_ids
    affected_ph <- paste0("$", 2 + seq_along(affected_ids), collapse = ", ")
    drop_ph_1   <- paste0("$", 2 + length(affected_ids) + seq_along(drop_ids),
                          collapse = ", ")

    DBI::dbExecute(con, sprintf("
      CREATE TEMP TABLE tmp_dedupe_new_rows ON COMMIT DROP AS
      SELECT
        mi.meal_id,
        $1::int AS ingredient_id,
        SUM(mi.quantity * i.portion_size) / $2::real AS quantity
      FROM meal_ingredients mi
      JOIN ingredients i ON mi.ingredient_id = i.ingredient_id
      WHERE mi.ingredient_id IN (%s)
        AND mi.meal_id IN (
          SELECT DISTINCT meal_id FROM meal_ingredients
          WHERE ingredient_id IN (%s)
        )
      GROUP BY mi.meal_id
    ", affected_ph, drop_ph_1),
    params = as.list(c(
      keep_id,
      keeper$portion_size,
      affected_ids,
      drop_ids
    )))

    # Step 2: delete the old rows in affected meals.
    #   $1 .. $(N_affected)                       = affected_ids
    #   $(N_affected+1) .. $(N_affected+N_drop)   = drop_ids
    affected_ph_2 <- paste0("$", seq_along(affected_ids), collapse = ", ")
    drop_ph_2     <- paste0("$", length(affected_ids) + seq_along(drop_ids),
                            collapse = ", ")

    rows_rewritten <- DBI::dbExecute(con, sprintf("
      DELETE FROM meal_ingredients
      WHERE ingredient_id IN (%s)
        AND meal_id IN (
          SELECT DISTINCT meal_id FROM meal_ingredients
          WHERE ingredient_id IN (%s)
        )
    ", affected_ph_2, drop_ph_2),
    params = as.list(c(affected_ids, drop_ids)))

    # Step 3: insert the consolidated rows.
    DBI::dbExecute(con, "
      INSERT INTO meal_ingredients (meal_id, ingredient_id, quantity)
      SELECT meal_id, ingredient_id, quantity FROM tmp_dedupe_new_rows
    ")

    DBI::dbExecute(con, "DROP TABLE tmp_dedupe_new_rows")

    # Step 4: delete the duplicate ingredient rows. Safe because their
    # meal_ingredients references have all been rewritten to the keeper.
    drop_ph_3 <- paste0("$", seq_along(drop_ids), collapse = ", ")
    rows_deleted <- DBI::dbExecute(
      con,
      sprintf("DELETE FROM ingredients WHERE ingredient_id IN (%s)", drop_ph_3),
      params = as.list(drop_ids)
    )

    invisible(c(
      rows_rewritten = as.integer(rows_rewritten),
      rows_deleted   = as.integer(rows_deleted)
    ))
  })
}