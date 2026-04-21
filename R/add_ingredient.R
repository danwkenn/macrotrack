#' Add an ingredient to the nutrition database
#'
#' Inserts a new ingredient into the ingredients table. The ingredient_id
#' is assigned by the database.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param name Character. The name of the ingredient.
#' @param calories Numeric. Calories per 100g.
#' @param protein Numeric. Protein per 100g in grams.
#' @param carbs Numeric. Carbohydrates per 100g in grams.
#' @param fat Numeric. Fat per 100g in grams.
#' @param portion_size Numeric. Weight of one standard portion in grams.
#' @param portion_name Character. Label for one standard portion (e.g. "egg", "slice").
#'
#' @return Integer. The ingredient_id assigned by the database, returned invisibly.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   add_ingredient(con, "Egg", 143, 12.5, 0.7, 9.9, 60, "egg")
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
add_ingredient <- function(con, name, calories, protein, carbs, fat,
                           portion_size, portion_name) {

  result <- DBI::dbGetQuery(
    con,
    "INSERT INTO ingredients
       (name, calories, protein, carbs, fat, portion_size, portion_name)
     VALUES ($1, $2, $3, $4, $5, $6, $7)
     RETURNING ingredient_id",
    params = list(name, calories, protein, carbs, fat, portion_size, portion_name)
  )

  invisible(result$ingredient_id)
}