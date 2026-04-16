#' Add an ingredient to the nutrition database
#'
#' Inserts a new ingredient into the ingredients table.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param name Character. The name of the ingredient.
#' @param calories Numeric. Calories per 100g.
#' @param protein Numeric. Protein per 100g in grams.
#' @param carbs Numeric. Carbohydrates per 100g in grams.
#' @param fat Numeric. Fat per 100g in grams.
#' @param portion_size Numeric. Weight of one standard portion in grams.
#' @param portion_name Character. Label for one standard portion (e.g. "egg", "slice").
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "nutrition.db")
#'   add_ingredient(con, "Egg", 143, 12.5, 0.7, 9.9, 60, "egg")
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
add_ingredient <- function(con, name, calories, protein, carbs, fat,
                           portion_size, portion_name) {

  max_id <- DBI::dbGetQuery(con, "SELECT MAX(ingredient_id) FROM ingredients")[[1]]
  if (is.na(max_id)) max_id <- 0

  ingredient_df <- data.frame(
    ingredient_id = max_id + 1,
    name          = name,
    calories      = calories,
    protein       = protein,
    carbs         = carbs,
    fat           = fat,
    portion_size  = portion_size,
    portion_name  = portion_name
  )

  DBI::dbAppendTable(con, "ingredients", ingredient_df)

  invisible(TRUE)
}