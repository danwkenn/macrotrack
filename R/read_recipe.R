#' Parse a meal ingredients CSV into two data frames
#'
#' Reads a CSV file containing meal ingredient data and splits it into
#' two data frames ready for insertion into the ingredients and
#' meal_ingredients tables.
#'
#' @param csv_path Character. Path to the CSV file to import.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{ingredients}{Data frame of ingredient nutritional data}
#'     \item{meal_ingredients}{Data frame of ingredient names and quantities}
#'   }
#'
#' @examples
#' \dontrun{
#'   parse_meal_csv("falafel_wrap.csv")
#' }
#'
#' @export
parse_meal_csv <- function(csv_path) {

  raw <- read.csv(csv_path)

  ingredients_df <- raw[, c("ingredient", "calories", "protein",
                             "carbs", "fat", "portion_size", "portion_name")]

  meal_ingredients_df <- raw[, c("ingredient", "quantity")]

  list(
    ingredients    = ingredients_df,
    meal_ingredients = meal_ingredients_df
  )

}

#' Import a meal into the nutrition database
#'
#' Inserts a meal and its ingredients into the database from a CSV file.
#' If macro nutrients are not provided, they will be calculated from
#' the ingredients.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param meal_name Character. The name of the meal to import.
#' @param csv_path Character. Path to the CSV file containing ingredient data.
#' @param use_ingredient_macros Boolean. If TRUE, then the macros for this are based on the ingrediants.
#' @param calories Optional numeric. Total calories for the meal. If NULL,
#'   calculated from ingredients.
#' @param protein Optional numeric. Total protein for the meal in grams. If NULL,
#'   calculated from ingredients.
#' @param carbs Optional numeric. Total carbohydrates for the meal in grams. If NULL,
#'   calculated from ingredients.
#' @param fat Optional numeric. Total fat for the meal in grams. If NULL,
#'   calculated from ingredients.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "nutrition.db")
#'   import_meal(con, meal_name = "Falafel Wrap", csv_path = "falafel_wrap.csv")
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
import_meal <- function(con, meal_name, csv_path, use_ingredient_macros = TRUE,
                        calories = NULL, protein = NULL,
                        carbs = NULL, fat = NULL) {

  # Parse the CSV
  parsed <- parse_meal_csv(csv_path)
  ingredients_df <- parsed$ingredients

  # Find the current maximum ingredient_id
  max_id <- DBI::dbGetQuery(con, "SELECT MAX(ingredient_id) FROM ingredients")[[1]]

  # If table is empty MAX() returns NA, so default to 0
  if (is.na(max_id)) max_id <- 0

  # Assign new IDs
  ingredients_df$ingredient_id <- seq(max_id + 1, max_id + nrow(ingredients_df))

  names(ingredients_df)[which(names(ingredients_df) == "ingredient")] <- "name"

  # Insert into ingredients table
  DBI::dbAppendTable(con, "ingredients", ingredients_df)

  # Find the current maximum meal_id
  max_meal_id <- DBI::dbGetQuery(con, "SELECT MAX(meal_id) FROM meals")[[1]]

  # If table is empty MAX() returns NA, so default to 0
  if (is.na(max_meal_id)) max_meal_id <- 0

  # Build the meals data frame
  meals_df <- data.frame(
    meal_id               = max_meal_id + 1,
    name                  = meal_name,
    use_ingredient_macros = as.numeric(use_ingredient_macros),
    calories              = if (is.null(calories)) NA_real_ else calories,
    protein               = if (is.null(protein)) NA_real_ else protein,
    carbs                 = if (is.null(carbs)) NA_real_ else carbs,
    fat                   = if (is.null(fat)) NA_real_ else fat
  )

  # Insert into meals table
  DBI::dbAppendTable(con, "meals", meals_df)

  # Build the meal_ingredients data frame
  meal_ingredients_df <- data.frame(
    meal_id       = max_meal_id + 1,
    ingredient_id = ingredients_df$ingredient_id,
    quantity      = parsed$meal_ingredients$quantity
  )

  # Insert into meal_ingredients table
  DBI::dbAppendTable(con, "meal_ingredients", meal_ingredients_df)

  invisible(TRUE)
}