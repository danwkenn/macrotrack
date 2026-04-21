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
    ingredients      = ingredients_df,
    meal_ingredients = meal_ingredients_df
  )

}

#' Import a meal into the nutrition database
#'
#' Inserts a meal and its ingredients into the database from a CSV file.
#' If macro nutrients are not provided, they will be calculated from
#' the ingredients. All inserts are performed in a single transaction:
#' if any step fails, no rows are inserted.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param meal_name Character. The name of the meal to import.
#' @param csv_path Character. Path to the CSV file containing ingredient data.
#' @param use_ingredient_macros Logical. If TRUE, the macros for this meal are
#'   derived from its ingredients.
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
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   import_meal(con, meal_name = "Falafel Wrap", csv_path = "falafel_wrap.csv")
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
import_meal <- function(con, meal_name, csv_path, use_ingredient_macros = TRUE,
                        calories = NULL, protein = NULL,
                        carbs = NULL, fat = NULL) {

  # Parse the CSV
  parsed         <- parse_meal_csv(csv_path)
  ingredients_df <- parsed$ingredients
  quantities     <- parsed$meal_ingredients$quantity

  # Everything below runs atomically: if anything fails, nothing is inserted.
  DBI::dbWithTransaction(con, {

    # Insert each ingredient, capturing its new ID in input order
    ingredient_ids <- vapply(
      seq_len(nrow(ingredients_df)),
      function(i) {
        add_ingredient(
          con          = con,
          name         = ingredients_df$ingredient[i],
          calories     = ingredients_df$calories[i],
          protein      = ingredients_df$protein[i],
          carbs        = ingredients_df$carbs[i],
          fat          = ingredients_df$fat[i],
          portion_size = ingredients_df$portion_size[i],
          portion_name = ingredients_df$portion_name[i]
        )
      },
      integer(1)
    )

    # Insert the meal, capture its new ID
    meal_result <- DBI::dbGetQuery(
      con,
      "INSERT INTO meals
         (name, use_ingredient_macros, calories, protein, carbs, fat)
       VALUES ($1, $2, $3, $4, $5, $6)
       RETURNING meal_id",
      params = list(
        meal_name,
        as.integer(use_ingredient_macros),
        if (is.null(calories)) NA_real_ else calories,
        if (is.null(protein))  NA_real_ else protein,
        if (is.null(carbs))    NA_real_ else carbs,
        if (is.null(fat))      NA_real_ else fat
      )
    )
    new_meal_id <- meal_result$meal_id

    # Link ingredients to the meal
    meal_ingredients_df <- data.frame(
      meal_id       = new_meal_id,
      ingredient_id = ingredient_ids,
      quantity      = quantities
    )
    DBI::dbAppendTable(con, "meal_ingredients", meal_ingredients_df)
  })

  invisible(TRUE)
}