#' Compute total macros for a meal
#'
#' Calculates the total macronutrients for a given meal, either by summing
#' ingredient contributions or using meal-level macros, multiplied by the
#' number of servings.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param meal_id Integer. The ID of the meal to calculate macros for.
#' @param servings Numeric. The number of servings. Defaults to 1.
#'
#' @return A data frame with columns: calories, protein, carbs, fat.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   get_meal_macros(con, meal_id = 1, servings = 2)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
get_meal_macros <- function(con, meal_id, servings = 1) {

  # Query meal-level data
  meal <- DBI::dbGetQuery(con, "SELECT * FROM meals WHERE meal_id = $1",
                          params = list(as.integer(meal_id)))

  # If meal-level macros are used, return them directly
  if (meal$use_ingredient_macros == 0) {
    return(data.table::data.table(
      meal_id   = meal_id,
      meal_name = meal$name,
      servings  = servings,
      calories  = meal$calories * servings,
      protein   = meal$protein  * servings,
      carbs     = meal$carbs    * servings,
      fat       = meal$fat      * servings
    ))
  }

 # Query all ingredient data for this meal
  sql <- "
    SELECT
      mi.meal_id,
      mi.quantity,
      i.name,
      i.calories,
      i.protein,
      i.carbs,
      i.fat,
      i.portion_size,
      i.portion_name,
      m.name AS meal_name
    FROM meal_ingredients mi
    JOIN ingredients i ON mi.ingredient_id = i.ingredient_id
    JOIN meals m ON mi.meal_id = m.meal_id
    WHERE mi.meal_id = $1
  "

  meal_data <- DBI::dbGetQuery(con, sql, params = list(as.integer(meal_id)))

  # Convert to data.table
  data.table::setDT(meal_data)

  # Compute macros per portion
  meal_data[, calories_per_portion := (portion_size / 100) * calories]
  meal_data[, protein_per_portion  := (portion_size / 100) * protein]
  meal_data[, carbs_per_portion    := (portion_size / 100) * carbs]
  meal_data[, fat_per_portion      := (portion_size / 100) * fat]

  # Compute total macros per ingredient
  meal_data[, calories_total := calories_per_portion * quantity * servings]
  meal_data[, protein_total  := protein_per_portion  * quantity * servings]
  meal_data[, carbs_total    := carbs_per_portion    * quantity * servings]
  meal_data[, fat_total      := fat_per_portion      * quantity * servings]

# Sum totals across all ingredients
  meal_summary <- meal_data[, .(
    meal_name = unique(meal_name),
    servings  = servings,
    calories  = sum(calories_total),
    protein   = sum(protein_total),
    carbs     = sum(carbs_total),
    fat       = sum(fat_total)
  ), by = meal_id]

  return(meal_summary)
}