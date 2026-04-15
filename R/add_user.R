#' Add a user to the nutrition database
#'
#' Inserts a new user into the users table with lifestyle inputs and
#' optional macro overrides. If macro overrides are not provided, targets
#' will be calculated from lifestyle inputs when retrieved.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param name Character. The user's name.
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param height_cm Numeric. Height in centimetres.
#' @param age_years Numeric. Age in years.
#' @param gender Character. Either "M" or "F".
#' @param goal Character. Either "maintenance" or "recomposition".
#' @param work_type Character. One of "sedentary", "partially_active", or "active".
#' @param exercise_days Integer. Number of exercise days per week (0-7).
#' @param calories Optional numeric. Calorie override. If NULL, calculated from lifestyle inputs.
#' @param protein Optional numeric. Protein override in grams. If NULL, calculated from lifestyle inputs.
#' @param carbs Optional numeric. Carbohydrate override in grams. If NULL, calculated from lifestyle inputs.
#' @param fat Optional numeric. Fat override in grams. If NULL, calculated from lifestyle inputs.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "nutrition.db")
#'   add_user(con, "John", 80, 180, 30, "M", "maintenance", "sedentary", 3)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
add_user <- function(con, name, weight_kg, height_cm, age_years, gender,
                     goal, work_type, exercise_days,
                     calories = NULL, protein = NULL,
                     carbs = NULL, fat = NULL) {

  user_df <- data.frame(
    name          = name,
    weight_kg     = weight_kg,
    height_cm     = height_cm,
    age_years     = age_years,
    gender        = gender,
    goal          = goal,
    work_type     = work_type,
    exercise_days = as.integer(exercise_days),
    calories      = if (is.null(calories)) NA_real_ else calories,
    protein       = if (is.null(protein))  NA_real_ else protein,
    carbs         = if (is.null(carbs))    NA_real_ else carbs,
    fat           = if (is.null(fat))      NA_real_ else fat
  )

  DBI::dbAppendTable(con, "users", user_df)

  invisible(TRUE)
}

#' Update an existing user in the nutrition database
#'
#' Updates the lifestyle inputs and optional macro overrides for an
#' existing user in the users table.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param user_id Integer. The ID of the user to update.
#' @param name Character. The user's name.
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param height_cm Numeric. Height in centimetres.
#' @param age_years Numeric. Age in years.
#' @param gender Character. Either "M" or "F".
#' @param goal Character. Either "maintenance" or "recomposition".
#' @param work_type Character. One of "sedentary", "partially_active", or "active".
#' @param exercise_days Integer. Number of exercise days per week (0-7).
#' @param calories Optional numeric. Calorie override. If NULL, calculated from lifestyle inputs.
#' @param protein Optional numeric. Protein override in grams. If NULL, calculated from lifestyle inputs.
#' @param carbs Optional numeric. Carbohydrate override in grams. If NULL, calculated from lifestyle inputs.
#' @param fat Optional numeric. Fat override in grams. If NULL, calculated from lifestyle inputs.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "nutrition.db")
#'   update_user(con, user_id = 1, name = "John", weight_kg = 82,
#'               height_cm = 180, age_years = 31, gender = "M",
#'               goal = "recomposition", work_type = "sedentary",
#'               exercise_days = 4)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
update_user <- function(con, user_id, name, weight_kg, height_cm, age_years,
                        gender, goal, work_type, exercise_days,
                        calories = NULL, protein = NULL,
                        carbs = NULL, fat = NULL) {

  DBI::dbExecute(con, "
    UPDATE users SET
      name          = ?,
      weight_kg     = ?,
      height_cm     = ?,
      age_years     = ?,
      gender        = ?,
      goal          = ?,
      work_type     = ?,
      exercise_days = ?,
      calories      = ?,
      protein       = ?,
      carbs         = ?,
      fat           = ?
    WHERE user_id = ?
  ",
  params = list(
    name,
    weight_kg,
    height_cm,
    age_years,
    gender,
    goal,
    work_type,
    as.integer(exercise_days),
    if (is.null(calories)) NA_real_ else calories,
    if (is.null(protein))  NA_real_ else protein,
    if (is.null(carbs))    NA_real_ else carbs,
    if (is.null(fat))      NA_real_ else fat,
    as.integer(user_id)
  ))

  invisible(TRUE)
}
