#' Add a user to the nutrition database
#'
#' Inserts a new user into the users table with lifestyle inputs and
#' optional macro overrides. If macro overrides are not provided, targets
#' will be calculated from lifestyle inputs when retrieved. The user_id
#' is assigned by the database.
#'
#' @param con A DBIConnection object to the Postgres database.
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
#' @return Integer. The user_id assigned by the database, returned invisibly.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   add_user(con, "John", 80, 180, 30, "M", "maintenance", "sedentary", 3)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
add_user <- function(con, name, weight_kg, height_cm, age_years, gender,
                     goal, work_type, exercise_days,
                     calories = NULL, protein = NULL,
                     carbs = NULL, fat = NULL) {

  result <- DBI::dbGetQuery(
    con,
    "INSERT INTO users
       (name, weight_kg, height_cm, age_years, gender, goal,
        work_type, exercise_days, calories, protein, carbs, fat)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
     RETURNING user_id",
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
      if (is.null(fat))      NA_real_ else fat
    )
  )

  invisible(result$user_id)
}

#' Update an existing user in the nutrition database
#'
#' Updates the lifestyle inputs and optional macro overrides for an
#' existing user in the users table.
#'
#' @param con A DBIConnection object to the Postgres database.
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
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
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
      name          = $1,
      weight_kg     = $2,
      height_cm     = $3,
      age_years     = $4,
      gender        = $5,
      goal          = $6,
      work_type     = $7,
      exercise_days = $8,
      calories      = $9,
      protein       = $10,
      carbs         = $11,
      fat           = $12
    WHERE user_id = $13
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