#' Calculate daily calorie target
#'
#' Estimates total daily energy expenditure (TDEE) using the Mifflin-St Jeor
#' equation and adjusts for goal.
#'
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param height_cm Numeric. Height in centimetres.
#' @param age_years Numeric. Age in years.
#' @param gender Character. Either "M" or "F".
#' @param goal Character. Either "maintenance" or "recomposition".
#' @param work_type Character. One of "sedentary", "partially_active", or "active".
#' @param exercise_days Integer. Number of exercise days per week (0-7).
#'
#' @return Numeric. Daily calorie target in kcal.
#'
#' @examples
#' \dontrun{
#'   calculate_calorie_target(80, 180, 30, "M", "maintenance", "sedentary", 3)
#' }
#'
#' @export
calculate_calorie_target <- function(weight_kg, height_cm, age_years, gender,
                                     goal, work_type, exercise_days) {

 # Determine activity multiplier from work type and exercise days
  activity_multiplier <- dplyr::case_when(
    work_type == "sedentary"        & exercise_days <= 1 ~ 1.200,
    work_type == "sedentary"        & exercise_days <= 3 ~ 1.375,
    work_type == "sedentary"        & exercise_days <= 5 ~ 1.550,
    work_type == "sedentary"        & exercise_days <= 7 ~ 1.725,
    work_type == "partially_active" & exercise_days <= 1 ~ 1.375,
    work_type == "partially_active" & exercise_days <= 3 ~ 1.550,
    work_type == "partially_active" & exercise_days <= 5 ~ 1.725,
    work_type == "partially_active" & exercise_days <= 7 ~ 1.900,
    work_type == "active"           & exercise_days <= 1 ~ 1.550,
    work_type == "active"           & exercise_days <= 3 ~ 1.725,
    work_type == "active"           & exercise_days <= 5 ~ 1.900,
    work_type == "active"           & exercise_days <= 7 ~ 1.900
  )

  # Calculate BMR using Mifflin-St Jeor equation
  bmr <- if (gender == "M") {
    (10 * weight_kg) + (6.25 * height_cm) - (5 * age_years) + 5
  } else {
    (10 * weight_kg) + (6.25 * height_cm) - (5 * age_years) - 161
  }

  # Calculate TDEE
  tdee <- bmr * activity_multiplier

  # Adjust for goal
  calories <- dplyr::case_when(
    goal == "maintenance"   ~ tdee,
    goal == "recomposition" ~ tdee - 200
  )

  return(round(calories))
}


#' Calculate daily protein target
#'
#' Estimates daily protein target based on body weight and goal.
#'
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param goal Character. Either "maintenance" or "recomposition".
#'
#' @return Numeric. Daily protein target in grams.
#'
#' @examples
#' \dontrun{
#'   calculate_protein_target(80, "recomposition")
#' }
#'
#' @export
calculate_protein_target <- function(weight_kg, goal) {

  protein <- dplyr::case_when(
    goal == "maintenance"   ~ weight_kg * 1.6,
    goal == "recomposition" ~ weight_kg * 2.0
  )

  return(round(protein))
}

#' Calculate daily fat target
#'
#' Estimates daily fat target based on body weight and goal.
#'
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param goal Character. Either "maintenance" or "recomposition".
#'
#' @return Numeric. Daily fat target in grams.
#'
#' @examples
#' \dontrun{
#'   calculate_fat_target(80, "recomposition")
#' }
#'
#' @export
calculate_fat_target <- function(weight_kg, goal) {

  fat <- dplyr::case_when(
    goal == "maintenance"   ~ weight_kg * 0.8,
    goal == "recomposition" ~ weight_kg * 1.0
  )

  return(round(fat))
}

#' Calculate daily carbohydrate target
#'
#' Estimates daily carbohydrate target as remaining calories after
#' protein and fat are accounted for.
#'
#' @param calories Numeric. Daily calorie target in kcal.
#' @param protein Numeric. Daily protein target in grams.
#' @param fat Numeric. Daily fat target in grams.
#'
#' @return Numeric. Daily carbohydrate target in grams.
#'
#' @examples
#' \dontrun{
#'   calculate_carb_target(2448, 128, 64)
#' }
#'
#' @export
calculate_carb_target <- function(calories, protein, fat) {

  carbs <- (calories - (protein * 4) - (fat * 9)) / 4

  return(round(carbs))
}

#' Calculate all daily macro targets
#'
#' Wrapper function that calculates daily calorie, protein, fat and
#' carbohydrate targets based on lifestyle inputs.
#'
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param height_cm Numeric. Height in centimetres.
#' @param age_years Numeric. Age in years.
#' @param gender Character. Either "M" or "F".
#' @param goal Character. Either "maintenance" or "recomposition".
#' @param work_type Character. One of "sedentary", "partially_active", or "active".
#' @param exercise_days Integer. Number of exercise days per week (0-7).
#'
#' @return A data frame with columns: calories, protein, fat, carbs.
#'
#' @examples
#' \dontrun{
#'   calculate_macro_targets(80, 180, 30, "M", "maintenance", "sedentary", 3)
#' }
#'
#' @export
calculate_macro_targets <- function(weight_kg, height_cm, age_years, gender,
                                    goal, work_type, exercise_days) {

  calories <- calculate_calorie_target(weight_kg, height_cm, age_years,
                                       gender, goal, work_type, exercise_days)
  protein  <- calculate_protein_target(weight_kg, goal)
  fat      <- calculate_fat_target(weight_kg, goal)
  carbs    <- calculate_carb_target(calories, protein, fat)

  data.frame(
    calories = calories,
    protein  = protein,
    carbs    = carbs,
    fat      = fat
  )
}

#' Get macro targets via interactive prompts
#'
#' Calculates daily macro targets from lifestyle inputs. Any arguments
#' not provided will be requested interactively via prompts.
#'
#' @param weight_kg Numeric. Body weight in kilograms.
#' @param height_cm Numeric. Height in centimetres.
#' @param age_years Numeric. Age in years.
#' @param gender Character. Either "M" or "F".
#' @param goal Character. Either "maintenance" or "recomposition".
#' @param work_type Character. One of "sedentary", "partially_active", or "active".
#' @param exercise_days Integer. Number of exercise days per week (0-7).
#'
#' @return A data frame with columns: calories, protein, fat, carbs.
#'
#' @examples
#' \dontrun{
#'   get_macro_targets_via_prompt(weight_kg = 80, height_cm = 180)
#' }
#'
#' @export
get_macro_targets_via_prompt <- function(weight_kg = NULL, height_cm = NULL,
                                         age_years = NULL, gender = NULL,
                                         goal = NULL, work_type = NULL,
                                         exercise_days = NULL) {

  if (is.null(weight_kg))    weight_kg    <- as.numeric(readline("Enter your weight (kg): "))
  if (is.null(height_cm))    height_cm    <- as.numeric(readline("Enter your height (cm): "))
  if (is.null(age_years))    age_years    <- as.numeric(readline("Enter your age (years): "))
  if (is.null(gender))       gender       <- readline("Enter your gender (M/F): ")
  if (is.null(goal))         goal         <- readline("Enter your goal (maintenance/recomposition): ")
  if (is.null(work_type))    work_type    <- readline("Enter your work type (sedentary/partially_active/active): ")
  if (is.null(exercise_days)) exercise_days <- as.integer(readline("Enter your exercise days per week (0-7): "))

  calculate_macro_targets(weight_kg, height_cm, age_years, gender,
                          goal, work_type, exercise_days)
}

#' Get macro targets for a user
#'
#' Retrieves a user's macro targets from the database. If macro overrides
#' are present they are returned directly, otherwise targets are calculated
#' from the user's lifestyle inputs.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param user_id Integer. The ID of the user to retrieve targets for.
#'
#' @return A data frame with columns: calories, protein, carbs, fat.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   get_user_targets(con, user_id = 1)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
get_user_targets <- function(con, user_id) {

  user <- DBI::dbGetQuery(con, "SELECT * FROM users WHERE user_id = $1",
                          params = list(as.integer(user_id)))

  # If macro overrides are present, return them directly
  if (!is.na(user$calories) && !is.na(user$protein) &&
      !is.na(user$carbs)    && !is.na(user$fat)) {
    return(data.frame(
      calories = user$calories,
      protein  = user$protein,
      carbs    = user$carbs,
      fat      = user$fat
    ))
  }

  # Otherwise calculate from lifestyle inputs
  calculate_macro_targets(
    weight_kg     = user$weight_kg,
    height_cm     = user$height_cm,
    age_years     = user$age_years,
    gender        = user$gender,
    goal          = user$goal,
    work_type     = user$work_type,
    exercise_days = user$exercise_days
  )
}