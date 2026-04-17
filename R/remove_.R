#' Remove a user from the nutrition database
#'
#' Deletes a user from the users table.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param user_id Integer. The ID of the user to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_user <- function(con, user_id) {
  DBI::dbExecute(con, "DELETE FROM users WHERE user_id = ?",
                 params = list(as.integer(user_id)))
  invisible(TRUE)
}

#' Remove a meal from the nutrition database
#'
#' Deletes a meal and its associated rows from meal_ingredients and plan_meals.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param meal_id Integer. The ID of the meal to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_meal <- function(con, meal_id) {
  DBI::dbExecute(con, "DELETE FROM meal_ingredients WHERE meal_id = ?",
                 params = list(as.integer(meal_id)))
  DBI::dbExecute(con, "DELETE FROM plan_meals WHERE meal_id = ?",
                 params = list(as.integer(meal_id)))
  DBI::dbExecute(con, "DELETE FROM meals WHERE meal_id = ?",
                 params = list(as.integer(meal_id)))
  invisible(TRUE)
}

#' Remove an ingredient from the nutrition database
#'
#' Deletes an ingredient and its associated rows from meal_ingredients.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param ingredient_id Integer. The ID of the ingredient to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_ingredient <- function(con, ingredient_id) {
  DBI::dbExecute(con, "DELETE FROM meal_ingredients WHERE ingredient_id = ?",
                 params = list(as.integer(ingredient_id)))
  DBI::dbExecute(con, "DELETE FROM ingredients WHERE ingredient_id = ?",
                 params = list(as.integer(ingredient_id)))
  invisible(TRUE)
}

#' Remove a plan from the nutrition database
#'
#' Deletes a plan and its associated rows from plan_meals.
#'
#' @param con A DBIConnection object to the SQLite database.
#' @param plan_id Integer. The ID of the plan to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_plan <- function(con, plan_id) {
  DBI::dbExecute(con, "DELETE FROM plan_meals WHERE plan_id = ?",
                 params = list(as.integer(plan_id)))
  DBI::dbExecute(con, "DELETE FROM plans WHERE plan_id = ?",
                 params = list(as.integer(plan_id)))
  invisible(TRUE)
}

#' Remove unused ingredients from the nutrition database
#'
#' Deletes any ingredients not referenced in meal_ingredients.
#'
#' @param con A DBIConnection object to the SQLite database.
#'
#' @return Numeric. Number of ingredients removed.
#' @export
clean_ingredients <- function(con) {
  result <- DBI::dbExecute(con, "
    DELETE FROM ingredients
    WHERE ingredient_id NOT IN (
      SELECT DISTINCT ingredient_id FROM meal_ingredients
    )
  ")
  return(result)
}

#' Remove unused meals from the nutrition database
#'
#' Deletes any meals not referenced in plan_meals. Also cleans
#' associated meal_ingredients rows.
#'
#' @param con A DBIConnection object to the SQLite database.
#'
#' @return Numeric. Number of meals removed.
#' @export
clean_meals <- function(con) {
  DBI::dbExecute(con, "
    DELETE FROM meal_ingredients
    WHERE meal_id NOT IN (
      SELECT DISTINCT meal_id FROM plan_meals
    )
  ")
  result <- DBI::dbExecute(con, "
    DELETE FROM meals
    WHERE meal_id NOT IN (
      SELECT DISTINCT meal_id FROM plan_meals
    )
  ")
  return(result)
}
