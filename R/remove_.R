#' Remove a user from the nutrition database
#'
#' Deletes a user from the users table.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param user_id Integer. The ID of the user to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_user <- function(con, user_id) {
  DBI::dbExecute(con, "DELETE FROM users WHERE user_id = $1",
                 params = list(as.integer(user_id)))
  invisible(TRUE)
}

#' Remove a meal from the nutrition database
#'
#' Deletes a meal. Associated rows in meal_ingredients and plan_meals
#' are removed automatically by ON DELETE CASCADE.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param meal_id Integer. The ID of the meal to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_meal <- function(con, meal_id) {
  DBI::dbExecute(con, "DELETE FROM meals WHERE meal_id = $1",
                 params = list(as.integer(meal_id)))
  invisible(TRUE)
}

#' Remove an ingredient from the nutrition database
#'
#' Deletes an ingredient. Associated rows in meal_ingredients are
#' removed automatically by ON DELETE CASCADE.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param ingredient_id Integer. The ID of the ingredient to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_ingredient <- function(con, ingredient_id) {
  DBI::dbExecute(con, "DELETE FROM ingredients WHERE ingredient_id = $1",
                 params = list(as.integer(ingredient_id)))
  invisible(TRUE)
}

#' Remove a plan from the nutrition database
#'
#' Deletes a plan. Associated rows in plan_meals are removed
#' automatically by ON DELETE CASCADE.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param plan_id Integer. The ID of the plan to remove.
#'
#' @return No return value, called for side effects.
#' @export
remove_plan <- function(con, plan_id) {
  DBI::dbExecute(con, "DELETE FROM plans WHERE plan_id = $1",
                 params = list(as.integer(plan_id)))
  invisible(TRUE)
}

#' Remove unused ingredients from the nutrition database
#'
#' Deletes any ingredients not referenced in meal_ingredients.
#'
#' @param con A DBIConnection object to the Postgres database.
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
#' Deletes any meals not referenced in plan_meals. Associated rows
#' in meal_ingredients are removed automatically by ON DELETE CASCADE.
#'
#' @param con A DBIConnection object to the Postgres database.
#'
#' @return Numeric. Number of meals removed.
#' @export
clean_meals <- function(con) {
  result <- DBI::dbExecute(con, "
    DELETE FROM meals
    WHERE meal_id NOT IN (
      SELECT DISTINCT meal_id FROM plan_meals
    )
  ")
  return(result)
}