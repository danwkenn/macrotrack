#' Initialise the nutrition database
#'
#' Creates a SQLite database file at the specified path and initialises
#' the required tables if they do not already exist.
#'
#' @param db_path Character. Path to the SQLite database file. If the file
#'   does not exist it will be created automatically.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   initialise_db(db_path = "nutrition.db")
#' }
#'
#' @export
initialise_db <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingredients (
      ingredient_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      name             TEXT NOT NULL,
      calories         REAL NOT NULL,
      protein          REAL NOT NULL,
      carbs            REAL NOT NULL,
      fat              REAL NOT NULL,
      portion_size     REAL NOT NULL,
      portion_name     TEXT NOT NULL
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meals (
      meal_id               INTEGER PRIMARY KEY AUTOINCREMENT,
      name                  TEXT NOT NULL,
      use_ingredient_macros INTEGER NOT NULL DEFAULT 1,
      calories              REAL,
      protein               REAL,
      carbs                 REAL,
      fat                   REAL
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meal_ingredients (
      meal_ingredient_id INTEGER PRIMARY KEY AUTOINCREMENT,
      meal_id            INTEGER NOT NULL,
      ingredient_id      INTEGER NOT NULL,
      quantity           REAL NOT NULL,
      FOREIGN KEY (meal_id)       REFERENCES meals(meal_id),
      FOREIGN KEY (ingredient_id) REFERENCES ingredients(ingredient_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name          TEXT NOT NULL,
      weight_kg     REAL NOT NULL,
      height_cm     REAL NOT NULL,
      age_years     REAL NOT NULL,
      gender        TEXT NOT NULL,
      goal          TEXT NOT NULL,
      work_type     TEXT NOT NULL,
      exercise_days INTEGER NOT NULL,
      calories      REAL,
      protein       REAL,
      carbs         REAL,
      fat           REAL
    )
  ")
}