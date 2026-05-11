#' Initialise the nutrition database
#'
#' Creates the required tables in a Postgres database if they do not
#' already exist. The caller is responsible for opening and closing
#' the connection.
#'
#' Tables created: \code{ingredients}, \code{meals}, \code{meal_ingredients},
#' \code{users}, \code{plans}, \code{plan_meals}, \code{measurement_data_type},
#' \code{measurement_type}, \code{measurements}, \code{measurement_context}.
#'
#' The \code{measurement_data_type} table is seeded from
#' \code{data/measurement_data_types.csv}. The seed insert is idempotent
#' (ON CONFLICT DO NOTHING), so existing rows are left untouched.
#'
#' @param con A DBIConnection object to a Postgres database.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     host     = Sys.getenv("NUTRITION_DB_HOST"),
#'     dbname   = Sys.getenv("NUTRITION_DB_NAME"),
#'     user     = Sys.getenv("NUTRITION_DB_USER"),
#'     password = Sys.getenv("NUTRITION_DB_PASSWORD"),
#'     port     = 5432,
#'     sslmode  = "require"
#'   )
#'   initialise_db(con)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
initialise_db <- function(con) {

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingredients (
      ingredient_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      owner_id      INTEGER,
      name          TEXT NOT NULL,
      calories      REAL NOT NULL,
      protein       REAL NOT NULL,
      carbs         REAL NOT NULL,
      fat           REAL NOT NULL,
      portion_size  REAL NOT NULL,
      portion_name  TEXT NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meals (
      meal_id               INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      owner_id              INTEGER,
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
      meal_ingredient_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      meal_id            INTEGER NOT NULL
                         REFERENCES meals(meal_id) ON DELETE CASCADE,
      ingredient_id      INTEGER NOT NULL
                         REFERENCES ingredients(ingredient_id) ON DELETE CASCADE,
      quantity           REAL NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id       INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      owner_id      INTEGER,
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

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plans (
      plan_id     INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      owner_id    INTEGER,
      name        TEXT NOT NULL,
      description TEXT,
      pct_to_plan REAL NOT NULL DEFAULT 1.0
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plan_meals (
      plan_meal_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      plan_id      INTEGER NOT NULL
                   REFERENCES plans(plan_id) ON DELETE CASCADE,
      slot_name    TEXT,
      meal_id      INTEGER NOT NULL
                   REFERENCES meals(meal_id) ON DELETE CASCADE,
      servings     REAL NOT NULL DEFAULT 1.0
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS measurement_data_type (
      data_type_id    TEXT PRIMARY KEY,
      label           TEXT NOT NULL,
      description     TEXT,
      base_type       TEXT NOT NULL,
      possible_values JSONB,
      ordered         BOOLEAN
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS measurement_type (
      measurement_type_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      name                TEXT NOT NULL UNIQUE,
      label               TEXT NOT NULL,
      description         TEXT,
      prompt              TEXT NOT NULL,
      method              TEXT,
      unit                TEXT,
      precision           INTEGER,
      min_value           REAL,
      max_value           REAL,
      data_type_id        TEXT NOT NULL
                          REFERENCES measurement_data_type(data_type_id),
      active              BOOLEAN NOT NULL DEFAULT TRUE
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS measurements (
      measurement_id      INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      user_id             INTEGER
                          REFERENCES users(user_id) ON DELETE CASCADE,
      measurement_type_id INTEGER NOT NULL
                          REFERENCES measurement_type(measurement_type_id) ON DELETE CASCADE,
      datetime            TIMESTAMPTZ NOT NULL,
      value               TEXT NOT NULL,
      notes               TEXT,
      created_at          TIMESTAMPTZ NOT NULL DEFAULT now()
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS measurement_context (
      measurement_context_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      measurement_id         INTEGER NOT NULL
                             REFERENCES measurements(measurement_id) ON DELETE CASCADE,
      context_type           TEXT NOT NULL,
      value                  TEXT NOT NULL,
      UNIQUE (measurement_id, context_type)
    )
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS measurement_context_type_idx
      ON measurement_context (context_type)
  ")

  seed_path <- "data/measurement_data_types.csv"
  if (!file.exists(seed_path)) {
    stop("Seed file not found: ", seed_path,
         ". Expected to be readable relative to the working directory.",
         call. = FALSE)
  }
  seed <- read.csv(seed_path,
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE)

  DBI::dbExecute(
    con,
    "INSERT INTO measurement_data_type
       (data_type_id, label, description, base_type, possible_values, ordered)
     VALUES ($1, $2, $3, $4, $5::jsonb, $6)
     ON CONFLICT (data_type_id) DO NOTHING",
    params = list(
      as.character(seed$data_type_id),
      as.character(seed$label),
      as.character(seed$description),
      as.character(seed$base_type),
      as.character(seed$possible_values),
      as.logical(seed$ordered)
    )
  )

  invisible(TRUE)
}

#' Drop all tables in the nutrition database
#'
#' Destroys all tables defined by \code{initialise_db()}, including any
#' data they contain. Uses DROP TABLE ... CASCADE so foreign-key
#' dependencies between tables are resolved automatically. Safe to call
#' on an already-empty database (tables that don't exist are skipped).
#'
#' Destructive. Must be called with \code{confirm = TRUE} to proceed.
#'
#' @param con A DBIConnection object to the Postgres database.
#' @param confirm Logical. Must be \code{TRUE} to proceed with the drop.
#'   Guards against accidental destruction of data.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RPostgres::Postgres(), ...)
#'   drop_all_tables(con, confirm = TRUE)
#'   initialise_db(con)
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
drop_all_tables <- function(con, confirm = FALSE) {
  if (!isTRUE(confirm)) {
    stop("drop_all_tables() is destructive. Pass confirm = TRUE to proceed.",
         call. = FALSE)
  }
  DBI::dbExecute(con, "
    DROP TABLE IF EXISTS
      measurement_context,
      measurements,
      measurement_type,
      measurement_data_type,
      meal_ingredients,
      plan_meals,
      meals,
      plans,
      ingredients,
      users
    CASCADE
  ")
  invisible(TRUE)
}