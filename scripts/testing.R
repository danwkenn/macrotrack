# Load the updated package functions
lapply(list.files("R", full.names = TRUE), FUN = source)

# Open a connection to Neon
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("NUTRITION_DB_HOST"),
  dbname   = Sys.getenv("NUTRITION_DB_NAME"),
  user     = Sys.getenv("NUTRITION_DB_USER"),
  password = Sys.getenv("NUTRITION_DB_PASSWORD"),
  port     = 5432,
  sslmode  = "require"
)

DBI::dbGetQuery(con, "SELECT 1 AS ok")

# Initialise the schema
initialise_db(con)

# Verify the tables were created
DBI::dbListTables(con)
# Should return: "ingredients", "meal_ingredients", "meals", "plan_meals", "plans", "users"

# Optional: inspect one of the tables' columns
DBI::dbGetQuery(con, "
  SELECT column_name, data_type, is_nullable
  FROM information_schema.columns
  WHERE table_name = 'ingredients'
  ORDER BY ordinal_position
")

DBI::dbDisconnect(con)





lapply(list.files("R", full.names = TRUE), FUN = source)
initialise_db("test.db")




meal <- parse_meal_csv("data/felafel_wrap.csv")

con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")

import_meal(con, meal_name = "Falafel Wrap", csv_path = "data/felafel_wrap.csv")
import_meal(con, meal_name = "Wholegrain Crackers with Cheese and Apple", csv_path = "data/crackers_cheese_apple.csv")
import_meal(con, meal_name = "Banana with Peanut Butter and Milk",        csv_path = "data/banana_peanut_butter_milk.csv")
import_meal(con, meal_name = "High Fibre Cereal with Milk",                csv_path = "data/cereal_milk.csv")
import_meal(con, meal_name = "Frozen Yogurt Bark",                         csv_path = "data/frozen_yogurt_bark.csv")

add_user(
  con           = con,
  name          = "Daniel",
  weight_kg     = 83,
  height_cm     = 182,
  age_years     = 34,
  gender        = "M",
  goal          = "recomponsition",
  work_type     = "sedentary",
  exercise_days = 6
)

DBI::dbGetQuery(con, "SELECT * FROM ingredients")
DBI::dbGetQuery(con, "SELECT * FROM meals")
DBI::dbGetQuery(con, "SELECT * FROM meal_ingredients")
DBI::dbGetQuery(con, "SELECT * FROM users")

get_meal_macros(con, meal_id = 5, servings = 3)

DBI::dbGetQuery(con, "SELECT * FROM plans")
get_user_targets(con, user_id = 1)

unlink("test.db")

calculate_calorie_target(
  weight_kg    = 80,
  height_cm    = 180,
  age_years    = 30,
  gender       = "M",
  goal         = "maintenance",
  work_type    = "sedentary",
  exercise_days = 3
)

calculate_macro_targets(80, 180, 30, "M", "recomposition", "sedentary", 3)

DBI::dbDisconnect(con)
