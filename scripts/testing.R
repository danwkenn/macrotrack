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
  name          = "John",
  weight_kg     = 80,
  height_cm     = 180,
  age_years     = 30,
  gender        = "M",
  goal          = "maintenance",
  work_type     = "sedentary",
  exercise_days = 3
)

DBI::dbGetQuery(con, "SELECT * FROM ingredients")
DBI::dbGetQuery(con, "SELECT * FROM meals")
DBI::dbGetQuery(con, "SELECT * FROM meal_ingredients")

get_meal_macros(con, meal_id = 5, servings = 3)

DBI::dbGetQuery(con, "SELECT * FROM users")
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

