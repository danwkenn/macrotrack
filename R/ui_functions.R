#' Meal Planner Tab UI
#'
#' Returns the UI for the meal planner tab, including meal slots
#' and daily summary and targets section.
#'
#' @return A shiny tabPanel object.
#'
#' @export
meal_planner_ui <- function() {
  tabPanel("Meal Planner",
    br(),
    uiOutput("meal_planner"),
    hr(),
    uiOutput("daily_summary"),hr(),
    downloadButton("download_report", "Download Report", class = "btn-info")
  )
}

#' Import Meal Tab UI
#'
#' Returns the UI for the import meal tab, allowing the user to
#' select a CSV file and import a meal into the database.
#'
#' @return A shiny tabPanel object.
#'
#' @export
#' Import Meal Tab UI
#'
#' Returns the UI for the import meal tab, allowing the user to
#' select a CSV file and import a meal into the database.
#'
#' @return A shiny tabPanel object.
#'
#' @export
import_meal_ui <- function() {
  tabPanel("Import Meal",
    br(),
    fluidRow(
      column(4, textInput("meal_name", "Meal Name"))
    ),
    fluidRow(
      column(4,
        fileInput("meal_csv", "Select CSV File",
                  accept      = ".csv",
                  buttonLabel = "Browse...",
                  placeholder = "No file selected")
      )
    ),
    fluidRow(
      column(4,
        checkboxInput("use_ingredient_macros",
                      "Calculate macros from ingredients", value = TRUE)
      )
    ),
    conditionalPanel(
      condition = "input.use_ingredient_macros == false",
      fluidRow(
        column(3, numericInput("meal_calories", "Calories",    value = NULL)),
        column(3, numericInput("meal_protein",  "Protein (g)", value = NULL)),
        column(3, numericInput("meal_carbs",    "Carbs (g)",   value = NULL)),
        column(3, numericInput("meal_fat",      "Fat (g)",     value = NULL))
      )
    ),
    br(),
    actionButton("import_meal_btn", "Import Meal", class = "btn-primary"),
    br(), br(),
    textOutput("import_meal_status")
  )
}

#' Add User Tab UI
#'
#' Returns the UI for the add user tab, allowing the user to
#' enter lifestyle inputs and optional macro overrides.
#'
#' @return A shiny tabPanel object.
#'
#' @export
add_user_ui <- function() {
  tabPanel("Add User",
    br(),
    fluidRow(
      column(4, textInput("user_name",   "Name")),
      column(4, numericInput("user_weight", "Weight (kg)", value = NULL)),
      column(4, numericInput("user_height", "Height (cm)", value = NULL))
    ),
    fluidRow(
      column(4, numericInput("user_age", "Age (years)", value = NULL)),
      column(4, selectInput("user_gender", "Gender", choices = c("M", "F"))),
      column(4, selectInput("user_goal", "Goal",
                            choices = c("maintenance", "recomposition")))
    ),
    fluidRow(
      column(4, selectInput("user_work_type", "Work Type",
                            choices = c("sedentary", "partially_active", "active"))),
      column(4, numericInput("user_exercise_days", "Exercise Days per Week (0-7)",
                             value = 0, min = 0, max = 7, step = 1))
    ),
    hr(),
    h4("Macro Overrides (optional)"),
    fluidRow(
      column(3, numericInput("user_calories", "Calories",    value = NULL)),
      column(3, numericInput("user_protein",  "Protein (g)", value = NULL)),
      column(3, numericInput("user_carbs",    "Carbs (g)",   value = NULL)),
      column(3, numericInput("user_fat",      "Fat (g)",     value = NULL))
    ),
    br(),
    actionButton("add_user_btn", "Add User", class = "btn-primary"),
    br(), br(),
    textOutput("add_user_status")
  )
}

#' Modify User Tab UI
#'
#' Returns the UI for the modify user tab, allowing the user to
#' select an existing user and update their details.
#'
#' @return A shiny tabPanel object.
#'
#' @export
modify_user_ui <- function() {
  tabPanel("Modify User",
    br(),
    uiOutput("modify_user_ui"),
    br(),
    fluidRow(
      column(4, textInput("modify_name",      "Name")),
      column(4, numericInput("modify_weight", "Weight (kg)", value = NULL)),
      column(4, numericInput("modify_height", "Height (cm)", value = NULL))
    ),
    fluidRow(
      column(4, numericInput("modify_age",   "Age (years)", value = NULL)),
      column(4, selectInput("modify_gender", "Gender",
                            choices = c("M", "F"))),
      column(4, selectInput("modify_goal",   "Goal",
                            choices = c("maintenance", "recomposition")))
    ),
    fluidRow(
      column(4, selectInput("modify_work_type", "Work Type",
                            choices = c("sedentary", "partially_active", "active"))),
      column(4, numericInput("modify_exercise_days", "Exercise Days per Week (0-7)",
                             value = 0, min = 0, max = 7, step = 1))
    ),
    hr(),
    h4("Macro Overrides (optional)"),
    fluidRow(
      column(3, numericInput("modify_calories", "Calories",    value = NULL)),
      column(3, numericInput("modify_protein",  "Protein (g)", value = NULL)),
      column(3, numericInput("modify_carbs",    "Carbs (g)",   value = NULL)),
      column(3, numericInput("modify_fat",      "Fat (g)",     value = NULL))
    ),
    br(),
    actionButton("save_user_btn", "Save Changes", class = "btn-primary"),
    br(), br(),
    textOutput("modify_user_status")
  )
}

#' Add Ingredients Tab UI
#'
#' Returns the UI for the add ingredients tab, allowing the user to
#' enter a single ingredient and its nutritional values.
#'
#' @return A shiny tabPanel object.
#'
#' @export
add_ingredients_ui <- function() {
  tabPanel("Add Ingredients",
    br(),
    fluidRow(
      column(4, textInput("ingredient_name",       "Ingredient Name")),
      column(4, numericInput("ingredient_calories","Calories (per 100g)",  value = NULL)),
      column(4, numericInput("ingredient_protein", "Protein (per 100g)",   value = NULL))
    ),
    fluidRow(
      column(4, numericInput("ingredient_carbs",   "Carbs (per 100g)",     value = NULL)),
      column(4, numericInput("ingredient_fat",     "Fat (per 100g)",       value = NULL))
    ),
    hr(),
    h4("Portion"),
    fluidRow(
      column(4, numericInput("ingredient_portion_size", "Portion Size (g)", value = NULL)),
      column(4, textInput("ingredient_portion_name",    "Portion Name (e.g. egg, slice)"))
    ),
    br(),
    actionButton("add_ingredient_btn", "Add Ingredient", class = "btn-primary"),
    br(), br(),
    textOutput("add_ingredient_status")
  )
}

#' Build Meal Tab UI
#'
#' Returns the UI for the build meal tab, allowing the user to
#' construct a meal from ingredients stored in the database using
#' 10 fixed ingredient slots with live macro calculation.
#'
#' @return A shiny tabPanel object.
#'
#' @export
build_meal_ui <- function() {
  tabPanel("Build Meal",
    br(),
    fluidRow(
      column(4, textInput("build_meal_name", "Meal Name"))
    ),
    hr(),
    uiOutput("build_meal_slots"),
    hr(),
    fluidRow(
      column(4,
        checkboxInput("build_use_ingredient_macros",
                      "Calculate macros from ingredients", value = TRUE)
      )
    ),
    conditionalPanel(
      condition = "input.build_use_ingredient_macros == false",
      fluidRow(
        column(3, numericInput("build_calories", "Calories",    value = NULL)),
        column(3, numericInput("build_protein",  "Protein (g)", value = NULL)),
        column(3, numericInput("build_carbs",    "Carbs (g)",   value = NULL)),
        column(3, numericInput("build_fat",      "Fat (g)",     value = NULL))
      )
    ),
    hr(),
    uiOutput("build_meal_summary"),
    br(),
    actionButton("save_meal_btn", "Save Meal", class = "btn-primary"),
    br(), br(),
    textOutput("build_meal_status")
  )
}

#' Add Ingredient by Serving Tab UI
#'
#' Returns the UI for the add ingredient by serving tab, allowing the
#' user to input nutrients per serving and a portion factor to convert
#' to per-100g values for storage.
#'
#' @return A shiny tabPanel object.
#'
#' @export
add_ingredient_by_serving_ui <- function() {
  tabPanel("Add Ingredient (by serving)",
    br(),
    wellPanel(
      p(strong("How this works:")),
      p("Enter the serving size and macros as shown on the packet, then set the",
        strong("portion factor"), "to define what one portion of this ingredient is."),
      p("For example: a packet shows a serving of 3 falafel balls weighing 75g total.",
        "Enter 75g as the serving size and the macros from the packet.",
        "Then set the portion factor to", strong("0.333"), "— this tells the app that",
        "one portion is one falafel ball (one third of the packet serving).",
        "The app will convert everything to per-100g values automatically.")
    ),
    fluidRow(
      column(4, textInput("serving_name", "Ingredient Name")),
      column(4, textInput("serving_portion_name", "Portion Name (e.g. falafel ball, slice)"))
    ),
    hr(),
    h4("Packet Serving"),
    fluidRow(
      column(3, numericInput("serving_size_g",   "Serving Size (g)",      value = NULL)),
      column(3, numericInput("serving_calories", "Calories per serving",  value = NULL)),
      column(3, numericInput("serving_protein",  "Protein per serving",   value = NULL))
    ),
    fluidRow(
      column(3, numericInput("serving_carbs",    "Carbs per serving",     value = NULL)),
      column(3, numericInput("serving_fat",      "Fat per serving",       value = NULL))
    ),
    hr(),
    h4("Portion Factor"),
    fluidRow(
      column(3,
        numericInput("portion_factor", "Portion factor",
                     value = 1, min = 0.01, step = 0.01)
      ),
      column(5,
        br(),
        textOutput("portion_preview")
      )
    ),
    br(),
    actionButton("add_ingredient_serving_btn", "Add Ingredient", class = "btn-primary"),
    br(), br(),
    textOutput("add_ingredient_serving_status")
  )
}

#' Build Plan Tab UI
#'
#' Returns the UI for the build plan tab, allowing the user to
#' construct a meal plan from meals stored in the database.
#'
#' @return A shiny tabPanel object.
#'
#' @export
build_plan_ui <- function() {
  tabPanel("Build Plan",
    br(),
    fluidRow(
      column(4, textInput("plan_name", "Plan Name")),
      column(4, textInput("plan_description", "Description (optional)"))
    ),
    fluidRow(
      column(3,
        numericInput("plan_pct_to_plan", "% of targets to plan for",
                     value = 100, min = 1, max = 100, step = 1)
      )
    ),
    hr(),
    h4("Add Meals"),
    fluidRow(
      column(4, uiOutput("build_plan_meal_select")),
      column(2, textInput("plan_slot_name", "Slot Name (e.g. Breakfast)")),
      column(2, numericInput("plan_meal_servings", "Servings",
                             value = 1, min = 0.5, step = 0.5)),
      column(2,
        br(),
        actionButton("add_to_plan_btn", "Add to Plan", class = "btn-success")
      )
    ),
    br(),
    tableOutput("build_plan_table"),
    fluidRow(
      column(2,
        actionButton("clear_plan_btn", "Clear", class = "btn-warning")
      )
    ),
    hr(),
    tableOutput("build_plan_summary"),
    br(),
    actionButton("save_plan_btn", "Save Plan", class = "btn-primary"),
    br(), br(),
    textOutput("build_plan_status")
  )
}

#' Remove Items Tab UI
#'
#' Returns the UI for the remove items tab, allowing the user to
#' remove users, meals, ingredients, and plans, as well as clean
#' unused items from the database.
#'
#' @return A shiny tabPanel object.
#' @export
remove_items_ui <- function() {
  tabPanel("Remove Items",
    br(),

    # --- Remove User ---
    h4("Remove User"),
    fluidRow(
      column(4, uiOutput("remove_user_select")),
      column(2,
        br(),
        actionButton("remove_user_btn", "Remove User", class = "btn-danger")
      )
    ),
    textOutput("remove_user_status"),

    hr(),

    # --- Remove Meal ---
    h4("Remove Meal"),
    fluidRow(
      column(4, uiOutput("remove_meal_select")),
      column(2,
        br(),
        actionButton("remove_meal_btn", "Remove Meal", class = "btn-danger")
      )
    ),
    textOutput("remove_meal_status"),

    hr(),

    # --- Remove Ingredient ---
    h4("Remove Ingredient"),
    fluidRow(
      column(4, uiOutput("remove_ingredient_select")),
      column(2,
        br(),
        actionButton("remove_ingredient_btn", "Remove Ingredient", class = "btn-danger")
      )
    ),
    textOutput("remove_ingredient_status"),

    hr(),

    # --- Remove Plan ---
    h4("Remove Plan"),
    fluidRow(
      column(4, uiOutput("remove_plan_select")),
      column(2,
        br(),
        actionButton("remove_plan_btn", "Remove Plan", class = "btn-danger")
      )
    ),
    textOutput("remove_plan_status"),

    hr(),

    # --- Clean Ingredients ---
    h4("Clean Ingredients"),
    p("Removes any ingredients not associated with a meal."),
    actionButton("clean_ingredients_btn", "Clean Ingredients", class = "btn-warning"),
    textOutput("clean_ingredients_status"),

    hr(),

    # --- Clean Meals ---
    h4("Clean Meals"),
    p("Removes any meals not associated with a plan."),
    actionButton("clean_meals_btn", "Clean Meals", class = "btn-warning"),
    textOutput("clean_meals_status"),

    br()
  )
}

#' Settings Tab UI
#'
#' Returns the UI for the settings tab.
#'
#' @return A shiny tabPanel object.
#' @export
settings_ui <- function() {
  tabPanel("Settings",
    br(),
    h4("Database"),
    p("Download a copy of the current SQLite database file."),
    downloadButton("download_db", "Download Database", class = "btn-info"),
    br()
  )
}