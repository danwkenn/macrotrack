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
    uiOutput("daily_summary")
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
import_meal_ui <- function() {
  tabPanel("Import Meal",
    br(),
    fluidRow(
      column(4, textInput("meal_name", "Meal Name"))
    ),
    fluidRow(
      column(4,
        shinyFilesButton(
          id       = "meal_csv",
          label    = "Select CSV File",
          title    = "Please select a meal CSV file",
          multiple = FALSE
        ),
        br(), br(),
        textOutput("csv_status")
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
#' construct a meal from ingredients stored in the database.
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
    h4("Add Ingredients"),
    fluidRow(
      column(4, uiOutput("build_ingredient_select")),
      column(2, numericInput("build_quantity", "Quantity (portions)",
                             value = 1, min = 0.5, step = 0.5)),
      column(2,
        br(),
        actionButton("add_to_meal_btn", "Add to Meal", class = "btn-success")
      )
    ),
    br(),
    tableOutput("build_meal_table"),
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
    br(),
    actionButton("save_meal_btn", "Save Meal", class = "btn-primary"),
    br(), br(),
    textOutput("build_meal_status")
  )
}

