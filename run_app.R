library(shiny)
library(DBI)
library(RSQLite)

source("global.R")

ui <- fluidPage(
  titlePanel("Meal Planner"),
  fileInput("db_file", "Upload Database (.db)",
            accept      = ".db",
            buttonLabel = "Browse...",
            placeholder = "No database selected"),
  textOutput("db_status"),
  hr(),
  tabsetPanel(
    meal_planner_ui(),
    build_meal_ui(),
    build_plan_ui(),
    import_meal_ui(),
    add_ingredients_ui(),
    add_ingredient_by_serving_ui(),
    add_user_ui(),
    modify_user_ui(),
    remove_items_ui(),
    settings_ui()
  )
)

server <- function(input, output, session) {

  con                <- reactiveVal(NULL)
  db_path            <- reactiveVal(NULL)
  user_refresh       <- reactiveVal(0)
  meal_refresh       <- reactiveVal(0)
  plan_refresh       <- reactiveVal(0)
  ingredient_refresh <- reactiveVal(0)

  observeEvent(input$db_file, {
    req(input$db_file)
    db_path(input$db_file$datapath)
    con(DBI::dbConnect(RSQLite::SQLite(), db_path()))
    output$db_status <- renderText(
      paste0("Database connected: ", input$db_file$name)
    )
  })

  onStop(function() {
    if (!is.null(con())) DBI::dbDisconnect(con())
  })

  meal_choices <- reactive({
    req(con())
    meal_refresh()
    meals <- DBI::dbGetQuery(con(), "SELECT meal_id, name FROM meals")
    setNames(meals$meal_id, meals$name)
  })

  user_choices <- reactive({
    req(con())
    user_refresh()
    users <- DBI::dbGetQuery(con(), "SELECT user_id, name FROM users")
    setNames(users$user_id, users$name)
  })

  ingredient_choices <- reactive({
    req(con())
    ingredient_refresh()
    ingredients <- DBI::dbGetQuery(con(), "SELECT ingredient_id, name FROM ingredients")
    setNames(ingredients$ingredient_id, ingredients$name)
  })

  plan_choices <- reactive({
    req(con())
    plan_refresh()
    plans <- DBI::dbGetQuery(con(), "SELECT plan_id, name FROM plans")
    setNames(plans$plan_id, plans$name)
  })

  meal_planner_server(input, output, session, con, meal_choices, user_choices)
  import_meal_server(input, output, session, con, meal_refresh)
  add_ingredients_server(input, output, session, con)
  add_ingredient_by_serving_server(input, output, session, con)
  build_meal_server(input, output, session, con, meal_refresh, ingredient_choices)
  build_plan_server(input, output, session, con, meal_choices, plan_refresh)
  add_user_server(input, output, session, con, user_refresh)
  modify_user_server(input, output, session, con, user_choices, user_refresh)
  remove_items_server(input, output, session, con,
                      user_refresh, meal_refresh,
                      plan_refresh, ingredient_refresh)
  settings_server(input, output, session, con, db_path)
}

shinyApp(ui, server)
