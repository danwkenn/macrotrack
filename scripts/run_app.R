library(shiny)
library(shinyFiles)
library(DBI)
library(RSQLite)

lapply(list.files("R", full.names = TRUE), FUN = source)
ui <- fluidPage(
  titlePanel("Meal Planner"),
  shinyFilesButton(
    id       = "db_file",
    label    = "Select Database",
    title    = "Please select your nutrition database",
    multiple = FALSE
  ),
  textOutput("db_status"),
  hr(),
  tabsetPanel(
    meal_planner_ui(),
    build_meal_ui(),
    import_meal_ui(),
    add_ingredients_ui(),
    add_user_ui(),
    modify_user_ui()
  )
)

server <- function(input, output, session) {

  roots        <- c(home = path.expand("~"))
  con          <- reactiveVal(NULL)
  user_refresh <- reactiveVal(0)
  meal_refresh <- reactiveVal(0)

  shinyFileChoose(input, "db_file", roots = roots, filetypes = c("db"))

  observeEvent(input$db_file, {
    path <- parseFilePaths(roots, input$db_file)
    if (nrow(path) > 0) {
      con(DBI::dbConnect(RSQLite::SQLite(), as.character(path$datapath)))
      output$db_status <- renderText("Database connected successfully")
    }
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
    ingredients <- DBI::dbGetQuery(con(), "SELECT ingredient_id, name FROM ingredients")
    setNames(ingredients$ingredient_id, ingredients$name)
  })

  meal_planner_server(input, output, session, con, meal_choices, user_choices)
  import_meal_server(input, output, session, con, meal_refresh, roots)
  add_ingredients_server(input, output, session, con)
  build_meal_server(input, output, session, con, meal_refresh, ingredient_choices)
  add_user_server(input, output, session, con, user_refresh)
  modify_user_server(input, output, session, con, user_choices, user_refresh)
}

shinyApp(ui, server)
