library(shiny)
library(DBI)
library(RPostgres)

source("global.R")

# Non-secret DB config read from .Renviron. Password is NOT read here;
# it's collected from the user at runtime via the password prompt.
db_host   <- Sys.getenv("NUTRITION_DB_HOST")
db_name   <- Sys.getenv("NUTRITION_DB_NAME")
db_user   <- Sys.getenv("NUTRITION_DB_USER")
db_port   <- as.integer(Sys.getenv("NUTRITION_DB_PORT", "5432"))

ui <- fluidPage(
  titlePanel("Meal Planner"),

  # --- Pre-connect panel ---
  # Shown until a successful connection is established.
  conditionalPanel(
    condition = "output.is_connected != true",
    wellPanel(
      h4("Connect to database"),
      p("This app connects to a Neon Postgres database. ",
        "The password is never stored on the server \u2014 you'll be prompted ",
        "at the start of each session."),
      fluidRow(
        column(6, strong("Host: "),     textOutput("info_host",   inline = TRUE)),
        column(6, strong("Database: "), textOutput("info_dbname", inline = TRUE))
      ),
      fluidRow(
        column(6, strong("User: "),     textOutput("info_user",   inline = TRUE)),
        column(6, strong("Port: "),     textOutput("info_port",   inline = TRUE))
      ),
      br(),
      passwordInput("db_password", "Password", placeholder = "Neon password"),
      actionButton("connect_btn", "Connect", class = "btn-primary"),
      br(), br(),
      textOutput("connect_status")
    )
  ),

  # --- Main app panel ---
  # Shown once connected.
  conditionalPanel(
    condition = "output.is_connected == true",
    tabsetPanel(
      meal_planner_ui(),
      meal_planner_beta_ui(),
      build_meal_ui(),
      build_plan_ui(),
      import_meal_ui(),
      add_ingredients_ui(),
      add_ingredient_by_serving_ui(),
      add_user_ui(),
      modify_user_ui(),
      remove_items_ui(),
      deduplicate_ingredients_ui(),
      biometrics_ui(),
      settings_ui()
    )
  )
)

server <- function(input, output, session) {

  # --- Connection state ---
  # con() is NULL until the user provides a valid password and clicks Connect.
  con <- reactiveVal(NULL)

  # --- Pre-connect info display ---
  output$info_host   <- renderText({ db_host })
  output$info_dbname <- renderText({ db_name })
  output$info_user   <- renderText({ db_user })
  output$info_port   <- renderText({ as.character(db_port) })

  # --- Connection-state flag ---
  # Exposed as an output so conditionalPanel can read it. suspendWhenHidden
  # is turned off so the value is always computed, not skipped when the
  # pre-connect panel is hidden.
  output$is_connected <- reactive({
    !is.null(con()) && DBI::dbIsValid(con())
  })
  outputOptions(output, "is_connected", suspendWhenHidden = FALSE)

  # --- Connect handler ---
  observeEvent(input$connect_btn, {
    req(input$db_password)

    tryCatch({
      new_con <- DBI::dbConnect(
        RPostgres::Postgres(),
        host     = db_host,
        dbname   = db_name,
        user     = db_user,
        password = input$db_password,
        port     = db_port,
        sslmode  = "require"
      )
      con(new_con)
      output$connect_status <- renderText("Connected.")

      # Clear the password input for hygiene (it's already been used)
      updateTextInput(session, "db_password", value = "")

    }, error = function(e) {
      con(NULL)
      output$connect_status <- renderText(paste("Connection failed:", e$message))
    })
  })

  # --- Session cleanup ---
  session$onSessionEnded(function() {
    current <- isolate(con())
    if (!is.null(current) && DBI::dbIsValid(current)) {
      DBI::dbDisconnect(current)
    }
  })

  # --- Auth hook (not yet used) ---
  # Populated by a future login flow; drives per-user ownership filters.
  current_user_id <- reactiveVal(NULL)

  # --- Refresh counters for reactive list updates ---
  user_refresh             <- reactiveVal(0)
  meal_refresh             <- reactiveVal(0)
  plan_refresh             <- reactiveVal(0)
  ingredient_refresh       <- reactiveVal(0)
  measurement_type_refresh <- reactiveVal(0)

  # --- Reactive dropdown choice lists ---
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

  # --- Wire up tab servers ---
  meal_planner_server(input, output, session, con,
                      meal_choices, user_choices, ingredient_choices)
  meal_planner_beta_server(input, output, session, con,
                           meal_choices, user_choices, ingredient_choices)
  import_meal_server(input, output, session, con, meal_refresh)
  add_ingredients_server(input, output, session, con, ingredient_refresh)
  add_ingredient_by_serving_server(input, output, session, con, ingredient_refresh)
  build_meal_server(input, output, session, con, meal_refresh, ingredient_choices)
  build_plan_server(input, output, session, con, meal_choices, plan_refresh)
  add_user_server(input, output, session, con, user_refresh)
  modify_user_server(input, output, session, con, user_choices, user_refresh)
  remove_items_server(input, output, session, con,
                      user_refresh, meal_refresh,
                      plan_refresh, ingredient_refresh)
  deduplicate_ingredients_server(input, output, session, con,
                                 ingredient_refresh, meal_refresh)
  biometrics_server(input, output, session, con,
                    user_choices, measurement_type_refresh)
  settings_server(input, output, session, con,
                  user_refresh, meal_refresh,
                  plan_refresh, ingredient_refresh)
}

shinyApp(ui, server)
