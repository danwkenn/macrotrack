library(shiny)
library(shinyFiles)
library(DBI)
library(RSQLite)

ui <- fluidPage(
  titlePanel("Meal Planner"),

  # Database selection
  shinyFilesButton(
    id       = "db_file",
    label    = "Select Database",
    title    = "Please select your nutrition database",
    multiple = FALSE
  ),
  textOutput("db_status"),

  hr(),

  tabsetPanel(

    # --- Tab 1: Meal Planner ---
    tabPanel("Meal Planner",
      br(),
      uiOutput("meal_planner"),
      hr(),
      uiOutput("daily_summary")
    ),

    # --- Tab 2: Import Meal ---
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
    ),

    # --- Tab 3: Add User ---
    tabPanel("Add User",
      br(),
      fluidRow(
        column(4, textInput("user_name", "Name")),
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
    ),

    # --- Tab 4: Modify User ---
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
        column(4, numericInput("modify_age",    "Age (years)", value = NULL)),
        column(4, selectInput("modify_gender",  "Gender",
                              choices = c("M", "F"))),
        column(4, selectInput("modify_goal",    "Goal",
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
  )
)

server <- function(input, output, session) {

  # Set up file system roots
  roots <- c(home = path.expand("~"))
  shinyFileChoose(input, "db_file",  roots = roots, filetypes = c("db"))
  shinyFileChoose(input, "meal_csv", roots = roots, filetypes = c("csv"))

  # Reactive values
  con          <- reactiveVal(NULL)
  user_refresh <- reactiveVal(0)
  meal_refresh <- reactiveVal(0)
  csv_path     <- reactiveVal(NULL)

  observeEvent(input$db_file, {
    path <- parseFilePaths(roots, input$db_file)
    if (nrow(path) > 0) {
      con(DBI::dbConnect(RSQLite::SQLite(), as.character(path$datapath)))
      output$db_status <- renderText("Database connected successfully")
    }
  })

  observeEvent(input$meal_csv, {
    path <- parseFilePaths(roots, input$meal_csv)
    if (nrow(path) > 0) {
      csv_path(as.character(path$datapath))
      output$csv_status <- renderText(paste("Selected:", basename(csv_path())))
    }
  })

  # Close connection when app stops
  onStop(function() {
    if (!is.null(con())) DBI::dbDisconnect(con())
  })

  # Reactive expression to fetch meal names from database
  meal_choices <- reactive({
    req(con())
    meal_refresh()
    meals <- DBI::dbGetQuery(con(), "SELECT meal_id, name FROM meals")
    setNames(meals$meal_id, meals$name)
  })

  # Reactive expression to fetch user names from database
  user_choices <- reactive({
    req(con())
    user_refresh()
    users <- DBI::dbGetQuery(con(), "SELECT user_id, name FROM users")
    setNames(users$user_id, users$name)
  })

  # Render modify user dropdown
  output$modify_user_ui <- renderUI({
    req(con())
    selectInput(
      inputId  = "modify_user_select",
      label    = "Select User",
      choices  = c("None" = "", user_choices()),
      selected = ""
    )
  })

  # Render the meal planner UI once database is connected
  output$meal_planner <- renderUI({
    req(con())
    tagList(
      lapply(1:7, function(i) {
        fluidRow(
          column(4,
            selectizeInput(
              inputId  = paste0("meal_", i),
              label    = paste("Slot", i),
              choices  = c("None" = "", meal_choices()),
              selected = "",
              options  = list(placeholder = "Search for a meal...")
            )
          ),
          column(2,
            numericInput(
              inputId = paste0("servings_", i),
              label   = "Servings",
              value   = 1,
              min     = 0.5,
              step    = 0.5
            )
          ),
          column(6,
            tableOutput(paste0("macros_", i))
          )
        )
      })
    )
  })

  # Render macros for each slot
  lapply(1:7, function(i) {
    output[[paste0("macros_", i)]] <- renderTable({
      req(con())
      meal_id  <- input[[paste0("meal_", i)]]
      servings <- input[[paste0("servings_", i)]]
      req(meal_id != "")
      get_meal_macros(con(), as.integer(meal_id), servings)
    })
  })

  # Render daily summary and targets section
  output$daily_summary <- renderUI({
    req(con())
    tagList(
      h3("Daily Summary & Targets"),
      fluidRow(
        column(4,
          selectizeInput(
            inputId = "selected_user",
            label   = "Select User",
            choices = c("None" = "", user_choices()),
            options = list(placeholder = "Type a name...")
          )
        )
      ),
      fluidRow(
        column(3, numericInput("target_calories", "Target Calories",    value = NA)),
        column(3, numericInput("target_protein",  "Target Protein (g)", value = NA)),
        column(3, numericInput("target_carbs",    "Target Carbs (g)",   value = NA)),
        column(3, numericInput("target_fat",      "Target Fat (g)",     value = NA))
      ),
      fluidRow(
  column(3,
    numericInput("pct_to_plan", "% of targets to plan for",
                 value = 100, min = 1, max = 100, step = 1)
  )
),
      tableOutput("comparison_table")
    )
  })

  # When a user is selected in meal planner, fill in their targets
  observeEvent(input$selected_user, {
    req(input$selected_user != "")
    targets <- get_user_targets(con(), as.integer(input$selected_user))
    updateNumericInput(session, "target_calories", value = targets$calories)
    updateNumericInput(session, "target_protein",  value = targets$protein)
    updateNumericInput(session, "target_carbs",    value = targets$carbs)
    updateNumericInput(session, "target_fat",      value = targets$fat)
  })

  # When a user is selected in modify tab, populate their details
  observeEvent(input$modify_user_select, {
    req(input$modify_user_select != "")
    req(input$modify_user_select != "None")
    user <- DBI::dbGetQuery(con(), "SELECT * FROM users WHERE user_id = ?",
                            params = list(as.integer(input$modify_user_select)))
    updateTextInput(session,    "modify_name",          value = user$name)
    updateNumericInput(session, "modify_weight",        value = user$weight_kg)
    updateNumericInput(session, "modify_height",        value = user$height_cm)
    updateNumericInput(session, "modify_age",           value = user$age_years)
    updateSelectInput(session,  "modify_gender",        selected = user$gender)
    updateSelectInput(session,  "modify_goal",          selected = user$goal)
    updateSelectInput(session,  "modify_work_type",     selected = user$work_type)
    updateNumericInput(session, "modify_exercise_days", value = user$exercise_days)
    updateNumericInput(session, "modify_calories",      value = user$calories)
    updateNumericInput(session, "modify_protein",       value = user$protein)
    updateNumericInput(session, "modify_carbs",         value = user$carbs)
    updateNumericInput(session, "modify_fat",           value = user$fat)
  })

  # Render comparison table
output$comparison_table <- renderTable({
    req(con())

    summaries <- lapply(1:7, function(i) {
      meal_id  <- input[[paste0("meal_", i)]]
      servings <- input[[paste0("servings_", i)]]
      if (is.null(meal_id) || meal_id == "") return(NULL)
      get_meal_macros(con(), as.integer(meal_id), servings)
    })

    summaries <- Filter(Negate(is.null), summaries)
    if (length(summaries) == 0) return(NULL)

    combined <- data.table::rbindlist(summaries)
    planned  <- combined[, .(
      calories = sum(calories),
      protein  = sum(protein),
      carbs    = sum(carbs),
      fat      = sum(fat)
    )]

    full_targets <- c(input$target_calories, input$target_protein,
                      input$target_carbs,    input$target_fat)

    pct          <- input$pct_to_plan / 100
    planning     <- round(full_targets * pct, 1)
    in_reserve   <- round(full_targets * (1 - pct), 1)
    planned_vals <- c(planned$calories, planned$protein,
                      planned$carbs,    planned$fat)
    planned_vals <- round(planned_vals, 1)

    diff <- planned_vals - planning

    data.frame(
      Macro            = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
      Full.Target      = full_targets,
      In.Reserve       = in_reserve,
      Planning.Target  = planning,
      Planned          = planned_vals,
      Difference       = ifelse(diff >= 0,
                                paste0("+", round(diff, 1)),
                                as.character(round(diff, 1)))
    )
  })

  # Import meal button
  observeEvent(input$import_meal_btn, {
    req(con())
    req(input$meal_name != "")
    req(csv_path())
    tryCatch({
      import_meal(
        con                   = con(),
        meal_name             = input$meal_name,
        csv_path              = csv_path(),
        use_ingredient_macros = input$use_ingredient_macros,
        calories              = if (is.na(input$meal_calories)) NULL else input$meal_calories,
        protein               = if (is.na(input$meal_protein))  NULL else input$meal_protein,
        carbs                 = if (is.na(input$meal_carbs))    NULL else input$meal_carbs,
        fat                   = if (is.na(input$meal_fat))      NULL else input$meal_fat
      )
      output$import_meal_status <- renderText("Meal imported successfully.")
      meal_refresh(meal_refresh() + 1)
    }, error = function(e) {
      output$import_meal_status <- renderText(paste("Error:", e$message))
    })
  })

  # Add user button
  observeEvent(input$add_user_btn, {
    req(con())
    req(input$user_name != "")
    tryCatch({
      add_user(
        con           = con(),
        name          = input$user_name,
        weight_kg     = input$user_weight,
        height_cm     = input$user_height,
        age_years     = input$user_age,
        gender        = input$user_gender,
        goal          = input$user_goal,
        work_type     = input$user_work_type,
        exercise_days = input$user_exercise_days,
        calories      = if (is.na(input$user_calories)) NULL else input$user_calories,
        protein       = if (is.na(input$user_protein))  NULL else input$user_protein,
        carbs         = if (is.na(input$user_carbs))    NULL else input$user_carbs,
        fat           = if (is.na(input$user_fat))      NULL else input$user_fat
      )
      output$add_user_status <- renderText("User added successfully.")
      user_refresh(user_refresh() + 1)
    }, error = function(e) {
      output$add_user_status <- renderText(paste("Error:", e$message))
    })
  })

  # Save user changes button
  observeEvent(input$save_user_btn, {
    req(con())
    req(input$modify_user_select != "")
    req(input$modify_user_select != "None")
    tryCatch({
      update_user(
        con           = con(),
        user_id       = as.integer(input$modify_user_select),
        name          = input$modify_name,
        weight_kg     = input$modify_weight,
        height_cm     = input$modify_height,
        age_years     = input$modify_age,
        gender        = input$modify_gender,
        goal          = input$modify_goal,
        work_type     = input$modify_work_type,
        exercise_days = input$modify_exercise_days,
        calories      = if (is.na(input$modify_calories)) NULL else input$modify_calories,
        protein       = if (is.na(input$modify_protein))  NULL else input$modify_protein,
        carbs         = if (is.na(input$modify_carbs))    NULL else input$modify_carbs,
        fat           = if (is.na(input$modify_fat))      NULL else input$modify_fat
      )
      output$modify_user_status <- renderText("User updated successfully.")
      user_refresh(user_refresh() + 1)
    }, error = function(e) {
      output$modify_user_status <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui, server)
