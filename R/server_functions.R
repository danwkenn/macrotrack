#' Meal Planner Tab Server
#'
#' Server logic for the meal planner tab, including meal slots,
#' daily summary, and macro targets comparison.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param meal_choices A reactive expression returning available meals.
#' @param user_choices A reactive expression returning available users.
#'
#' @export
meal_planner_server <- function(input, output, session, con,
                                meal_choices, user_choices) {

  # Render the meal planner UI once database is connected
  output$meal_planner <- renderUI({
    req(con())
    tagList(
      lapply(1:10, function(i) {
        fluidRow(
          column(3,
            textInput(
              inputId = paste0("slot_name_", i),
              label   = paste("Slot", i, "Name"),
              value   = ""
            )
          ),
          column(3,
            selectizeInput(
              inputId  = paste0("meal_", i),
              label    = "Meal",
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
          column(4,
            tableOutput(paste0("macros_", i))
          )
        )
      })
    )
  })

  # Render macros for each slot
  lapply(1:10, function(i) {
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
        ),
        column(3,
          numericInput("pct_to_plan", "% of targets to plan for",
                       value = 100, min = 1, max = 100, step = 1)
        )
      ),
      fluidRow(
        column(3, numericInput("target_calories", "Target Calories",    value = NA)),
        column(3, numericInput("target_protein",  "Target Protein (g)", value = NA)),
        column(3, numericInput("target_carbs",    "Target Carbs (g)",   value = NA)),
        column(3, numericInput("target_fat",      "Target Fat (g)",     value = NA))
      ),
      tableOutput("comparison_table")
    )
  })

  # When a user is selected, fill in their targets
  observeEvent(input$selected_user, {
    req(input$selected_user != "")
    targets <- get_user_targets(con(), as.integer(input$selected_user))
    updateNumericInput(session, "target_calories", value = targets$calories)
    updateNumericInput(session, "target_protein",  value = targets$protein)
    updateNumericInput(session, "target_carbs",    value = targets$carbs)
    updateNumericInput(session, "target_fat",      value = targets$fat)
  })

  # Render comparison table
  output$comparison_table <- renderTable({
    req(con())
    summaries <- lapply(1:10, function(i) {
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
    planned_vals <- round(c(planned$calories, planned$protein,
                            planned$carbs,    planned$fat), 1)
    diff <- planned_vals - planning
    data.frame(
      Macro           = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
      Full.Target     = full_targets,
      In.Reserve      = in_reserve,
      Planning.Target = planning,
      Planned         = planned_vals,
      Difference      = ifelse(diff >= 0,
                               paste0("+", round(diff, 1)),
                               as.character(round(diff, 1)))
    )
  })

# Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("meal_plan_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      req(con())

      # --- User name ---
      user_name <- if (!is.null(input$selected_user) && input$selected_user != "") {
        user <- DBI::dbGetQuery(con(), "SELECT name FROM users WHERE user_id = ?",
                                params = list(as.integer(input$selected_user)))
        user$name
      } else {
        "Unknown"
      }

      # --- Targets table ---
      full_targets <- c(input$target_calories, input$target_protein,
                        input$target_carbs,    input$target_fat)
      pct          <- input$pct_to_plan / 100
      planning     <- round(full_targets * pct, 1)
      in_reserve   <- round(full_targets * (1 - pct), 1)

      # Collect planned totals
      summaries <- lapply(1:10, function(i) {
        meal_id  <- input[[paste0("meal_", i)]]
        servings <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)
        get_meal_macros(con(), as.integer(meal_id), servings)
      })
      summaries    <- Filter(Negate(is.null), summaries)
      combined     <- data.table::rbindlist(summaries)
      planned      <- combined[, .(
        calories = sum(calories),
        protein  = sum(protein),
        carbs    = sum(carbs),
        fat      = sum(fat)
      )]
      planned_vals <- round(c(planned$calories, planned$protein,
                              planned$carbs,    planned$fat), 1)
      diff         <- planned_vals - planning

      targets_df <- data.frame(
        Macro           = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
        Full.Target     = full_targets,
        In.Reserve      = in_reserve,
        Planning.Target = planning,
        Planned         = planned_vals,
        Difference      = ifelse(diff >= 0,
                                 paste0("+", round(diff, 1)),
                                 as.character(round(diff, 1)))
      )

      # --- Slots table ---
      slots_df <- do.call(rbind, lapply(1:10, function(i) {
        meal_id   <- input[[paste0("meal_", i)]]
        slot_name <- input[[paste0("slot_name_", i)]]
        servings  <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)
        meal_name <- DBI::dbGetQuery(con(), "SELECT name FROM meals WHERE meal_id = ?",
                                     params = list(as.integer(meal_id)))$name
        data.frame(
          slot_name = if (slot_name == "") paste("Slot", i) else slot_name,
          meal_name = meal_name,
          servings  = servings
        )
      }))

      # --- Meal details list ---
      meal_details <- lapply(1:10, function(i) {
        meal_id   <- input[[paste0("meal_", i)]]
        slot_name <- input[[paste0("slot_name_", i)]]
        servings  <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)

        meal_id_int <- as.integer(meal_id)
        meal_name   <- DBI::dbGetQuery(con(), "SELECT name FROM meals WHERE meal_id = ?",
                                       params = list(meal_id_int))$name

        # Get ingredients for this meal
        sql <- "
          SELECT
            i.name          AS ingredient,
            mi.quantity     AS portions,
            i.portion_size,
            i.calories,
            i.protein,
            i.carbs,
            i.fat
          FROM meal_ingredients mi
          JOIN ingredients i ON mi.ingredient_id = i.ingredient_id
          WHERE mi.meal_id = ?
        "
        ingredients <- DBI::dbGetQuery(con(), sql, params = list(meal_id_int))

        # Calculate macros per ingredient multiplied by servings
        ingredients$calories <- round((ingredients$portion_size / 100) *
                                        ingredients$calories *
                                        ingredients$portions * servings, 1)
        ingredients$protein  <- round((ingredients$portion_size / 100) *
                                        ingredients$protein *
                                        ingredients$portions * servings, 1)
        ingredients$carbs    <- round((ingredients$portion_size / 100) *
                                        ingredients$carbs *
                                        ingredients$portions * servings, 1)
        ingredients$fat      <- round((ingredients$portion_size / 100) *
                                        ingredients$fat *
                                        ingredients$portions * servings, 1)

        # Add totals row
        totals_row <- data.frame(
          ingredient  = "Total",
          portions    = NA,
          portion_size = NA,
          calories    = sum(ingredients$calories),
          protein     = sum(ingredients$protein),
          carbs       = sum(ingredients$carbs),
          fat         = sum(ingredients$fat)
        )
        ingredients <- rbind(ingredients, totals_row)

        # Drop portion_size from display
        ingredients$portion_size <- NULL

        list(
          slot_name    = if (slot_name == "") paste("Slot", i) else slot_name,
          meal_name    = meal_name,
          servings     = servings,
          ingredients  = ingredients
        )
      })
      meal_details <- Filter(Negate(is.null), meal_details)

      # --- Render the report ---
      rmarkdown::render(
        input       = "templates/meal_plan_report.Rmd",
        output_file = file,
        params      = list(
          user_name   = user_name,
          report_date = Sys.Date(),
          targets     = targets_df,
          pct_to_plan = pct,
          slots       = slots_df,
          meal_details = meal_details
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )

}

#' Import Meal Tab Server
#'
#' Server logic for the import meal tab, handling CSV file selection
#' and meal import into the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param meal_refresh A reactiveVal used to trigger meal list refresh.
#'
#' @export
import_meal_server <- function(input, output, session, con, meal_refresh) {

  csv_path <- reactiveVal(NULL)

  observeEvent(input$meal_csv, {
    req(input$meal_csv)
    csv_path(input$meal_csv$datapath)
  })

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
}

#' Add User Tab Server
#'
#' Server logic for the add user tab, handling user creation
#' and insertion into the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param user_refresh A reactiveVal used to trigger user list refresh.
#'
#' @export
add_user_server <- function(input, output, session, con, user_refresh) {

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
}

#' Modify User Tab Server
#'
#' Server logic for the modify user tab, handling user selection,
#' field population, and saving changes to the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param user_choices A reactive expression returning available users.
#' @param user_refresh A reactiveVal used to trigger user list refresh.
#'
#' @export
modify_user_server <- function(input, output, session, con,
                               user_choices, user_refresh) {

  output$modify_user_ui <- renderUI({
    req(con())
    selectInput(
      inputId  = "modify_user_select",
      label    = "Select User",
      choices  = c("None" = "", user_choices()),
      selected = ""
    )
  })

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

#' Add Ingredients Tab Server
#'
#' Server logic for the add ingredients tab, handling ingredient
#' insertion into the database and resetting the form on success.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#'
#' @export
add_ingredients_server <- function(input, output, session, con) {

  observeEvent(input$add_ingredient_btn, {
    req(con())
    req(input$ingredient_name != "")

    tryCatch({
      add_ingredient(
        con          = con(),
        name         = input$ingredient_name,
        calories     = input$ingredient_calories,
        protein      = input$ingredient_protein,
        carbs        = input$ingredient_carbs,
        fat          = input$ingredient_fat,
        portion_size = input$ingredient_portion_size,
        portion_name = input$ingredient_portion_name
      )
      output$add_ingredient_status <- renderText("Ingredient added successfully.")

      # Reset form
      updateTextInput(session,    "ingredient_name",         value = "")
      updateNumericInput(session, "ingredient_calories",     value = NA)
      updateNumericInput(session, "ingredient_protein",      value = NA)
      updateNumericInput(session, "ingredient_carbs",        value = NA)
      updateNumericInput(session, "ingredient_fat",          value = NA)
      updateNumericInput(session, "ingredient_portion_size", value = NA)
      updateTextInput(session,    "ingredient_portion_name", value = "")

    }, error = function(e) {
      output$add_ingredient_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Build Meal Tab Server
#'
#' Server logic for the build meal tab, handling ingredient search,
#' running ingredient list, and saving the completed meal to the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param meal_refresh A reactiveVal used to trigger meal list refresh.
#' @param ingredient_choices A reactive expression returning available ingredients.
#'
#' @export
build_meal_server <- function(input, output, session, con,
                              meal_refresh, ingredient_choices) {

  # Reactive data frame storing ingredients added so far
  meal_ingredients <- reactiveVal(data.frame(
    ingredient_id = integer(),
    name          = character(),
    quantity      = numeric(),
    calories      = numeric(),
    protein       = numeric(),
    carbs         = numeric(),
    fat           = numeric()
  ))

  # Render ingredient search dropdown
  output$build_ingredient_select <- renderUI({
    req(con())
    selectizeInput(
      inputId = "build_ingredient_id",
      label   = "Search Ingredient",
      choices = c("None" = "", ingredient_choices()),
      options = list(placeholder = "Type to search...")
    )
  })

  # Add ingredient to running list
  observeEvent(input$add_to_meal_btn, {
    req(con())
    req(input$build_ingredient_id != "")

    # Fetch ingredient details
    ingredient <- DBI::dbGetQuery(
      con(),
      "SELECT * FROM ingredients WHERE ingredient_id = ?",
      params = list(as.integer(input$build_ingredient_id))
    )

    quantity <- input$build_quantity

    # Calculate macros for this ingredient at given quantity
    new_row <- data.frame(
      ingredient_id = ingredient$ingredient_id,
      name          = ingredient$name,
      quantity      = quantity,
      calories      = round((ingredient$portion_size / 100) * ingredient$calories * quantity, 1),
      protein       = round((ingredient$portion_size / 100) * ingredient$protein  * quantity, 1),
      carbs         = round((ingredient$portion_size / 100) * ingredient$carbs    * quantity, 1),
      fat           = round((ingredient$portion_size / 100) * ingredient$fat      * quantity, 1)
    )

    # Append to running list
    meal_ingredients(rbind(meal_ingredients(), new_row))
  })

  # Render running ingredient table with remove buttons
  output$build_meal_table <- renderTable({
    req(nrow(meal_ingredients()) > 0)
    meal_ingredients()[, c("name", "quantity", "calories", "protein", "carbs", "fat")]
  })

  # Remove ingredient row
  observeEvent(input$remove_ingredient, {
    current <- meal_ingredients()
    current <- current[-input$remove_ingredient, ]
    meal_ingredients(current)
  })

  # Save meal to database
  observeEvent(input$save_meal_btn, {
    req(con())
    req(input$build_meal_name != "")
    req(nrow(meal_ingredients()) > 0)

    tryCatch({

      # Insert ingredients and get their IDs
      max_id <- DBI::dbGetQuery(con(), "SELECT MAX(ingredient_id) FROM ingredients")[[1]]
      if (is.na(max_id)) max_id <- 0

      # Insert meal
      max_meal_id <- DBI::dbGetQuery(con(), "SELECT MAX(meal_id) FROM meals")[[1]]
      if (is.na(max_meal_id)) max_meal_id <- 0

      meals_df <- data.frame(
        meal_id               = max_meal_id + 1,
        name                  = input$build_meal_name,
        use_ingredient_macros = as.numeric(input$build_use_ingredient_macros),
        calories              = if (is.na(input$build_calories)) NA_real_ else input$build_calories,
        protein               = if (is.na(input$build_protein))  NA_real_ else input$build_protein,
        carbs                 = if (is.na(input$build_carbs))    NA_real_ else input$build_carbs,
        fat                   = if (is.na(input$build_fat))      NA_real_ else input$build_fat
      )

      DBI::dbAppendTable(con(), "meals", meals_df)

      # Insert meal_ingredients
      meal_ingredients_df <- data.frame(
        meal_id       = max_meal_id + 1,
        ingredient_id = meal_ingredients()$ingredient_id,
        quantity      = meal_ingredients()$quantity
      )

      DBI::dbAppendTable(con(), "meal_ingredients", meal_ingredients_df)

      output$build_meal_status <- renderText("Meal saved successfully.")
      meal_refresh(meal_refresh() + 1)

      # Reset form
      meal_ingredients(data.frame(
        ingredient_id = integer(),
        name          = character(),
        quantity      = numeric(),
        calories      = numeric(),
        protein       = numeric(),
        carbs         = numeric(),
        fat           = numeric()
      ))
      updateTextInput(session, "build_meal_name", value = "")

    }, error = function(e) {
      output$build_meal_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Add Ingredient by Serving Tab Server
#'
#' Server logic for the add ingredient by serving tab. Converts
#' packet serving data and portion factor into per-100g values
#' and inserts the ingredient into the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#'
#' @export
add_ingredient_by_serving_server <- function(input, output, session, con) {

  # Live preview of portion size in grams
  output$portion_preview <- renderText({
    req(input$serving_size_g, input$portion_factor)
    portion_g <- round(input$serving_size_g * input$portion_factor, 1)
    paste0("One portion = ", portion_g, "g")
  })

  observeEvent(input$add_ingredient_serving_btn, {
    req(con())
    req(input$serving_name != "")

    tryCatch({

      # Convert to per-100g values
      portion_size <- input$serving_size_g * input$portion_factor
      multiplier   <- 100 / input$serving_size_g

      add_ingredient(
        con          = con(),
        name         = input$serving_name,
        calories     = round(input$serving_calories * multiplier, 2),
        protein      = round(input$serving_protein  * multiplier, 2),
        carbs        = round(input$serving_carbs    * multiplier, 2),
        fat          = round(input$serving_fat      * multiplier, 2),
        portion_size = round(portion_size, 1),
        portion_name = input$serving_portion_name
      )

      output$add_ingredient_serving_status <- renderText("Ingredient added successfully.")

      # Reset form
      updateTextInput(session,    "serving_name",         value = "")
      updateTextInput(session,    "serving_portion_name", value = "")
      updateNumericInput(session, "serving_size_g",       value = NA)
      updateNumericInput(session, "serving_calories",     value = NA)
      updateNumericInput(session, "serving_protein",      value = NA)
      updateNumericInput(session, "serving_carbs",        value = NA)
      updateNumericInput(session, "serving_fat",          value = NA)
      updateNumericInput(session, "portion_factor",       value = 1)

    }, error = function(e) {
      output$add_ingredient_serving_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Build Plan Tab Server
#'
#' Server logic for the build plan tab, handling meal search,
#' running meal list, macro summary, and saving the plan to the database.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param meal_choices A reactive expression returning available meals.
#' @param plan_refresh A reactiveVal used to trigger plan list refresh.
#'
#' @export
build_plan_server <- function(input, output, session, con,
                              meal_choices, plan_refresh) {

  # Reactive data frame storing meals added so far
  plan_meals <- reactiveVal(data.frame(
    meal_id  = integer(),
    name     = character(),
    servings = numeric(),
    calories = numeric(),
    protein  = numeric(),
    carbs    = numeric(),
    fat      = numeric()
  ))

  # Render meal search dropdown
  output$build_plan_meal_select <- renderUI({
    req(con())
    selectizeInput(
      inputId = "build_plan_meal_id",
      label   = "Search Meal",
      choices = c("None" = "", meal_choices()),
      options = list(placeholder = "Type to search...")
    )
  })

  # Add meal to running list
  observeEvent(input$add_to_plan_btn, {
    req(con())
    req(input$build_plan_meal_id != "")

    servings <- input$plan_meal_servings
    macros   <- get_meal_macros(con(), as.integer(input$build_plan_meal_id), servings)

    new_row <- data.frame(
      meal_id   = as.integer(input$build_plan_meal_id),
      name      = macros$meal_name,
      slot_name = input$plan_slot_name,
      servings  = servings,
      calories  = macros$calories,
      protein   = macros$protein,
      carbs     = macros$carbs,
      fat       = macros$fat
    )

    plan_meals(rbind(plan_meals(), new_row))
  })

  # Clear plan
  observeEvent(input$clear_plan_btn, {
    plan_meals(data.frame(
      meal_id  = integer(),
      name     = character(),
      servings = numeric(),
      calories = numeric(),
      protein  = numeric(),
      carbs    = numeric(),
      fat      = numeric()
    ))
  })

  # Render running meal table
  output$build_plan_table <- renderTable({
    req(nrow(plan_meals()) > 0)
    plan_meals()[, c("slot_name", "name", "servings", "calories", "protein", "carbs", "fat")]
  })

  # Render macro summary adjusted for pct_to_plan
  output$build_plan_summary <- renderTable({
    req(nrow(plan_meals()) > 0)

    pct      <- input$plan_pct_to_plan / 100
    totals   <- colSums(plan_meals()[, c("calories", "protein", "carbs", "fat")])

    data.frame(
      Macro           = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
      Total           = round(totals, 1),
      In.Reserve      = round(totals * (1 - pct), 1),
      Planning.Target = round(totals * pct, 1)
    )
  })

  # Save plan to database
  observeEvent(input$save_plan_btn, {
    req(con())
    req(input$plan_name != "")
    req(nrow(plan_meals()) > 0)

    tryCatch({

      # Get next plan_id
      max_plan_id <- DBI::dbGetQuery(con(), "SELECT MAX(plan_id) FROM plans")[[1]]
      if (is.na(max_plan_id)) max_plan_id <- 0

      # Insert plan
      plans_df <- data.frame(
        plan_id     = max_plan_id + 1,
        name        = input$plan_name,
        description = if (input$plan_description == "") NA_character_ else input$plan_description,
        pct_to_plan = input$plan_pct_to_plan / 100
      )
      DBI::dbAppendTable(con(), "plans", plans_df)

      # Insert plan_meals
      plan_meals_df <- data.frame(
        plan_id   = max_plan_id + 1,
        slot_name = plan_meals()$slot_name,
        meal_id   = plan_meals()$meal_id,
        servings  = plan_meals()$servings
      )
      DBI::dbAppendTable(con(), "plan_meals", plan_meals_df)

      output$build_plan_status <- renderText("Plan saved successfully.")
      plan_refresh(plan_refresh() + 1)

      # Reset form
      plan_meals(data.frame(
        meal_id  = integer(),
        name     = character(),
        servings = numeric(),
        calories = numeric(),
        protein  = numeric(),
        carbs    = numeric(),
        fat      = numeric()
      ))
      updateTextInput(session,    "plan_name",        value = "")
      updateTextInput(session,    "plan_description", value = "")
      updateNumericInput(session, "plan_pct_to_plan", value = 100)

    }, error = function(e) {
      output$build_plan_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Remove Items Tab Server
#'
#' Server logic for the remove items tab, handling removal of users,
#' meals, ingredients, and plans, as well as cleaning unused items.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param user_refresh A reactiveVal used to trigger user list refresh.
#' @param meal_refresh A reactiveVal used to trigger meal list refresh.
#' @param plan_refresh A reactiveVal used to trigger plan list refresh.
#' @param ingredient_refresh A reactiveVal used to trigger ingredient list refresh.
#'
#' @export
remove_items_server <- function(input, output, session, con,
                                user_refresh, meal_refresh,
                                plan_refresh, ingredient_refresh) {

  # --- Render dropdowns ---
  output$remove_user_select <- renderUI({
    req(con())
    user_refresh()
    users <- DBI::dbGetQuery(con(), "SELECT user_id, name FROM users")
    selectInput("remove_user_id", "Select User",
                choices = c("None" = "", setNames(users$user_id, users$name)))
  })

  output$remove_meal_select <- renderUI({
    req(con())
    meal_refresh()
    meals <- DBI::dbGetQuery(con(), "SELECT meal_id, name FROM meals")
    selectInput("remove_meal_id", "Select Meal",
                choices = c("None" = "", setNames(meals$meal_id, meals$name)))
  })

  output$remove_ingredient_select <- renderUI({
    req(con())
    ingredient_refresh()
    ingredients <- DBI::dbGetQuery(con(), "SELECT ingredient_id, name FROM ingredients")
    selectInput("remove_ingredient_id", "Select Ingredient",
                choices = c("None" = "", setNames(ingredients$ingredient_id, ingredients$name)))
  })

  output$remove_plan_select <- renderUI({
    req(con())
    plan_refresh()
    plans <- DBI::dbGetQuery(con(), "SELECT plan_id, name FROM plans")
    selectInput("remove_plan_id", "Select Plan",
                choices = c("None" = "", setNames(plans$plan_id, plans$name)))
  })

  # --- Remove handlers ---
  observeEvent(input$remove_user_btn, {
    req(con())
    req(input$remove_user_id != "")
    tryCatch({
      remove_user(con(), as.integer(input$remove_user_id))
      output$remove_user_status <- renderText("User removed successfully.")
      user_refresh(user_refresh() + 1)
    }, error = function(e) {
      output$remove_user_status <- renderText(paste("Error:", e$message))
    })
  })

  observeEvent(input$remove_meal_btn, {
    req(con())
    req(input$remove_meal_id != "")
    tryCatch({
      remove_meal(con(), as.integer(input$remove_meal_id))
      output$remove_meal_status <- renderText("Meal removed successfully.")
      meal_refresh(meal_refresh() + 1)
    }, error = function(e) {
      output$remove_meal_status <- renderText(paste("Error:", e$message))
    })
  })

  observeEvent(input$remove_ingredient_btn, {
    req(con())
    req(input$remove_ingredient_id != "")
    tryCatch({
      remove_ingredient(con(), as.integer(input$remove_ingredient_id))
      output$remove_ingredient_status <- renderText("Ingredient removed successfully.")
      ingredient_refresh(ingredient_refresh() + 1)
    }, error = function(e) {
      output$remove_ingredient_status <- renderText(paste("Error:", e$message))
    })
  })

  observeEvent(input$remove_plan_btn, {
    req(con())
    req(input$remove_plan_id != "")
    tryCatch({
      remove_plan(con(), as.integer(input$remove_plan_id))
      output$remove_plan_status <- renderText("Plan removed successfully.")
      plan_refresh(plan_refresh() + 1)
    }, error = function(e) {
      output$remove_plan_status <- renderText(paste("Error:", e$message))
    })
  })

  # --- Clean handlers ---
  observeEvent(input$clean_ingredients_btn, {
    req(con())
    tryCatch({
      n <- clean_ingredients(con())
      output$clean_ingredients_status <- renderText(
        paste0(n, " ingredient(s) removed.")
      )
      ingredient_refresh(ingredient_refresh() + 1)
    }, error = function(e) {
      output$clean_ingredients_status <- renderText(paste("Error:", e$message))
    })
  })

  observeEvent(input$clean_meals_btn, {
    req(con())
    tryCatch({
      n <- clean_meals(con())
      output$clean_meals_status <- renderText(
        paste0(n, " meal(s) removed.")
      )
      meal_refresh(meal_refresh() + 1)
    }, error = function(e) {
      output$clean_meals_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Settings Tab Server
#'
#' Server logic for the settings tab.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param db_path A reactiveVal containing the path to the database file.
#'
#' @export
settings_server <- function(input, output, session, con, db_path) {

  output$download_db <- downloadHandler(
    filename = function() {
      paste0("nutrition_", format(Sys.Date(), "%Y%m%d"), ".db")
    },
    content = function(file) {
      req(con())
      file.copy(db_path(), file)
    }
  )
}