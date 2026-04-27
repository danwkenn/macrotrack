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
                                meal_choices, user_choices,
                                ingredient_choices) {

  n_extra_ingredients <- 10

  # Toggle state for the 10 extra (hidden by default) slots
  show_extra_slots <- reactiveVal(FALSE)

  observeEvent(input$toggle_extra_slots, {
    show_extra_slots(!show_extra_slots())
    updateActionButton(
      session, "toggle_extra_slots",
      label = if (show_extra_slots()) "Hide extra slots" else "Show 10 more slots"
    )
  })

  output$show_extra_slots <- reactive({ show_extra_slots() })
  outputOptions(output, "show_extra_slots", suspendWhenHidden = FALSE)

  slot_row <- function(i, meal_choices) {
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
          choices  = c("None" = "", meal_choices),
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
  }

  # Render the meal planner UI once database is connected
  output$meal_planner <- renderUI({
    req(con())
    choices <- meal_choices()
    tagList(
      lapply(1:10, function(i) slot_row(i, choices)),
      br(),
      actionButton("toggle_extra_slots", "Show 10 more slots", class = "btn-default"),
      br(), br(),
      conditionalPanel(
        condition = "output.show_extra_slots == true",
        lapply(11:20, function(i) slot_row(i, choices))
      )
    )
  })

  # Render macros for each slot
  lapply(1:20, function(i) {
    output[[paste0("macros_", i)]] <- renderTable({
      req(con())
      meal_id  <- input[[paste0("meal_", i)]]
      servings <- input[[paste0("servings_", i)]]
      req(meal_id != "")
      get_meal_macros(con(), as.integer(meal_id), servings)
    })
  })

  # --- Additional individual ingredients ---
  # Toggle state for the (hidden by default) ingredient slots
  show_extra_ingredients <- reactiveVal(FALSE)

  observeEvent(input$toggle_extra_ingredients, {
    show_extra_ingredients(!show_extra_ingredients())
    updateActionButton(
      session, "toggle_extra_ingredients",
      label = if (show_extra_ingredients())
                "Hide individual ingredients"
              else
                "Add individual ingredients"
    )
  })

  output$show_extra_ingredients <- reactive({ show_extra_ingredients() })
  outputOptions(output, "show_extra_ingredients", suspendWhenHidden = FALSE)

  ingredient_slot_row <- function(i, choices_val) {
    fluidRow(
      column(5,
        selectizeInput(
          inputId  = paste0("extra_ing_", i),
          label    = paste("Ingredient", i),
          choices  = c("None" = "", choices_val),
          selected = "",
          options  = list(placeholder = "Search for an ingredient...")
        )
      ),
      column(2,
        numericInput(
          inputId = paste0("extra_portions_", i),
          label   = "Portions",
          value   = 1,
          min     = 0.5,
          step    = 0.5
        )
      ),
      column(5,
        tableOutput(paste0("extra_macros_", i))
      )
    )
  }

  output$extra_ingredients <- renderUI({
    req(con())
    choices <- ingredient_choices()
    tagList(
      actionButton("toggle_extra_ingredients",
                   "Add individual ingredients", class = "btn-default"),
      br(), br(),
      conditionalPanel(
        condition = "output.show_extra_ingredients == true",
        h4("Additional Ingredients"),
        lapply(seq_len(n_extra_ingredients),
               function(i) ingredient_slot_row(i, choices))
      )
    )
  })

  # Returns one row (ingredient, portions, calories, protein, carbs, fat) for
  # the additional-ingredient slot i, or NULL if the slot is empty/invalid.
  extra_ingredient_row <- function(i) {
    ingredient_id <- input[[paste0("extra_ing_", i)]]
    portions      <- input[[paste0("extra_portions_", i)]]
    if (is.null(ingredient_id) || ingredient_id == "") return(NULL)
    if (is.null(portions) || is.na(portions) || portions <= 0) return(NULL)

    ingredient <- DBI::dbGetQuery(
      con(),
      "SELECT name, calories, protein, carbs, fat, portion_size
       FROM ingredients
       WHERE ingredient_id = $1",
      params = list(as.integer(ingredient_id))
    )
    if (nrow(ingredient) == 0) return(NULL)

    factor <- (ingredient$portion_size / 100) * portions
    data.frame(
      ingredient = ingredient$name,
      portions   = portions,
      calories   = round(ingredient$calories * factor, 1),
      protein    = round(ingredient$protein  * factor, 1),
      carbs      = round(ingredient$carbs    * factor, 1),
      fat        = round(ingredient$fat      * factor, 1)
    )
  }

  # Combined extras across all slots: NULL if no slots populated.
  extra_ingredients_combined <- function() {
    rows <- lapply(seq_len(n_extra_ingredients), extra_ingredient_row)
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }

  # Per-slot macro tables
  lapply(seq_len(n_extra_ingredients), function(i) {
    output[[paste0("extra_macros_", i)]] <- renderTable({
      req(con())
      extra_ingredient_row(i)
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
    summaries <- lapply(1:20, function(i) {
      meal_id  <- input[[paste0("meal_", i)]]
      servings <- input[[paste0("servings_", i)]]
      if (is.null(meal_id) || meal_id == "") return(NULL)
      get_meal_macros(con(), as.integer(meal_id), servings)
    })
    summaries <- Filter(Negate(is.null), summaries)
    extras    <- extra_ingredients_combined()
    if (length(summaries) == 0 && is.null(extras)) return(NULL)

    if (length(summaries) > 0) {
      combined <- data.table::rbindlist(summaries)
      planned  <- combined[, .(
        calories = sum(calories),
        protein  = sum(protein),
        carbs    = sum(carbs),
        fat      = sum(fat)
      )]
    } else {
      planned <- data.table::data.table(
        calories = 0, protein = 0, carbs = 0, fat = 0
      )
    }
    if (!is.null(extras)) {
      planned$calories <- planned$calories + sum(extras$calories)
      planned$protein  <- planned$protein  + sum(extras$protein)
      planned$carbs    <- planned$carbs    + sum(extras$carbs)
      planned$fat      <- planned$fat      + sum(extras$fat)
    }

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
        user <- DBI::dbGetQuery(con(), "SELECT name FROM users WHERE user_id = $1",
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
      summaries <- lapply(1:20, function(i) {
        meal_id  <- input[[paste0("meal_", i)]]
        servings <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)
        get_meal_macros(con(), as.integer(meal_id), servings)
      })
      summaries <- Filter(Negate(is.null), summaries)
      extras    <- extra_ingredients_combined()

      if (length(summaries) > 0) {
        combined <- data.table::rbindlist(summaries)
        planned  <- combined[, .(
          calories = sum(calories),
          protein  = sum(protein),
          carbs    = sum(carbs),
          fat      = sum(fat)
        )]
      } else {
        planned <- data.table::data.table(
          calories = 0, protein = 0, carbs = 0, fat = 0
        )
      }
      if (!is.null(extras)) {
        planned$calories <- planned$calories + sum(extras$calories)
        planned$protein  <- planned$protein  + sum(extras$protein)
        planned$carbs    <- planned$carbs    + sum(extras$carbs)
        planned$fat      <- planned$fat      + sum(extras$fat)
      }
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
      slots_df <- do.call(rbind, lapply(1:20, function(i) {
        meal_id   <- input[[paste0("meal_", i)]]
        slot_name <- input[[paste0("slot_name_", i)]]
        servings  <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)
        meal_name <- DBI::dbGetQuery(con(), "SELECT name FROM meals WHERE meal_id = $1",
                                     params = list(as.integer(meal_id)))$name
        data.frame(
          slot_name = if (slot_name == "") paste("Slot", i) else slot_name,
          meal_name = meal_name,
          servings  = servings
        )
      }))
      if (!is.null(extras)) {
        slots_df <- rbind(slots_df, data.frame(
          slot_name = "Additional Ingredients",
          meal_name = "Additional Ingredients",
          servings  = 1
        ))
      }

      # --- Meal details list ---
      meal_details <- lapply(1:20, function(i) {
        meal_id   <- input[[paste0("meal_", i)]]
        slot_name <- input[[paste0("slot_name_", i)]]
        servings  <- input[[paste0("servings_", i)]]
        if (is.null(meal_id) || meal_id == "") return(NULL)

        meal_id_int <- as.integer(meal_id)
        meal_name   <- DBI::dbGetQuery(con(), "SELECT name FROM meals WHERE meal_id = $1",
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
          WHERE mi.meal_id = $1
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

      # Append Additional Ingredients as a synthetic "meal" entry
      if (!is.null(extras)) {
        totals_row <- data.frame(
          ingredient = "Total",
          portions   = NA,
          calories   = sum(extras$calories),
          protein    = sum(extras$protein),
          carbs      = sum(extras$carbs),
          fat        = sum(extras$fat)
        )
        meal_details <- c(meal_details, list(list(
          slot_name     = "Additional Ingredients",
          meal_name     = "Additional Ingredients",
          servings      = 1,
          is_additional = TRUE,
          ingredients   = rbind(extras, totals_row)
        )))
      }

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
    user <- DBI::dbGetQuery(con(), "SELECT * FROM users WHERE user_id = $1",
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
#' If the "Enter in kJ" checkbox is ticked, the calories field is
#' interpreted as kilojoules and converted to kcal before storage
#' (1 kcal = 4.184 kJ). The database always stores kcal.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param ingredient_refresh A reactiveVal used to trigger ingredient list refresh.
#'
#' @export
add_ingredients_server <- function(input, output, session, con, ingredient_refresh) {

  # Update calories label when kJ checkbox is toggled
  observeEvent(input$ingredient_calories_kj, {
    new_label <- if (isTRUE(input$ingredient_calories_kj)) {
      "Energy (kJ, per 100g)"
    } else {
      "Calories (per 100g)"
    }
    updateNumericInput(session, "ingredient_calories", label = new_label)
  }, ignoreInit = TRUE)

  observeEvent(input$add_ingredient_btn, {
    req(con())
    req(input$ingredient_name != "")

    tryCatch({

      # Convert kJ to kcal if the checkbox is ticked
      calories_kcal <- if (isTRUE(input$ingredient_calories_kj)) {
        input$ingredient_calories / 4.184
      } else {
        input$ingredient_calories
      }

      add_ingredient(
        con          = con(),
        name         = input$ingredient_name,
        calories     = calories_kcal,
        protein      = input$ingredient_protein,
        carbs        = input$ingredient_carbs,
        fat          = input$ingredient_fat,
        portion_size = input$ingredient_portion_size,
        portion_name = input$ingredient_portion_name
      )
      output$add_ingredient_status <- renderText("Ingredient added successfully.")
      ingredient_refresh(ingredient_refresh() + 1)

      # Reset form
      updateTextInput(session,     "ingredient_name",         value = "")
      updateNumericInput(session,  "ingredient_calories",     value = NA)
      updateCheckboxInput(session, "ingredient_calories_kj",  value = FALSE)
      updateNumericInput(session,  "ingredient_protein",      value = NA)
      updateNumericInput(session,  "ingredient_carbs",        value = NA)
      updateNumericInput(session,  "ingredient_fat",          value = NA)
      updateNumericInput(session,  "ingredient_portion_size", value = NA)
      updateTextInput(session,     "ingredient_portion_name", value = "")

    }, error = function(e) {
      output$add_ingredient_status <- renderText(paste("Error:", e$message))
    })
  })
}

#' Build Meal Tab Server
#'
#' Server logic for the build meal tab. Meals are built by filling
#' numbered ingredient slots. Each slot reads an ingredient id and a
#' portion count; macros are computed live per slot and totaled in
#' a summary table. On save, filled slots are inserted transactionally.
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

  n_slots <- 10

  # Render the slot UI once the database is connected
  output$build_meal_slots <- renderUI({
    req(con())
    choices_val <- ingredient_choices()
    tagList(
      lapply(seq_len(n_slots), function(i) {
        fluidRow(
          column(5,
            selectizeInput(
              inputId  = paste0("bm_ingredient_", i),
              label    = paste("Slot", i, "- Ingredient"),
              choices  = c("None" = "", choices_val),
              selected = "",
              options  = list(placeholder = "Search for an ingredient...")
            )
          ),
          column(2,
            numericInput(
              inputId = paste0("bm_portions_", i),
              label   = "Portions",
              value   = 1,
              min     = 0.5,
              step    = 0.5
            )
          ),
          column(5,
            tableOutput(paste0("bm_macros_", i))
          )
        )
      })
    )
  })

  # Helper: compute macros for a given slot i (returns NULL if empty)
  slot_macros <- function(i) {
    ingredient_id <- input[[paste0("bm_ingredient_", i)]]
    portions      <- input[[paste0("bm_portions_", i)]]
    if (is.null(ingredient_id) || ingredient_id == "") return(NULL)
    if (is.null(portions) || is.na(portions) || portions <= 0) return(NULL)

    ingredient <- DBI::dbGetQuery(
      con(),
      "SELECT name, calories, protein, carbs, fat, portion_size
       FROM ingredients
       WHERE ingredient_id = $1",
      params = list(as.integer(ingredient_id))
    )
    if (nrow(ingredient) == 0) return(NULL)

    factor <- (ingredient$portion_size / 100) * portions
    data.frame(
      Ingredient = ingredient$name,
      Portions   = portions,
      Calories   = round(ingredient$calories * factor, 1),
      Protein    = round(ingredient$protein  * factor, 1),
      Carbs      = round(ingredient$carbs    * factor, 1),
      Fat        = round(ingredient$fat      * factor, 1)
    )
  }

  # Per-slot macro tables
  lapply(seq_len(n_slots), function(i) {
    output[[paste0("bm_macros_", i)]] <- renderTable({
      req(con())
      slot_macros(i)
    })
  })

  # Update portion input label when ingredient changes
  lapply(seq_len(n_slots), function(i) {
    observeEvent(input[[paste0("bm_ingredient_", i)]], {
      req(con())
      ingredient_id <- input[[paste0("bm_ingredient_", i)]]

      new_label <- if (is.null(ingredient_id) || ingredient_id == "") {
        "Portion"
      } else {
        ingredient <- DBI::dbGetQuery(
          con(),
          "SELECT portion_name FROM ingredients WHERE ingredient_id = $1",
          params = list(as.integer(ingredient_id))
        )
        if (nrow(ingredient) == 0) {
          "Portion"
        } else {
          paste0("Portion (", ingredient$portion_name, ")")
        }
      }

      updateNumericInput(session, paste0("bm_portions_", i), label = new_label)
    }, ignoreInit = TRUE)
  })

  # Summary table: totals from slots, plus override + difference if enabled
  output$build_meal_summary <- renderTable({
    req(con())

    rows <- lapply(seq_len(n_slots), slot_macros)
    rows <- Filter(Negate(is.null), rows)

    if (length(rows) == 0) {
      totals <- c(Calories = 0, Protein = 0, Carbs = 0, Fat = 0)
    } else {
      combined <- do.call(rbind, rows)
      totals <- c(
        Calories = sum(combined$Calories),
        Protein  = sum(combined$Protein),
        Carbs    = sum(combined$Carbs),
        Fat      = sum(combined$Fat)
      )
    }

    summary_df <- data.frame(
      Macro = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
      Total = round(as.numeric(totals), 1)
    )

    # Add override and difference columns when override mode is active
    if (isTRUE(!input$build_use_ingredient_macros)) {
      overrides <- c(
        if (is.null(input$build_calories) || is.na(input$build_calories)) NA_real_ else input$build_calories,
        if (is.null(input$build_protein)  || is.na(input$build_protein))  NA_real_ else input$build_protein,
        if (is.null(input$build_carbs)    || is.na(input$build_carbs))    NA_real_ else input$build_carbs,
        if (is.null(input$build_fat)      || is.na(input$build_fat))      NA_real_ else input$build_fat
      )
      diffs <- overrides - summary_df$Total
      summary_df$Override   <- round(overrides, 1)
      summary_df$Difference <- ifelse(
        is.na(diffs), NA_character_,
        ifelse(diffs >= 0, paste0("+", round(diffs, 1)), as.character(round(diffs, 1)))
      )
    }

    summary_df
  })

  # Save meal to database
  observeEvent(input$save_meal_btn, {
    req(con())
    req(input$build_meal_name != "")

    # Collect filled slots as (ingredient_id, portions) pairs
    filled <- lapply(seq_len(n_slots), function(i) {
      ingredient_id <- input[[paste0("bm_ingredient_", i)]]
      portions      <- input[[paste0("bm_portions_", i)]]
      if (is.null(ingredient_id) || ingredient_id == "")     return(NULL)
      if (is.null(portions) || is.na(portions) || portions <= 0) return(NULL)
      data.frame(
        ingredient_id = as.integer(ingredient_id),
        quantity      = portions
      )
    })
    filled <- Filter(Negate(is.null), filled)

    if (length(filled) == 0) {
      output$build_meal_status <- renderText("Error: no ingredients selected.")
      return()
    }

    meal_ingredients_df <- do.call(rbind, filled)

    tryCatch({

      DBI::dbWithTransaction(con(), {

        # Insert the meal, capture its new ID
        meal_result <- DBI::dbGetQuery(
          con(),
          "INSERT INTO meals
             (name, use_ingredient_macros, calories, protein, carbs, fat)
           VALUES ($1, $2, $3, $4, $5, $6)
           RETURNING meal_id",
          params = list(
            input$build_meal_name,
            as.integer(input$build_use_ingredient_macros),
            if (is.null(input$build_calories) || is.na(input$build_calories)) NA_real_ else input$build_calories,
            if (is.null(input$build_protein)  || is.na(input$build_protein))  NA_real_ else input$build_protein,
            if (is.null(input$build_carbs)    || is.na(input$build_carbs))    NA_real_ else input$build_carbs,
            if (is.null(input$build_fat)      || is.na(input$build_fat))      NA_real_ else input$build_fat
          )
        )
        new_meal_id <- meal_result$meal_id

        # Link selected ingredients to the new meal
        meal_ingredients_df$meal_id <- new_meal_id
        meal_ingredients_df <- meal_ingredients_df[, c("meal_id", "ingredient_id", "quantity")]
        DBI::dbAppendTable(con(), "meal_ingredients", meal_ingredients_df)
      })

      output$build_meal_status <- renderText("Meal saved successfully.")
      meal_refresh(meal_refresh() + 1)

      # Reset form
      updateTextInput(session, "build_meal_name", value = "")
      for (i in seq_len(n_slots)) {
        updateSelectizeInput(session, paste0("bm_ingredient_", i), selected = "")
        updateNumericInput(session,  paste0("bm_portions_", i),    value = 1)
      }

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
#' If the "Enter in kJ" checkbox is ticked, the calories field is
#' interpreted as kilojoules and converted to kcal before storage
#' (1 kcal = 4.184 kJ). The database always stores kcal.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param ingredient_refresh A reactiveVal used to trigger ingredient list refresh.
#'
#' @export
add_ingredient_by_serving_server <- function(input, output, session, con, ingredient_refresh) {

  # Live preview of portion size in grams
  output$portion_preview <- renderText({
    req(input$serving_size_g, input$portion_factor)
    portion_g <- round(input$serving_size_g * input$portion_factor, 1)
    paste0("One portion = ", portion_g, "g")
  })

  # Update calories label when kJ checkbox is toggled
  observeEvent(input$serving_calories_kj, {
    new_label <- if (isTRUE(input$serving_calories_kj)) {
      "Energy per serving (kJ)"
    } else {
      "Calories per serving"
    }
    updateNumericInput(session, "serving_calories", label = new_label)
  }, ignoreInit = TRUE)

  observeEvent(input$add_ingredient_serving_btn, {
    req(con())
    req(input$serving_name != "")

    tryCatch({

      # Convert kJ to kcal if the checkbox is ticked
      calories_kcal <- if (isTRUE(input$serving_calories_kj)) {
        input$serving_calories / 4.184
      } else {
        input$serving_calories
      }

      # Convert to per-100g values
      portion_size <- input$serving_size_g * input$portion_factor
      multiplier   <- 100 / input$serving_size_g

      add_ingredient(
        con          = con(),
        name         = input$serving_name,
        calories     = round(calories_kcal              * multiplier, 2),
        protein      = round(input$serving_protein      * multiplier, 2),
        carbs        = round(input$serving_carbs        * multiplier, 2),
        fat          = round(input$serving_fat          * multiplier, 2),
        portion_size = round(portion_size, 1),
        portion_name = input$serving_portion_name
      )

      output$add_ingredient_serving_status <- renderText("Ingredient added successfully.")
      ingredient_refresh(ingredient_refresh() + 1)

      # Reset form
      updateTextInput(session,     "serving_name",         value = "")
      updateTextInput(session,     "serving_portion_name", value = "")
      updateNumericInput(session,  "serving_size_g",       value = NA)
      updateNumericInput(session,  "serving_calories",     value = NA)
      updateCheckboxInput(session, "serving_calories_kj",  value = FALSE)
      updateNumericInput(session,  "serving_protein",      value = NA)
      updateNumericInput(session,  "serving_carbs",        value = NA)
      updateNumericInput(session,  "serving_fat",          value = NA)
      updateNumericInput(session,  "portion_factor",       value = 1)

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

      DBI::dbWithTransaction(con(), {

        # Insert the plan, capture its new ID
        plan_result <- DBI::dbGetQuery(
          con(),
          "INSERT INTO plans (name, description, pct_to_plan)
           VALUES ($1, $2, $3)
           RETURNING plan_id",
          params = list(
            input$plan_name,
            if (input$plan_description == "") NA_character_ else input$plan_description,
            input$plan_pct_to_plan / 100
          )
        )
        new_plan_id <- plan_result$plan_id

        # Link the meals to the new plan
        plan_meals_df <- data.frame(
          plan_id   = new_plan_id,
          slot_name = plan_meals()$slot_name,
          meal_id   = plan_meals()$meal_id,
          servings  = plan_meals()$servings
        )
        DBI::dbAppendTable(con(), "plan_meals", plan_meals_df)
      })

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
#' Server logic for the settings tab. Provides three admin actions:
#' initialise DB tables, download all data as a CSV ZIP, and drop
#' all tables (with a typed confirmation).
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
settings_server <- function(input, output, session, con,
                            user_refresh, meal_refresh,
                            plan_refresh, ingredient_refresh) {

  # Helper to bump all refresh counters
  bump_all <- function() {
    user_refresh(user_refresh() + 1)
    meal_refresh(meal_refresh() + 1)
    plan_refresh(plan_refresh() + 1)
    ingredient_refresh(ingredient_refresh() + 1)
  }

  # --- Initialise new DB ---
  observeEvent(input$initialise_db_btn, {
    req(con())
    tryCatch({
      initialise_db(con())
      output$initialise_db_status <- renderText("Tables created (or already existed).")
      bump_all()
    }, error = function(e) {
      output$initialise_db_status <- renderText(paste("Error:", e$message))
    })
  })

  # --- Download all data as CSV ZIP ---
  output$download_db_zip <- downloadHandler(
    filename = function() {
      paste0("nutrition_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      req(con())

      tables <- c("ingredients", "meals", "meal_ingredients",
                  "users", "plans", "plan_meals")

      tmp_dir <- tempfile("nutrition_dump_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      csv_paths <- character()
      for (tbl in tables) {
        df <- tryCatch(
          DBI::dbGetQuery(con(), paste0("SELECT * FROM ", tbl)),
          error = function(e) NULL
        )
        if (is.null(df)) next
        csv_path <- file.path(tmp_dir, paste0(tbl, ".csv"))
        write.csv(df, csv_path, row.names = FALSE)
        csv_paths <- c(csv_paths, csv_path)
      }

      # Zip the CSVs. Use relative paths inside the archive.
      old_wd <- setwd(tmp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      utils::zip(zipfile = file, files = basename(csv_paths))
    }
  )

  # --- Remove all tables (with confirmation modal) ---
  observeEvent(input$drop_all_tables_btn, {
    showModal(modalDialog(
      title = "Remove all tables",
      p("This will permanently destroy all tables and their data."),
      p("To confirm, type YES below and click Remove."),
      textInput("drop_all_confirm", label = NULL, placeholder = "Type YES"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("drop_all_confirm_btn", "Remove", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$drop_all_confirm_btn, {
    req(con())
    if (!isTRUE(input$drop_all_confirm == "YES")) {
      output$drop_all_tables_status <- renderText(
        "Confirmation failed: you must type YES exactly."
      )
      removeModal()
      return()
    }
    tryCatch({
      drop_all_tables(con(), confirm = TRUE)
      output$drop_all_tables_status <- renderText("All tables removed.")
      bump_all()
    }, error = function(e) {
      output$drop_all_tables_status <- renderText(paste("Error:", e$message))
    })
    removeModal()
  })
}

#' Deduplicate Ingredients Tab Server
#'
#' Server logic for the deduplicate ingredients tab. Users select
#' ingredients to dedupe, pick a keeper, preview the changes, then
#' confirm. The dropdown lists only ingredients referenced in
#' meal_ingredients.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param con A reactiveVal containing the database connection.
#' @param ingredient_refresh A reactiveVal used to trigger ingredient list refresh.
#' @param meal_refresh A reactiveVal used to trigger meal list refresh.
#'
#' @export
deduplicate_ingredients_server <- function(input, output, session, con,
                                           ingredient_refresh, meal_refresh) {

  # Preview data is held here. Confirm is only enabled when non-NULL.
  preview_data <- reactiveVal(NULL)

  # Ingredient list (only those used in at least one meal), refreshed
  # on ingredient_refresh or meal_refresh changes.
  used_ingredients <- reactive({
    req(con())
    ingredient_refresh()
    meal_refresh()
    DBI::dbGetQuery(con(), "
      SELECT i.ingredient_id, i.name, i.portion_size, i.portion_name
      FROM ingredients i
      WHERE i.ingredient_id IN (
        SELECT DISTINCT ingredient_id FROM meal_ingredients
      )
      ORDER BY i.name, i.ingredient_id
    ")
  })

  # Multi-select of ingredients to consider. Label disambiguates
  # duplicates sharing a name by including the id and portion size.
  output$dedupe_select_ui <- renderUI({
    req(con())
    ing <- used_ingredients()
    choices <- setNames(
      ing$ingredient_id,
      paste0(ing$name, " (id=", ing$ingredient_id,
             ", ", ing$portion_size, "g per ", ing$portion_name, ")")
    )
    selectizeInput(
      inputId  = "dedupe_selected",
      label    = "Select ingredients to dedupe (2 or more)",
      choices  = choices,
      multiple = TRUE,
      options  = list(placeholder = "Search ingredients...")
    )
  })

  # Keeper radio: appears only once 2+ ingredients are selected.
  output$dedupe_keeper_ui <- renderUI({
    req(con())
    selected <- input$dedupe_selected
    if (is.null(selected) || length(selected) < 2) {
      return(helpText("Select at least two ingredients to choose a keeper."))
    }
    ing <- used_ingredients()
    ing <- ing[ing$ingredient_id %in% as.integer(selected), , drop = FALSE]
    choices <- setNames(
      ing$ingredient_id,
      paste0(ing$name, " (id=", ing$ingredient_id,
             ", ", ing$portion_size, "g per ", ing$portion_name, ")")
    )
    radioButtons(
      inputId  = "dedupe_keeper",
      label    = "Which one to keep",
      choices  = choices,
      selected = character(0)
    )
  })

  # Clear preview whenever the selection or keeper changes, so the user
  # can't preview one set and confirm against another.
  observeEvent(input$dedupe_selected, { preview_data(NULL) }, ignoreInit = TRUE)
  observeEvent(input$dedupe_keeper,   { preview_data(NULL) }, ignoreInit = TRUE)

  # Preview button
  observeEvent(input$dedupe_preview_btn, {
    req(con())
    selected <- input$dedupe_selected
    keeper   <- input$dedupe_keeper

    if (is.null(selected) || length(selected) < 2) {
      output$dedupe_status <- renderText("Select at least two ingredients.")
      preview_data(NULL)
      return()
    }
    if (is.null(keeper) || keeper == "") {
      output$dedupe_status <- renderText("Choose which ingredient to keep.")
      preview_data(NULL)
      return()
    }

    keep_id  <- as.integer(keeper)
    drop_ids <- setdiff(as.integer(selected), keep_id)

    tryCatch({
      pv <- preview_dedupe_ingredients(con(), keep_id, drop_ids)
      if (nrow(pv) == 0) {
        output$dedupe_status <- renderText(
          "No meals reference the selected duplicates. Nothing to do."
        )
        preview_data(NULL)
      } else {
        preview_data(list(preview = pv, keep_id = keep_id, drop_ids = drop_ids))
        output$dedupe_status <- renderText(
          paste0("Preview ready: ", nrow(pv),
                 " meal(s) will be updated. Click Confirm Dedupe to apply.")
        )
      }
    }, error = function(e) {
      output$dedupe_status <- renderText(paste("Error:", e$message))
      preview_data(NULL)
    })
  })

  # Preview table
  output$dedupe_preview_table <- renderTable({
    pd <- preview_data()
    req(pd)
    pd$preview[, c("meal_name", "before", "after",
                   "grams_before_total", "grams_after")]
  })

  # Confirm button
  observeEvent(input$dedupe_confirm_btn, {
    req(con())
    pd <- preview_data()
    if (is.null(pd)) {
      output$dedupe_status <- renderText(
        "Nothing to confirm. Click Preview first."
      )
      return()
    }

    tryCatch({
      result <- dedupe_ingredients(con(), pd$keep_id, pd$drop_ids)
      output$dedupe_status <- renderText(sprintf(
        "Dedupe complete: %d meal_ingredients rows rewritten, %d ingredient(s) removed.",
        result[["rows_rewritten"]], result[["rows_deleted"]]
      ))
      preview_data(NULL)
      ingredient_refresh(ingredient_refresh() + 1)
      meal_refresh(meal_refresh() + 1)
    }, error = function(e) {
      output$dedupe_status <- renderText(paste("Error:", e$message))
    })
  })
}