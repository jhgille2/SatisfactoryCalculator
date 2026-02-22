##################################################
## Satisfactory Calculator Shiny App
## Purpose: Interactive factory optimization interface with user authentication
##################################################

# Load all required packages - explicit for rsconnect deployment
library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(tidyr)
library(jsonlite)
library(igraph)
library(visNetwork)
library(lpSolve)
library(purrr)
library(tibble)
library(here)
library(RSQLite)
library(DBI)
library(digest)
library(shinyjs)

# Load all R functions from the project
lapply(list.files("./R", full.names = TRUE), source)

# Initialize database
init_database()

# Load recipe data once at app startup
recipe_json_path <- here::here("data", "recipe_jsons", "en-US_utf8.json")
recipe_data <- clean_recipe_json(recipe_json_path)

# Get list of available products and resources for UI
available_products <- recipe_data$AllRecipes %>% 
  select(product_item, name) %>% 
  distinct() %>% 
  arrange(product_item) %>% 
  pull(product_item)

available_resources <- recipe_data$AllRecipes %>% 
  filter(ingredient_item != "") %>%
  select(ingredient_item) %>% 
  distinct() %>% 
  arrange(ingredient_item) %>% 
  pull(ingredient_item)

available_recipes <- recipe_data$AllRecipes %>% 
  select(slug, name) %>% 
  distinct() %>% 
  arrange(slug) %>% 
  pull(slug)

default_alternates <- c("alternate:-iron-wire")

##################################################
## UI Definition
##################################################

ui <- dashboardPage(
  # Header
  dashboardHeader(title = "Satisfactory Factory Calculator"),
  
  # Sidebar with conditional menu based on login state
  dashboardSidebar(
    uiOutput("sidebar_menu")
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tabItems(
      ##################################################
      ## Login Tab
      ##################################################
      tabItem(tabName = "login",
              fluidRow(
                column(4, offset = 4,
                       box(title = "Satisfactory Calculator Login", 
                           status = "primary",
                           solidHeader = TRUE,
                           width = 12,
                           style = "margin-top: 100px;",
                           textInput("login_username", "Username:", placeholder = "Enter username"),
                           passwordInput("login_password", "Password:", placeholder = "Enter password"),
                           br(),
                           actionButton("login_button", "Login", class = "btn-primary", width = "100%"),
                           br(), br(),
                           uiOutput("login_message"),
                           br(),
                           tags$div(
                             "New user? ",
                             actionLink("nav_to_register", "Create an account"),
                             style = "text-align: center; margin-top: 15px;"
                           )
                       )
                )
              )
      ),
      
      ##################################################
      ## Register Tab
      ##################################################
      tabItem(tabName = "register",
              fluidRow(
                column(4, offset = 4,
                       box(title = "Create Account", 
                           status = "success",
                           solidHeader = TRUE,
                           width = 12,
                           style = "margin-top: 100px;",
                           textInput("register_username", "Username:", placeholder = "Enter username"),
                           passwordInput("register_password", "Password:", placeholder = "Enter password"),
                           passwordInput("register_confirm", "Confirm Password:", placeholder = "Confirm password"),
                           br(),
                           actionButton("register_button", "Create Account", class = "btn-success", width = "100%"),
                           br(), br(),
                           uiOutput("register_message"),
                           br(),
                           tags$div(
                             "Already have an account? ",
                             actionLink("nav_to_login", "Back to login"),
                             style = "text-align: center; margin-top: 15px;"
                           )
                       )
                )
              )
      ),
      
      ##################################################
      ## Available Resources and Alternate Recipes Tab
      ##################################################
      tabItem(tabName = "resources",
              fluidRow(
                column(12,
                       box(title = "Manage Available Resources",
                           status = "success",
                           solidHeader = TRUE,
                           width = 12,
                           tags$p("Logged in as: ", tags$strong(textOutput("current_username", inline = TRUE))),
                           br(),
                           uiOutput("resource_management"),
                           br(),
                           actionButton("save_resources_db", "Save Resources to Account", 
                                       class = "btn-success", icon = icon("save")),
                           actionButton("load_resources_db", "Load Saved Resources", 
                                       class = "btn-info", icon = icon("upload")),
                           br(), br(),
                           uiOutput("resource_save_message"),
                           br(), br()
                       ),
                       
                       box(title = "Manage Alternate Recipes",
                           status = "warning",
                           solidHeader = TRUE,
                           width = 12,
                           tags$p("Add any alternate recipes you've unlocked:"),
                           br(),
                           uiOutput("saved_alternate_recipes_management"),
                           br(),
                           actionButton("add_saved_alternate", "Add Recipe", 
                                       class = "btn-warning", icon = icon("plus")),
                           br(), br(),
                           actionButton("save_alternates_db", "Save Recipes to Account", 
                                       class = "btn-warning", icon = icon("save")),
                           actionButton("load_alternates_db", "Load Saved Recipes", 
                                       class = "btn-info", icon = icon("upload")),
                           br(), br(),
                           uiOutput("alternate_save_message"),
                           br(), br(),
                           actionButton("logout_button", "Logout", 
                                       class = "btn-danger", icon = icon("sign-out"))
                       )
                )
              )
      ),
      
      ##################################################
      ## Optimizer Tab
      ##################################################
      tabItem(tabName = "optimizer",
              fluidRow(
                # Input column
                column(4,
                       box(title = "Products to Produce", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           uiOutput("product_inputs"),
                           actionButton("add_product", "Add Product", 
                                       class = "btn-primary", 
                                       icon = icon("plus")),
                           br(), br()
                       ),
                       
                       box(title = "Available Resources", 
                           status = "success", 
                           solidHeader = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           uiOutput("resource_inputs"),
                           actionButton("add_resource", "Add Resource", 
                                       class = "btn-success", 
                                       icon = icon("plus")),
                           br(), br()
                       ),
                       
                       box(title = "Alternate Recipes", 
                           status = "warning", 
                           solidHeader = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           uiOutput("alternate_inputs"),
                           actionButton("add_alternate", "Add Recipe", 
                                       class = "btn-warning", 
                                       icon = icon("plus")),
                           br(), br()
                       ),
                       
                       box(title = "Optimization Settings", 
                           status = "info", 
                           solidHeader = TRUE,
                           width = 12,
                           checkboxInput("whole_factories", 
                                        "Integer Factories", 
                                        value = FALSE),
                           checkboxInput("slack", 
                                        "Allow Slack (produce extra items)", 
                                        value = TRUE),
                           br(),
                           actionButton("optimize", "Run Optimization", 
                                       class = "btn-danger", 
                                       size = "lg",
                                       icon = icon("play")),
                           br(), br(),
                           uiOutput("status_message", style = "max-height: 300px; overflow-y: auto; padding: 10px; background-color: #f5f5f5; border-radius: 4px; word-wrap: break-word;"),
                           br(),
                           actionButton("logout_optimizer", "Logout", 
                                       class = "btn-danger", icon = icon("sign-out"))
                       )
                ),
                
                # Results column with larger graph
                column(8,
                       box(title = "Factory Network Visualization", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           height = 1100,
                           visNetworkOutput("factory_graph", height = "1050px")
                       ),
                       
                       box(title = "Factory Statistics", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           height = 300,
                           uiOutput("factory_stats")
                       )
                )
              )
      ),
      
      ##################################################
      ## About Tab
      ##################################################
      tabItem(tabName = "about",
              box(width = 12,
                  h2("About Satisfactory Factory Calculator"),
                  p("This app helps you optimize factory designs for the game Satisfactory."),
                  h3("How to Use:"),
                  tags$ol(
                    tags$li("Select products you want to produce and enter required quantities"),
                    tags$li("Specify available resources with negative values"),
                    tags$li("Add any alternate recipes you've unlocked"),
                    tags$li("Click 'Run Optimization' to find the optimal factory configuration"),
                    tags$li("The resulting network shows recipes and production rates")
                  ),
                  h3("Notes:"),
                  tags$ul(
                    tags$li("Integer Factories: Check this to ensure whole factories (no underclocking)"),
                    tags$li("Allow Slack: Check this to allow producing extra intermediates beyond requirements"),
                    tags$li("Water is effectively unlimited (9007199254740991)")
                  )
              )
      )
    )
  )
)

##################################################
## Server Logic
##################################################

server <- function(input, output, session) {
  
  # Authentication reactive values
  auth_state <- reactiveVal(list(logged_in = FALSE, user_id = NULL, username = NULL))
  
  # Render sidebar menu based on login state
  output$sidebar_menu <- renderUI({
    if (auth_state()$logged_in) {
      sidebarMenu(
        menuItem("Optimizer", tabName = "optimizer", icon = icon("cogs")),
        menuItem("Available Resources and Recipes", tabName = "resources", icon = icon("database")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    } else {
      sidebarMenu(
        menuItem("Login", tabName = "login", icon = icon("sign-in")),
        menuItem("Register", tabName = "register", icon = icon("user-plus")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    }
  })
  
  # Navigation observers for auth links
  observeEvent(input$nav_to_register, {
    updateTabItems(session, "register")
  })
  
  observeEvent(input$nav_to_login, {
    updateTabItems(session, "login")
  })
  
  # Display current username
  output$current_username <- renderText({
    if (auth_state()$logged_in) {
      auth_state()$username
    } else {
      "Not logged in"
    }
  })
  
  # Login button handler
  observeEvent(input$login_button, {
    username <- input$login_username
    password <- input$login_password
    
    if (username == "" || password == "") {
      output$login_message <- renderUI({
        tags$div(class = "alert alert-warning", "Please enter username and password")
      })
      return()
    }
    
    auth_result <- authenticate_user(username, password)
    
    if (auth_result$success) {
      auth_state(list(logged_in = TRUE, user_id = auth_result$user_id, username = username))
      
      # Load saved alternate recipes for this user
      saved_alts <- load_user_alternate_recipes(auth_result$user_id)
      saved_alternates(saved_alts)
      
      # Populate the alternates form with saved recipes
      if (length(saved_alts) > 0) {
        new_alternates <- data.frame(id = seq_along(saved_alts), recipe = saved_alts, stringsAsFactors = FALSE)
        alternates(new_alternates)
        alternate_counter(length(saved_alts))
      }
      
      output$login_message <- renderUI({
        tags$div(class = "alert alert-success", "Login successful! Redirecting...")
      })
      # Redirect to optimizer tab
      shinyjs::delay(500, updateTabItems(session, "optimizer"))
    } else {
      output$login_message <- renderUI({
        tags$div(class = "alert alert-danger", auth_result$message)
      })
    }
  })
  
  # Register button handler
  observeEvent(input$register_button, {
    username <- input$register_username
    password <- input$register_password
    confirm <- input$register_confirm
    
    if (username == "" || password == "" || confirm == "") {
      output$register_message <- renderUI({
        tags$div(class = "alert alert-warning", "Please fill in all fields")
      })
      return()
    }
    
    if (password != confirm) {
      output$register_message <- renderUI({
        tags$div(class = "alert alert-warning", "Passwords do not match")
      })
      return()
    }
    
    if (nchar(password) < 6) {
      output$register_message <- renderUI({
        tags$div(class = "alert alert-warning", "Password must be at least 6 characters")
      })
      return()
    }
    
    result <- create_user(username, password)
    
    if (result$success) {
      output$register_message <- renderUI({
        tags$div(class = "alert alert-success", 
                 "Account created successfully! Redirecting to login...")
      })
      shinyjs::delay(1000, {
        updateTabItems(session, "login")
        updateTextInput(session, "login_username", value = username)
        updateTextInput(session, "login_password", value = "")
      })
    } else {
      output$register_message <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })
  
  # Logout handlers
  observeEvent(input$logout_button, {
    auth_state(list(logged_in = FALSE, user_id = NULL, username = NULL))
    updateTabItems(session, "login")
  })
  
  observeEvent(input$logout_optimizer, {
    auth_state(list(logged_in = FALSE, user_id = NULL, username = NULL))
    updateTabItems(session, "login")
  })
  
  ##################################################
  ## Resources Management Section
  ##################################################
  
  output$resource_management <- renderUI({
    if (!auth_state()$logged_in) {
      return(tags$p("Please log in to manage resources", style = "color: red;"))
    }
    
    raw_resources <- c(
      "Desc_Coal_C",
      "Desc_OreIron_C",
      "Desc_OreCopper_C",
      "Desc_OreGold_C",
      "Desc_OreUranium_C",
      "Desc_Stone_C",
      "Desc_RawQuartz_C",
      "Desc_Sulfur_C",
      "Desc_NitrogenGas_C",
      "Desc_LiquidOil_C",
      "Desc_Water_C"
    )
    
    # Create input fields for each resource
    resource_inputs <- lapply(seq_along(raw_resources), function(i) {
      fluidRow(
        column(8,
               tags$label(raw_resources[i], style = "font-weight: bold;")
        ),
        column(4,
               numericInput(paste0("res_manage_", i), "", value = 0, min = 0)
        ),
        br()
      )
    })
    
    do.call(tagList, resource_inputs)
  })
  
  # Save resources to database
  observeEvent(input$save_resources_db, {
    if (!auth_state()$logged_in) {
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-danger", "Please log in first")
      })
      return()
    }
    
    raw_resources <- c(
      "Desc_Coal_C",
      "Desc_OreIron_C",
      "Desc_OreCopper_C",
      "Desc_OreGold_C",
      "Desc_OreUranium_C",
      "Desc_Stone_C",
      "Desc_RawQuartz_C",
      "Desc_Sulfur_C",
      "Desc_NitrogenGas_C",
      "Desc_LiquidOil_C",
      "Desc_Water_C"
    )
    
    # Collect resource values
    resources_to_save <- data.frame(
      item = raw_resources,
      qty = sapply(seq_along(raw_resources), function(i) {
        input[[paste0("res_manage_", i)]] %||% 0
      }),
      stringsAsFactors = FALSE
    )
    
    result <- save_user_resources(auth_state()$user_id, resources_to_save)
    
    if (result$success) {
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-success", "Resources saved successfully!")
      })
    } else {
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })
  
  # Load resources from database
  observeEvent(input$load_resources_db, {
    if (!auth_state()$logged_in) {
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-danger", "Please log in first")
      })
      return()
    }
    
    user_resources <- load_user_resources(auth_state()$user_id)
    
    if (!is.null(user_resources)) {
      # Update resource inputs
      for (i in 1:nrow(user_resources)) {
        updateNumericInput(session, paste0("res_manage_", i), value = user_resources[i, "qty"])
      }
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-info", "Resources loaded successfully!")
      })
    } else {
      output$resource_save_message <- renderUI({
        tags$div(class = "alert alert-warning", "No saved resources found")
      })
    }
  })
  
  ##################################################
  ## Alternate Recipes Management Section
  ##################################################
  
  output$saved_alternate_recipes_management <- renderUI({
    if (!auth_state()$logged_in) {
      return(tags$p("Please log in to manage alternate recipes", style = "color: red;"))
    }
    
    saved_alts <- saved_alternates()
    
    if (length(saved_alts) == 0) {
      return(tags$p("No recipes saved yet. Add one below.", style = "color: #666;"))
    }
    
    # Create inputs for each saved recipe
    recipe_inputs <- lapply(seq_along(saved_alts), function(i) {
      fluidRow(
        column(11,
               selectizeInput(paste0("saved_alternate_recipe_", i),
                             label = NULL,
                             choices = c("Select Recipe..." = "", available_recipes),
                             selected = saved_alts[i])
        ),
        column(1,
               actionButton(paste0("remove_saved_alternate_", i),
                           label = "×",
                           class = "btn-sm btn-danger",
                           style = "margin-top: 25px;")
        ),
        br()
      )
    })
    
    do.call(tagList, recipe_inputs)
  })
  
  # Add saved alternate Recipe
  observeEvent(input$add_saved_alternate, {
    saved_alts <- saved_alternates()
    # Save current values first
    for (i in seq_along(saved_alts)) {
      recipe_val <- input[[paste0("saved_alternate_recipe_", i)]]
      if (!is.null(recipe_val) && recipe_val != "") {
        saved_alts[i] <- recipe_val
      }
    }
    # Add new empty entry
    saved_alts <- c(saved_alts, "")
    saved_alternates(saved_alts)
  })
  
  # Remove saved alternate recipe observer (dynamic)
  observe({
    saved_alts <- saved_alternates()
    for (i in seq_along(saved_alts)) {
      local({
        idx <- i
        observeEvent(input[[paste0("remove_saved_alternate_", idx)]], {
          current_alts <- saved_alternates()
          if (length(current_alts) > 0 && idx <= length(current_alts)) {
            current_alts <- current_alts[-idx]
            saved_alternates(current_alts)
          }
        })
      })
    }
  })
  
  # Save alternate recipes to database
  observeEvent(input$save_alternates_db, {
    if (!auth_state()$logged_in) {
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-danger", "Please log in first")
      })
      return()
    }
    
    # Collect current recipe values from inputs
    saved_alts <- saved_alternates()
    recipes_to_save <- character(0)
    
    for (i in seq_along(saved_alts)) {
      recipe_val <- input[[paste0("saved_alternate_recipe_", i)]]
      if (!is.null(recipe_val) && recipe_val != "") {
        recipes_to_save <- c(recipes_to_save, recipe_val)
      }
    }
    
    result <- save_user_alternate_recipes(auth_state()$user_id, recipes_to_save)
    
    if (result$success) {
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-success", "Alternate recipes saved successfully!")
      })
      # Update the saved_alternates reactiveVal
      saved_alternates(recipes_to_save)
    } else {
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })
  
  # Load alternate recipes from database
  observeEvent(input$load_alternates_db, {
    if (!auth_state()$logged_in) {
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-danger", "Please log in first")
      })
      return()
    }
    
    user_alternates <- load_user_alternate_recipes(auth_state()$user_id)
    
    if (length(user_alternates) > 0) {
      saved_alternates(user_alternates)
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-info", "Alternate recipes loaded successfully!")
      })
    } else {
      saved_alternates(character(0))
      output$alternate_save_message <- renderUI({
        tags$div(class = "alert alert-warning", "No saved alternate recipes found")
      })
    }
  })
  
  # Reactive values for dynamic inputs
  products <- reactiveVal(data.frame(id = 1, item = "", qty = 100, stringsAsFactors = FALSE))
  
  # Initialize resources with all raw resources
  raw_resources <- c(
    "Desc_Coal_C",
    "Desc_OreIron_C",
    "Desc_OreCopper_C",
    "Desc_OreGold_C",
    "Desc_OreUranium_C",
    "Desc_Stone_C",
    "Desc_RawQuartz_C",
    "Desc_Sulfur_C",
    "Desc_NitrogenGas_C",
    "Desc_LiquidOil_C",
    "Desc_Water_C"
  )
  
  resources_initial <- data.frame(
    id = seq_along(raw_resources),
    item = raw_resources,
    qty = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9999999),  # Water set to 9999999 by default
    stringsAsFactors = FALSE
  )
  
  resources <- reactiveVal(resources_initial)
  alternates <- reactiveVal(data.frame(id = 1, recipe = "", stringsAsFactors = FALSE))
  saved_alternates <- reactiveVal(character(0))  # Store saved alternate recipes from database
  
  # Product input counter
  product_counter <- reactiveVal(1)
  resource_counter <- reactiveVal(11)  # Start at 11 since we have 11 pre-loaded resources
  alternate_counter <- reactiveVal(1)
  
  # Add product
  observeEvent(input$add_product, {
    # First, save current input values to reactive value
    prod_df <- products()
    for (id in prod_df$id) {
      item <- input[[paste0("product_item_", id)]]
      qty <- input[[paste0("product_qty_", id)]]
      # Only update if input exists and is not NULL
      if (!is.null(item) && item != "") {
        prod_df[prod_df$id == id, "item"] <- item
      }
      if (!is.null(qty) && !is.na(qty)) {
        prod_df[prod_df$id == id, "qty"] <- qty
      }
    }
    products(prod_df)
    
    # Then add new row
    new_id <- product_counter() + 1
    product_counter(new_id)
    new_products <- products()
    new_products <- rbind(new_products, 
                          data.frame(id = new_id, item = "", qty = 100, stringsAsFactors = FALSE))
    products(new_products)
  })
  
  # Add resource
  observeEvent(input$add_resource, {
    # First, save current input values to reactive value
    res_df <- resources()
    for (id in res_df$id) {
      item <- input[[paste0("resource_item_", id)]]
      qty <- input[[paste0("resource_qty_", id)]]
      if (!is.null(item)) res_df[res_df$id == id, "item"] <- item
      if (!is.null(qty)) res_df[res_df$id == id, "qty"] <- qty
    }
    resources(res_df)
    
    # Then add new row
    new_id <- resource_counter() + 1
    resource_counter(new_id)
    new_resources <- resources()
    new_resources <- rbind(new_resources, 
                           data.frame(id = new_id, item = "", qty = 0, stringsAsFactors = FALSE))
    resources(new_resources)
  })
  
  # Add alternate
  observeEvent(input$add_alternate, {
    # First, save current input values to reactive value
    alt_df <- alternates()
    for (id in alt_df$id) {
      recipe <- input[[paste0("alternate_recipe_", id)]]
      if (!is.null(recipe)) alt_df[alt_df$id == id, "recipe"] <- recipe
    }
    alternates(alt_df)
    
    # Then add new row
    new_id <- alternate_counter() + 1
    alternate_counter(new_id)
    new_alternates <- alternates()
    new_alternates <- rbind(new_alternates, 
                            data.frame(id = new_id, recipe = "", stringsAsFactors = FALSE))
    alternates(new_alternates)
  })
  
  # Render product inputs
  output$product_inputs <- renderUI({
    prod_df <- products()
    lapply(prod_df$id, function(id) {
      idx <- which(prod_df$id == id)
      fluidRow(
        column(8,
               selectizeInput(paste0("product_item_", id),
                             label = NULL,
                             choices = c("Select Product..." = "", available_products),
                             selected = prod_df[idx, "item"])
        ),
        column(3,
               numericInput(paste0("product_qty_", id),
                           label = NULL,
                           value = prod_df[idx, "qty"],
                           min = 0)
        ),
        column(1,
               actionButton(paste0("remove_product_", id),
                           label = "×",
                           class = "btn-sm btn-danger",
                           style = "margin-top: 25px;")
        ),
        br()
      )
    })
  })
  
  # Render resource inputs
  output$resource_inputs <- renderUI({
    res_df <- resources()
    lapply(res_df$id, function(id) {
      idx <- which(res_df$id == id)
      fluidRow(
        column(8,
               selectizeInput(paste0("resource_item_", id),
                             label = NULL,
                             choices = c("Select Resource..." = "", available_resources),
                             selected = res_df[idx, "item"])
        ),
        column(3,
               numericInput(paste0("resource_qty_", id),
                           label = NULL,
                           value = res_df[idx, "qty"],
                           min = 0)
        ),
        column(1,
               actionButton(paste0("remove_resource_", id),
                           label = "×",
                           class = "btn-sm btn-danger",
                           style = "margin-top: 25px;")
        ),
        br()
      )
    })
  })
  
  # Render alternate inputs
  output$alternate_inputs <- renderUI({
    alt_df <- alternates()
    lapply(alt_df$id, function(id) {
      idx <- which(alt_df$id == id)
      fluidRow(
        column(11,
               selectizeInput(paste0("alternate_recipe_", id),
                             label = NULL,
                             choices = c("Select Recipe..." = "", available_recipes),
                             selected = alt_df[idx, "recipe"])
        ),
        column(1,
               actionButton(paste0("remove_alternate_", id),
                           label = "×",
                           class = "btn-sm btn-danger",
                           style = "margin-top: 25px;")
        ),
        br()
      )
    })
  })
  
  # Remove product observers
  observeEvent(input$add_product, {
    prod_df <- products()
    for (id in prod_df$id) {
      local({
        btn_id <- paste0("remove_product_", id)
        observeEvent(input[[btn_id]], {
          prod_df <- products()
          prod_df <- prod_df %>% filter(id != !!id)
          products(prod_df)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Remove resource observers
  # First, set up observers for pre-loaded resources
  for (id in seq_len(11)) {
    local({
      btn_id <- paste0("remove_resource_", id)
      observeEvent(input[[btn_id]], {
        res_df <- resources()
        res_df <- res_df %>% filter(id != !!id)
        resources(res_df)
      }, ignoreInit = TRUE)
    })
  }
  
  # Then set up new observers when add is clicked
  observeEvent(input$add_resource, {
    res_df <- resources()
    for (id in res_df$id) {
      local({
        btn_id <- paste0("remove_resource_", id)
        observeEvent(input[[btn_id]], {
          res_df <- resources()
          res_df <- res_df %>% filter(id != !!id)
          resources(res_df)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Remove alternate observers
  observeEvent(input$add_alternate, {
    alt_df <- alternates()
    for (id in alt_df$id) {
      local({
        btn_id <- paste0("remove_alternate_", id)
        observeEvent(input[[btn_id]], {
          alt_df <- alternates()
          alt_df <- alt_df %>% filter(id != !!id)
          alternates(alt_df)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Store optimization results
  lp_result <- reactiveVal(NULL)
  graph_result <- reactiveVal(NULL)
  graph_vertices <- reactiveVal(NULL)
  optimization_status <- reactiveVal("Ready to optimize")
  
  # Main optimization function
  observeEvent(input$optimize, {
    tryCatch({
      optimization_status("Running optimization...")
      session$sendCustomMessage(type = 'load-page', list())
      
      # Gather product inputs from dynamic UI
      prod_df <- products()
      prod_rows <- list()
      for (id in prod_df$id) {
        item <- input[[paste0("product_item_", id)]]
        qty <- input[[paste0("product_qty_", id)]]
        if (!is.null(item) && !is.null(qty) && item != "" && qty > 0) {
          prod_rows[[length(prod_rows) + 1]] <- data.frame(id = id, item = item, qty = qty, stringsAsFactors = FALSE)
        }
      }
      if (length(prod_rows) > 0) {
        prod_df <- do.call(rbind, prod_rows)
      } else {
        prod_df <- data.frame(id = numeric(0), item = character(0), qty = numeric(0), stringsAsFactors = FALSE)
      }
      
      # Gather resource inputs from dynamic UI
      res_df <- resources()
      res_rows <- list()
      for (id in res_df$id) {
        item <- input[[paste0("resource_item_", id)]]
        qty <- input[[paste0("resource_qty_", id)]]
        if (!is.null(item) && !is.null(qty) && item != "") {
          res_rows[[length(res_rows) + 1]] <- data.frame(id = id, item = item, qty = qty, stringsAsFactors = FALSE)
        }
      }
      if (length(res_rows) > 0) {
        res_df <- do.call(rbind, res_rows)
      } else {
        res_df <- data.frame(id = numeric(0), item = character(0), qty = numeric(0), stringsAsFactors = FALSE)
      }
      
      # Gather alternate recipe inputs from dynamic UI
      alt_df <- alternates()
      alt_rows <- list()
      for (id in alt_df$id) {
        recipe <- input[[paste0("alternate_recipe_", id)]]
        if (!is.null(recipe) && recipe != "") {
          alt_rows[[length(alt_rows) + 1]] <- data.frame(id = id, recipe = recipe, stringsAsFactors = FALSE)
        }
      }
      if (length(alt_rows) > 0) {
        alt_df <- do.call(rbind, alt_rows)
      } else {
        alt_df <- data.frame(id = numeric(0), recipe = character(0), stringsAsFactors = FALSE)
      }
      
      # Validate inputs
      if (nrow(prod_df) == 0) {
        optimization_status("Error: Please select at least one product")
        return()
      }
      
      if (nrow(res_df) == 0) {
        optimization_status("Error: Please select at least one resource")
        return()
      }
      
      # Build named vectors
      opt_products <- setNames(rep(0, nrow(prod_df)), prod_df$item)
      
      # Handle resources - negate the quantities (resources are entered as positive, solver expects negative)
      resource_values <- -res_df$qty
      available_resources_vec <- setNames(resource_values, res_df$item)
      
      # Get alternate recipes
      alternate_recipes <- if (nrow(alt_df) > 0) alt_df$recipe else NULL
      
      # Make current recipes
      current_recipes_obj <- make_current_recipes(recipe_data, alternate_recipes)
      
      # Run optimization
      optimization_status("Solving linear program...")
      
      binary_lp <- factory_binary_search_continuous(
        Opt_products = opt_products,
        current_recipes = current_recipes_obj,
        available_resources = available_resources_vec,
        req_amt = prod_df$qty,
        whole_number_factories = input$whole_factories,
        slack = input$slack
      )
      
      # Clean results for visualization
      optimization_status("Preparing visualization...")
      
      cytoscape_ready <- clean_binary_lp_results(
        lp_result = binary_lp$soln,
        recipeData = current_recipes_obj$data_frame,
        recipeGraph = current_recipes_obj$graph,
        products = opt_products
      )
      
      # Get tidy factory results with partitioned pathways
      optimization_status("Tidying factory results...")
      
      tidy_results <- tidy_factory_results(
        CytoscapeReady_binary = cytoscape_ready,
        current_recipes = current_recipes_obj,
        Opt_products = opt_products
      )
      
      # Extract the group graph which shows factory counts and pathways
      group_network <- tidy_results$group_graph
      
      # Get vertex data for factory counts
      vertex_data <- igraph::as_data_frame(group_network, what = "vertices")
      
      # Store results
      lp_result(binary_lp$soln)
      graph_result(group_network)
      graph_vertices(vertex_data)
      optimization_status("Optimization complete!")
      
    }, error = function(e) {
      optimization_status(paste("Error:", str_trunc(as.character(e), 100)))
    })
  })
  
  # Render status message
  output$status_message <- renderUI({
    tags$div(
      style = "margin-top: 15px; padding: 10px; background-color: #f0f0f0; border-radius: 4px;",
      p(optimization_status(), style = "margin: 0; font-weight: bold;")
    )
  })
  
  # Render the network graph
  output$factory_graph <- renderVisNetwork({
    graph <- graph_result()
    vertices_data <- graph_vertices()
    
    if (is.null(graph) || is.null(vertices_data)) {
      return(visNetwork(
        nodes = data.frame(id = 1, label = "Run optimization to see results", title = "Help"),
        edges = data.frame(from = numeric(0), to = numeric(0))
      ) %>%
        visOptions(height = "750px")
      )
    }
    
    # Convert igraph to visNetwork format
    edges_df <- igraph::as_data_frame(graph)
    nodes_df <- vertices_data
    
    # Prepare nodes with better labels and coloring
    nodes_df <- nodes_df %>%
      rownames_to_column("id") %>%
      mutate(
        # Use factory_label if available, otherwise use name
        label = if_else(!is.na(factory_label), factory_label, name),
        title = label,
        # Color by factory type (building type)
        color = case_when(
          factory_type == "Assembler" ~ "#FF6B6B",
          factory_type == "Smelter" ~ "#FFA500",
          factory_type == "Constructor" ~ "#4ECDC4",
          factory_type == "Refinery" ~ "#45B7D1",
          factory_type == "Manufacturer" ~ "#96CEB4",
          factory_type == "source_node" ~ "#FFD700",
          !is.na(factory_type) ~ "#95E1D3",
          TRUE ~ "#E8E8E8"
        ),
        # Size based on number of factories
        size = if_else(!is.na(total_factory_count), 
                       as.numeric(total_factory_count) * 15 + 30, 
                       30)
      ) %>%
      select(id, label, title, color, size)
    
    # Prepare edges with factory count information from source node
    edges_df <- edges_df %>%
      select(from, to, everything())
    
    # Create a lookup for factory counts from the vertices data
    vertices_lookup <- vertices_data %>%
      rownames_to_column("id") %>%
      select(id, factory_count) %>%
      filter(!is.na(factory_count))
    
    # Add labels showing factory counts from the from node
    edges_df <- edges_df %>%
      mutate(
        # Use left_join approach to get factory counts
        from_char = as.character(from)
      ) %>%
      left_join(
        vertices_lookup %>% rename(from_char = id, edge_label = factory_count),
        by = "from_char"
      ) %>%
      mutate(
        label = case_when(
          !is.na(edge_label) ~ round(edge_label, 2) %>% as.character(),
          TRUE ~ "Unknown"
        ),
        title = paste0("Factories needed: ", label)
      ) %>%
      select(from, to, label, title)
    
    # Create visNetwork with improved layout to minimize edge overlaps
    visNetwork(nodes_df, edges_df, height = "1050px") %>%
      visOptions(
        height = "1050px",
        highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visEdges(
        arrows = "to",
        smooth = list(enabled = TRUE, type = "cubicBezier"),
        font = list(size = 11, color = "black", strokeWidth = 1.5, strokeColor = "white"),
        widthConstraint = list(maximum = 80)
      ) %>%
      visPhysics(
        enabled = TRUE,
        stabilization = list(
          enabled = TRUE, 
          iterations = 300, 
          updateInterval = 50,
          fit = TRUE
        ),
        barnesHut = list(
          gravitationalConstant = -25000,
          centralGravity = 0.5,
          springLength = 300,
          springConstant = 0.03,
          damping = 0.4
        ),
        maxVelocity = 25,
        minVelocity = 0.2,
        solver = "barnesHut",
        timestep = 0.35,
        adaptiveTimestep = TRUE
      )
  })
  
  # Render factory statistics
  output$factory_stats <- renderUI({
    result <- lp_result()
    
    if (is.null(result)) {
      return(tags$p("Run optimization to see factory statistics", style = "color: #999; font-style: italic;"))
    }
    
    # Get the product names and their production rates
    prod_df <- products() %>% 
      filter(item != "")
    
    # Extract solution vector and get rates for requested products
    solution_vector <- result$solution
    
    # Extract recipe matrix from the global environment (approximate)
    rate_info <- tryCatch({
      # Create summary of products being produced
      summary_items <- lapply(1:nrow(prod_df), function(i) {
        item <- prod_df[i, "item"]
        tags$div(
          tags$strong(item), ": ",
          tags$span("Rate: TBD items/min", style = "color: #666;"),
          tags$br()
        )
      })
      
      tags$div(
        tags$h4("Product Production Rates:", style = "margin-top: 0;"),
        summary_items,
        tags$h4("Factory Summary:", style = "margin-top: 10px;"),
        tags$div(
          tags$strong("Total Factories: "), "See network nodes",
          tags$br(),
          tags$strong("Power Consumption: "), "N/A (requires power calculation function)",
          tags$br(),
          tags$p("Note: Power consumption will require implementation of a power calculation function.", 
                 style = "font-size: 12px; color: #999; margin-top: 10px;")
        )
      )
    }, error = function(e) {
      tags$p("Error calculating statistics", style = "color: red;")
    })
    
    rate_info
  })
}

##################################################
## Run the app
##################################################

shinyApp(ui = ui, server = server)
