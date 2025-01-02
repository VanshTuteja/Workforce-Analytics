library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(digest)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(prophet)
library(randomForest)
library(cluster)
library(DT)
library(scales)
library(lubridate)
library(fresh)
library(RColorBrewer)
library(caret)
library(xgboost)
library(googleAuthR)
library(shinyjs)

# Google AuthR Configuration
#options(googleAuthR.client_id = "16323160695-vb18hm163tm270d9blb8tm770i5sd713.apps.googleusercontent.com")
#options(googleAuthR.client_secret = "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")
#options(googleAuthR.scopes.selected = c("openid", "email", "profile"))
google_client_id <- "16323160695-vb18hm163tm270d9blb8tm770i5sd713.apps.googleusercontent.com"
google_client_secret <- "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"
google_redirect_uri <- "http://127.0.0.1:6030/"
# Custom color palettes
custom_colors <- list(
  primary = c("#6366f1", "#818cf8", "#a5b4fc","#3A3EF8","#01048F"),
  secondary = c("#10b981", "#34d399", "#6ee7b7"),
  accent = c("#f59e0b", "#fbbf24", "#fcd34d"),
  complementary = c("#ec4899", "#f472b6", "#f9a8d4")
)

# Theme configuration
mytheme <- create_theme(
  adminlte_color(light_blue = "#6366f1"),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#1e1b4b",
    dark_hover_bg = "#312e81",
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#0f172a",
    box_bg = "#1e293b",
    info_box_bg = "#1e293b"
  )
)

# Initialize SQLite database for user authentication
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  
  # Create users table if it doesn't exist
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      username TEXT PRIMARY KEY,
      password TEXT,
      email TEXT
    )
  ")
  
  # Add demo user if not exists
  demo_exists <- dbGetQuery(con, "SELECT * FROM users WHERE username = 'demo'")
  if (nrow(demo_exists) == 0) {
    dbExecute(con, "
      INSERT INTO users (username, password, email)
      VALUES ('demo', ?, 'demo@example.com')
    ", params = list(digest("demo123", algo = "sha256")))
  }
  
  return(con)
}

# Generate sample data with more realistic patterns
# Sample data structure (replace with your actual data)
workforce_data <- data.frame(
  Department = character(),
  Job_Role = character(),
  Work_Hours = numeric(),
  Performance = numeric(),
  Efficiency = numeric(),
  Workload = character(),
  Date = as.Date(character())
)
# Initialize database connection and sample data
db <- init_db()
#workforce_data <- generate_sample_data()

# Enhanced UI Components
loginUI <- function(id) {
  ns <- NS(id)
  div(
    style = "max-width: 400px; margin: 100px auto;padding:20px; background-color:white; border-radius: 8px; box-shadow: 1px 12px 12px white;",
    h2("Workforce Analytics Login", style = "text-align: center; color: black; margin-bottom: 30px;font-weight:bolder"),
    div(
      style = "margin-bottom: 20px;margin-left: 25px;color:black;",
      textInput(ns("username"), "Username", value = "demo", 
                placeholder = "Enter your username"),
      passwordInput(ns("password"), "Password", value = "demo123", 
                    placeholder = "Enter your password")
    ),
    actionButton(ns("login"), "Login", 
                 style = "width: 84%; padding: 10px;background-color: #0013e6; color: white; border: none; border-radius: 4px; font-size: 16px; margin-left: 25px;"),
    div(
      style = "text-align: center; margin-top: 20px;",
      actionLink(ns("show_register"), "Or sign in with:", 
                 style = "color: #2196F3;")
    ),
    div(class = "auth-switch",
        "",
        actionButton("google_signin", "Sign in with Google",
                     class = "auth-btn",
                     style = "background-color: #e60000;font-weight:900px;color:white; margin-top: 10px;margin-left:100px;",
                     icon = icon("google")
        )
    ),
    # googleAuthUI("google_login"),
    uiOutput(ns("message"))
  )
}

registerUI <- function(id) {
  ns <- NS(id)
  div(
    style = "max-width: 400px; margin: 100px auto; padding: 20px; background-color:white; border-radius: 8px;box-shadow: 0px 12px 12px white;",
    h2("Create Account", style = "text-align: center; color: black; margin-bottom: 30px;font-weight:bolder;"),
    div(
      style = "margin-bottom: 20px;margin-left: 25px;color:black;",
      textInput(ns("username"), "Username", placeholder = "Choose a username"),
      passwordInput(ns("password"), "Password", placeholder = "Choose a password"),
      textInput(ns("email"), "Email", placeholder = "Enter your email")
    ),
    actionButton(ns("register"), "Register", 
                 style = "width: 84%; padding: 10px; background-color: #0013e6; color: white; border: none; border-radius: 4px; font-size: 16px; margin-left: 25px;"),
    div(
      style = "text-align: center; margin-top: 20px;",
      actionLink(ns("show_login"), "or sign up with:", 
                 style = "color: #2196F3;")
    ),
    div(class = "auth-switch",
        "",
        actionButton("google_signin", "Sign up with Google",
                     class = "auth-btn",
                     style = "background-color: #e60000;font-weight:900px;color:white; margin-top: 10px;margin-left:100px;",
                     icon = icon("google")
        )),
    uiOutput(ns("message"))
  )
}


# Main UI with enhanced styling
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .content-wrapper { 
        background-color: #f4f6f9; 
      }
      .box { 
        border-top: 3px solid #2196F3;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .box-header {
        background-color: #000000;
        border-bottom: 1px solid #f4f4f4;
      }
      .box-title {
        font-size: 18px;
        font-weight: 600;
      }
      .sidebar-menu > li > a {
        padding: 12px 15px;
        
      }
      .treeview-menu > li > a {q
        padding: 8px 15px 8px 30px;
      }
      .btn-primary {
        background-color: #2196F3;
        border-color: #1e88e5;
      }
      .btn-primary:hover {
        background-color: #1e88e5;
        border-color: #1976d2;
      }
       .main-sidebar {
          background-color: #0f172a;
          color: white; /* White text color */
       }
       
        .navbar {
          background-color: #0f172a;
          color: white; /* White text color */
        }
        body{
         background: rgb(28,36,0);
background: linear-gradient(90deg, rgba(28,36,0,0) 0%, rgba(0,19,230,0.8010853999803046) 0%, rgba(34,183,227,1) 27%, rgba(84,245,255,0.9243346997001926) 67%, rgba(0,19,230,0.8627100498402486) 96%);
         color: white;
         font-weight: bolder;
        }
         a{
        color:white;
        }
       
    "))
  ),
  uiOutput("page"),
  useShinyjs(),
  tags$head(
    tags$script("
        Shiny.addCustomMessageHandler('redirectToGoogle', function(authUrl) {
          window.location.href = authUrl;
        });
      ")
  ),
  uiOutput("currentPage")
)
  
## Google OAuth
# Add these Google authentication functions
setup_google_auth <- function() {
  options(googleAuthR.scopes.selected = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile"
  ))
  
  # Replace with your OAuth credentials
  options(googleAuthR.webapp.client_id =google_client_id,
          googleAuthR.webapp.client_secret =google_client_secret,
         googleAuthR.webapp.redirect_uri =google_redirect_uri)
  # Configure for same window authentication
  #options(googleAuthR.webapp.use_basic_auth = TRUE,
     #      googleAuthR.webapp.redirect_on_signout = TRUE,
     #    googleAuthR.webapp.popup = FALSE)
}

# Modify the handle_google_auth function to handle the OAuth code
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = "http://127.0.0.1:6030",
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info_response <- GET(
      "https://www.googleapis.com/oauth2/v1/userinfo",
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(user_info_response) == 200) {
      user_info <- fromJSON(rawToChar(user_info_response$content))
      
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
                  "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    user_info$email,
                    user_info$email,
                    user_info$id,
                    user_info$email,
                    user_info$picture
                  )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
                  "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
                  params = list(
                    user_info$id,
                    user_info$email,
                    user_info$picture,
                    user_info$email
                  )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}

get_google_user_info <- function(access_token) {
  # Get user info from Google using the access token directly in the header
  response <- GET(
    "https://www.googleapis.com/oauth2/v1/userinfo",
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  if (status_code(response) == 200) {
    user_info <- fromJSON(rawToChar(response$content))
    return(user_info)
  }
  return(NULL)
}

# Handle the OAuth code exchange and user authentication
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = google_redirect_uri,
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info <- get_google_user_info(access_token)
    
    if (!is.null(user_info)) {
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
                  "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    user_info$email,
                    user_info$email,
                    user_info$id,
                    user_info$email,
                    user_info$picture
                  )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
                  "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
                  params = list(
                    user_info$id,
                    user_info$email,
                    user_info$picture,
                    user_info$email
                  )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      # Return user data
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}


# Home page content
home_content <- fluidRow(
  column(
    width = 12,
    style = "padding: 20px;",
    div(
      style = "text-align: center; padding: 40px 20px; background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%); border-radius: 15px; margin-bottom: 30px;",
      tags$h1("Welcome to Workforce Analytics", style = "color: #ffffff; font-size: 3em; margin-bottom: 20px;"),
      tags$p("Empowering organizations with data-driven workforce insights", 
             style = "color: #94a3b8; font-size: 1.2em; margin-bottom: 30px;"),
      actionButton("get_started", "Get Started", 
                   style = "background-color: #6366f1; color: white; padding: 10px 30px; font-size: 1.2em; border: none; border-radius: 8px; cursor: pointer; transition: background-color 0.3s;",
                   onmouseover = "this.style.backgroundColor='#4f46e5'",
                   onmouseout = "this.style.backgroundColor='#6366f1'")
    )
  ),
  column(
    width = 4,
    div(
      style = "background: #1e293b; padding: 25px; border-radius: 12px; height: 300px; margin-bottom: 20px;",
      tags$h3("Real-time Analytics", style = "color: #ffffff; margin-bottom: 15px;"),
      icon("chart-line", style = "color: #6366f1; font-size: 3em; margin-bottom: 15px;"),
      tags$p("Monitor workforce performance metrics in real-time with interactive dashboards and visualizations.",
             style = "color: #94a3b8;")
    )
  ),
  column(
    width = 4,
    div(
      style = "background: #1e293b; padding: 25px; border-radius: 12px; height: 300px; margin-bottom: 20px;",
      tags$h3("Predictive Insights", style = "color: #ffffff; margin-bottom: 15px;"),
      icon("brain", style = "color: #10b981; font-size: 3em; margin-bottom: 15px;"),
      tags$p("Leverage advanced machine learning models to predict workforce trends and optimize performance.",
             style = "color: #94a3b8;")
    )
  ),
  column(
    width = 4,
    div(
      style = "background: #1e293b; padding: 25px; border-radius: 12px; height: 300px; margin-bottom: 20px;",
      tags$h3("Advanced Analytics", style = "color: #ffffff; margin-bottom: 15px;"),
      icon("magnifying-glass-chart", style = "color: #f59e0b; font-size: 3em; margin-bottom: 15px;"),
      tags$p("Discover deep insights with clustering analysis and advanced statistical modeling.",
             style = "color: #94a3b8;")
    )
  )
)

# About page content
about_content <- fluidRow(
  column(
    width = 12,
    style = "padding: 20px;",
    div(
      style = "background: #1e293b; padding: 40px; border-radius: 15px; margin-bottom: 30px;",
      tags$h2("About Workforce Analytics", style = "color: #ffffff; margin-bottom: 20px;"),
      tags$p("Workforce Analytics is a comprehensive platform designed to help organizations optimize their workforce management through data-driven insights and advanced analytics.",
             style = "color: #94a3b8; font-size: 1.2em; margin-bottom: 20px;"),
      tags$hr(style = "border-color: #312e81; margin: 30px 0;"),
      tags$h3("Key Features", style = "color: #ffffff; margin-bottom: 20px;"),
      tags$ul(
        style = "color: #94a3b8; font-size: 1.1em;",
        tags$li("Interactive dashboards with real-time metrics"),
        tags$li("Advanced predictive analytics and forecasting"),
        tags$li("Employee clustering and pattern recognition"),
        tags$li("Customizable filters and data visualization"),
        tags$li("Performance tracking and optimization")
      )
    ),
    div(
      style = "background: #1e293b; padding: 40px; border-radius: 15px;",
      tags$h3("Technology Stack", style = "color: #ffffff; margin-bottom: 20px;"),
      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px;",
        div(
          style = "text-align: center; padding: 20px; background: #0f172a; border-radius: 8px;",
          icon("r-project", style = "color: #6366f1; font-size: 2em; margin-bottom: 10px;"),
          tags$h4("R Shiny", style = "color: #ffffff;"),
          tags$p("Interactive web applications", style = "color: #94a3b8;")
        ),
        div(
          style = "text-align: center; padding: 20px; background: #0f172a; border-radius: 8px;",
          icon("chart-simple", style = "color: #10b981; font-size: 2em; margin-bottom: 10px;"),
          tags$h4("Plotly", style = "color: #ffffff;"),
          tags$p("Dynamic visualizations", style = "color: #94a3b8;")
        ),
        div(
          style = "text-align: center; padding: 20px; background: #0f172a; border-radius: 8px;",
          icon("brain", style = "color: #f59e0b; font-size: 2em; margin-bottom: 10px;"),
          tags$h4("Machine Learning", style = "color: #ffffff;"),
          tags$p("Advanced analytics", style = "color: #94a3b8;")
        )
      )
    ),
    div(
      style = "text-align: center; margin-top: 30px; padding: 20px; background: #0f172a; border-radius: 8px;",
      tags$p("Made by Vansh Tuteja", 
             style = "color: #ffffff; font-size: 1.2em; font-weight: 600; letter-spacing: 0.05em;")
    )
  )
)


# Enhanced server logic
server <- function(input, output, session) {
  # Authentication state
  credentials <- reactiveVal(NULL)
  
  # Setup Google Authentication
  setup_google_auth()
  
  # Handle Google Sign-In button click
  observeEvent(input$google_signin, {
    # Create OAuth URL
    auth_url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", google_client_id,
      "&redirect_uri=", URLencode("http://127.0.0.1:6030", reserved = TRUE),
      "&response_type=code",
      "&scope=", URLencode("email profile openid https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email", reserved = TRUE)
    )
    
    # Redirect to Google login
    session$sendCustomMessage("redirectToGoogle", auth_url)
  })
  
  # Handle the OAuth callback observer
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # Handle the authorization code
      user <- handle_google_auth(query$code, session)
      
      if (!is.null(user)) {
        user_data(user)
        appState("dashboard")
        
      } else {
        output$login_message <- renderText("Failed to authenticate with Google")
        appState("login")
      }
    }
  })
  
  # Login Module
  callModule(
    function(input, output, session) {
      observeEvent(input$login, {
        user <- dbGetQuery(db, "SELECT * FROM users WHERE username = ?", 
                           params = list(input$username))
        
        if (nrow(user) > 0 && user$password == digest(input$password, algo = "sha256")) {
          credentials(list(
            username = input$username,
            authenticated = TRUE
          ))
          showNotification("Login successful!", type = "message")
        } else {
          output$message <- renderUI(
            div(style = "color: #f44336; margin-top: 15px; text-align: center;",
                icon("exclamation-circle"), "Invalid username or password")
          )
        }
      })
      
      observeEvent(input$show_register, {
        updateTabsetPanel(session, "auth", selected = "register")
      })
    },
    id = "login"
  )
  
  # Registration Module
  callModule(
    function(input, output, session) {
      observeEvent(input$register, {
        req(input$username, input$password, input$email)
        
        if (nchar(input$password) < 6) {
          output$message <- renderUI(
            div(style = "color: #f44336; margin-top: 15px; text-align: center;",
                icon("exclamation-circle"), "Password must be at least 6 characters")
          )
          return()
        }
        
        existing <- dbGetQuery(db, "SELECT username FROM users WHERE username = ?", 
                               params = list(input$username))
        
        if (nrow(existing) > 0) {
          output$message <- renderUI(
            div(style = "color: #f44336; margin-top: 15px; text-align: center;",
                icon("exclamation-circle"), "Username already exists")
          )
        } else {
          dbExecute(db, 
                    "INSERT INTO users (username, password, email) VALUES (?, ?, ?)",
                    params = list(
                      input$username,
                      digest(input$password, algo = "sha256"),
                      input$email
                    ))
          
          showNotification("Registration successful! Please login.", type = "message")
          updateTabsetPanel(session, "auth", selected = "login")
        }
      })
      
      observeEvent(input$show_login, {
        updateTabsetPanel(session, "auth", selected = "login")
      })
      
    },
    id = "register"
  )
  
  # Initialize with sample data
  initial_data <- data.frame(
    Date = as.Date(c("2024-02-10", "2024-05-31", "2024-06-19", "2024-01-27")),
    Department = c("Finance", "Finance", "Finance", "Finance"),
    Efficiency = c(75, 75, 75, 76),
    Employee = c("Christopher Morrow", "William Hartman", "Christina Vaughan", "Christopher Ellis"),
    Job_Role = c("Analyst", "Manager", "Manager", "Analyst"),
    Performance = c(92, 92, 90, 79),
    Satisfaction = c(3.3, 3.8, 3.5, 4.8),
    Work_Hours = c(33, 36, 40, 34),
    Workload = c("Low", "High", "Medium", "Medium")
  )
  # Download template handler
  output$download_template <- downloadHandler(
    filename = function() {
      "workforce_template.csv"
    },
    content = function(file) {
      write.csv(initial_data, file, row.names = FALSE)
    }
  )
  # Home page navigation
  observeEvent(input$get_started, {
    updateTabItems(session, "sidebarMenu", "dashboard")
  })
  # Main page content with enhanced dashboard
  output$page <- renderUI({
    if (is.null(credentials()$authenticated)) {
      tabsetPanel(
        id = "auth",
        tabPanel("login", loginUI("login")),
        tabPanel("register", registerUI("register"))
      )
    } else {
      dashboardPage(
        skin = "black",
        dashboardHeader(
          title = span("Workforce Analytics", 
                       style = "color: #ffffff; font-size: 24px; font-weight: 600;"),
          titleWidth = 300
        ),
        dashboardSidebar(
          width = 300,
          sidebarMenu(
            fileInput("file_upload", "Upload CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      ),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected",
                      width = "100%",
            ),
            downloadButton("download_template", "Download Template"),
            menuItem("Home", tabName = "home", icon = icon("house")),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Predictive Analytics", tabName = "predictive", icon = icon("chart-line")),
            menuItem("Advanced Analytics", tabName = "advanced", icon = icon("brain")),
            menuItem("About", tabName = "about", icon = icon("circle-info")),
            menuItem(
              "Filters",
              icon = icon("filter"),
              uiOutput("dynamic_department_input"),
              uiOutput("dynamic_role_input"),
              uiOutput("dynamic_hours_input"),
              actionButton(
                "apply_filters",
                "Apply Filters",
                class = "btn-primary",
                style = "width: 80%; background-color: #6366f1; border: none;"
              )
            ),
            actionButton("logout", "Logout",
                         style="background-color:red;width:260px;border:none;color:white;font-weight:900px;")
          )
        ),
        dashboardBody(
          use_theme(mytheme),
          tags$head(
            tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #0f172a;
          color: white;
        }
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
          border: 1px solid #312e81;
          background-color: #1e293b;
        }
        .box-header {
          border-bottom: 1px solid #312e81;
        }
        .box-title {
          color: #ffffff;
          font-weight: 600;
        }
        .btn-file {
          background-color: #6366f1 !important;
          color: white !important;
          border: none !important;
          border-radius: 8px !important;
        }
        .progress-bar {
          background-color: #6366f1 !important;
        }
         body{
        
         color: white;
         font-weight: bolder;
background: rgb(28,36,0);
background: linear-gradient(90deg, rgba(28,36,0,0) 0%, rgba(0,19,230,0.8010853999803046) 0%, rgba(34,183,227,1) 27%, rgba(84,245,255,0.9243346997001926) 67%, rgba(0,19,230,0.8627100498402486) 96%);
         }
        .content{
        height:1400px;
        }
         #download_template {
          background-color: #10b981;
          border-color: #10b981;
          color: white;
          margin-left:12px;
        }
        #download_template:hover {
          background-color: #059669;
          border-color: #059669;
        }
        a{
        color:white;
        }
      "))
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("upload_status")
            )
          ),
          tabItems(
            tabItem(tabName = "home", home_content),
            tabItem(tabName = "about", about_content),
            tabItem(
              tabName = "dashboard",
              fluidRow(
                column(
                  width = 12,
                  tags$h2("Workforce Optimization Dashboard", 
                          style = "color: #ffffff; text-align: center; margin-top: 20px; font-weight: 700;")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Department Distribution",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("pieChart", height = "300px")
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Workload Distribution",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("workloadBarChart", height = "300px")
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Performance & Efficiency Trends",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("combinedChart", height = "400px")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Work Hours Distribution",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("histogram", height = "300px")
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Efficiency Analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("scatterPlot", height = "300px")
                  )
                )
              )
            ),
            tabItem(
              tabName = "predictive",
              fluidRow(
                box(
                  width = 12,
                  title = "Performance Prediction Model",
                  status = "primary",
                  solidHeader = TRUE,
                  fluidRow(
                    column(
                      width = 4,
                      numericInput("work_hours_input", "Work Hours:", 
                                   value = 40, min = 0, max = 100),
                      selectInput("workload_input", "Workload Level:",
                                  choices = c("Low", "Medium", "High")),
                      actionButton("predict_btn", "Predict Performance",
                                   style = "background-color: #6366f1; color: white;")
                    ),
                    column(
                      width = 8,
                      plotlyOutput("prediction_plot", height = "300px")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Feature Importance",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("feature_importance", height = "300px")
                ),
                box(
                  width = 6,
                  title = "Model Performance",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("model_performance", height = "300px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Time Series Forecast",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("time_series_forecast", height = "400px")
                )
              )
            ),
            tabItem(
              tabName = "advanced",
              fluidRow(
                box(
                  width = 6,
                  title = "Employee Clustering",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("cluster_plot", height = "400px")
                ),
                box(
                  width = 6,
                  title = "Cluster Characteristics",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("cluster_profile", height = "400px")
                )
              )
            )
          )
        )
      )
    }
  })
 
  # Logout action logic
  observeEvent(input$logout, {
    credentials(NULL)
    # Clear the authentication state
  })
  #observeEvent(input$logout, {
  # session$close()  # Close the session when the button is clicked
  #})
  # Value Boxes
  
  # Reactive value for storing uploaded data
  uploaded_data <- reactiveVal(workforce_data)
  
  # File upload handling
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    ext <- tools::file_ext(input$file_upload$name)
    if(ext != "csv") {
      showNotification("Please upload a CSV file", type = "error")
      return()
    }
    
    tryCatch({
      data <- read.csv(input$file_upload$datapath)
      required_cols <- c("Department", "Job_Role", "Work_Hours", "Performance", 
                         "Efficiency", "Workload", "Date")
      missing_cols <- setdiff(required_cols, names(data))
      
      if(length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return()
      }
      
      data$Date <- as.Date(data$Date)
      uploaded_data(data)
      
      output$upload_status <- renderUI({
        div(
          style = "padding: 15px; margin: 10px 0; background-color: #10b981; color: white; border-radius: 8px;",
          icon("check-circle"), 
          paste("Successfully loaded", nrow(data), "records from", input$file_upload$name)
        )
      })
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  # Random Forest Model
  rf_model <- reactive({
    req(filtered_data())
    train_data <- filtered_data() %>%
      select(Work_Hours, Workload, Performance, Efficiency)
    randomForest(Performance ~ ., data = train_data, ntree = 100)
  })
  
  # XGBoost Model
  xgb_model <- reactive({
    req(filtered_data())
    train_data <- filtered_data() %>%
      select(Work_Hours, Workload, Performance, Efficiency)
    
    # Convert categorical variables
    train_matrix <- model.matrix(~ . - 1, data = train_data)
    
    xgboost(
      data = train_matrix,
      label = train_data$Performance,
      nrounds = 100,
      objective = "reg:squarederror",
      verbose = 0
    )
  })
  
  # Clustering Model
  cluster_model <- reactive({
    req(filtered_data())
    
    # Prepare data for clustering
    cluster_data <- filtered_data() %>%
      select(Work_Hours, Performance, Efficiency) %>%
      scale()
    
    kmeans(cluster_data, centers = 3)
  })
  # Prediction Plot
  output$prediction_plot <- renderPlotly({
    req(input$predict_btn, rf_model())
    
    new_data <- data.frame(
      Work_Hours = input$work_hours_input,
      Workload = input$workload_input,
      Efficiency = mean(filtered_data()$Efficiency)
    )
    
    prediction <- predict(rf_model(), new_data)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prediction,
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = custom_colors$primary[1]),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 40), color = custom_colors$complementary[1]),
          list(range = c(40, 70), color = custom_colors$accent[1]),
          list(range = c(70, 100), color = custom_colors$secondary[1])
        )
      )
    ) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  # Feature Importance Plot
  output$feature_importance <- renderPlotly({
    req(rf_model())
    
    importance_df <- data.frame(
      Feature = rownames(importance(rf_model())),
      Importance = importance(rf_model())[,1]
    ) %>%
      arrange(desc(Importance))
    
    plot_ly(importance_df, x = ~reorder(Feature, Importance), y = ~Importance,
            type = "bar", marker = list(color = custom_colors$primary)) %>%
      layout(
        title = "Feature Importance",
        xaxis = list(title = "Features"),
        yaxis = list(title = "Importance"),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  # Model Performance Plot
  output$model_performance <- renderPlotly({
    req(rf_model())
    
    predictions <- predict(rf_model(), filtered_data())
    actual <- filtered_data()$Performance
    
    plot_ly() %>%
      add_trace(x = actual, y = predictions, type = "scatter", mode = "markers",
                marker = list(color = custom_colors$primary[1]),
                name = "Predictions") %>%
      add_trace(x = c(0, 100), y = c(0, 100), type = "scatter", mode = "lines",
                line = list(color = custom_colors$primary[1], dash = "dash"),
                name = "Perfect Prediction") %>%
      layout(
        title = "Actual vs Predicted Performance",
        xaxis = list(title = "Actual Performance"),
        yaxis = list(title = "Predicted Performance"),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  # Time Series Forecast Plot
  output$time_series_forecast <- renderPlotly({
    req(filtered_data())
    
    ts_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(Performance = mean(Performance)) %>%
      arrange(Date)
    
    prophet_data <- ts_data %>%
      rename(ds = Date, y = Performance)
    
    m <- prophet(prophet_data)
    future <- make_future_dataframe(m, periods = 30)
    forecast <- predict(m)
    
    plot_ly() %>%
      add_trace(x = prophet_data$ds, y = prophet_data$y,
                type = "scatter", mode = "markers",
                name = "Actual",
                marker = list(color = custom_colors$primary[1])) %>%
      add_trace(x = forecast$ds, y = forecast$yhat,
                type = "scatter", mode = "lines",
                name = "Forecast",
                line = list(color = custom_colors$primary[2])) %>%
      add_ribbons(x = forecast$ds,
                  ymin = forecast$yhat_lower,
                  ymax = forecast$yhat_upper,
                  name = "95% Confidence",
                  fillcolor = paste0(custom_colors$primary[3], "40"),
                  line = list(color = "transparent")) %>%
      layout(
        title = "Performance Forecast",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Performance"),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff'),
        showlegend = TRUE
      )
  })
  
  # Dynamic UI elements
  output$dynamic_department_input <- renderUI({
    selectInput(
      "department", 
      "Department:",
      choices = unique(uploaded_data()$Department),
      selected = unique(uploaded_data()$Department)[1],
      width = "100%"
    )
  })
  
  output$dynamic_role_input <- renderUI({
    selectInput(
      "role", 
      "Job Role:",
      choices = unique(uploaded_data()$Job_Role),
      selected = unique(uploaded_data()$Job_Role)[1],
      width = "100%"
    )
  })
  
  output$dynamic_hours_input <- renderUI({
    sliderInput(
      "hours",
      "Work Hours Range:",
      min = min(uploaded_data()$Work_Hours),
      max = max(uploaded_data()$Work_Hours),
      value = c(min(uploaded_data()$Work_Hours), max(uploaded_data()$Work_Hours)),
      width = "100%"
    )
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(input$apply_filters)
    uploaded_data() %>%
      filter(
        Department == input$department,
        Job_Role == input$role,
        Work_Hours >= input$hours[1],
        Work_Hours <= input$hours[2]
      )
  })
  
  # Charts
  output$pieChart <- renderPlotly({
    req(uploaded_data())
    pie_data <- uploaded_data() %>%
      group_by(Department) %>%
      summarise(Count = n())
    
    plot_ly(pie_data, labels = ~Department, values = ~Count, type = 'pie',
            marker = list(colors = custom_colors$primary),
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(Department, "\nCount:", Count)) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff'),
        legend = list(bgcolor = 'rgba(0,0,0,0)')
      )
  })
  
  output$workloadBarChart <- renderPlotly({
    plot_ly(
      filtered_data(), 
      x = ~Job_Role, 
      color = ~Workload, 
      type = "histogram",
      colors = custom_colors$primary,
      marker = list(line = list(color = 'rgb(0, 0, 0)', width = 1.5))
    ) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',  # Transparent background
        plot_bgcolor = 'rgba(0,0,0,0)',  # Transparent plot area
        title = list(
          text = "Workload Distribution",  # Chart title
          font = list(color = '#ffffff')   # Title text color (white)
        ),
        legend = list(
          font = list(color = '#ffffff')   # Legend text color (white)
        ),
        xaxis = list(
          title = "Job Role",
          titlefont = list(color = '#ffffff'),  # X-axis title text color
          tickfont = list(color = '#ffffff')   # X-axis tick labels color
        ),
        yaxis = list(
          title = "Count",
          titlefont = list(color = '#ffffff'),  # Y-axis title text color
          tickfont = list(color = '#ffffff')   # Y-axis tick labels color
        )
      )
  })
  
  
  output$combinedChart <- renderPlotly({
    req(filtered_data())
    performance_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        Performance = mean(Performance),
        High_Efficiency = sum(Efficiency >= 0.7),
        Medium_Efficiency = sum(Efficiency >= 0.4 & Efficiency < 0.7),
        Low_Efficiency = sum(Efficiency < 0.4)
      )
    
    plot_ly() %>%
      add_trace(
        data = performance_data,
        x = ~Date,
        y = ~Performance,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Performance',
        line = list(color = custom_colors$primary[1])
      ) %>%
      layout(
        barmode = 'stack',
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff'),
        xaxis = list(
          title = "Date",
          showgrid = FALSE  # Remove gridlines from x-axis
        ),
        yaxis = list(
          title = "Count / Performance",
          showgrid = FALSE  # Remove gridlines from y-axis
        )
      )
  })
  
  
  # Histogram
  output$histogram <- renderPlotly({
    plot_ly(filtered_data(), x = ~Work_Hours, type = 'histogram',
            marker = list(color = custom_colors$primary[1],
                          line = list(color = '#ffffff', width = 1))) %>%
      layout(
        title = "Work Hours Distribution",
        xaxis = list(title = "Work Hours"),
        yaxis = list(title = "Frequency"),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  output$scatterPlot <- renderPlotly({
    req(filtered_data())
    plot_ly(filtered_data(), x = ~Work_Hours, y = ~Efficiency,
            type = 'scatter', mode = 'markers',
            marker = list(color = custom_colors$primary[1])) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff'),
        xaxis = list(title = "Work Hours"),
        yaxis = list(title = "Efficiency")
      )
  })
  
  # Predictive Analytics
  output$prediction_plot <- renderPlotly({
    req(input$predict_btn, filtered_data())
    
    # Simple prediction based on average performance
    avg_performance <- mean(filtered_data()$Performance)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = avg_performance,
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = custom_colors$primary[1]),
        steps = list(
          list(range = c(0, 40), color = "red"),
          list(range = c(40, 70), color = "yellow"),
          list(range = c(70, 100), color = custom_colors$secondary[1])
        )
      )
    ) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  # Clustering
  output$cluster_plot <- renderPlotly({
    req(filtered_data())
    cluster_data <- filtered_data() %>%
      select(Work_Hours, Performance, Efficiency) %>%
      scale()
    
    clusters <- kmeans(cluster_data, centers = 3)
    cluster_data <- data.frame(
      Work_Hours = filtered_data()$Work_Hours,
      Performance = filtered_data()$Performance,
      Efficiency = filtered_data()$Efficiency,
      Cluster = as.factor(clusters$cluster)
    )
    
    plot_ly(cluster_data, 
            x = ~Work_Hours, 
            y = ~Performance, 
            z = ~Efficiency,
            color = ~Cluster,
            colors = custom_colors$primary,
            type = "scatter3d", 
            mode = "markers") %>%
      layout(
        scene = list(
          xaxis = list(title = "Work Hours"),
          yaxis = list(title = "Performance"),
          zaxis = list(title = "Efficiency")
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff')
      )
  })
  
  
  output$cluster_profile <- renderPlotly({
    req(cluster_model())
    
    cluster_data <- filtered_data() %>%
      select(Work_Hours, Performance, Efficiency)
    cluster_data$Cluster <- as.factor(cluster_model()$cluster)
    
    cluster_summary <- cluster_data %>%
      group_by(Cluster) %>%
      summarise(
        avg_hours = mean(Work_Hours),
        avg_performance = mean(Performance),
        avg_efficiency = mean(Efficiency)
      )
    
    plot_ly() %>%
      add_trace(
        data = cluster_summary,
        x = ~Cluster,
        y = ~avg_hours,
        name = "Avg Hours",
        type = "bar",
        marker = list(color = custom_colors$primary[3])
      ) %>%
      add_trace(
        data = cluster_summary,
        x = ~Cluster,
        y = ~avg_performance,
        name = "Avg Performance",
        type = "bar",
        marker = list(color = custom_colors$primary[1])
      ) %>%
      layout(
        barmode = "group",
        title = "Cluster Characteristics",
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "Value"),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#ffffff'),
        showlegend = TRUE,
        legend = list(bgcolor = 'rgba(0,0,0,0)')
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)