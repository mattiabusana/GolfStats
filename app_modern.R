library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(bslib)
library(plotly)
library(fontawesome)
library(thematic)

# Set up theme
thematic_shiny(font = "auto")
custom_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#005E3C",
  base_font = font_google("Aptos"),
  spacer = "1.5rem"
)

# Google Sheet ID
sheet_id <- "1-Zw5mq8gjuXxQVwNxn-ZVqrHlxvQ_kBPvR_gHY_7SQE"

# UI
ui <- page_navbar(
  title = "GolfStats Dashboard",
  theme = custom_theme,
  bg = "#005E3C",
  inverse = TRUE,
  
  nav_panel(
    "Overview",
    div(
      style = "position: relative; min-height: 100vh;",
      tags$img(
        src = "color.jpg",
        style = "position: fixed; top: 0; left: 0; width: 120%; height: 120%; object-fit: cover; z-index: -1; opacity: 0.2; transform: translate(-8.33%, -8.33%);"
      ),
      div(
        style = "padding: 1.5rem; position: relative;",
        tags$style(HTML("
          @font-face {
            font-family: 'Aptos';
            src: url('https://fonts.cdnfonts.com/css/aptos');
          }
          html, body {
            margin: 0;
            padding: 0;
            height: 100%;
            width: 100%;
          }
          .card, .value-box, .card-header {
            background-color: rgba(255, 255, 255, 0.95);
            position: relative;
            z-index: 1;
            border-radius: 1rem;
            box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          }
          .navbar {
            position: relative;
            z-index: 2;
            box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          }
          .shiny-html-output {
            position: relative;
            z-index: 1;
          }
          .bslib-value-box .plotly .modebar-container {
            display: none;
          }
        ")),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "9-Hole Rounds",
            value = textOutput("total_rounds_9"),
            showcase = icon("golf-ball", class = "fa-lg"),
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "18-Hole Rounds",
            value = textOutput("total_rounds_18"),
            showcase = icon("golf-ball", class = "fa-lg"),
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "Tournaments",
            value = textOutput("total_tournaments"),
            showcase = icon("trophy", class = "fa-lg"),
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "Handicap",
            value = textOutput("current_handicap"),
            showcase = icon("chart-line", class = "fa-lg"),
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Handicap Trend"),
            plotlyOutput("handicap_plot", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Course Selection"),
            div(
              style = "max-height: 200px; overflow-y: auto;",
              uiOutput("selector_ui")
            ),
            class = "shadow-sm",
            height = "auto"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Normalized Points"),
            plotlyOutput("points_plot", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ),
          card(
            card_header("Putts per Hole"),
            plotlyOutput("putts_plot", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Club Data",
    div(
      style = "position: relative; min-height: 100vh;",
      tags$img(
        src = "color.jpg",
        style = "position: fixed; top: 0; left: 0; width: 120%; height: 120%; object-fit: cover; z-index: -1; opacity: 0.2; transform: translate(-8.33%, -8.33%);"
      ),
      div(
        style = "padding: 1.5rem; position: relative;",
        tags$style(HTML("
          @font-face {
            font-family: 'Aptos';
            src: url('https://fonts.cdnfonts.com/css/aptos');
          }
          html, body {
            margin: 0;
            padding: 0;
            height: 100%;
            width: 100%;
          }
          .card, .value-box, .card-header {
            background-color: rgba(255, 255, 255, 0.95);
            position: relative;
            z-index: 1;
            border-radius: 1rem;
            box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          }
          .navbar {
            position: relative;
            z-index: 2;
            box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          }
          .shiny-html-output {
            position: relative;
            z-index: 1;
          }
          .bslib-value-box .plotly .modebar-container {
            display: none;
          }
        ")),
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Club Performance Analysis"),
            div(
              style = "text-align: center; padding: 2rem;",
              h3("Club Data Section Coming Soon"),
              p("This section will contain detailed analysis of club performance and statistics.")
            ),
            class = "shadow-sm",
            height = "auto"
          )
        )
      )
    )
  )
) 

# Server
server <- function(input, output, session) {
  # Read data from Google Sheet
  read_gsheet_data <- reactive({
    tryCatch({
      sheet_data <- read_sheet(sheet_id)
      if (nrow(sheet_data) == 0) {
        return(data.frame(
          Date = as.Date(character()),
          Course = character(),
          Holes = numeric(),
          Points = numeric(),
          Tournament = logical(),
          Putts = numeric(),
          stringsAsFactors = FALSE
        ))
      }
      return(sheet_data)
    }, error = function(e) {
      showNotification("Error reading data from Google Sheet", type = "error")
      return(data.frame(
        Date = as.Date(character()),
        Course = character(),
        Holes = numeric(),
        Points = numeric(),
        Tournament = logical(),
        Putts = numeric(),
        stringsAsFactors = FALSE
      ))
    })
  })

  # Calculate statistics
  output$total_rounds_9 <- renderText({
    data <- read_gsheet_data()
    sum(data$Holes == 9, na.rm = TRUE)
  })

  output$total_rounds_18 <- renderText({
    data <- read_gsheet_data()
    sum(data$Holes == 18, na.rm = TRUE)
  })

  output$total_tournaments <- renderText({
    data <- read_gsheet_data()
    sum(data$Tournament, na.rm = TRUE)
  })

  output$current_handicap <- renderText({
    data <- read_gsheet_data()
    if (nrow(data) > 0) {
      round(tail(data$Handicap, 1), 1)
    } else {
      "N/A"
    }
  })

  # Course selector UI
  output$selector_ui <- renderUI({
    data <- read_gsheet_data()
    courses <- unique(data$Course)
    if (length(courses) > 0) {
      selectInput("course_select", "Select Course:",
                choices = c("All", courses), selected = "All", width = "100%")
    } else {
      selectInput("course_select", "Select Course:",
                choices = c("All"), selected = "All", width = "100%")
    }
  })

  # Filtered data based on course selection
  filtered_data <- reactive({
    data <- read_gsheet_data()
    if (input$course_select != "All") {
      data <- data[data$Course == input$course_select, ]
    }
    data
  })

  # Handicap plot
  output$handicap_plot <- renderPlotly({
    data <- read_gsheet_data()
    if (nrow(data) > 0) {
      p <- plot_ly(data, x = ~Date, y = ~Handicap, type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#005E3C'), marker = list(color = '#005E3C')) %>%
           layout(title = "",
                  xaxis = list(title = "Date", rangemode = "normal"),
                  yaxis = list(title = "Handicap", rangemode = "normal"))
      p
    }
  })

  # Points plot
  output$points_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      p <- plot_ly(data, x = ~Date, y = ~Points, type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#005E3C'), marker = list(color = '#005E3C')) %>%
           layout(title = "",
                  xaxis = list(title = "Date", rangemode = "normal"),
                  yaxis = list(title = "Points", rangemode = "normal"))
      p
    }
  })

  # Putts plot
  output$putts_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      p <- plot_ly(data, x = ~Date, y = ~Putts, type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#005E3C'), marker = list(color = '#005E3C')) %>%
           layout(title = "",
                  xaxis = list(title = "Date", rangemode = "normal"),
                  yaxis = list(title = "Putts per Hole", rangemode = "normal"))
      p
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server) 
