library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(bslib)
library(plotly)
library(fontawesome)
library(thematic)


# Set Pantone 342C as the accent color (RGB: 0, 94, 60)
thematic_shiny(font = "auto", accent = "#005E3C")

sheet_id <- "1GM_VmaCJ_A_WqsjLrdAgjMnV48RXqDBimVIzismitqE"


gs4_deauth()

read_gsheet_data <- function(sheet_id, n) {
  tryCatch({
    df <- read_sheet(sheet_id, sheet = n) %>%
      mutate(date = as.Date(date))
    if (nrow(df) == 0) {
      stop("No data found inn the sheet")
    }
    df
  }, error = function(e) {
    message("Error reading data, following error: ", e$message)
    data.frame(
      date = as.Date(character()),
      handicap_index = numeric(),
      points_normalized = numeric(),
      putts_normalized = numeric(),
      holes = numeric(),
      tournament = character(),
      course = character()
    )
  })
} 


read_gsheet_data_2 <- function(sheet_id, n) {
  tryCatch({
    df <- read_sheet(sheet_id, sheet = n) %>%
      mutate(date = as.Date(date, format = "%d.%m.%y"))
    if (nrow(df) == 0) {
      stop("No data found inn the sheet")
    }
    df
  }, error = function(e) {
    message("Error reading data, following error: ", e$message)
    data.frame(
      date = as.Date(character()),
      handicap_index = numeric(),
      points_normalized = numeric(),
      putts_normalized = numeric(),
      holes = numeric(),
      tournament = character(),
      course = character()
    )
  })
} 

# Custom theme
custom_theme <- bs_theme(
  version = 5,
  primary = "#005E3C",
  secondary = "#ffc41e",
  success = "#b71313",
  "navbar-bg" = "#005E3C",
  "card-border-color" = "#005E3C", 
  "card-border-width" = "2px",
  "card-border-radius" = "1rem",
  "card-box-shadow" = "0 2px 4px rgba(0,0,0,0.15)",
  "spacer" = "1.5rem",
  # Aptos typography
  "font-family-base" = "'Aptos', sans-serif",
  "headings-font-family" = "'Aptos', sans-serif",
  "headings-font-weight" = "normal",
  "navbar-brand-font-family" = "'Aptos', sans-serif",
  "navbar-brand-font-size" = "2rem",
  "navbar-brand-font-weight" = "normal",
  "navbar-brand-text-transform" = "uppercase",
  "navbar-brand-letter-spacing" = "0.2em"
)

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
        style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; object-fit: cover; z-index: -1; opacity: 0.3;"
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
            box-shadow: 0 4px 8px rgba(0,0,0,0.2);
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
            value = uiOutput("n_9"),
            showcase = icon("golf-ball-tee", class = "fa-lg"),
            theme = "primary",
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "18-Hole Rounds",
            value = uiOutput("n_18"),
            showcase = icon("golf-ball-tee", class = "fa-lg"),
            theme = "secondary",
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "Tournaments",
            value = uiOutput("n_tourn"),
            showcase = icon("trophy", class = "fa-lg"),
            theme = "primary",
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          ),
          value_box(
            title = "Handicap",
            value = uiOutput("hcp"),
            showcase = icon("chart-line", class = "fa-lg"),
            theme = "success",
            class = "shadow-sm",
            height = "150px",
            style = "font-size: 1.2rem;"
          )
        ),
        
        div(style = "margin-top: 1.5rem;"),
        
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Handicap Trend"),
            plotlyOutput("plot_hcp", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          )
        ),
        
        div(style = "margin-top: 1.5rem;"),
        
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Course Selection"),
            div(
              style = "max-height: 200px; overflow-y: auto;",
              uiOutput("selector_ui")
            ),
            class = "shadow-sm",
            height = "auto",
            style = "padding: 1rem;"
          )
        ),
        
        div(style = "margin-top: 1.5rem;"),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Normalized Points"),
            plotlyOutput("plot_points", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ),
          card(
            card_header("Putts per Hole"),
            plotlyOutput("putts_per_hole", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ),
          card(
            card_header("Green in Regulation"),
            plotlyOutput("green_in_regulation", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ),
          card(
            card_header("Fairways hit"),
            plotlyOutput("fairways_hit", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          )
          
        ),
        
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Fairways Miss"),
            plotlyOutput("fairways_dir", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ))
        
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
          col_widths = c(6,6),
          card(
            card_header("Club Selection"),
            div(
              style = "max-height: 200px; overflow-y: auto;",
              uiOutput("selector_club_ui")
            ),
            class = "shadow-sm",
            height = "auto",
            style = "padding: 1rem;"
          ),
          card(
            card_header("Time interval "),
            div(
              style = "max-height: 200px; overflow-y: auto;",
              uiOutput("selector_time_interval")
            ),
            class = "shadow-sm",
            height = "auto",
            style = "padding: 1rem;"
          ),
          
        ),
        
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Summary carry"),
            plotlyOutput("summary_carry", height = "300px"),
            class = "shadow-sm",
            height = "400px"
          ))
        
        
      )
    )
  )
) 

# Server
server <- function(input, output, session) {
  data <- reactive({
    df <- read_gsheet_data(sheet_id, n = 1)
    if (nrow(df) == 0) {
      showNotification("No data available. Please check the Google Sheet connection.", type = "error")
    }
    df
  })
  
  output$hcp <- renderText({ 
    req(data())
    if (nrow(data()) > 0) {
      hcp_str <- data()$handicap_index[length(data()$handicap_index)]
      if (is.na(hcp_str)) "N/A" else hcp_str
    } else {
      "N/A"
    }
  }) 
  
  output$n_9 <- renderText({ 
    req(data())
    if (nrow(data()) > 0) {
      n_9 <- data() %>% 
        filter(holes == 9) %>% 
        nrow()
      if (is.na(n_9)) "0" else n_9
    } else {
      "0"
    }
  }) 
  
  output$n_18 <- renderText({ 
    req(data())
    if (nrow(data()) > 0) {
      n_18 <- data() %>% 
        filter(holes == 18) %>% 
        nrow()
      if (is.na(n_18)) "0" else n_18
    } else {
      "0"
    }
  }) 
  
  output$n_tourn <- renderText({ 
    req(data())
    if (nrow(data()) > 0) {
      tours <- sum(data()$tournament == "y", na.rm = TRUE)
      if (is.na(tours)) "0" else tours
    } else {
      "0"
    }
  }) 
  
  output$selector_ui <- renderUI({
    req(data())
    if (nrow(data()) > 0) {
      courses <- unique(data()$course)
      radioButtons("course", "Select course:",
                   choices = c("All", courses), 
                   selected = "All",
                   inline = TRUE)
    } else {
      radioButtons("course", "Select course:",
                   choices = "No courses available",
                   selected = "No courses available",
                   inline = TRUE)
    }
  })
  
  filtered_data <- reactive({
    req(data(), input$course)
    if (nrow(data()) == 0) {
      return(data())
    }
    if (input$course == "All") {
      data()
    } else {
      data() %>% filter(course == input$course)
    }
  })
  
  output$plot_hcp <- renderPlotly({
    req(data())
    if (nrow(data()) == 0) {
      return(plotly_empty())
    }
    fig <- plot_ly(data(), height = 300, x = ~date, y= ~handicap_index, 
                   type = "scatter", mode = "lines+markers",
                   fill = "tozeroy", 
                   alpha = 0.1, color = I("#005E3C")) %>%
      layout( 
        xaxis = list(visible = TRUE, showgrid = FALSE, title = "Time"), 
        yaxis = list(visible = TRUE, showgrid = FALSE, title = "Handicap", rangemode = "normal"), 
        hovermode = "x", 
        margin = list(t = 30, r = 30, l = 30, b = 30), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" )
    fig
  })
  
  output$plot_points <- renderPlotly({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    fig <- plot_ly(filtered_data(), height = 300, x = ~date, y= ~points_normalized, 
                   type = "scatter", mode = "lines+markers",
                   fill = "tozeroy", 
                   alpha = 0.1, color = I("#005E3C")) %>%
      layout( 
        xaxis = list(visible = TRUE, showgrid = FALSE, title = "Time"), 
        yaxis = list(visible = TRUE, showgrid = FALSE, title = "Points", rangemode = "normal"), 
        hovermode = "x", 
        margin = list(t = 30, r = 30, l = 30, b = 30), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" )
    fig
  })
  
  output$putts_per_hole <- renderPlotly({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    fig <- plot_ly(filtered_data(), height = 300, x = ~date, y= ~putts_normalized, 
                   type = "scatter", mode = "lines+markers",
                   fill = "tozeroy", 
                   alpha = 0.1, color = I("#005E3C")) %>%
      layout( 
        xaxis = list(visible = TRUE, showgrid = FALSE, title = "Time"), 
        yaxis = list(visible = TRUE, showgrid = FALSE, title = "Putts per Hole", rangemode = "normal"), 
        hovermode = "x", 
        margin = list(t = 30, r = 30, l = 30, b = 30), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" )
    fig
  })
  
  
  output$green_in_regulation <- renderPlotly({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    fig <- plot_ly(filtered_data(), height = 300, x = ~date, y= ~gir_normalized, 
                   type = "scatter", mode = "lines+markers",
                   fill = "tozeroy", 
                   alpha = 0.1, color = I("#005E3C")) %>%
      layout( 
        xaxis = list(visible = TRUE, showgrid = FALSE, title = "Time"), 
        yaxis = list(visible = TRUE, showgrid = FALSE, title = "Green in regulation", rangemode = "normal"), 
        hovermode = "x", 
        margin = list(t = 30, r = 30, l = 30, b = 30), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" )
    fig
  })
  
  
  output$fairways_hit <- renderPlotly({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    fig <- plot_ly(filtered_data(), height = 300, x = ~date, y= ~fairway_hits_normalized, 
                   type = "scatter", mode = "lines+markers",
                   fill = "tozeroy", 
                   alpha = 0.1, color = I("#005E3C")) %>%
      layout( 
        xaxis = list(visible = TRUE, showgrid = FALSE, title = "Time"), 
        yaxis = list(visible = TRUE, showgrid = FALSE, title = "Fairways Hit", rangemode = "normal"), 
        hovermode = "x", 
        margin = list(t = 30, r = 30, l = 30, b = 30), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" )
    fig
  })
  
  output$fairways_dir <- renderPlotly({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
  fig <- plot_ly(filtered_data(), x = ~date) %>%
    add_trace(y = ~fairway_l_normalized, name = "Left Miss", type = 'bar', opacity = 0.8,   marker = list(color = "#b71313",      line = list(color = 'rgba(0, 0, 0, 0.8)', width = 1)) ) %>%
    add_trace(y = ~fairway_hits_normalized, name = "Hits", type = 'bar', opacity = 0.8,   marker = list(color = '#005E3C',      line = list(color = 'rgba(0, 0, 0, 0.8)', width = 1)) ) %>%
    add_trace(y = ~fairway_r_normalized, name = "Right Miss", type = 'bar',   opacity = 0.8, marker = list(color = "#ffc41e",      line = list(color = 'rgba(0, 0, 0, 0.8)', width = 1)) ) %>%
    layout(
      barmode = 'group',
      xaxis = list(title = "Time", showgrid = FALSE),
      yaxis = list(title = "Fairway normalized", showgrid = FALSE),
      hovermode = "x",
      margin = list(t = 30, r = 30, l = 30, b = 30),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    )
  fig
  })
  
  
  ############### Club Data #############
  
  output$selector_club_ui <- renderUI({
    req(data())
    if (nrow(data()) > 0) {
      club_list <- c("D", "3W", "3H", "4i", "5i", "6i", "7i", "8i", "9i", "PW", "GW", "SW", "LW")
      checkboxGroupInput("club", "Select clubs:",
                         choices = c("All", club_list), 
                         selected = "All",
                         inline = TRUE)
    }
  })
  
  # Observe the selection and toggle 'All' based on other selections
  observe({
    selected_clubs <- input$club
    
    if ("All" %in% selected_clubs) {
      # If "All" is selected, unselect all others
      updateCheckboxGroupInput(session, "club", selected = "All")
    } else {
      # If any club is selected, ensure "All" is not selected
      updateCheckboxGroupInput(session, "club", selected = selected_clubs)
    }
  })
  
  output$selector_time_interval <- renderUI({
    req(data())
    if (nrow(data()) > 0) {
      intervals <- c("Always", "Last month", "Last 3 months", "Last 6 months", "Last year")
      radioButtons("time_intervals", "Select time interval:",
                   choices = intervals, 
                   selected = "Last month",
                   inline = TRUE)
    }
  })
  
  
  data_club <- reactive({
    df <- read_gsheet_data_2(sheet_id, n = 2)
    if (nrow(df) == 0) {
      showNotification("No data available. Please check the Google Sheet connection.", type = "error")
    }
    df
  })
  
    
  filtered_club_data <- reactive({
    req(data_club(), input$time_intervals, input$club)
    if (nrow(data_club()) == 0) {
      return(data_club())
    }
    if (input$club == "All") {
      data_club()
    } else {
      data_club() %>% filter(club_type %in% input$club)
    }
  })
  
  
  ##### Sistemare qui. In piu. 
  
  
  output$summary_carry <- renderPlotly({
    req(filtered_club_data())
    
    
    
    
    club_levels <- sort(unique(filtered_df$club_type))
    colors <- c(
      "#DDFFE7", "#98D7C2", "#29A0B1", "#167D7F",
      "#FABEC0", "#F85C70", "#F37970", "#E43D40",
      "#E7625F", "#C85250", "#BFD7ED", "#60A3D9", "#0074B7"
    )
    names(colors) <- club_levels
    
    
    fig <- p <- plot_ly()
    
    for (club in club_levels) {
      club_data <- filtered_df %>% filter(club_type == club)
      p <- add_trace(
        p,
        y = club_data$club_type,
        x = club_data$carry,
        type = "box",
        name = club,
        orientation = 'h',
        boxpoints = FALSE,
        line = list(color = "black", width = 1),  # Border around boxes
        fillcolor = colors[club],
        marker = list(color = colors[club]),
        showlegend = FALSE
      )
    }
    
    # Add median labels to the left
    median_text_x <- 25
    
    p <- add_trace(
      p,
      type = "scatter",
      mode = "text",
      x = rep(median_text_x, nrow(list_medians)),
      y = list_medians$club_type,
      text = list_medians$median_carry,
      textposition = "middle right",
      textfont = list(size = 15, color = "black"),
      showlegend = FALSE,
      hoverinfo = "none"
    )
    
    # Layout with stronger axes
    p <- layout(
      p,
      xaxis = list(
        title = "Carry (m)",
        tickvals = seq(30, 230, 10),
        range = c(20, 230),
        showline = TRUE,
        linewidth = 1,
        linecolor = "black"
      ),
      yaxis = list(
        title = "Club",
        tickfont = list(size = 14),
        showline = TRUE,
        linewidth = 1,
        linecolor = "black"
      ),
      margin = list(l = 120, r = 50),
      template = "plotly_white"
    )
    
     
  })
  
  
  
}

shinyApp(ui, server) 