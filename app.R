# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(magrittr)
library(shinydashboard)
library(rvest)
library(stringr)
library(readr)
library(purrr)
library(tidyr)
library(bslib)
library(markdown)

# Unzip the file (will extract PitcherPCA.csv to a temp directory)
unzip("data/PitcherPCA.zip", exdir = tempdir())
pitcher_path <- file.path(tempdir(), "PitcherPCA.csv")

# Unzip Hitter PCA
unzip("data/Hitters.zip", exdir = tempdir())
hitter_path <- file.path(tempdir(), "Hitters.csv")

# Read the CSV
Rendering_Data <- function(type){
  if(type == "pitcher"){
  file <- na.omit(read.csv(pitcher_path))
  } else{
   file <- na.omit(read.csv(hitter_path))
  }
  return(file)
}


### Functions
# Reusable R function that scrape player information from pioneers.grinnell.edu/sports/baseball/stats
get_player_athletic_stats <- function(year, type){
  
  tryCatch({
    # Read page (this example is public - replace with real URL if possible)
    page <- read_html(paste0("https://pioneers.grinnell.edu/sports/baseball/stats/",year))
    
    # Get player tables
    table_nodes <- html_nodes(page, "table")
    
    index <- ifelse(type == "pitcher", 2, 1)
    
    if (length(table_nodes) < index) {
      stop("Expected table not found on the page.")
    }
    
    # Extract tables
    stats_table <- html_table(table_nodes[[index]], fill = TRUE)
    # Convert to data frame
    stats_pioneer <- as.data.frame(stats_table)
    
    #  Format name
    stats_pioneer <- stats_pioneer[1:(nrow(stats_pioneer)-2),]
    
    if(type == "pitcher") {
      result <- stats_pioneer %>%
        filter(IP >= 10) %>% #included a hitter if they met the requirements of throwing 10 innings in a season
        mutate(
          XBH = `2B` + `3B` + HR,
          Year = as.integer(year),
          B.AVG = `B/AVG`,
          # Extract only the first "Last, First" part before the junk
          Name = str_extract(Player, "^[A-Za-z]+,\\s*[A-Za-z]+"),
          # Reformat to "First Last"
          Name = str_replace(Name, "(.*),\\s*(.*)", "\\2 \\1")
        )%>%
        select(
          Name, Year, ERA, WHIP, H, XBH, BB, WP, HBP, B.AVG, SO, IP
        )
    } else {
      result <- stats_pioneer %>%
        filter(AB >= 25) %>%
        mutate(
          XBH = `2B` + `3B` + HR,
          Year = as.integer(year),
          OBP = `OB%`,
          SLG = `SLG%`,
          PA = AB + BB + HBP + SF + SH,
          `BB%` = BB / PA,
          `K%` = SO / PA,
          # Extract only the first "Last, First" part before the junk
          Name = str_extract(Player, "^[A-Za-z]+,\\s*[A-Za-z]+"),
          # Reformat to "First Last"
          Name = str_replace(Name, "(.*),\\s*(.*)", "\\2 \\1")
        )%>%
        select(
          Name, Year, AVG, OBP, SLG, XBH, `BB%`,`K%`
        )
    }
    return(result)
  }, error = function(e) {
    message <- paste("⚠️ Error scraping year", type, "year", year, ":", e$message)
    print(message)
    return(NULL)  # Return NULL so downstream logic can skip it
  })
}



# Reusable R function that scrape player information from https://quest.inside-edge.com
get_player_edge_stats <- function(year, data, type){
  players <- data %>%
    mutate(
      player_link = paste0(
        "https://quest.inside-edge.com/",
        type,
        "EvaluationReport/",
        type,
        "Evaluation?id=",
        ID,
        "&reg_season_start=",year,"-01-01",
        "&reg_season_end=",year,"-12-31",
        "&last_season_reg_season_start=null",
        "&last_season_reg_season_end=null",
        "&drange=undefined",
        "&vs=B",
        "&mlb_sportcode=amateur",
        "&StatYear=",year,
        "&GameIds=",
        "&GameDates=",
        "&charterteamid=15#panelShareButtonsList"
      )
    )
  # Initialize an empty dataframe
  all_players <- data.frame()
  # Loop over each individual player
  for (i in seq_len(nrow(players))) {
    player_name <- players$Name[i]
    url <- players$player_link[i]
    
    # Print URL to debug
    print(paste("Scraping URL for:", player_name, "URL:", url))
    
    # tryCatch to handle errors gracefully
    tryCatch({
      # Scrape the page
      page <- read_html(url)
      
      # Extract all tables from the page(pitchers is the second table, hitter 3rd)
      table <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
      table_index <- ifelse(type == "pitcher", 2, 3)
      final_df <- table[[table_index]]
      
      # Check if table is valid and write to CSV
      if (length(final_df) > 0 && is.data.frame(final_df)) {
        
        # clean the data
        final_df <- final_df[, 1:2] # only concerned with overall column
        
        # Transpose rows and columns
        transposed <- as.data.frame(t(final_df))
        colnames(transposed) <- transposed[1, ]
        transposed <- transposed[-1, ]
        transposed$myfactor <- factor(row.names(transposed))
        
        if (type == "pitcher") {
          # select and rename the 5 stats
          selected  <- transposed[, c(3,7,9,5,11)]
          colnames(selected ) <- c("WellHit", "Chase", "Miss", "Strike", "GroundBall")
        } else {
          # select and rename the 5 stats
          selected  <- transposed[, c(3,7,11,15)]
          colnames(selected ) <- c("WellHit%", "Miss%", "Chase%", "GroundBall")
        }
        
        # Convert percentage/ratio char to numeric decimal
        selected  <- selected  %>%
          mutate(across(everything(), ~ {
            val <- str_extract(., "^\\d*\\.?\\d*")        # Extract number before % or /
            val <- as.numeric(val)
            ifelse(val > 1, val / 100, val)               # Convert to decimal if percentage
          })) %>%
          mutate(Name = player_name) %>% # Add playername to row
          select(Name, everything())
        
        # Add to the master dataframe
        all_players <- bind_rows(all_players, selected)
        
        print(paste("Successfully wrote table for", player_name))
      } else {
        print(paste("No valid table found for", player_name))
      }
      
    }, error = function(e) {
      # If an error occurs, print the message and continue
      print(paste("Error for", player_name, ":", e$message))
    })
    
  }
  print("All players processed and saved.")
  print(all_players)
  return(all_players)
}

loadings <- NULL

# Reusable R function that performs PCA on a given dataframe
perform_player_pca <- function(df, type) {
  print(df)
  ## Data Cleaning
  df <- df%>%
    na.omit()
  # Clean Year column (ensure it's numeric)
  df$Year <- as.numeric(as.character(df$Year))
  # Prepare PCA data (excluding Name)
  PlayerPCA_Data <- df%>% select(-Name)
  if(type =="pitcher"){
    PlayerPCA_Data <- df%>% select(-IP)
  }
  
  # Scale the data (excluding Year)
  PlayerPCA_Data <- PlayerPCA_Data[, sapply(PlayerPCA_Data, is.numeric)]
  Scaler <- scale(PlayerPCA_Data[, !names(PlayerPCA_Data) %in% "Year"])
  
  # Perform PCA
  PCC <- prcomp(Scaler, center = TRUE, scale. = TRUE)
  
  # Create PCA results dataframe
  PlayerPCA <- data.frame(PCC$x)
  PlayerPCA$Year <- PlayerPCA_Data$Year
  
  # Define categories based on Year
  latestyear <- max(PlayerPCA$Year, na.rm = TRUE)
  Bestyear <- 2016
  PlayerPCA$Category <- case_when(
    PlayerPCA$Year == latestyear ~ paste0(latestyear, " Team"),
    PlayerPCA$Year == Bestyear ~ paste0(Bestyear, " Team"),
    TRUE ~ "Other Years"
  )
  
  # Add Name back for labeling
  PlayerPCA$Name <- df$Name
  
  # Gets rid of blank space and checkboxes for names that aren't there
  
  valid_names <- unique(df$Name)
  valid_names <- valid_names[!is.na(valid_names)]
  valid_names <- trimws(valid_names)
  valid_names <- valid_names[valid_names != ""]
  
  loadings <- PCC$rotation  # This is the PCA rotation matrix
  print(loadings)
  return(list(PlayerPCA = PlayerPCA, loadings = loadings))  # Return both PCA resul
}

# Function to interpret the principal components
interpret_pc <- function(pc_loadings, type, top_n = 4) {
  print("Top vars:")
  print(pc_loadings)
  
  # Find top variable names (by absolute loading)
  top_vars <- names(sort(abs(pc_loadings), decreasing = TRUE))[1:top_n]
  print(top_vars)
  
  # Define stat themes
  if (type == "pitcher") {
    themes <- list(
      Command = c("BB", "BB%", "WP", "HBP", "Strike"),
      Contact = c("H", "AVG", "OBP", "SLG", "XBH", "WellHit%", "B.AVG"),
      Dominance = c("SO", "K%", "Miss%", "Chase%"),
      Groundball = c("GroundBall", "GroundBall%"),
      Efficiency = c("ERA", "WHIP")
    )
  } else if (type == "hitter") {
    themes <- list(
      Contact = c("H", "AVG", "OBP", "SLG", "XBH", "WellHit%", "B.AVG"),
      Discipline = c("BB%", "K%", "Chase%", "WellHit%"),
      Power = c("SLG", "XBH", "K%", "SO"),
      Groundball = c("GroundBall", "GroundBall%"),
      Efficiency = c("BA", "OBP")
    )
  }
  
  # Match top vars to themes
  theme_matches <- sapply(top_vars, function(var) {
    theme <- names(Filter(function(stats) var %in% stats, themes))
    paste(theme, collapse = ", ")
  })
  
  # Return interpretation
  interpretation <- data.frame(Variable = top_vars, Theme = theme_matches)
  
  # Return unique themes for labeling buttons
  unique_themes <- unique(theme_matches)
  print(interpretation)
  return(paste(unique_themes, collapse = " + "))
}
# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Grinnell PCA Explorer"),
  
  dashboardSidebar(
    radioButtons("type", "Select Player Type:",
                 choices = c("Pitcher" = "pitcher", "Hitter" = "hitter"),
                 selected = "pitcher"),
    sidebarMenu(
      menuItem("Getting Started", tabName = "a"),
      menuItem("Upload Data", tabName = "b", icon = icon("file")),
      menuItem("Interactive Graphs", tabName = "c"),
      menuItem("Help", tabName = "d", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "a", 
              h2("Getting Started"),
              br(),
              
              
              ## NEW SECTION: Pitcher/Hitter Radio Button
              h3(icon("baseball"), "Select Pitcher or Hitter"),
              p("Before starting, make sure to choose either ", strong("Pitcher"), " or ", strong("Hitter"), " at the top of the dashboard."),
              p("This selection determines which type of player stats you'll be working with."),
              
              h3("Understanding the Data"),
              p("• The default dataset includes seasons from ", strong("2015 to 2023"), "."),
              p("• If you want to include ", strong("2024 or later"), ", you'll need to scrape it yourself."),
              p(icon("triangle-exclamation", style = "color: rgb(244,102,102)")," If you try to scrape a future year (e.g., 2026 while it's still 2025), the app will return an error because that data doesn't exist yet."),
              br(),
              
              h3("Step 1: Select a Year to Add"),
              p("• Select the year you want to analyze (e.g., 2024)."),
              numericInput( 
                "year", 
                "Input Year", 
                value = 2024, 
                min = 2023, 
                max = NA 
              ), 
              p("• The app will scrape names from ", em("Grinnell Athletics"), " for that year and return a CSV file."),
              uiOutput("download_button"),
              textOutput("errorText"),
              p("• The CSV includes cleaned ", code("PlayerName"), " values and a blank ", code("PlayerID"), " column for you to fill in."),
              br(),
              
              h3("Step 2: Fill in Player IDs"),
              p("• Open the CSV file from Step 1."),
              p("• Use ", em("Inside Edge"), " to find and copy the correct ", code("PlayerID"), " for each ", code("PlayerName"), "."),
              p("• Save the completed file."),
              br(),
              
              h3(icon("upload"),"Step 3: Upload Your File"),
              p("• Go to the ", strong("'Upload'"), " page."),
              p("• Upload the completed CSV file (with names + player IDs)."),
              p(icon("check", style = "color: rgb(99,230,190)"), "If uploaded: the app will include that year in the analysis."),
              p(icon("ban", style = "color: rgb(238,17,17)"),"If not uploaded: the app will default to showing data from 2015–2023 only."),
              br(),
              
              h3("View Interactive Graphs"),
              p("• After uploading, go to the ", strong("'Interactive Graphs'"), " page."),
              p("• The app will perform PCA (Principal Component Analysis) to visualize player performance."),
              br(),
              
              ## NEW SECTION: Help Guide
              h3(icon("circle-info"), "Need Help?"),
              p("We've added a built-in ", strong("Help Guide"), " to walk you through every step of the app."),
              p("Use it to learn how to scrape data, upload files, and interpret PCA results.")
      ),
      tabItem(tabName = "b", 
              fileInput("file1", "Choose CSV File", accept = ".csv"),
              verbatimTextOutput("file1_contents"),
              tableOutput("contents")
      ),
      tabItem(tabName = "c",
              sidebarLayout(
                sidebarPanel(selectInput("xPC", "X Variable", choices = NULL),
                             selectInput("ycol", "Y Variable", choices = NULL),
                             selectInput("startYear", "Select a start Year", choices = NULL),
                             selectInput("endYear", "Select an end Year", choices = NULL),
                             
                             checkboxInput("showLabels", "Show Labels", TRUE),
                             # For controlling the density curve bandwidth
                             sliderInput("bw", "Density Bandwidth:", min = 0.1, max = 3, value = 1, step = 0.1),
                             
                             conditionalPanel(
                               condition = "input.type == 'pitcher'",
                               # your pitcher-specific info
                               checkboxGroupInput("selected_pitchers_players", 
                                                  "Select Pitchers to Display:",
                                                  choices = NULL)
                             ),
                             conditionalPanel(
                               condition = "input.type == 'hitter'",
                               # your hitter-specific info
                               checkboxGroupInput("selected_hitters_players", 
                                                  "Select Hitters to Display:",
                                                  choices = NULL)
                             )
                ),
                
                mainPanel(
                  tabsetPanel(
                    
                    # Tab to upload PCA biplot for players
                    tabPanel("Biplot", plotOutput("biplot"), 
                             h4("How to Read This Chart"),  tags$ul(
                               tags$li("Each arrow represents a variable (e.g., strikeout rate, contact %, etc.)."),
                               tags$li("The direction of the arrow shows how that variable contributes to the PCs being plotted."),
                               tags$li("Variables pointing in the same direction are positively correlated."),
                               tags$li("Variables pointing in opposite directions are negatively correlated."),
                               tags$li("Longer arrows indicate stronger influence on the component."),
                               tags$li("If several arrows point strongly along the same axis (e.g., PC1), they likely define what that component is measuring."),
                               tags$li("You can use the grouping and direction of arrows to infer what each PC represents — whether it's plate discipline, power, consistency, etc.")
                             )),
                    # Tab to upload PCA data for players
                    tabPanel("PCA Plot", plotOutput("pcaPlot")),
                    # Tab to upload density plot for players
                    tabPanel("Density Plot",       
                             fluidRow(
                               column(12,
                                      actionButton("btn_density1", ""),
                                      actionButton("btn_density2", ""),
                                      actionButton("btn_density3", ""),
                                      br(), br(),
                                      plotOutput("densityPlot") 
                               )
                             )),
                    # Tab for hoverplots to be able to hover over player names and see where they are at pca
                    tabPanel("Hover Plot", 
                             fluidRow(
                               column(12,
                                      actionButton("btn_pc1", ""),
                                      actionButton("btn_pc2", ""),
                                      actionButton("btn_pc3", ""),
                                      br(), br(),
                                      plotlyOutput("hoverPlot")
                               )
                             ))
                  )
                ),
                
              )
      ),
      tabItem(tabName = "d",
                includeMarkdown("doc/Stats User Help Guide.md")
        )
      
    ))
)

# Define server logic ----
server <- function(input, output, session) {
  # Getting Started Page Logic
  ## df containing player name and empty ID field for name
  PlayerName <- reactive({
    get_player_athletic_stats(input$year, input$type) %>%
      select(Name) %>%
      mutate(ID = NA_real_)
  })
  
  player_data <- reactiveVal(NULL)
  error_message <- reactiveVal("")
  
  observeEvent(input$year, {
    data <- get_player_athletic_stats(input$year, input$type)
    if (is.null(data)) {
      player_data(NULL)
      error_message(paste("⚠️ Error: Could not retrieve data for year", input$year))
    } else {
      player_data(data)
      error_message("")  # clear previous error
    }
  })
  
  output$download_button <- renderUI({
    if (input$type == "pitcher") {
      downloadButton("downloadData", "Download PitcherName")
    } else {
      downloadButton("downloadData", "Download HitterName")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$type == "pitcher") {
        paste0("PitcherNames_", input$year, ".csv")
      } else {
        paste0("HitterNames_", input$year, ".csv")
      }
    },
    content = function(file) {
      data <- PlayerName()
      if (is.null(data)) {
        showNotification("⚠️ Cannot download: No data available for that year.", type = "error")
        return(NULL)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$errorText <- renderText({
    error_message()
  })
  
  # Upload Data Page Logic
  output$contents <- renderTable({
    file <- input$file1
    req(file)
    
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
  
  # combine both web-scrapped datasets
  combined_data <- reactive({
    req(input$file1)
    req(input$type)  # Ensure type updates this reactive
    new_data <- read.csv(input$file1$datapath)
    full_join(get_player_athletic_stats(input$year, input$type), 
              get_player_edge_stats(input$year, new_data, input$type), by = "Name")
  })
  
  # Check whether a file is uploaded and switch accordingly
  PlayerUpload <- reactive({
    req(input$type)  # Ensure type updates this reactive
    base_data <- Rendering_Data(input$type)  # Default dataset(2015-2023)
    colnames(base_data) <- gsub("[%.]", "", colnames(base_data))
    if (!is.null(input$file1)) {
      new_data <- combined_data()  # Depend on input$type
      colnames(new_data) <- gsub("[%.]", "", colnames(new_data))
      binded <- bind_rows(base_data, new_data)
    } else {
        binded <- base_data
    }
    
    perform_player_pca(binded, input$type)
  })
  
  # Reactive expression to get filtered data
  filtered_data <- reactive({
    data <- PlayerUpload()$PlayerPCA
    # Filter by year range
    if (!is.null(input$startYear) && !is.null(input$endYear)) {
      data <- data %>% filter(Year >= input$startYear, Year <= input$endYear)
    }
    
    # Filter by selected players depending on type
    selected_players <- if (input$type == "pitcher") {
      input$selected_pitchers_players
    } else if (input$type == "hitter") {
      input$selected_hitters_players
    } else {
      NULL
    }
    
    # Use correct input ID for players
    if (!is.null(selected_players) && length(selected_players) > 0) {
      data <- data %>% filter(Name %in% selected_players)
    } else {
      data <- NULL
    }
    
    return(data)
  })
  
  # Filtering years for sidebar/names 
  year_filtered_data <- reactive({
    data <- PlayerUpload()$PlayerPCA
    
    if (!is.null(input$startYear) && !is.null(input$endYear)) {
      data <- data %>% filter(Year >= input$startYear, Year <= input$endYear)
    }
    
    return(data)
  })
  
  # Updates user input side bar values
  observe({
    pca_result <- PlayerUpload()$PlayerPCA
    req(pca_result)
    
    pcs <- c("PC1", "PC2", "PC3")[c("PC1", "PC2", "PC3") %in% names(pca_result)] # Get PC1, PC2, PC3
    
    updateSelectInput(session, "xPC", choices = pcs, selected = pcs[1])
    updateSelectInput(session, "ycol", choices = pcs, selected = pcs[2])
    updateSelectInput(session, "startYear", choices = sort(unique(pca_result$Year)), selected = min(pca_result$Year, na.rm = TRUE))
    updateSelectInput(session, "endYear", choices = sort(unique(pca_result$Year)), selected = max(pca_result$Year, na.rm = TRUE))
  })
  # Updates user input side bar values
  observe({
    data <- year_filtered_data()
    # Update the players list in the checkboxGroupInput
    updateCheckboxGroupInput(session, "selected_pitchers_players",
                             choices = unique(data$Name[!is.na(data$Name) & trimws(data$Name) != ""]),
                             selected = unique(data$Name[!is.na(data$Name) & trimws(data$Name) != ""]))
    
    updateCheckboxGroupInput(session, "selected_hitters_players",
                             choices = unique(data$Name[!is.na(data$Name) & trimws(data$Name) != ""]),
                             selected = unique(data$Name[!is.na(data$Name) & trimws(data$Name) != ""]))
    
  })
  
  output$pcaPlot <- renderPlot({
    req(input$xPC, input$ycol)
    df <- filtered_data()
    
    ggplot(df, aes(x = .data[[input$xPC]], y = .data[[input$ycol]], color = Category)) +
      geom_point(size = 2) +
      { if (input$showLabels) geom_text(aes(label = Name), size = 2, hjust = 0, vjust = 0) } +
      labs(
        title = paste0(tools::toTitleCase(input$type), " PCA Scatterplot"),
        x = input$xPC,
        y = input$ycol
      ) +
      theme_minimal()
  })
  
  
  output$densityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = PC1, fill = Category)) +
      geom_density(alpha = 0.6, bw = input$bw) +  # Adjust the bandwidth based on slider input
      scale_fill_manual(values = c("blue", "red", "grey")) +
      labs(title = "Density of X-Variable by Category",
           x = "PC1 Value", y = "Density") +
      theme_minimal()
  })
  
  # Creates density plots 
  create_density_plot <- function(data, pc_name, x_axis_title, plot_title, bw_value) {
    ggplot(data, aes(x = !!sym(pc_name), fill = Category)) +
      geom_density(alpha = 0.6, bw = bw_value) +
      labs(title = plot_title, x = x_axis_title, y = "Density") +
      theme_minimal()
  }
  
  # Updates desnity plots based on the button the user clicks
  
  observeEvent(input$btn_density1, {
    density_plot(create_density_plot(filtered_data(), "PC1", "PC1", paste0("PC1: Density for ",  tools::toTitleCase(input$type)), input$bw))
  })
  
  observeEvent(input$btn_density2, {
    density_plot(create_density_plot(filtered_data(), "PC2", "PC2", paste0("PC2: Density for ",  tools::toTitleCase(input$type)), input$bw))
  })
  
  observeEvent(input$btn_density3, {
    density_plot(create_density_plot(filtered_data(), "PC3", "PC3", paste0("PC3: Density for ",  tools::toTitleCase(input$type)), input$bw))
  })
  
  # Renders density plot
  output$densityPlot <- renderPlot({
    req(density_plot())
    density_plot()
  })
  
  # Creates the hover plots of PC graphs over time
  create_pc_plotly <- function(data, pc_name, y_axis_title, plot_title) {
    # Assigns each player a unique color for their graphs 
    unique_players <- length(unique(data$Name))
    custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(unique_players)
    plot_ly(data,
            x = ~Year,
            y = ~get(pc_name),
            color = ~Name,
            colors = custom_colors,
            type = 'scatter',
            mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste("Player: ", Name, 
                          "<br>Year: ", Year,
                          "<br>", pc_name, ": ", round(get(pc_name), 2)),
            line = list(width = 1),
            marker = list(size = 8)) %>%
      layout(
        title = plot_title,
        xaxis = list(title = "Year"),
        yaxis = list(title = y_axis_title),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white")
      )
  }
  
  # Initializes plots to null, so they don't give errors
  current_plot <- reactiveVal(NULL)
  density_plot <- reactiveVal(NULL)
  
  observe({
    data <- as.data.frame(PlayerUpload()$loadings)  
    
    # Attach variable names to each PC vector before passing to interpret_pc
    pc1 <- data[, "PC1"]
    names(pc1) <- rownames(data)
    
    pc2 <- data[, "PC2"]
    names(pc2) <- rownames(data)
    
    pc3 <- data[, "PC3"]
    names(pc3) <- rownames(data)
    
    pc1_text <- interpret_pc(pc1, input$type)
    pc2_text <- interpret_pc(pc2, input$type)
    pc3_text <- interpret_pc(pc3, input$type)
    
    # Example condition to change button names dynamically
    updateActionButton(session, "btn_density1", label = paste("PC1:", pc1_text))
    updateActionButton(session, "btn_density2", label = paste("PC2:", pc2_text))
    updateActionButton(session, "btn_density3", label = paste("PC3:", pc3_text))
  })
  
  observe({
    data <- as.data.frame(PlayerUpload()$loadings)  
    
    # Attach variable names to each PC vector before passing to interpret_pc
    pc1 <- data[, "PC1"]
    names(pc1) <- rownames(data)
    
    pc2 <- data[, "PC2"]
    names(pc2) <- rownames(data)
    
    pc3 <- data[, "PC3"]
    names(pc3) <- rownames(data)
    
    pc1_text <- interpret_pc(pc1, input$type)
    pc2_text <- interpret_pc(pc2, input$type)
    pc3_text <- interpret_pc(pc3, input$type)
    
    # Example condition to change button names dynamically
    updateActionButton(session, "btn_pc1", label = paste("PC1:", pc1_text))
    updateActionButton(session, "btn_pc2", label = paste("PC2:", pc2_text))
    updateActionButton(session, "btn_pc3", label = paste("PC3:", pc3_text))
  })
  
  # Updates hover plots or PC plots over time 
  observeEvent(input$btn_pc1, {
    current_plot(create_pc_plotly(filtered_data(), "PC1", "PC1", paste0("PC1: Hover Plot for ",  tools::toTitleCase(input$type))))
  })
  
  observeEvent(input$btn_pc2, {
    current_plot(create_pc_plotly(filtered_data(), "PC2", "PC2", paste0("PC2: Hover Plot for ",  tools::toTitleCase(input$type))))
  })
  
  observeEvent(input$btn_pc3, {
    current_plot(create_pc_plotly(filtered_data(), "PC3", "PC3", paste0("PC3: Hover Plot for ",  tools::toTitleCase(input$type))))
  })
  
  # Renders hover plot 
  output$hoverPlot <- renderPlotly({
    current_plot()
  })
  
  # Render biplot
  output$biplot <- renderPlot({
    loadings <- PlayerUpload()$loadings
    
    pc_x <- input$xPC 
    pc_y <- input$ycol  
    
    arrows <- data.frame(
      varname = rownames(loadings),
      PCX = loadings[, pc_x],
      PCY = loadings[, pc_y]
    )
    
    ggplot() +
      geom_point(alpha = 0.5, color = "steelblue") +
      geom_segment(data = arrows, aes(x = 0, y = 0, xend = PCX * 5, yend = PCY * 5),
                   arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
      geom_text(data = arrows, aes(x = PCX * 5.5, y = PCY * 5.5, label = varname),
                size = 3, hjust = 0.5, vjust = 0.5) +
      xlab(paste0( pc_x, " (", interpret_pc(loadings[, pc_x], input$type), ")")) +
      ylab(paste0(pc_y, " (", interpret_pc(loadings[, pc_y], input$type), ")")) +
      theme_minimal() +
      ggtitle("PCA Biplot: Player Profiles")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
