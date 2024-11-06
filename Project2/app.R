#Eric Song
#ST558 Project-2

library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)


# Define UI for the Shiny app

ui <- dashboardPage(
  dashboardHeader(title = "Seoul Bike Data"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Exploration", tabName = "explore"),
      menuItem("Data Download", tabName = "datadownload")
    )
  ),
  dashboardBody(
    tabItems(
      
      # About tab
      tabItem(
        tabName = "about",
        titlePanel("About"),
        box(
        img(src = "bicycle.jpg",style = "display: block")),
        box(
          width=12,
          title = "Welcome to the Seoul Bike Data Explorer",
          "This app is designed to help users interactively explore bike rental data from Seoul, South Korea. 
    You can use it to analyze bike usage patterns across different times, seasons, weather conditions, 
    and holidays, which might help in understanding trends and usage behaviors."
        ),
        
        box(
          width=12,
          title = "About the Data",
          "The data comes from the Seoul Open Data Plaza. 
    This specific dataset includes information on bike rental counts, weather details, and timestamps for each 
    observation, allowing for in-depth analysis of rental trends. For more information about this dataset, you 
    can visit the Seoul Open Data Plaza's website:",
          tags$a(href = "https://data.seoul.go.kr/", "Seoul Open Data Plaza")
        ),
        box(
          width=12,
          title="How to use this site:",
          "Use the Data exploration tab to select the variables you want.  Hit the explore button to then generate graphs and statistical information.
          If you want to download the data you requested, click on the data download tab and download button.  It will save as a csv."
        )
      ),
      # Data Exploration tab
      tabItem(
        tabName = "explore",
        titlePanel("Data Exploration"),
        
        # Dynamic plot output
        fluidRow(
          box(
            title = "Plot",
            plotOutput("dynamic_plot")
          )
        ),
        
        # Data table and summary UI
        fluidRow(
            title = "Statistics",
            uiOutput("data_table_ui")
        ),
        
        # Seasons select
        selectInput(
          inputId = "seasons",
          label = "Select Season:",
          choices = c("All Seasons", "Spring", "Summer", "Autumn", "Winter"),
          selected = "All Seasons"
        ),
        # Holiday select
        selectInput(
          inputId = "holiday",
          label = "Holiday or Not:",
          choices = c("All Days", "Holiday", "No Holiday"),
          selected = "All Days"
        ),
        # Date select
        dateRangeInput(
          inputId = "date_range",
          label = "Select Date Range:",
          start = as.Date("2017-12-01"),   
          end = as.Date("2018-11-30"),     
          min = as.Date("2017-12-01"),
          max = as.Date("2018-11-30")
        ),
        # Hours select
        sliderInput(
          inputId = "hours_range",
          label = "Hours:",
          min = 0,
          max = 24,
          value = c(0, 24),
          step = 1
        ),
        # Numerical variables (y variables)
        checkboxGroupInput(
          inputId = "variables",
          label = "Select Variables to Display:",
          choices = list(
            "Temperature (°C)" = "Temperature",
            "Humidity (%)" = "Humidity",
            "Rain (mm)" = "Rainfall",
            "Wind (m/s)" = "Wind speed ",
            "UV (MJ/m2)" = "Solar Radiation ",
            "Snowfall (cm)" = "Snowfall ",
            "Rented Bike Count" = "Rented Bike Count"
          )
        ),
        # Select Plot
        radioButtons(
          inputId = "plot_type",
          label = "Select Plot Type:",
          choices = list(
            "Bar Plot" = "bar", 
            "Scatter Plot" = "scatter", 
            "Box Plot" = "box",
            "Density Plot" = "density",
            "Facet Plot" = "facet" ),
          selected = "scatter"
        ),
        #Action
        actionButton(inputId = "action_button", label = "Explore!")
      ),
      # Data Download tab
      tabItem(
        tabName = "datadownload",
        titlePanel("Data Download"),
        box(
          title = "Data Preview",
          width = 12,
          dataTableOutput("data_preview")
        ),
        downloadButton(
          outputId = "download_data", 
          label = "Download Data"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load and clean the dataset
  bike_data <- read.csv("SeoulBikeData.csv", fileEncoding = "ISO-8859-1", check.names = FALSE)
  # remove special characters
  names(bike_data) <- gsub("\\(.*\\)", "", names(bike_data))
  # Convert Date/Hour/seasons columns into correct datatypes, handling missing values
  bike_data$Date <- as.Date(bike_data$Date, format = "%d/%m/%Y")
  bike_data$Hour <- as.numeric(bike_data$Hour)
  bike_data$Seasons <- as.character(bike_data$Seasons)
  
  # Reactive values
  plot_data <- reactiveValues(data = NULL, summary = NULL, plot_type = NULL, y_var = NULL)
  
  # Update the reactive values based on the action button
  observeEvent(input$action_button, {
    # Error check for 0 variables
    if (length(input$variables) == 0) {
      showModal(modalDialog(
        title = "Selection Error",
        "Please select at least one variable to display on the y-axis."
      ))
      return()
    }
    y_vars <- input$variables
    y_var <- y_vars[1]
    
    # Filter the data based on user inputs
    filtered_data <- bike_data |>
      filter(
        (Seasons %in% if (input$seasons == "All Seasons") c("Winter", "Summer", "Autumn", "Spring") else input$seasons),
        (Holiday == input$holiday | input$holiday == "All Days"),
        Date >= input$date_range[1] & Date <= input$date_range[2],
        Hour >= input$hours_range[1] & Hour < input$hours_range[2]
      ) |>
      select(c("Date", "Hour", "Seasons", "Holiday", all_of(y_vars)))
    
    # Create a statistics table
    summary_table <- filtered_data |>
      pivot_longer(cols = all_of(y_vars), names_to = "Variable", values_to = "Value") |>
      group_by(Variable) |>
      summarise(
        Count = n(),
        Min = min(Value),
        Mean = mean(Value),
        Median = median(Value),
        Max = max(Value),
        SD = sd(Value)
      )
    
    # Update reactive values
    plot_data$data <- filtered_data  
    plot_data$summary <- summary_table  
    plot_data$plot_type <- input$plot_type
    plot_data$y_var <- y_var  
    
  })
  
  # Draw plot
  output$dynamic_plot <- renderPlot({
    req(plot_data$data, plot_data$y_var, plot_data$plot_type)
    
    data <- plot_data$data
    y_var <- plot_data$y_var
    
    # Removes spaces 
    y_var <- if (grepl(" ", y_var)) paste0("`", y_var, "`") else y_var
    
    # Different plot types based on the selection
    if (plot_data$plot_type == "scatter") {
      # Scatter plot with color by season
      react_plot <- ggplot(data, aes_string(x = "Hour", y = y_var, color = "Seasons")) +
        geom_point() +
        labs(title = paste("Scatter Plot of", y_var, "by Hour and Season"),
             x = "Hour", y = y_var)
      
    } else if (plot_data$plot_type == "bar") {
      # Bar plot grouped by season
      react_plot <- ggplot(data, aes_string(x = "Seasons", y = y_var, fill = "Seasons")) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Bar Plot of", y_var, "by Season"),
             x = "Season", y = y_var) 
      
    } else if (plot_data$plot_type == "box") {
      # Box plot by season 
      react_plot <- ggplot(data, aes_string(x = "Seasons", y = y_var, fill = "Seasons")) +
        geom_boxplot() +
        labs(title = paste("Box Plot of",y_var, "by Season"),
             x = "Season", y = y_var)

    } else if (plot_data$plot_type == "density") {
      # Density plot by season
      react_plot <- ggplot(data, aes_string(x = y_var, fill = "Seasons")) +
        geom_density() +
        labs(title = paste("Density Plot of", y_var, "by Season"),
             x = y_var, y = "Density")
    } else if (plot_data$plot_type == "facet") {
      # Scatter plot with facet
      react_plot <- ggplot(data, aes_string(x = "Hour", y = y_var, color = "Holiday")) +
        geom_point() +
        labs(title = paste("Faceted Scatter Plot of", y_var, "by Hour, Season, and Holiday"),
             x = "Hour", y = y_var) +
        facet_wrap(~Seasons)
    }
    
    # Print the plot
    print(react_plot)
  })
  
  
  
  # Statistics table
  output$data_table_ui <- renderUI({
    req(plot_data$summary)
    dataTableOutput("data_table")
  })
  #Data table in download
  output$data_preview <- renderDataTable({
    req(plot_data$data)  
    plot_data$data      
  })
  # Download csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_seoul_bike_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plot_data$summary, file)
    }
  )
}




# Run the Shiny app
shinyApp(ui = ui, server = server)