#Eric Song
#ST558 Project-2

library(shiny)
library(shinydashboard)
library(shinyalert)
library(bslib)
library(tidyverse)




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
          img(src = "bicycle.jpg", style = "display: block")
        ),
        box(
          width = 12,
          title = "Welcome to the Seoul Bike Data Explorer",
          "This app is designed to help users interactively explore bike rental data from Seoul, South Korea. 
          You can use it to analyze bike usage patterns across different times, seasons, weather conditions, 
          and holidays, which might help in understanding trends and usage behaviors."
        ),
        box(
          width = 12,
          title = "About the Data",
          "The data comes from the Seoul Open Data Plaza. 
          This specific dataset includes information on bike rental counts, weather details, and timestamps for each 
          observation, allowing for in-depth analysis of rental trends. Not all variables were used. For more information about this dataset, you 
          can visit the Seoul Open Data Plaza's website:
          https://data.seoul.go.kr/"
        ),
        box(
          width = 12,
          title = "How to use this site:",
          "Use the Data exploration tab to select the variables you want. Hit the explore button to then generate graphs and statistical information.
          If you want to download the data you requested, click on the data download tab and download button. It will save as a CSV."
        )
      ),
      
      # Data Exploration tab
      tabItem(
        tabName = "explore",
        titlePanel("Data Exploration"),
        sidebarLayout(
          sidebarPanel(
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
                "Line Plot" = "line", 
                "Box Plot" = "box",
                "Density Plot" = "density",
                "Facet Plot" = "facet" ),
              selected = "bar"
            ),
            # Action Button
            actionButton(inputId = "action_button", label = "Explore!")
          ),
          mainPanel(
            # Dynamic plot output
            box(
              title = "Plot",
              plotOutput("dynamic_plot"),
              width = 12
            ),
            # Data table and summary UI
            box(
              title = "Statistics",
              uiOutput("data_table_ui"),
              width = 12
            )
          )
        )
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
  # Remove special characters from encoding
  names(bike_data) <- gsub("\\(.*\\)", "", names(bike_data))
  # Convert Date/Hour/seasons columns into correct datatypes so things behave properly.
  bike_data$Date <- as.Date(bike_data$Date, format = "%d/%m/%Y")
  bike_data$Hour <- as.numeric(bike_data$Hour)
  bike_data$Seasons <- as.character(bike_data$Seasons)
  
  # creating reactive values var 
  rv <- reactiveValues(
    filtered_data = NULL,
    summary_table = NULL,
    plot = NULL
  )
  
  # When you click the explore button:
  observeEvent(input$action_button, {
    # Error check for 0 variables
    if (length(input$variables) == 0) {
      shinyalert(title = "Selection Error: Please select at least one variable to display on the y-axis.",
                 type = "error")
      return()
    }
    
    y_vars <- input$variables
    
    # Filter the data based on user inputs
    filtered_data <- bike_data |>
      filter(
        #there is no all seasons category so it creates vector with all 4 variables.
        (Seasons %in% if (input$seasons == "All Seasons") c("Winter", "Summer", "Autumn", "Spring") else input$seasons),
        (Holiday == input$holiday | input$holiday == "All Days"),
        #date filter
        Date >= input$date_range[1] & Date <= input$date_range[2],
        #hour filter
        Hour >= input$hours_range[1] & Hour < input$hours_range[2]
      ) |>
      select(c("Date", "Hour", "Seasons", "Holiday", all_of(y_vars)))
    
    rv$filtered_data <- filtered_data
    
    # summary table
    rv$summary_table <- filtered_data |>
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
    
    # Generate the plot
    data <- filtered_data
    #isolate so it reactive values ignored
    plot_type <- isolate(input$plot_type)
    #if more than 1 y variable
    if (length(y_vars) > 1) {
      data_long <- data |>
        pivot_longer(cols = all_of(y_vars), names_to = "Variable", values_to = "Value")
    } else {
      data_long <- data
      data_long$Variable <- y_vars[1]
      data_long$Value <- data[[y_vars[1]]]
    }
    
    # Create the plot based on the selected plot type
    #scatter
    if (plot_type == "scatter") {
      react_plot <- ggplot(data_long, aes(x = Hour, y = Value, color = Variable)) +
        geom_point() +
        labs(title = paste("Scatter Plot of", paste(y_vars), "by Hour"),
             x = "Hour", y = "Value")
    #Bar  
    } else if (plot_type == "bar") {
      react_plot <- ggplot(data_long, aes(x = Seasons, y = Value, fill = Variable)) +
        geom_bar(stat="identity") +
        labs(title = paste("Bar Plot of", paste(y_vars), "by Season"),
             x = "Season", y = "Mean Value")
    #Box  
    } else if (plot_type == "box") {
      react_plot <- ggplot(data_long, aes(x = Seasons, y = Value, fill = Variable)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", paste(y_vars), "by Season"),
             x = "Season", y = "Value")
    #line
      } else if (plot_type == "line"){
      react_plot <- ggplot(data_long, aes(x = Hour, y = Value, color = Variable, group = Variable)) +
        geom_line()  +
        labs(title = paste("Line Plot of", paste(y_vars), "by Season"))
    #Density  
        } else if (plot_type == "density") {
      react_plot <- ggplot(data_long, aes(x = Value, fill = Variable)) +
        geom_density() +
        labs(title = paste("Density Plot of", paste(y_vars)),
             x = "Value", y = "Density")
    #Facet  
    } else if (plot_type == "facet") {
      react_plot <- ggplot(data_long, aes(x = Hour, y = Value, color = Holiday)) +
        geom_point() +
        facet_grid(Variable ~ Seasons) +
        labs(title = paste("Faceted Scatter Plot of", paste(y_vars), "by Hour, Season, and Holiday"),
             x = "Hour", y = "Value")
    }
    
    rv$plot <- react_plot
  })
  
  # Render the plot from the stored reactive value
  output$dynamic_plot <- renderPlot({
    print(rv$plot)
  })
  
  # Statistics table
  output$data_table_ui <- renderUI({
    dataTableOutput("data_table")
  })
  
  output$data_table <- renderDataTable({
    rv$summary_table
  })
  
  # Data table in Data Download tab
  output$data_preview <- renderDataTable({
    rv$filtered_data      
  })
  
  # Download CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_seoul_bike_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(rv$filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
