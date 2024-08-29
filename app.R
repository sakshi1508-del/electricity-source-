library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Global Renewable Energy Trends (1996-2015)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = NULL), 
      sliderInput("yearRange", "Select Year Range:", 
                  min = 1996, max = 2015, value = c(1996, 2015)),
      checkboxGroupInput("renewableType", "Include Renewable Types:", 
                         choices = c("Geothermal" = "geothermal", 
                                     "Solar" = "solar", 
                                     "Wind" = "wind", 
                                     "Biomass" = "biomass"),
                         selected = c("geothermal", "solar", "wind", "biomass")),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      plotOutput("trendPlot"),
      tableOutput("dataTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Path to the dataset (update this path)
  file_path <- "C:/Data/electricity_data.csv"
  
  # Verify file accessibility
  if (file.exists(file_path)) {
    print("File exists and is accessible.")
  } else {
    stop(paste("File not found:", file_path))
  }
  
  # Load the dataset
  data <- tryCatch({
    read.csv(file_path)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })
  
  # Place the browser() function here to troubleshoot after loading data
  browser()
  
  # Populate the country dropdown
  updateSelectInput(session, "country", choices = unique(data$Country))
  
  # Reactive expression to filter the dataset
  filteredData <- reactive({
    data %>%
      filter(Country == input$country,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2]) %>%
      filter(Renewable_Type %in% input$renewableType)
  })
  
  # Generate the trend plot
  output$trendPlot <- renderPlot({
    # Place the browser() function here to troubleshoot before generating the plot
    browser()
    
    ggplot(filteredData(), aes(x = Year, y = Electricity_Production, color = Renewable_Type)) +
      geom_line() +
      labs(title = paste("Renewable Energy Generation in", input$country),
           x = "Year", y = "Electricity Production (GWh)") +
      theme_minimal()
  })
  
  # Display the data table
  output$dataTable <- renderTable({
    # Place the browser() function here to troubleshoot the table data rendering
    browser()
    
    filteredData()
  })
  
  # Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("renewable_energy_", input$country, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
