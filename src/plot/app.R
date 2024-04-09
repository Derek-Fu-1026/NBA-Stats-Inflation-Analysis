################################################################################
# The following code builds a shiny app to show plots generated in the analysis#
################################################################################

# Set the working directory
setwd("D:/NBA-Stats-Inflation/src")

# Load necessary libraries
library(shiny)

# Define the UI for the app:
ui = fluidPage(
  
  titlePanel("NBA Stats Inflation Analysis"),
  sidebarLayout(
    
    sidebarPanel(
      
      # Create a drop-down menu:
      selectInput("plotType", "Select Plot:",
                  choices = c("Trend of average team score per season",
                              "Simulation Result")),
    ),
    
    # Main panel:
    mainPanel(
      imageOutput("myPlot")
    )
  )
)

# Define the server function:
server = function(input, output) {
  
  output$myPlot <- renderImage({

    if (input$plotType == "Trend of average team score per season") {
      
      filename <- "plot/team_average_points_plot.png"
      
    } else if (input$plotType == "Simulation Result") {
      
      filename <- "plot/post_k_sum_scorers_hist.png"
      
    }
    
    list(src = filename, alt = "Plot")}, deleteFile = FALSE)
  
}

# Run the app:
shinyApp(ui = ui, server = server)


