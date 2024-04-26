################################################################################
# The following code builds a shiny app to show plots generated in the analysis#
################################################################################

# Set the working directory
library(shiny)

# Define UI for saved plots app ----
ui <- fluidPage(
  
  # App title with instructions ----
  titlePanel("NBA Stats Inflation Analysis"),
  
  fluidRow(
    column(12, 
           p("Welcome to the NBA Stats Inflation Analysis app! Choose the type 
             of statistics you want to analyze using the radio buttons on the left. 
             Adjust the number of change-points using the slider below to show simulation
             results with different presumed change-points. The app will display scatter plots, 
             histograms of potential change-points, and simulation results with 95% 
             credible intervals based on your selections. Keep in mind that some of the
             statistics may not have plots available with more than 1 change-point. Hope you enjoy!")
    )
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the type of statistics ----
      radioButtons("stats", "Statistics:",
                   c("Total Points by Top 10 Scorers" = "plot1",
                     "Total Fouls by top 10 Teams" = "plot2",
                     "Average Team Scores" = "plot3",
                     "Average Team Pace" = "plot4")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of change-points ----
      selectInput("change_points",
                  "Number of change-points:",
                  choices = c("1" = 1, "2" = 2),
                  selected = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plots ----
      tabsetPanel(type = "tabs",
                  tabPanel("Scatter Plot", imageOutput("scatter_plot")),
                  tabPanel("Histograms of Potential Change-points", 
                           conditionalPanel(
                             condition = "input.change_points == 1",
                             imageOutput("histogram")
                           ),
                           conditionalPanel(
                             condition = "input.change_points == 2 && 
                                            (input.stats == 'plot1' || 
                                             input.stats == 'plot2')",
                             verbatimTextOutput("histogram_text")
                           ),
                           conditionalPanel(
                             condition = "input.change_points == 2 && 
                                            !(input.stats == 'plot1' || 
                                              input.stats == 'plot2')",
                             imageOutput("histogram_1"),
                             br(),
                             imageOutput("histogram_2")
                           )
                  ),
                  tabPanel("Simulation Results with 95% CI", 
                           conditionalPanel(
                             condition = "input.change_points == 1",
                             imageOutput("ci_plot")
                           ),
                           conditionalPanel(
                             condition = "input.change_points == 2 && 
                                            (input.stats == 'plot1' || 
                                             input.stats == 'plot2')",
                             verbatimTextOutput("ci_plot_text")
                           ),
                           conditionalPanel(
                             condition = "input.change_points == 2 && 
                                            !(input.stats == 'plot1' || 
                                              input.stats == 'plot2')",
                             imageOutput("ci_plot_1"),
                             br(),
                             imageOutput("ci_plot_2")
                           )
                  )
      )
      
    )
  )
)

# Define server logic for saved plots app ----
server <- function(input, output) {
  
  # Render the selected scatter plot ----
  output$scatter_plot <- renderImage({
    # Get the filename based on the selected statistics
    filename <- switch(input$stats,
                       plot1 = "poisson_total_points_scatter.png",
                       plot2 = "poisson_total_fouls_scatter.png",
                       plot3 = "normal_average_score_scatter.png",
                       plot4 = "normal_average_pace_scatter.png")
    
    # Return the image file
    list(src = filename, alt = "Scatter Plot")
  }, deleteFile = FALSE)
  
  # Render the selected histograms based on the number of change points ----
  output$histogram <- renderImage({
    # Get the filename based on the selected statistics and number of change points
    filename <- switch(input$stats,
                       plot1 = "poisson_total_points_hist.png",
                       plot2 = "poisson_total_fouls_hist.png",
                       plot3 = "normal_average_score_hist.png",
                       plot4 = "normal_average_pace_hist.png")
    
    # Check if the file exists
    if (file.exists(filename)) {
      list(src = filename, alt = "Histogram")
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
  output$histogram_text <- renderText({
    "No plots available"
  })
  
  # Render the selected histograms for change-points 1 and 2 ----
  output$histogram_1 <- renderImage({
    # Check if the statistics has a second histogram
    if (input$stats %in% c("plot3", "plot4")) {
      # Get the filename based on the selected statistics and number of change points
      filename <- switch(input$stats,
                         plot3 = "normal_average_score_hist_1.png",
                         plot4 = "normal_average_pace_hist_1.png")
      
      # Check if the file exists
      if (file.exists(filename)) {
        list(src = filename, alt = "Histogram 1")
      } else {
        list(src = "", alt = "Histogram 1")
      }
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
  output$histogram_2 <- renderImage({
    # Check if the statistics has a second histogram
    if (input$stats %in% c("plot3", "plot4")) {
      # Get the filename based on the selected statistics and number of change points
      filename <- switch(input$stats,
                         plot3 = "normal_average_score_hist_2.png",
                         plot4 = "normal_average_pace_hist_2.png")
      
      # Check if the file exists
      if (file.exists(filename)) {
        list(src = filename, alt = "Histogram 2")
      } else {
        list(src = "", alt = "Histogram 2")
      }
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
  # Render the selected CI plots based on the number of change points ----
  output$ci_plot <- renderImage({
    # Get the filename based on the selected statistics and number of change points
    filename <- switch(input$stats,
                       plot1 = "poisson_total_points_ci.png",
                       plot2 = "poisson_total_fouls_ci.png",
                       plot3 = "normal_average_score_ci.png",
                       plot4 = "normal_average_pace_ci.png")
    
    # Check if the file exists
    if (file.exists(filename)) {
      list(src = filename, alt = "Simulation Results with 95% CI")
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
  output$ci_plot_text <- renderText({
    "No plots available"
  })
  
  # Render the selected CI plots for change-points 1 and 2 ----
  output$ci_plot_1 <- renderImage({
    # Check if the statistics has a second CI plot
    if (input$stats %in% c("plot3", "plot4")) {
      # Get the filename based on the selected statistics and number of change points
      filename <- switch(input$stats,
                         plot3 = "normal_average_score_ci_1.png",
                         plot4 = "normal_average_pace_ci_1.png")
      
      # Check if the file exists
      if (file.exists(filename)) {
        list(src = filename, alt = "Simulation Results with 95% CI 1")
      } else {
        list(src = "", alt = "No plots available")
      }
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
  output$ci_plot_2 <- renderImage({
    # Check if the statistics has a second CI plot
    if (input$stats %in% c("plot3", "plot4")) {
      # Get the filename based on the selected statistics and number of change points
      filename <- switch(input$stats,
                         plot3 = "normal_average_score_ci_2.png",
                         plot4 = "normal_average_pace_ci_2.png")
      
      # Check if the file exists
      if (file.exists(filename)) {
        list(src = filename, alt = "Simulation Results with 95% CI 2")
      } else {
        list(src = "", alt = "No plots available")
      }
    } else {
      list(src = "", alt = "No plots available")
    }
  }, deleteFile = FALSE)
  
}


# Run the app ----
shinyApp(ui = ui, server = server)

