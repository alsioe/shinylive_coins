# Creating an interactive plot for presentation purposes
# JA 30/10/2024

library(tidyverse)
library(cowplot)
library(shiny)
library(bslib)

# Create a function to simulate coin tosses
flipr <- function(n = 1, size = 10, prob = 0.5) {
  rbinom(n = n,
         size = size,
         prob = prob)
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Simulate a coin toss experiment with a biased coin"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input 1: n ----
      sliderInput(
        inputId = "n",
        label = "n (number of experiments):",
        min = 0,
        max = 100,
        value = 10,
        step = 1 # Step size for finer control
      ),
      
      # Input 2: size ----
      sliderInput(
        inputId = "size",
        label = "size (number of coin tosses in each experiment):",
        min = 1,
        max = 20,
        value = 5,
        step = 1 # Step size for finer control
      ),
      
      # Input 3: theta ----
      sliderInput(
        inputId = "theta",
        label = "theta ('bias' - the (true) probability of heads):",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1 # Step size for finer control
      ),
      
      hr(), # Add a horizontal rule
      
      # Input 4: fit_model ----
      checkboxInput(inputId = "fit_model",
                    label = "Fit model to data",
                    value = FALSE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram of data w/ or w/o density ----
      plotOutput(outputId = "plot")
    )
  )
)

# Define server logic to plot example rat data ----
server <- function(input, output) {
  
  # Plot example data (histogram)
  output$plot <- renderPlot({
    
    
    flips <- data.frame(
      expt = 1:input$n,
      result = flipr(n = input$n,
                     size = input$size,
                     prob = input$theta
      )
    )
    
    plot_histogram <-
      ggplot(data = flips,
             aes(x = result)) +
      geom_histogram() +
      geom_vline(aes(xintercept = mean(result)),
                 linetype = 'dashed',
                 colour = 'blue') +
      theme_bw()
    
    # We already have a result (number of heads) -
    # what is the LIKELIHOOD of each theta?
    likelihood_density <- 
      data.frame(theta = seq(0, 1, by = 0.01),
                 likelihood = dbinom(x = sum(flips$result),
                                     size = input$n * input$size,
                                     prob = seq(from = 0, 
                                                to = 1,
                                                by = 0.01)
                 )
      )
    
    # Let's graph that with ggplot
    plot_density <-
      ggplot(data = data.frame(likelihood_density),
             aes(x = theta,
                 y = likelihood)
      ) +
      geom_line() +
      theme_bw() +
      xlab('LIKELIHOOD of the parameter theta ("prob") given (all) the data in flip')
    
    if (input$fit_model == TRUE) {
      to_plot <- cowplot::plot_grid(plotlist = list(plot_histogram,
                                                    plot_density),
                                    ncol = 1)
    } else if (input$fit_model == FALSE) {
      to_plot <- plot_histogram
    }
    
    to_plot
    
  })
  
}

# Launch the shiny app
shinyApp(ui = ui, server = server)