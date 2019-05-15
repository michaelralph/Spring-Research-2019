#OSCILLATOR WITH SLIDERS

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a lot for avoided crossing
ui <- fluidPage(
  
  # Application title
  titlePanel("Coupled Oscillators (with Sliders)"),
  
  # Sidebar with a slider input for value of w 
  sidebarLayout(
    sidebarPanel(
      sliderInput("j",
                  "K12 (Middle Spring) Value:",
                  min = 0,
                  max = 10,
                  value = .1,
                  step = .1),
      sliderInput("k",
                  "k (Two Outside Springs) Value:",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = .1),
      sliderInput("n",
                  "Mass 2 Value:",
                  min = 0,
                  max = 1,
                  value = .450,
                  step = .001)
    ),
    
    # Show a plot of the generated function
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a function
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate j, k, and, n based on input$j, input$k, and input$n from ui.R
    j = input$j
    
    k = input$k
    
    n = input$n
    
    m = seq(0, 1, .001)
    
    x_vals = m
    
    frequency_high_function <- function(j, m, n, k) {
      ang_frequency_high <- sqrt((sqrt((-j*m - j*n - k*m - k*n)**2 - (4*m*n)*(2*j*k + k**2))
                                  / (m*n)) + ((j/m) + (j/n) + (k/m) + (k/n)))/(sqrt(2))
      return(ang_frequency_high/(2*pi))
    }
    
    frequency_low_function <- function(j, m, n, k) {
      ang_freqeuncy_low <- sqrt((-sqrt((-j*m - j*n - k*m - k*n)**2 - (4*m*n)*(2*j*k + k**2))
                                 / (m*n)) + ((j/m) + (j/n) + (k/m) + (k/n)))/(sqrt(2))
      return(ang_freqeuncy_low/(2*pi))
    }
    
    # draw the function with the specified values
    plot(x_vals, frequency_high_function(j, m, n, k), type="l", ylim = c(0,2),
         col="blue", main="Coupled Oscillators (with Sliders)", xlab="Mass 1", ylab="Frequency Values")
    lines(x_vals, frequency_low_function(j, m, n, k), col="red")
    legend("topright", legend=c("Frequency Low", "Frequency High"),
           col=c("red", "blue"), lty=1:1, cex=0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

