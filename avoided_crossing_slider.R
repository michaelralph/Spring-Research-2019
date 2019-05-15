#AVOIDED CROSSING

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
   titlePanel("Avoided Crossing"),
   
   # Sidebar with a slider input for value of w 
   sidebarLayout(
      sidebarPanel(
         sliderInput("w",
                     "W Value:",
                     min = 0,
                     max = 10,
                     value = 1,
                     step = .1)
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
      # generate w based on input$w from ui.R
      w = input$w
     
      E1 = seq(-10, 10, by=.001)
     
      E2 = 0#seq(10, -10, by=-.001)
     
      x_vals = E1 - E2
     
      e_plus_function <- function(w, E1, E2) {
        e_plus <- (1/2)*(E1 + E2) + (1/2)*sqrt((E1 - E2)**2 + 4*(abs(w))**2)
        return(e_plus)
      }
     
      e_minus_function <- function(w, E1, E2) {
        e_minus <- (1/2)*(E1 + E2) - (1/2)*sqrt((E1 - E2)**2 + 4*(abs(w))**2)
        return(e_minus)
      }
      
      # draw the function with the specified value of w
      plot(x_vals, e_plus_function(w, E1, E2), type="l", #ylim=c(0,10)
           ylim=c(e_minus_function(w, -10, 10),e_plus_function(w, -10, 10)), col="blue",
           main="Avoided Crossing", xlab="E1 - E2", ylab="Energy Level")
      lines(x_vals, e_minus_function(w, E1, E2), col="red")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

