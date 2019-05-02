#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Goal Setting Scoring Fx"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("credit",
                     "Credit for Meeting Goal",
                     min = .5,
                     max = 1,
                     step = .01,
                     value = 0.9),
         numericInput("pcredit_slope",
                      "Rate of accrual prior to meeting goal",
                      min=0, 
                      max=1, 
                      step=.01, 
                      value=.15),
         numericInput("ecredit_slope",
                      "Rate of accural after meeting goal",
                      min=0,
                      max=1,
                      value=.15),
         numericInput("c1",
                      "Partial Credit Cut Point",
                      min=-10,
                      max=0,
                      step=.25,
                      value=-4),
         numericInput("c2",
                      "Extra Credit Cut Point",
                      min=0, 
                      max=10,
                      step=.25,
                      value=2.5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatter"),
         plotOutput("histogram"),
         tableOutput("summary")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  df = reactive({
    residual = rnorm(n=1000)
    credit = ifelse(residual < input$c1, 0, 
                    ifelse(residual >input$c2, 1.25,
                        ifelse(residual == 0, 0.9, 
                           ifelse(residual < 0 & residual >= input$c1, 0.9 + (residual * input$pcredit_slope),
                               0.9 + (residual * input$ecredit_slope)))))
    
    df = data.frame(residual, credit)
    
  })

  desc = reactive({
    desc =  df() %>% summarise(mean=mean(credit), median=median(credit), n=n())
  })
  
    output$scatter <-renderPlot({
    
    ggplot(df(), aes(x=residual, y=credit))+
        geom_point()
    })
    
    output$histogram <-renderPlot({
      
      ggplot(df(), aes(x=credit))+
        geom_histogram(binwidth = .1, aes(y=..count../sum(..count..) * 100))+
        ylab("Percent")
      
    })
    
    output$summary <- renderTable({
        desc()
    })

  }
  

# Run the application 
shinyApp(ui = ui, server = server)
