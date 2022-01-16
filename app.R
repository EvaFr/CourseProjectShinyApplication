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
  titlePanel("Relation between mpg and other properties"),

  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select variable for comparison:",
                  c("Number of cylinders" = "cyl",
                    "Displacement" = "disp",
                    "Gross horsepower" = "hp",
                    "Rear axle ratio" = "drat",
                    "Weight (lb/1000)" = "wt",
                    "1/4 mile time" = "qsec",
                    "V/S" = "vs",
                    "Transmission" = "am",
                    "Number of gears" = "gear")),
      
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Linear model:",textOutput("caption")),
      plotOutput("mpgPlot"),
      verbatimTextOutput("fit")
    )
  )
)

server <- function(input, output) {
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  formulaTextPoint <- reactive({
    paste("mpg ~", "as.integer(", input$variable, ")")
  })
  
  fit <- reactive({
    lm(as.formula(formulaTextPoint()), data=mtcars)
  })
  
  output$caption <- renderText({
    formulaText()
  })
 
  output$mpgPlot <- renderPlot({
    with(mtcars, {
      plot(as.formula(formulaTextPoint()))
      abline(fit(), col=4)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
