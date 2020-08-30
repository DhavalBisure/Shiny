
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predict Horse Power of Car"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slidermpg",
                        "What is the Miles per gallon offered by the car",
                        min = 10,
                        max = 35,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1"),
           h3("Predicted Horse Power"),
           textOutput("pred1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    model1 <- lm(hp~mpg,data = mtcars)
    model1predict <- reactive({
        mpgInput <- input$slidermpg
        predict(model1,newdata=data.frame(mpg=mpgInput))
    })
    
    output$plot1 <- renderPlot({
        mpgInput <- input$slidermpg
        plot(mtcars$mpg,mtcars$hp,xlab = "Miles per gallon",ylab = "Horse Power",
             bty="n", pch=16,
             xlim = c(10,35), ylim = c(10,350))
        abline(model1,col="red",lwd=2)
        points(mpgInput,model1predict(),col="red", pch =16, cex = 2)
    })
    output$pred1 <- renderText({
        model1predict()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
