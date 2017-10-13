library(shiny)

source("Model.R")

server = function(input, output, session){
    # Input text
    output$text = renderText(
        paste("Input text:",input$text)
    )
    
    observe({
        # Initial time to calculate computing time to predict next word
        initialTime = Sys.time()
        
        # Cleaned text
        textCleaned = clean(input$text)
        output$cleaned = renderText(
            paste("Cleaned text:",textCleaned)
        )
        
        # Prediction words
        predictedWords = predictModel(textCleaned)

        updateSelectInput(session = session, inputId = "predictions", choices = predictedWords)
        
        # Final time
        finalTime = Sys.time()
        options(digits = 4)
        processingTime = format(finalTime-initialTime, scientific=TRUE)
        
        output$msg = renderText(
            paste(msg, "\n", "Processing time:", processingTime)
        )
    })
    
}

ui = fluidPage(
    # Application title
    titlePanel("Next Word Predictor"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Insert a word or a sentence and click on 'Predict' button to see next word(s) suggestions:"),
            textInput(inputId = "text", label = ""),
            submitButton("Predict")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel(condition = "input.text != ''",
                             verbatimTextOutput("text"),
                             verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
                             selectInput("predictions","Word predictions:",choices=c(""))
            )
            
        )
    ),
    
    fluidRow(HTML("<div style='margin-left:18px;margin-bottom:12px;margin-top:-12px;color:grey;'><strong><big>juanan4290@gmail.com</a></big></strong>&nbsp;&nbsp;&nbsp;&nbsp")),
    fluidRow(HTML("<div style='margin-left:32px;margin-bottom:12px;margin-top:-12px;color:black;'>May - 2017</div>"))
    )

shinyApp(ui = ui, server = server)
