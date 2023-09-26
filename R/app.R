
library("shiny")
library("shinyBS")

methods <- c("Ngram", "Katz Backoff")

ui <- fluidPage(

        titlePanel("Next Word Prediction"),

        sidebarLayout(

                sidebarPanel(

                        selectInput("method", "Prediction Method", methods),

                        bsTooltip("method", "Select the method used to make a prediction.",
                                  "right", options = list(container = "body")),

                        textInput("input_phrase", "Input Phrase"),

                        bsTooltip("input_phrase", "Provide a text phrase to be used for the prediction.",
                                  "right", options = list(container = "body")),

                        fileInput("source", "Source File", multiple = TRUE),

                        bsTooltip("source", "Upload a source file containing text to be used to build a corpus.",
                                  "right", options = list(container = "body")),

                        numericInput("n", "Ngram Size", value = 3, min = 2, max = 6),

                        bsTooltip("n", "Select the size of ngrams to be created, e.g. selecting 3 will create trigrams.",
                                  "right", options = list(container = "body")),

                        numericInput("dec_pos", "Decimal Position", value = 5, min = 2, max = 6),

                        bsTooltip("dec_pos", "Select the decimal position for ngram frequencies.",
                                  "right", options = list(container = "body")),

                        numericInput("min_count", "Minimum Ngram Count", value = 1, min = 1, max = 6),

                        bsTooltip("min_count", "Select the minimum number of ngrams to return.",
                                  "right", options = list(container = "body")),

                        actionButton("predict", "Run Prediction", class = "btn-primary"),

                        bsTooltip("predict", "Click to run the prediction model.",
                                  "right", options = list(container = "body")),

                ), # sidebarPanel

                mainPanel(

                        tabsetPanel(

                                id = "tabset",

                                tabPanel("Instructions"



                                ),

                                tabPanel("Plot"



                                ),

                                tabPanel("Table",

                                        tableOutput("data")

                                ),

                                tabPanel("Notes"



                                )

                        ) # tabsetPanel

                ) # mainPanel

        ) # sidebarLayout

) # fluidPage

server <- function(input, output, session) {

        data <- reactive({

                req(input$source)

        })

        output$data <- renderTable(input$source)

}

shinyApp(ui, server)
