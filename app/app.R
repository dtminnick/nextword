
library("ggplot2")
library("plotly")
library("shiny")
library("shinyBS")

source("../R/ngram_predictor.R")

methods <- c("Ngram", "Katz Backoff")

ui <- fluidPage(

        titlePanel("Next Word Prediction"),

        sidebarLayout(

                sidebarPanel(

                        fileInput("file_name", "Source File"),

                        bsTooltip("file_name", "Upload a source file containing text to be used to build a corpus.",
                                  "right", trigger = "hover", options = list(container = "body")),

                        textInput("input_phrase", "Input Phrase"),

                        bsTooltip("input_phrase", "Provide a text phrase to be used for the prediction.",
                                  "right", options = list(container = "body")),

                        selectInput("model", "Prediction Model", methods),

                        bsTooltip("model", "Select the method used to make a prediction.",
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

                        br(),

                        actionButton("predict", "Generate Prediction", class = "btn-primary"),

                        bsTooltip("predict", "Click to run the selected model and generate a prediction.",
                                  "right", options = list(container = "body")),

                ), # sidebarPanel

                mainPanel(

                        tabsetPanel(

                                id = "tabset",

                                tabPanel("Prediction",

                                         br(),

                                         strong("Next Word Predicted", align = "left"),

                                         br(),

                                         verbatimTextOutput("predicted_word", placeholder = TRUE),

                                ),

                                tabPanel("Instructions",

                                        br(),

                                        includeHTML("instructions.html")

                                ),

                                tabPanel("Table",

                                         br(),

                                         p("Add description of this table..."),

                                         tableOutput("source_data")

                                ),

                                tabPanel("Ngrams",

                                         br(),

                                         p("Add description of this table..."),

                                         tableOutput("ngrams")

                                 ),

                                tabPanel("Plot",

                                        br(),

                                        p("Add description of this plot chart..."),

                                        plotOutput("plot"),

                                ),

                                tabPanel("Notes",

                                        br(),

                                        includeHTML("notes.html")

                                )

                        ) # tabsetPanel

                ) # mainPanel

        ) # sidebarLayout

) # fluidPage

server <- function(input, output, session) {

        dataset <- eventReactive(input$file_name, {

                read_source(as.character(input$file_name$datapath))

        })

        output$source_data <- renderTable({

                dataset()

        })

        prediction <- eventReactive(input$predict, {

                ngram_predictor(input$input_phrase,
                                dataset(),
                                input$n,
                                input$dec_pos,
                                input$min_count)

        })

        output$ngrams <- renderTable({

                prediction()

        })

        output$plot <- renderPlot({

                get_plot(prediction())

        })

}

shinyApp(ui, server)
