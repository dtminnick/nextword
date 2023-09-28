
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

                        selectInput("model", "Prediction Model", methods),

                        bsTooltip("model", "Select the method used to make a prediction.",
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

                        actionButton("predict", "Generate Prediction", class = "btn-primary"),

                        bsTooltip("predict", "Click to run the selected model and generate a prediction.",
                                  "right", options = list(container = "body")),

                ), # sidebarPanel

                mainPanel(

                        br(),

                        strong("Next Word Predicted", align = "left"),

                        verbatimTextOutput("predicted_word", placeholder = TRUE),

                        tabsetPanel(

                                id = "tabset",

                                tabPanel("Instructions"



                                ),

                                tabPanel("Plot",

                                        br(),

                                        p("This plot shows the ten words determined by the algorithm to have the
                                          highest probability of being the next word in the phrase provided by the
                                          user. Words are plotted in decsending order of probability."),

                                        plotlyOutput("plot"),

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

        # output$data <- renderTable(input$source)


        prediction <- eventReactive(input$predict, {

                source <- read_source(as.character(input$source$datapath))

                ngram_predictor(input$input_phrase,
                                source,
                                input$n,
                                input$dec_pos,
                                input$min_count)

        })

        # output$predicted_word <- renderPrint(get_predicted_word(prediction))

        dataTable <- eventReactive(input$predict, {

                prediction()

        })

        output$data <- renderDataTable(dataTable())

        dataPlot <- eventReactive(input$predict, {

                prediction()

        })

        output$plot <- renderPlotly({

                ggplotly(get_plot(dataPlot()))

        })

}

shinyApp(ui, server)
