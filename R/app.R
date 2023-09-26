
library("shiny")

methods <- c("Ngram", "Katz Backoff")

ui <- fluidPage(

        titlePanel("Next Word Prediction"),

        sidebarLayout(

                sidebarPanel(

                        selectInput("method", "Prediction Method", methods),

                        textInput("input_phrase", "Input Phrase"),

                        fileInput("source", "Source File"),

                        numericInput("n", "Ngram Size", value = 3, min = 2, max = 6),

                        numericInput("dec_pos", "Decimal Position", value = 5, min = 2, max = 6),

                        numericInput("min_count", "Minimum Ngram Count", value = 1, min = 1, max = 6),

                        actionButton("prediction", "Run Prediction", class = "btn-primary")

                ), # sidebarPanel

                mainPanel(

                        tabsetPanel(

                                id = "tabset",

                                tabPanel("Instructions"),

                                tabPanel("Plot"),

                                tabPanel("Table"),

                                tabPanel("Notes")

                        ) # tabsetPanel

                ) # mainPanel

        ) # sidebarLayout

) # fluidPage

server <- function(input, output, session) {



}

shinyApp(ui, server)
