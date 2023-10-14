
library("ggplot2")
library("magrittr")
library("plotly")
library("shiny")
library("shinyBS")
library("shinydashboard")

source("../R/ngram_predictor.R")

addResourcePath("html", "c:/R/Packages/nextword/html")

ui <- dashboardPage(

  dashboardHeader(title = "Next Word Predictor"),

  dashboardSidebar(

    sidebarMenu(

      menuItem("Prediction", tabName = "prediction"),

      menuItem("Source Data", tabName = "data"),

      menuItem("Notes", tabName = "notes")

    )

  ),

  dashboardBody(

    tabItems(

      tabItem(tabName = "prediction",

        fluidRow(

          box(title = "Inputs", collapsible = TRUE, collapsed = FALSE, width = 12,

            p(paste("Upload a source file you'd like to use to build a corpus",
                    "of words on which to base your prediction.",
                    sep = " ")),

            fileInput("file_name", "Source File"),

            p(paste("Input a phrase for which you'd like to predict the",
                    "next word.",
                    sep = " ")),

            textInput("input_phrase", "Input Phrase"),

            p(paste("Select an ngram size, e.g. a value of '3' will generate",
                    "trigrams.",
                    sep = " ")),

            sliderInput("n", "Ngram Size", value = 3, min = 1, max = 6),

            p(paste("Select a minimum count of ngrams, i.e. a value of '1' will return",
                    "all ngrams that occur at least once in the source data.",
                    sep = " ")),

            sliderInput("min_count", "Minimum Ngram Count", value = 1, min = 1, max = 10),

            p(paste("And click the button below to generate a predicton.",
                    sep = " ")),

            actionButton("predict", "Generate Prediction")

          ),

          valueBoxOutput("predicted_word", width = 12),

          box(title = "Predicted Word Dispersion", collapsible = TRUE, collapsed = TRUE, width = 12,

            p(paste("The plot below shows the dispersion of the predicted word throughout",
                    "the source data.",
                    sep = " ")),

          ),

          box(title = "Other Possible Matches", collapsible = TRUE, collapsed = TRUE, width = 12,

            p(paste("The plot below shows the matched word along with other possible",
                    "matched and the number of times each occurs in the source data.",
                    sep = " ")),

            plotlyOutput("plot"),

          )

        ),

      ),

      tabItem(tabName = "data",

        fluidRow(

          box(title = "Raw Data", collapsible = TRUE, collapsed = FALSE, width = 12,

            tableOutput("source_data")

          ),

          box(title = "Text Heatmap", collapsible = TRUE, collapsed = TRUE, width = 12,

              p(paste("The plot below shows...",
                      sep = " ")),

              plotlyOutput("heatmap"),

          )

        )

      ),

      tabItem(tabName = "notes",

              fluidRow(

                box(title = "Overview", collapsible = TRUE, collapsed = FALSE, width = 12,

                  p("This is a test."),

                  htmlOutput("notes")

                )

              )

      )

    )

  )

)

server <- function(input, output) {

  data <- eventReactive(input$file_name, {

    read_source(as.character(input$file_name$datapath))

  })

  prediction <- eventReactive(input$predict, {

    ngram_predictor(input$input_phrase, data(), n = input$n, min_count = input$min_count)

  })

  output$source_data <- renderTable({

    data()

  })

  output$predicted_word <- renderValueBox({

    valueBox(get_predicted_word(prediction()),
             "is the most likely next word in your phrase",
             icon = icon("angles-left"))

  })

  output$plot <- renderPlotly({

    get_plot(prediction())

  })

  output$heatmap <- renderPlotly({

    # get_heatmap(data())

  })

  output$notes <- renderUI({

    tags$iframe(frameborder = "0", src = "html/notes.html")

  })

}

shinyApp(ui, server)
