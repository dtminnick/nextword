
library(ggplot2)
library(magrittr)
library(nextword)
library(plotly)
library(shiny)
library(shinyBS)
library(shinydashboard)

# Add resource path for HTML files (use relative path)
html_dir <- file.path(getwd(), "html")
if (dir.exists(html_dir)) {
  addResourcePath("html", html_dir)
}

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

            p(paste("And click the button below to generate a prediction.",
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
    if (is.null(input$file_name)) {
      return(NULL)
    }
    tryCatch({
      read_source(as.character(input$file_name$datapath))
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })

  prediction <- eventReactive(input$predict, {
    if (is.null(data()) || is.null(input$input_phrase) || input$input_phrase == "") {
      return(NULL)
    }
    tryCatch({
      ngram_predictor(input$input_phrase, data(), n = input$n, min_count = input$min_count)
    }, error = function(e) {
      showNotification(paste("Error generating prediction:", e$message), type = "error")
      NULL
    })
  })

  output$source_data <- renderTable({

    data()

  })

  output$predicted_word <- renderValueBox({
    pred_word <- NULL
    if (!is.null(prediction())) {
      pred_word <- tryCatch({
        get_predicted_word(prediction())
      }, error = function(e) {
        NULL
      })
    }
    valueBox(value = ifelse(is.null(pred_word), "N/A", pred_word),
             subtitle = "is the most likely next word in your phrase",
             icon = icon("angles-left"))
  })

  output$plot <- renderPlotly({
    if (is.null(prediction())) {
      return(NULL)
    }
    tryCatch({
      p <- get_plot(prediction())
      if (!is.null(p)) {
        plotly::ggplotly(p)
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
  })

  output$heatmap <- renderPlotly({
    # Heatmap functionality can be implemented here if needed
    # if (!is.null(data())) {
    #   tryCatch({
    #     p <- get_heatmap(data())
    #     if (!is.null(p)) {
    #       plotly::ggplotly(p)
    #     } else {
    #       NULL
    #     }
    #   }, error = function(e) {
    #     NULL
    #   })
    # } else {
    #   NULL
    # }
    NULL
  })

  output$notes <- renderUI({

    tags$iframe(frameborder = "0", src = "html/notes.html")

  })

}

shinyApp(ui, server)
