#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(jtools)

# Define UI for application that draws a histogram

ui = fluidPage(
    fluidRow(
        column(
            uiOutput("select_dataset"),
            uiOutput("select_dependent"),
            uiOutput("select_independent"),
            uiOutput("select_model"),
            actionButton(inputId = "submitmodel", label = "run model"),
            width = 3),
        column(h3(textOutput("datadescription")),
               tableOutput("data"),
               width = 9)
    ),
    column(shiny::br(),
    fluidRow(textOutput("description")), width = 12),
    fluidRow(verbatimTextOutput("modeloutput"))
)

server = function(input, output) {
    data_sets <- vcdExtra::datasets("datasets") %>%
        filter(class %in% c("data.frame", "matrix"))

    data_set_choices <- data_sets %>%
        select("Item") %>%
        unlist() %>%
        as.character()

    output$select_dataset <- renderUI({selectInput("dataset", "Dataset:", data_set_choices)})


    used_dataset <- reactive({get(input$dataset)})

    output$select_dependent <- renderUI({selectInput("dependent", "Dependent:",
                                                     names(get(input$dataset)),
                                                     multiple = F)})

    output$select_independent <- renderUI({selectInput("independent", "Independent:",
                                                       c(names(get(input$dataset)), "-1"),
                                                       multiple = T)})

    output$data <- renderTable({head(used_dataset())})

    output$datadescription <- renderText(paste(data_sets %>%
                                                   filter(Item == input$dataset) %>%
                                                   select("Title")))

    output$description <- renderText({paste0("formula: ", input$dependent,
                                             " ~ ",
                                             paste0(input$independent, collapse = " + "))})

    output$select_model = renderUI({selectInput("modeltype", "Model Type:", c("gaussian", "logistic", "poisson"))})


    # observeEvent(input$mutatevars, {
    #     transformed_data <- eval(input$dataset)
    #     transformed_data <- eval(parse(text = "transformed_data %>% mutate(", input$mutate_statement, ")"))
    # })    # observeEvent(input$mutatevars, {
    #     transformed_data <- eval(input$dataset)
    #     transformed_data <- eval(parse(text = "transformed_data %>% mutate(", input$mutate_statement, ")"))
    # })

    observeEvent(input$submitmodel, {
        used_data <- used_dataset()

        formula_text <- paste0(input$dependent, "~", paste0(input$independent, collapse = " + "))

        model_call <- paste0("glm(data = used_data, formula = ", formula_text, ", family = ", input$modeltype, ")")
        print(model_call)

        model_fit <- eval(parse(text = model_call))

        output$modeloutput <- renderText(paste(capture.output(summ(model_fit)),collapse = "\n"))

    })




}


# Run the application
shinyApp(ui = ui, server = server)
