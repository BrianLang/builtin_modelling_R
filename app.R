
library(shiny)
library(dplyr)
library(jtools)



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
    fluidRow(textOutput("formula")), width = 12),
    fluidRow(verbatimTextOutput("modeloutput"))
)

server = function(input, output) {
    data_sets <- vcdExtra::datasets("datasets") %>%
        filter(class %in% c("data.frame", "matrix")) ## don't want time series and other types of data.

    data_set_choices <- data_sets %>%
        select("Item") %>%
        unlist() %>%
        as.character()

    output$select_dataset <- renderUI({selectInput("dataset", "Dataset:", data_set_choices)})

    used_dataset <- reactive({get(input$dataset)}) ## doing it like this might get hairy when it's time to change the data with mutate calls.

    output$select_dependent <- renderUI({selectInput("dependent", "Dependent:",
                                                     names(get(input$dataset)),
                                                     multiple = F)})

    output$select_independent <- renderUI({selectInput("independent", "Independent:",
                                                       c(names(get(input$dataset)), "-1"),
                                                       multiple = T)})

    output$data <- renderTable({head(used_dataset())}) ##view the data

    output$datadescription <- renderText(paste(data_sets %>%
                                                   filter(Item == input$dataset) %>%
                                                   select("Title"))) ## show the descriptive name of the dataset

    output$formula <- renderText({paste0("formula: ", input$dependent,
                                             " ~ ",
                                             paste0(input$independent, collapse = " + "))}) ## show the formula being built

    output$select_model = renderUI({selectInput("modeltype", "Model Type:", c("gaussian", "logistic", "poisson"))}) ## only kind of makes sense, so far haven't found any y/n data in Datasets. Could be more useful once you can transform the data.
    ## would also be good to analyse the datasets and get the types and ranges of the data. How do you determine if something is integer for count data? probably mod? x%%1==0

## Will eventually add functionality to create new variables and add them to "used_data" object. This will help with transformation of variables. Perhaps only offer users pre-set transformations (change type, change scale, add splines)

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

        model_call <- paste0("glm(data = used_data, formula = ", formula_text, ", family = ", input$modeltype, ")") ## for now using pasted-together calls.
        print(model_call)

        model_fit <- eval(parse(text = model_call))

        output$modeloutput <- renderText(paste(capture.output(summ(model_fit)),collapse = "\n"))## Jtools has a decent looking output for models.

    })


}

# Run the application
shinyApp(ui = ui, server = server)
