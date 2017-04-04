tabPanel("Model Fitting",
    sidebarLayout(
        sidebarPanel(
                     h4("Modelling transition hazards"),
                     p("Build a multi-state model of the data using the available options below."),
                     p("If satisfied with a model after building, save it using the option that will appear for use in future comparisons and prognosis"),
                     hr(),
                     uiOutput("modelcontent")
                    ),
        mainPanel(
                  uiOutput("model")
                 )
    )
)
                             
