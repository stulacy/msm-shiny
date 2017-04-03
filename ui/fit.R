tabPanel("Model",
    sidebarLayout(
        sidebarPanel(
                     h4("Modelling transition hazards"),
                     uiOutput("modelcontent")
                    ),
        mainPanel(
                  uiOutput("model")
                 )
    )
)
                             
