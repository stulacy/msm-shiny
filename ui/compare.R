tabPanel("Model Comparison",
    sidebarLayout(
        sidebarPanel(
                     h4("Model Comparison"),
                     uiOutput("modelselect")
                    ),
        mainPanel(
                  plotOutput("curvecomparison"),
                  hr(),
                  tableOutput("coefficientcomparison"),
                  hr(),
                  verbatimTextOutput("modelsummary")
                 )
    )
)
                             
