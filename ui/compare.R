tabPanel("Model Comparison",
         sidebarLayout(
             sidebarPanel(
                 uiOutput("modelselect")
             ),
             mainPanel(
                 tabsetPanel(
                     tabPanel("Summary",
                             br(),
                             verbatimTextOutput("modelsummary")
                     ),
                     tabPanel("Coefficients",
                              br(),
                              tableOutput("coefficientcomparison")
                     ),
                     tabPanel("Curve",
                              br(),
                              plotOutput("curvecomparison")
                     )
                 )
             )
         )
)
    #sidebarLayout(
    #    sidebarPanel(
    #                 h4("Model Comparison"),
    #                 uiOutput("modelselect")
    #                ),
    #    mainPanel(
    #              plotOutput("curvecomparison"),
    #              hr(),
    #              tableOutput("coefficientcomparison"),
    #              hr(),
    #              verbatimTextOutput("modelsummary")
    #             )
    #)

