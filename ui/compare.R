tabPanel("Model Comparison",
         sidebarLayout(
             sidebarPanel(
                 h4("Compare Models"),
                 p("Compare any previously saved models either in terms of their hazard ratio estimates for coefficients in common, or in terms of predicting the survival of an 'average' individual."),
                 conditionalPanel(condition="input.comptab==1",
                                  p("View coefficient estimates and related statistics for a single model."),
                                  uiOutput("modelselect")
                 ),
                 conditionalPanel(condition="input.comptab==2",
                                  p("The table opposite displays the estimates of all coefficients in common across the saved models.")
                 ),
                 conditionalPanel(condition="input.comptab==3",
                                  p("The figure opposite displays the predicted survival curves of an 'average' individual, alongside the Kaplan-Meier estimate."),
                                  p("The 'average' individual is determined as having the modal value for any categorical predictors and the mean of any continuous covariates."),
                                  p("The survival curves are in terms of probability of being in the first absorbtive state in the model (death for an illness-death model) as a function of time from first observation"),
                                  p("The curves from the fitted models are determined as the transition probabilities of the relevant absorbtive state, while the Kaplan-Meier is derived from the time until entry of the relevant state.")
                 )
             ),
             mainPanel(
                 tabsetPanel(
                     tabPanel("Single model summary",
                             br(),
                             verbatimTextOutput("modelsummary"),
                             value=1
                     ),
                     tabPanel("Coefficients",
                              br(),
                              tableOutput("coefficientcomparison"),
                              value=2
                     ),
                     tabPanel("Overall survival",
                              br(),
                              plotOutput("curvecomparison"),
                              vlaue=3
                     ),
                     id="comptab"
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

