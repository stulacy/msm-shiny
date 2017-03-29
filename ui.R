library(shiny)
library(DiagrammeR)

ui <- fluidPage(
    # *Input() functions,
    # *Output() functions
    titlePanel("Multi-State Modelling"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition="input.selectedtab==1",
                             h4("Data Requirements"),
                             HTML("Data <strong>must</strong> contain the following fields:"),
                             HTML("<ul><li><code>id</code>: patient key</li><li><code>&#60x&#62.time</code>: The time of entry into state x.</li><li><code>&#60x&#62.status</code>: Boolean indicating whether the entry into state x at time &#60x&#62.time was observed or censored.</li><li>Any covariates of interest</li></ul>"),
                             HTML("Note: <code>&#60x&#62</code> must be replaced by appropriate state names in the above column names. E.g. <code>prog.time</code> represents entry into a state <code>prog</code> and <code>death.status</code> indicates whether entry into a state <code>death</code> was observed or not."),
                             br(),
                             HTML("This leads to there being <code>max(x)+1</code> states, since the starting state is implicitly assumed."),
                             hr(),
                             h4("Select Data"),
                             fileInput("filein", "Select file"),
                             uiOutput("covarcheck"),
                             uiOutput("rawobs")
                             ),
            conditionalPanel(condition="input.selectedtab==2",
                             h4("Add Transitions"),
                             uiOutput("transwarning"),
                             uiOutput("updatestartingstate"),
                             uiOutput("seltrans"),
                             uiOutput("addtrans"),
                             uiOutput("currtransheader"),
                             uiOutput("currtrans")
                             ),
            conditionalPanel(condition="input.selectedtab==3",
                             h4("Modelling transition hazards"),
                             uiOutput("modelcontent")
                             ),
            conditionalPanel(condition="input.selectedtab==4",
                             h4("Model Comparison"),
                             uiOutput("modelselect")
                             ),
            conditionalPanel(condition="input.selectedtab==5",
                             h4("Transition Probabilities"),
                             h5("Predict transition probabilities for new patients"),
                             uiOutput("transitioneq"),
                             uiOutput("predmodelselect"),
                             uiOutput("newdata"),
                             uiOutput("plotpredbutton"),
                             uiOutput("animationparams")
                             )
        ),
        mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel("Raw Data",
                                 tableOutput("rawtable"),
                                 uiOutput("rawsummary"),
                                 value=1),
                        tabPanel("State Transitions",
                                 grVizOutput("statedia"),
                                 value=2),
                        tabPanel("Model",
                                 uiOutput("model"),
                                 value=3),
                        tabPanel("Model Comparison",
                                 verbatimTextOutput("modelsummary"),
                                 tableOutput("coefficientcomparison"),
                                 plotOutput("curvecomparison"),
                                 value=4),
                        tabPanel("Prediction",
                                 plotOutput("plotprob"),
                                 br(),
                                 grVizOutput("animation"),
                                 value=5),
                        id = "selectedtab"
            )
        )
    )
)
