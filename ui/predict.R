tabPanel("Prognosis",
    sidebarLayout(
        sidebarPanel(
                     h4("Transition Probabilities"),
                     h5("Predict transition probabilities for new individuals."),
                     uiOutput("transitioneq"),
            conditionalPanel(condition="input.predicttab==1",
                             uiOutput("predmodelselect"),
                             uiOutput("newdata"),
                             uiOutput("plotpredbutton")
                             ),
            conditionalPanel(condition="input.predicttab==2",
                             uiOutput("animationparams")
                             )
                    ),
        mainPanel(
            tabsetPanel(
                tabPanel("Transition probability curves",
                         br(),
                         plotOutput("plotprob"),
                         value=1
                ),
                tabPanel("Animation",
                         br(),
                         grVizOutput("animation"),
                         value=2

                ),
                id="predicttab"
            )
        )
    )
)

