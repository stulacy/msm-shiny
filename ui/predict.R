tabPanel("Prognosis",
    sidebarLayout(
        sidebarPanel(
                     h4("Transition Probabilities"),
                     h5("Predict transition probabilities for new patients"),
                     uiOutput("transitioneq"),
            conditionalPanel(condition="input.predicttab==1",
                             h2("IN SURVIVAL")
                             ),
            conditionalPanel(condition="input.predicttab==2",
                             h2("IN ANIMATION")
                             ),
                     uiOutput("predmodelselect"),
                     uiOutput("newdata"),
                     uiOutput("plotpredbutton"),
                     uiOutput("animationparams")
                    ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival curve",
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

