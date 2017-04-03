tabPanel("Prediction",
    sidebarLayout(
        sidebarPanel(
                     h4("Transition Probabilities"),
                     h5("Predict transition probabilities for new patients"),
                     uiOutput("transitioneq"),
                     uiOutput("predmodelselect"),
                     uiOutput("newdata"),
                     uiOutput("plotpredbutton"),
                     uiOutput("animationparams")
                    ),
        mainPanel(
                  plotOutput("plotprob"),
                  br(),
                  grVizOutput("animation")
                 )
    )
)
                             
