tabPanel("State Transitions",
    sidebarLayout(
        sidebarPanel(
                     h4("Add Transitions"),
                     uiOutput("transwarning"),
                     uiOutput("updatestartingstate"),
                     uiOutput("seltrans"),
                     uiOutput("addtrans")
                    ),
        mainPanel(
                 grVizOutput("statedia")
                 )
    )
)
