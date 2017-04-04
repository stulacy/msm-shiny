tabPanel("State Transitions",
    sidebarLayout(
        sidebarPanel(
                     h4("Specify Transitions"),
                     p("Specify the permissible transitions between the states found in the uploaded CSV file. If the number of states displayed opposite is not as expected, please refer to the data preparation instructions on the previous page."),
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
