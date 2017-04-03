tabPanel("Raw Data",
    sidebarLayout(
        sidebarPanel(
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
        mainPanel(tableOutput("rawtable"),
                  uiOutput("rawsummary")
                 )
    )
)
