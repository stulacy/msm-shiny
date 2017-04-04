tabPanel("Raw Data",
    sidebarLayout(
        sidebarPanel(
                     h4("Data Preparation"),
                     HTML("To use the MSM app, a CSV file containing multi-state data needs to be provided which <strong>must</strong> contain the following fields:"),
                     HTML("<ul><li><code>id</code>: patient key</li><li><code>&#60x&#62.time</code>: The time of entry into state x.</li><li><code>&#60x&#62.status</code>: Boolean indicating whether the entry into state <code>x</code> at time <code>&#60x&#62.time</code> was observed or censored.</li><li>Any covariates of interest</li></ul>"),
                     HTML("Note: <code>&#60x&#62</code> must be replaced by appropriate state names in the above column names. E.g. <code>prog.time</code> represents entry into a state <code>prog</code> and <code>death.status</code> indicates whether entry into a state <code>death</code> was observed or not."),
                     br(),
                     p("The starting state does not need to be explicitly specified, it is assumed that the initial state is entered at a time t=0 with the time scale used in the independent for each individual and relative to their entry into the system. The starting state is typically initial diagnosis. This leads to there being <code>max(x)+1</code> states."),
                     br(),
                     fileInput("filein", "Upload file",
                               accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                     uiOutput("covarcheck"),
                     uiOutput("rawobs")
                    ),
        mainPanel(tableOutput("rawtable")
                 )
    )
)
