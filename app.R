library(shiny)
library(dplyr)
library(mstate)
library(tidyr)
library(ggplot2)
library(DiagrammeR)
library(flexsurv)
library(stringr)

ui <- navbarPage("Multi-State Modelling",
                 source(file.path("ui", "data.R"), local=T)$value,
                 source(file.path("ui", "transitions.R"), local=T)$value,
                 source(file.path("ui", "fit.R"), local=T)$value,
                 source(file.path("ui", "compare.R"), local=T)$value,
                 source(file.path("ui", "predict.R"), local=T)$value
)

server <- function(input, output, session) {
    # Include the logic (server) for each tab
    source(file.path("server", "static.R"),  local = TRUE)$value
    source(file.path("server", "data.R"),  local = TRUE)$value
    source(file.path("server", "transitions.R"),  local = TRUE)$value
    source(file.path("server", "fit.R"),  local = TRUE)$value
    source(file.path("server", "compare.R"),  local = TRUE)$value
    source(file.path("server", "predict.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
