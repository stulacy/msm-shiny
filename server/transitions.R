transitions <- reactiveValues()

starting_state <- reactive({
    if (is.null(input$startstate) || input$updatestartbutton == 0)
        "starting"

    input$updatestartbutton
    isolate({
        return(input$startstate)
    })
})

output$transwarning <- renderUI({
    if (length(states()) < 1)
        return(HTML("Please upload a data file containing state entry information in the specified format in the <strong>Raw Data</strong> tab before proceeding."))
})

output$updatestartingstate <- renderUI({
    item_list <- list()
    item_list[[1]] <- textInput("startstate", "Starting State Name", value="starting")
    item_list[[2]] <- actionButton("updatestartbutton", "Update")
    item_list[[3]] <- hr()
    do.call(tagList, item_list)
})

output$seltrans <- renderUI({
    if (length(states()) < 1)
        return()

    input$updatestartbutton

    isolate({
        item_list <- list()
        item_list[[1]] <- selectInput("transfrom", "From", choices=states())
        item_list[[2]] <- selectInput("transto", "To", choices=states())
        do.call(tagList, item_list)
    })
})

output$addtrans <- renderUI({
    if (length(states()) < 1)
        return()

    actionButton("addtrans", "Add")
})

observeEvent(input$addtrans, {
    new_index <- length(reactiveValuesToList(transitions)) + 1
    from <- which(states() == input$transfrom)
    to <- which(states() == input$transto)
    transitions[[paste(from, to, sep='-')]] <- list(from=from, to=to, index=new_index)
})

states <- reactive({
    if (is.null(all_df()) || is.null(input$updatestartbutton))
        return()

    input$updatestartbutton

    isolate({
        time_names <- get_time_status_vars(all_df(), 'time')
        status_names <- get_time_status_vars(all_df(), 'status')
        states_times <- gsub("\\.time", "", time_names)
        states_status <- gsub("\\.status", "", status_names)

        if (!all(states_times == states_status))
            stop("Error: Time and status state names do not match up")

        c(starting_state(), states_times)
    })
})

output$statedia <- renderGrViz({

    # TODO Remove if doesn't help. Trying to get UI to refresh when press button
    input$addtrans

    edges <- sapply(reactiveValuesToList(transitions), function(x) paste0("'", states()[x$from], "' -> '", states()[x$to], "'"))
    edge_vals <- sapply(reactiveValuesToList(transitions), function(x) x$index)

    states_dot <- paste("node [shape=circle, fixedsize=true, width=1.5, height=1.5, fontsize=12]",
                        paste0("'", states(), "'", collapse='\n'),
                        sep='\n')
    edges_dot <- paste(
                       mapply(function(e, v)
                             paste(e, "[label='", v, "']"), edges, edge_vals),
                       collapse=" \n ")

    full <- paste("digraph states {", states_dot, edges_dot, "}")
    grViz(full)
})

############################### State Data ############################################################
Q <- reactive({
    mat <- matrix(NA, length(states()), length(states()), dimnames = list(states(), states()))
    for (i in names(transitions)) {
        mat[transitions[[i]]$from, transitions[[i]]$to] <- transitions[[i]]$index
    }
    mat
})

# Obtains absorptive (sink) states from a transition matrix
get_sink_states <- function(trans_mat) {
    row.names(trans_mat)[apply(trans_mat, 1, function(x) all(is.na(x)))]
}

# Transition matrix used for predicting outcomes. The difference with this and Q()
# is that this matrix can separate sink states, whereas Q() is in 'unexpanded' format,
# just as the user entered it
Q_prediction <- function(split_sinks=FALSE){
    trans_mat <- Q()

    if (split_sinks) {
        this_states <- states()
        # Create new transition matrix
        multi_source <- which(apply(trans_mat, 2, function(c) sum(!is.na(c))) > 1)
        trans <- trans_mat[, multi_source][!is.na(trans_mat[, multi_source])]
        nstates <- length(this_states) + length(trans) - 1

        # Create new state transition matrix
        Q2 <- matrix(NA, nstates, nstates)
        # Create vector to hold dimension names
        dimn <- rep(NA, nstates)
        # Add in the initial states for the first ones
        dimn[seq(multi_source-1)] <- this_states[seq(multi_source-1)]

        # Add the values related to states unconnected to the sink state of interest
        Q2[seq(multi_source-1), seq(multi_source-1)] <- trans_mat[seq(multi_source-1), seq(multi_source-1)]
        # Next need to add in the new values
        for (i in seq_along(trans)) {
            # Obtain the source state by obtaining row number containing this transition number
            source <- which(trans_mat[, multi_source] == trans[i])
            # Now add in the value for this row, with column number given by its order in the transition matrix
            Q2[source, (multi_source+i-1)] <- trans[i]
            dimn[multi_source+i-1] <- paste(this_states[multi_source], this_states[source], sep=' from ')
        }

        dimnames(Q2) <- list(dimn, dimn)
        trans_mat <- Q2
    }
    trans_mat
}

proc_df <- reactive({
    if (length(reactiveValuesToList(transitions)) > 0) {
        # TODO Should these calls to all_df instead refer to raw_df
        prepare_data(raw_df(), Q(), input$selcovar, get_time_status_vars(raw_df(), 'time'),
                     get_time_status_vars(raw_df(), 'status'), state_arrivals())
    }
    else {
        data.frame(id=c(), from=c(), to=c(), trans=c(), Tstart=c(), Tstop=c(), time=c(), status=c())
    }
})

