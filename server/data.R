all_df <- reactive({
    infile <- input$filein

    if (is.null(infile))
        return()

    isolate({
        read.table(infile$datapath, sep=',', header=T,
                   stringsAsFactors = TRUE)
    })
})

# TODO Rename both raw_df and all_df
raw_df <- reactive({
    if (is.null(all_df()))
        return()

    # Have the time and status columns ordered by state
    # NB: This won't work if the state times and status columns aren't paired in the original CSV
    time_vars <- get_time_status_vars(all_df(), 'time')
    status_vars <- get_time_status_vars(all_df(), 'status')
    comb_vars <- as.character(sapply(seq_along(time_vars), function(i) c(time_vars[i], status_vars[i])))

    cols <- c('id', comb_vars, input$selcovar)
    all_df() %>%
        select(one_of(cols))
})

output$covarcheck <- renderUI({
    if (is.null(all_df()))
        return()

    covar_names <- names(all_df()[!grepl(".*\\.time", names(all_df())) &
                                  !grepl(".*\\.status", names(all_df())) &
                                  names(all_df()) != "id"])

    checkboxGroupInput("selcovar", HTML("<h5><strong>Select covariates</strong></h5>"), choices=covar_names)
})

output$rawsummary <- renderUI({
    if (is.null(raw_df()))
        return(h5("Select a file containing time-to-event data in the tab on the left."))

    item_list <- list()
    item_list[[1]] <- h4("Data summary")
    item_list[[2]] <- renderPrint(summary(raw_df()))
    do.call(tagList, item_list)
})

output$rawtable <- DT::renderDataTable({
   if (is.null(raw_df()))
       return()

    raw_df()
}, rownames=F)
