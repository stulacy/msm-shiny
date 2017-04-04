output$modelselect <- renderUI({
    mods <- reactiveValuesToList(models)
    if (length(mods) < 1)
        return(h5("Please save models in the previous tab"))

    selectInput("modselect", "Select model", choices=names(mods))
})

output$modelsummary <- renderPrint({
    if (is.null(input$modselect))
        return()

    models[[input$modselect]]
})

output$coefficientcomparison <- renderTable({
    mods <- reactiveValuesToList(models)
    # Find coefficient names that are in all the models
    all_coefs <- Reduce(intersect, lapply(mods, function(mod) names(coef(mod))))

    # Obtain values of coefficients
    coef_vals <- lapply(mods, function(mod) coef(mod)[all_coefs])
    comp_df <- data.frame(Coefficient = all_coefs)
    cbind(comp_df, data.frame(coef_vals)) %>%
        arrange(Coefficient)
}, digits=3)

get_km_estimate <- function(){
    # Obtain sink state (get first name if multiple)
    sink_state <- get_sink_states(Q())[1]

    # Columns we want are the sink state values
    cols <- paste(sink_state, c('time', 'status'), sep='.')
    this_df <- raw_df()[, cols]
    colnames(this_df) <- c('time', 'status')

    km_estimate <- survfit(Surv(time, status) ~ 1, data=this_df)
    data.frame(time=km_estimate$time, surv=km_estimate$surv)
}

output$curvecomparison <- renderPlot({
    # Combine with KM estimates and plot
    curves <- reactiveValuesToList(mean_survival)
    if (length(curves) < 1)
        return()

    comb_df <- bind_rows(curves, .id="method")
    ggplot(comb_df, aes(x=time, y=surv, colour=method)) +
        geom_line() +
        labs(x='Time', y='Survival Probability') +
        theme_bw()
})

calculate_mean_covariates <- function() {
    # Now to get the probabilities for each of the models
    # Make a new data frame comprising an 'average' individual from the data set

    trans_mat <- Q()
    raw_df <- proc_df()

    sel_covar <- input$selcovar
    new_list <- setNames(lapply(sel_covar, function(var) {
        col <- raw_df[[var]]
        if (get_var_type(var, raw_df) == 'continuous') {
            mean(col, na.rm=T)
        } else {
            # This is the modal value
            unique(col)[which.max(table(col))]
        }
    }), sel_covar)

    # add mean arrival times just in case
    arrival_trans <- state_arrivals()
    if (length(arrival_trans) > 0) {
        arrival_states <- unique(sapply(arrival_trans, function(id) {
            coords <- which(trans_mat == id, arr.ind=T)
            coords[1]
        }))

        mean_arrival_time <- lapply(arrival_states, function(s) {
            as.numeric(raw_df %>% filter(from == s) %>% summarise(arrival = mean(Tstart)))
        })

        arrival_cols <- setNames(mean_arrival_time,
                                 paste('arrival',
                                       sapply(arrival_states, function(s) row.names(trans_mat)[s]),
                                       sep='.'))
    } else {
        arrival_cols <- list()
    }

    list(covars=new_list, arrival=arrival_cols)
}


summarise_col <- function(data, col) {
    sum_call <- lazyeval::interp(~mean(a), a = as.name(col))
    data %>% summarize_(.dots = setNames(list(sum_call), col))
}


