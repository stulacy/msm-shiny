models <- reactiveValues()

output$modelcontent <- renderUI({
    if (length(reactiveValuesToList(transitions)) < 1) {
        return(h5("Please define state transitions before attempting to build a model of the transition hazards."))
    } else {
        item_list <- list(
                modelcovars,
                modelstrata,
                modeltype,
                selectInput("modelclock", "Time scale", choices=c('Clock-forward (Markov)'='forward',
                                                                  'Clock-reset (semi-Markov)'='reset')),
                modelstatearrival,
                buildmodelbutton,
                savemodelbutton
        )
        do.call(tagList, item_list)
    }
})

buildmodelbutton <- renderUI({
     actionButton("buildmodelbutton", "Build model")
})

savemodelbutton <- renderUI({
    if (! display_save_button$display)
        return()
    item_list <- list(hr(),
                      textInput("modelname", "Model name", ''),
                      actionButton("savemodelbutton", "Save model")
                      )
    do.call(tagList, item_list)
})

mean_survival <- reactiveValues()

observeEvent(input$savemodelbutton, {
    if (input$modelname == '') {
        message("Please enter a name for the model!")
        return()
    }

    withProgress(message="Saving model...", {

        this_model <- mod()
        models[[input$modelname]] <- this_model
        display_save_button$display <- FALSE

        # Calculate mean survival curve for each model and save
        mods <- reactiveValuesToList(models)
        # Add KM
        if (length(mods) == 1) {
            mean_survival[['km']] <- get_km_estimate()
        }

        # Calculate mean survival curve for this model
        mean_data <- calculate_mean_covariates()

        newdf_wide <- convert_wide_to_long(mean_data$covars, this_model$transitions, mean_data$arrival)
        raw_probs <- predict_transition(this_model, newdata=newdf_wide, times=mean_survival[['km']]$time, split_sinks=FALSE)
        # Filter where source = starting state and the target is the sink state
        probs <- raw_probs %>%
                    filter(source == starting_state(),
                           target == get_sink_states(Q())[1]) %>%
                    select(time, surv=prob) %>%
                    mutate(surv = 1 - surv)  # Since we want survival probability rather than prob of being dead

        mean_survival[[input$modelname]] <- probs
    })
})

modelcovars <- renderUI({
    if (length(input$selcovar) == 0)
        return()

    this_trans <- reactiveValuesToList(transitions)

    item_list <- list(header=h4('Specify covariates'))
    trans_vals <- sapply(this_trans, function(t) t$index)
    trans_names <- sapply(this_trans,
                          function(t) paste(states()[t$from], states()[t$to], sep=' to '))

    # Loop through covariates
    for (var in input$selcovar) {
        # Add checkbox list for each transition
        item_list[[var]] <- checkboxGroupInput(paste0("sel", var), h5(var),
                                               choices=setNames(trans_vals, trans_names),
                                               selected=trans_vals)
    }
    item_list[['end']] <- hr()

    do.call(tagList, item_list)
})

# TODO Add ability to delete transitions

# Maybe use index more often than just going through length(transitions). Check that order I added
# transitions doesn't matter. This shouldn't be an issue at the moment as the index is the same as the order they are added in, but
# when removing states is implemented then this will change.
modelstrata <- renderUI({
    item_list <- list()
    item_list[['header']] <- h4("Specify strata")
    for (i in names(transitions)) {
        ind <- transitions[[i]]$index
        item_list[[i]] <- selectInput(paste0("strata", ind), label=paste(states()[transitions[[i]]$from],
                                                                         states()[transitions[[i]]$to],
                                                                         sep=' to '),
                                        choices=seq_along(reactiveValuesToList(transitions)),
                                        selected = ind)
    }
    item_list[['end']] <- hr()
    do.call(tagList, item_list)
})

modeltype <- renderUI({
    choices <- names(model_constructors)
    labels <- sapply(model_constructors, function(x) x$long)

    selectInput("modeltype", "Model Type",
                choices=setNames(choices, labels))
})

modelstatearrival <- renderUI({
    trans_ids <- state_arrivals()
    trans <- reactiveValuesToList(transitions)
    # Obtain the label for each transition for checkbox
    trans_labels <- sapply(trans_ids, function(id) {
        # Assuming a single transition per id
        this_trans <- trans[sapply(trans, function(t) t$index == id)][[1]]
        paste(states()[this_trans$from], states()[this_trans$to], sep=' to ')
    })

    item_list <- list(
                    checkboxGroupInput("modelstatearrival", "Include state arrival times",
                                       choices=setNames(trans_ids,
                                                        trans_labels)),
                    hr()
                    )
    do.call(tagList, item_list)
})


# Returns a vector of transitions where it is possible
# to add state arrival information in a meaningful manner.
# This is in the form of a vector of transition indexes
state_arrivals <- reactive({
    # Make a checkbox for each transition that starts on a transient state, so can include time at
    # state arrival as a variable
    trans_mat <- Q()
    # Finds transient states
    transient_states <- (rowSums(trans_mat, na.rm=T) > 0) & (colSums(trans_mat, na.rm=T) > 0)
    # Find all transitions that these states are the source of. Returns a vector of transition ids
    trans_ids <- trans_mat[transient_states, ][!is.na(trans_mat[transient_states, ])]
    trans_ids
})

display_save_button <- reactiveValues(display=FALSE)

mod <- eventReactive(input$buildmodelbutton, {
    withProgress(message="Fitting model...", {
        # This function firstly obtains the full data in long format, i.e. with all selected
        # covariates and all state arrival times.
        # It then builds up the formula to select the variables of interest, as well as adding the
        # 'strata' column as requried
        curr_data <- proc_df()
        trans_mat <- Q()
        trans_list <- reactiveValuesToList(transitions)

        ### Strata ###
        # If have more than 2 strata with 2 values in, or one strata with > 2 values then don't build model
        strata_list <- sapply(seq_along(trans_list), function(i) input[[paste0('strata', i)]])
        strata_tab <- table(strata_list)
        if (sum(strata_tab > 2) > 1 || max(strata_tab) > 2)
            return()

        # Add strata to data frame
        for (i in seq_along(trans_list)) {
            curr_data[curr_data$trans == i, 'strata'] <- input[[paste0('strata', i)]]
        }
        # If have proportional baseline hazard create an indicator variable to differentiate between them
        if (anyDuplicated(strata_list)) {
            ref_value <- which(duplicated(strata_list))
            proportional_transition <- paste0('trans.', ref_value)
            curr_data[[proportional_transition]] <- as.numeric(curr_data$trans == ref_value)
        } else {
            proportional_transition <- c()
        }

        ### Covariates ###
        # Update formula by finding the covariate-transitions selected by the user to include
        covar_str <- unlist(sapply(input$selcovar, function(var) {
            sapply(input[[paste0('sel', var)]], function(tnum) {
                names(curr_data)[grepl(paste0(var, ".*\\.", tnum), names(curr_data))]
            })
        }))

        ### State arrival times ###
        # Determine the state arrival / transition columns that have been specified to include in formula
        ids <- input$modelstatearrival
        if (length(ids) > 0) {
            arrival_cols <- sapply(ids, function(id) {
                coords <- which(trans_mat == id, arr.ind=T)
                from_state <- row.names(trans_mat)[coords[1]]
                paste('arrival', from_state, id, sep='.')
            })
        } else {
            arrival_cols <- c()
        }

        display_save_button$display <- TRUE

        # Determine if model is semi-markov or not
        semi_markov <- (input$modelclock == 'reset') || (length(ids) > 0)

        ### Time scale ###
        time_var <- if (input$modelclock == 'forward') "Tstart, Tstop" else 'time'
        surv_form <- paste0("Surv(", time_var, ", status)")

        ### Tie it all together ###
        covar_str <- c(covar_str, arrival_cols, proportional_transition)
        covar_form <- paste(covar_str, collapse='+')

        curr_data <- curr_data %>% filter(!(time == 0 & status == 0))
        model <- model_constructors[[input$modeltype]]$func(surv_form, covar_form, curr_data, semi_markov)
        stratas <- sapply(seq_along(trans_list), function(i) input[[paste0('strata', i)]])
        model$transitions <- stratas
        model$arrival <- arrival_cols
        model$covars <- input$selcovar
        model
    })
})

######################## Model constructors ##########################

build_cox <- function(surv_form, covar_form, data, semi_markov=F) {
    sep_char <- if (covar_form == '') '' else '+'
    full_form <- paste(paste(surv_form, '~', 'strata(strata)'),
                       covar_form, sep=sep_char)
    mod <- coxph(as.formula(full_form), data=data)
    new_class <- if (semi_markov) 'spsmarkov' else 'spmarkov'

    class(mod) <- c(class(mod), new_class)
    mod
}

# Closure to build parametric model of any distribution
build_parametric <- function(dist) {
    function(surv_form, covar_form, data, semi_markov=F) {
        # TODO Somewhere add option to select the distribution
        sep_char <- if (covar_form == '') '' else '+'
        full_form <- paste(paste(surv_form, '~', 'strata'),
                           covar_form, sep=sep_char)
        mod <- flexsurvreg(as.formula(full_form), anc=list(shape=~ strata), data=data, 
                           dist=dist)
    
        new_class <- if (semi_markov) 'psmarkov' else 'pmarkov'
    
        class(mod) <- c(class(mod), new_class)
        mod
    }
}

model_constructors <- list('semi-parametric'=list('long'='Cox',
                                                  func=build_cox),
                           'exponential' = list('long'='Exponential',
                                               func=build_parametric('exp')),
                           'weibull' = list('long'='Weibull',
                                               func=build_parametric('weibull')),
                           'loglog' = list('long'='Log-Logistic',
                                               func=build_parametric('llogis')),
                           'gamma' = list('long'='Gamma)',
                                               func=build_parametric('gamma')),
                           'gompertz' = list('long'='Gompertz',
                                               func=build_parametric('gompertz')),
                           'lnorm' = list('long'='Log-Normal',
                                               func=build_parametric('lnorm'))
                           )
