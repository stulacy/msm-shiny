library(shiny)
library(dplyr)
library(mstate)
library(tidyr)
library(ggplot2)
library(DiagrammeR)
library(flexsurv)
library(stringr)

get_time_status_vars <- function(df, var='time') {
    names(df)[grep(paste0(".+\\.", var), names(df))]
}

get_var_type <- function(v, data) {
    if (is.factor(data[, v])) 'categorical' else 'continuous'
}

prepare_data <- function(raw, transmat, covars, time_v, status_v, state_arrival_v) {
    # Hacks to get round mspreps inability to allow a single covariate, and also
    # so that the `keep` argument isn't provided if no covariates of interest, since
    # the msprep function doesn't allow for an argument of length zero
    if (length(covars) == 1) {
        covars <- c(covars, "foo")
        raw$foo <- rep(NA, nrow(raw))
        mstate1 <- msprep(time = c(NA, time_v),
                          status = c(NA, status_v),
                          data = raw, trans = transmat, id="id",
                          keep=covars)
        mstate1 <- mstate1[, -ncol(mstate1)]
        attr(mstate1, "trans") <- transmat
    } else if (length(covars) == 0) {
        mstate1 <- msprep(time = c(NA, time_v),
                          status = c(NA, status_v),
                          data = raw, trans = transmat, id="id"
                          )
    } else {
        mstate1 <- msprep(time = c(NA, time_v),
                          status = c(NA, status_v),
                          data = raw, trans = transmat, id="id",
                          keep=covars)
    }

    if (length(covars) > 0)
        mstate1 <- expand.covs(mstate1, covars)

    # TODO Add state arrival times
    # Obtain the ids of transitions where can have state arrival
    ids <- state_arrival_v
    if (length(ids) > 0) {
        all_state_arrivals <- sapply(ids, function(id) {
            coords <- which(transmat == id, arr.ind=T)
            row.names(transmat)[coords[1]]
        })
        state_arrivals <- unique(all_state_arrivals)
        new_cols <- paste('arrival', state_arrivals, sep='.')
        # Create new columns containing this information and expand for every transition
        for (i in seq_along(state_arrivals)) {
            mstate1[[new_cols[i]]] <- mstate1$Tstart
        }
        mstate1 <- expand.covs(mstate1, new_cols)
    }

    mstate1
}

ggprob <- function(obj) {
    ggplot(obj, aes(x=time, y=prob, colour=as.factor(target))) +
        geom_line() +
        labs(x="Time", y="Probability") +
        scale_colour_discrete("Destination state") +
        theme_bw()
}

server <- function(input, output) {

    ############################### Raw Data ###########################################################

    all_df <- reactive({
        infile <- input$filein

        if (is.null(infile))
            return()

        isolate({
            read.table(infile$datapath, sep=',', header=T)
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
        comb_vars <- as.vector(sapply(seq_along(time_vars), function(i) c(time_vars[i], status_vars[i])))

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

    output$rawobs <- renderUI({
        if (is.null(raw_df()))
            return()

        numericInput("rawobs", label=HTML("<h5><strong>Number of observations</strong></h5>"), 10)
    })

    output$rawsummary <- renderUI({
        if (is.null(raw_df()))
            return(h5("Select a file containing time-to-event data in the tab on the left."))

        item_list <- list()
        item_list[[1]] <- h4("Data summary")
        item_list[[2]] <- renderPrint(summary(raw_df()))
        do.call(tagList, item_list)
    })

    output$rawtable <- renderTable({
       if (is.null(raw_df()) || is.null(input$rawobs))
           return()

        isolate({
            raw_df() %>%
                head(n=input$rawobs)
        })
    })

    # TODO Histograms of selected covariates?

    ############################### States ############################################################
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

    output$currtransheader <- renderUI({
        txt <- if (length(reactiveValuesToList(transitions)) >= 1) 'Current transitions' else ''
        h4(txt)
    })

    output$currtrans <- renderTable({
        from <- sapply(reactiveValuesToList(transitions), function(x) states()[x$from])
        to <- sapply(reactiveValuesToList(transitions), function(x) states()[x$to])
        data.frame(From=from, To=to)
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


    ############################### Modelling ############################################################
    models <- reactiveValues()

    output$modelcontent <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1) {
            return(h5("Please define state transitions before attempting to build a model of the transition hazards."))
        } else {
            item_list <- list(
                    h5("Building regression models for the transition hazards."),
                    hr(),
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

    # TODO Maybe use index more often than just going through length(transitions). Check that order I added
    # transitions doesn't matter. This shouldn't be an issue at the moment as the index is the same as the order they are added in, but
    # when removing states is implemented then this will change.
    modelstrata <- renderUI({
        item_list <- list(header=h4("Specify strata"))
        for (i in names(transitions)) {
            ind <- transitions[[i]]$index
            item_list[[ind]] <- selectInput(paste0("strata", ind), label=paste(states()[transitions[[i]]$from],
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
        trans_ids <- trans_mat[transient_states, !is.na(trans_mat[transient_states, ])]
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
            stratas <- sapply(seq_along(transitions), function(i) input[[paste0('strata', i)]])
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

    build_parametric <- function(surv_form, covar_form, data, semi_markov=F) {
        # TODO Somewhere add option to select the distribution
        sep_char <- if (covar_form == '') '' else '+'
        full_form <- paste(paste(surv_form, '~', 'strata'),
                           covar_form, sep=sep_char)
        mod <- flexsurvreg(as.formula(full_form), anc=list(shape=~ strata), data=data, dist='weibull')

        new_class <- if (semi_markov) 'psmarkov' else 'pmarkov'

        class(mod) <- c(class(mod), new_class)
        mod
    }

    model_constructors <- list('semi-parametric'=list('long'='Semi-Parametric',
                                                      func=build_cox),
                               'parametric' = list('long'='Parametric (Weibull)',
                                                   func=build_parametric)
                               )

    ######################### Model predict methods ##########################
    predict_transition <- function(obj, ...) UseMethod("predict_transition", obj)

    predict_transition.spmarkov <- function(obj, newdata, times, split_sinks=TRUE) {
        predict_transition_cox(obj, newdata, times, split_sinks)
    }

    predict_transition.spsmarkov <- function(obj, newdata, times, split_sinks=TRUE) {
        predict_transition_cox(obj, newdata, times, split_sinks)
    }

    predict_transition_cox <- function(obj, newdata, times, split_sinks=TRUE) {
        trans_mat <- Q_prediction(split_sinks=split_sinks)
        haz <- msfit(obj, newdata=newdata, trans=trans_mat)
        haz$trans <- trans_mat # Just to double check in case separated sink states

        sink_states <- get_sink_states(trans_mat)
        obj <- probtrans(haz, predt=PREDICTION_TIME)

        # Work out how many states there are and subset just these values
        nstates <- dim(obj$trans)[1]
        probs_list <- lapply(seq(nstates), function(i) obj[[i]])
        # Convert into long data frame, taking state names from original probtrans object
        probs_df <- bind_rows(probs_list, .id="source") %>%
                        select(source, time, starts_with("pstate")) %>%
                        gather(target, prob, -source, -time) %>%
                        mutate(target = stringr::str_replace(target, "pstate", ""),
                               source = factor(source, labels=rownames(obj$trans)),
                               target = factor(target, labels=rownames(obj$trans))) %>%
                        select(time, source, target, prob) %>%
                        filter(!source %in% sink_states) %>%
                        droplevels() %>%
                        arrange(time)
        probs_df
    }

    predict_transition.pmarkov <- function(obj, newdata, times, split_sinks=TRUE) {
        trans_mat <- Q_prediction(split_sinks=split_sinks)
        this_states <- row.names(trans_mat)
        sink_states <- get_sink_states(trans_mat)

        probs <- pmatrix.fs(obj, trans=trans_mat, newdata=newdata, t=times, tvar='strata')
        long_df <- bind_rows(lapply(probs, function(mat) {
            this_df <- data.frame(mat)
            colnames(this_df) <- this_states
            this_df$source <- this_states
            gather(this_df, target, prob, -source) %>%
                select(source, target, prob)}),
            .id='time') %>%
            mutate(time=as.numeric(time)) %>%
            filter(! source %in% sink_states) %>%
            droplevels()
        long_df
    }

    predict_transition.psmarkov <- function(obj, newdata, times, split_sinks=TRUE) {
        max_times <- 10
        trans_mat <- Q_prediction(split_sinks=split_sinks)
        this_states <- row.names(trans_mat)
        sink_states <- get_sink_states(trans_mat)

        # TODO Since this takes more time to run than usual, segment into a maximum
        # number of times
        if (length(times) > max_times) {
            times <- seq(min(times), max(times), length.out=max_times)
        }
        probs <- lapply(times, function(t) {
            pmatrix.simfs(obj, trans=trans_mat, newdata=newdata, t=t, tvar='strata')
        })

        long_df <- bind_rows(setNames(lapply(probs, function(mat) {
            this_df <- data.frame(mat)
            colnames(this_df) <- this_states
            this_df$source <- this_states
            gather(this_df, target, prob, -source) %>%
                select(source, target, prob)}), times),
            .id='time') %>%
            mutate(time=as.numeric(time)) %>%
            filter(! source %in% sink_states) %>%
            droplevels()
        long_df
    }

    output$model <- renderUI({
        if (is.null(input$buildmodelbutton) || input$buildmodelbutton == 0) {
            return()
        }

        isolate({
            if (is.null(mod())) {
                return(h5("Error: model can currently only handle one strata with more than one transition, and no more than two transitions in a single strata."))
            }
            fluidRow(
                     h4("Model Summary"),
                     renderPrint(mod())
                    )
        })
    })

    ############################### Comparison ############################################################
    output$modelselect <- renderUI({
        mods <- reactiveValuesToList(models)
        if (length(mods) < 1)
            return(h5("Please save models in the previous tab"))

        item_list <- list(h5("View saved models here and compare them on both hazard ratios and predictions"),
                          selectInput("modselect", "Select model", choices=names(mods)))
        do.call(tagList, item_list)
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
        cbind(comp_df, data.frame(coef_vals))
    })

    ### Functions for survival comparison
    # TODO Does this need to be reactive or can a general function work?
    get_km_estimate <- reactive({
        # Obtain sink state (get first name if multiple)
        sink_state <- get_sink_states(Q())[1]

        # Columns we want are the sink state values
        cols <- paste(sink_state, c('time', 'status'), sep='.')
        this_df <- raw_df()[, cols]
        colnames(this_df) <- c('time', 'status')

        km_estimate <- survfit(Surv(time, status) ~ 1, data=this_df)
        data.frame(time=km_estimate$time, surv=km_estimate$surv)
    })

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




    ############################### Prediction ############################################################
    output$predmodelselect <- renderUI({
        if (length(reactiveValuesToList(models)) < 1) {
            return(h5("Please build a model before attempting to predict transition probabilities."))
        }
        item_list <- list()
        item_list[[1]] <- hr()
        item_list[[2]] <- selectInput("modselectpred", "Select model", choices=names(models))
        item_list[[3]] <- hr()
        do.call(tagList, item_list)
    })

    output$newdata <- renderUI({
        if (is.null(input$modselectpred))
            return()

        trans_mat <- Q_prediction(split_sinks = T)

        mod <- models[[input$modselectpred]]

        # Obtain all covars that are used in model
        used_covars <- mod$covars
        included_arrival_cols <- mod$arrival

        # If categorical variable then offer dropdown of types, otherwise slider
        var_types <- sapply(used_covars, get_var_type, all_df())
        item_list <- list()
        item_list[[1]] <- h4("New data")

        # Create entry elements for new data
        for (i in seq(length(used_covars))) {
            col <- all_df()[[used_covars[i]]]
            if (var_types[i] == 'continuous') {
                mx <- max(col, na.rm=T)
                mn <- min(col, na.rm=T)
                item_list[[i+1]] <- sliderInput(paste0('in', used_covars[i]), used_covars[i],
                                              min=round(mn),
                                              max=round(mx),
                                              value=round(((mn + mx) / 2), 2),
                                              width='40%')
            } else if (var_types[i] == 'categorical') {
                item_list[[i+1]] <- selectInput(paste0('in', used_covars[i]), used_covars[i],
                                              choices=unique(col),
                                              width='40%')
            }
        }

        # Add UI elements for state arrival terms
        # Determine all possible state arrival transitions
        #possible_trans <- state_arrivals()
        ## Determine the column names for each of these
        #arrival_col_names <- sapply(possible_trans, function(t) {
        #    coords <- which(trans_mat == t, arr.ind=T)
        #    state <- row.names(trans_mat)[coords[1]]
        #    paste('arrival', state, t, sep='.')
        #})
        ## See which of these are in the data frame
        #included_arrival_cols <- arrival_col_names[arrival_col_names %in% names(coef(mod))]

        # Make slider for each of these
        for (col_name in included_arrival_cols) {
            # TODO calculate mean and max values
            col <- proc_df()[[col_name]]
            item_list[[col_name]] <- sliderInput(paste0('in', col_name), col_name,
                                            min=0,
                                            max=max(col),
                                            value = mean(col[col > 0]),
                                            width='40%')
        }

        # Add an input for state arrival term if have added it to the specific model
        do.call(tagList, item_list)
    })

    get_newdata <- function(mod) {
        num_trans <- length(reactiveValuesToList(transitions))
        newd <- data.frame(trans=seq(num_trans), strata=mod$transitions)
        class(newd) <- c(class(newd), "msdata")
        attr(newd, "trans") <- Q()

        # TODO Get this from the inputs not from anything else
        if (length(input$selcovar) > 0) {
            for (i in input$selcovar) {
                col <- rep(input[[paste0('in', i)]], num_trans)
                var_type <- get_var_type(i, all_df())
                if (var_type == 'continuous') {
                    col <- as.numeric(col)
                } else if (var_type == 'categorical') {
                    col <- factor(col, levels=levels(all_df()[[i]]))
                }
                newd[[i]] <- col
            }
            newd <- expand.covs(newd, input$selcovar)
        }

        # TODO Get state arrival times too

        # Add variable to indicate the source states if have two proportional baseline hazards
        #strata_list <- sapply(seq_along(transitions), function(i) input[[paste0('strata', i)]])
        strata_list <- mod$transitions
        if (anyDuplicated(strata_list)) {
            ref_value <- which(duplicated(strata_list))
            new_var <- paste0('trans.', ref_value)
            newd[[new_var]] <- as.numeric(newd$trans == ref_value)
        }
        newd
    }

    # Converts a wide data frame to long, as well as setting up the required class so that
    # it can be used with mstate
    # Covars is a list of covariates with their value
    # Strata is a vector of # transitions long with an integer indicating which strata each one
    # belongs to
    convert_wide_to_long <- function(covars, strata, arrival_cols) {
        num_trans <- length(reactiveValuesToList(transitions))
        newd <- data.frame(trans=seq(num_trans), strata=strata)
        class(newd) <- c(class(newd), "msdata")
        attr(newd, "trans") <- Q()

        # Add covariates to wide data frame
        if (length(covars) > 0) {
            covar_cols <- data.frame(setNames(lapply(names(covars), function(var) {
                var_type <- get_var_type(var, all_df())
                col <- rep(covars[[var]], num_trans)
                if (var_type == 'categorical') {
                    col <- factor(col, levels=levels(all_df()[[var]]))
                }
                col
            }), names(covars)))
            newd <- cbind(newd, covar_cols)
        }

        # Add arrival columns to wide data frame
        if (length(arrival_cols) > 0)
            newd <- cbind(newd, arrival_cols)

        # Expand columns
        class(newd) <- c(class(newd), "msdata")
        attr(newd, "trans") <- Q()

        if (length(covars) > 0)
            newd <- expand.covs(newd, names(covars))
        if (length(arrival_cols) > 0)
            newd <- expand.covs(newd, names(arrival_cols))

        # Add variable to indicate the source states if have two proportional baseline hazards
        if (anyDuplicated(strata)) {
            ref_value <- which(duplicated(strata))
            new_var <- paste0('trans.', ref_value)
            newd[[new_var]] <- as.numeric(newd$trans == ref_value)
        }
        newd
    }


    MAX_TRANSPROB_PREDICTION_TIME <- 1000

    transprobs <- eventReactive(input$updatepred, {
        mods <- reactiveValuesToList(models)
        # TODO Specify model from a dropdown
        validate(
            need(length(mods) >= 1, "Please build a model before attempting to predict transition probabilities")
        )

        # Calculate all possible times
        # TODO Obtain more sensible values here
        times <- seq(0, max(proc_df()$Tstop), length.out=MAX_TRANSPROB_PREDICTION_TIME)

        mod <- mods[[input$modselectpred]]

        # Get input data
        covars <- setNames(lapply(mod$covars, function(i) input[[paste0('in', i)]]),
                           mod$covars)

        arrival_times <- setNames(lapply(mod$arrival, function (a) input[[paste0('in', a)]]),
                                  mod$arrival)
        # TODO The covariates should be taken from the model
        newdata <- convert_wide_to_long(covars, mod$transitions, arrival_times)
        withProgress(message="Calculating...", {
                     predict_transition(mod, newdata, times) %>%
                        filter(source == starting_state())
        })
    })

    timer <- reactiveValues()

    output$transitioneq <- renderUI({
        item_list <- list()
        item_list[[1]] <- HTML("Estimate the probability of being in state <code>j</code> at time <code>t</code> given being in state <code>i</code> at time <code>s</code>. I.e.:")
        item_list[[2]] <- withMathJax(
            helpText("$$P_{ij}(s, t) = P(X(t) == j | X(s) == i)$$")
        )
        do.call(tagList, item_list)
    })

    # have ability to separate sink states by arrival states
    output$plotpredbutton <- renderUI({
        if (length(reactiveValuesToList(models)) < 1)
            return()
        actionButton("updatepred", "Estimate transition probabilities")
    })

    output$plotprob <- renderPlot({
        probs <- transprobs()
        if (is.null(probs))
            return()
        ggprob(probs)
    })

    MAX_DECIMAL_COLOUR <- 255
    EDGE_COLOUR <- '000000'

    ########################## Animation ########################################

    output$animationparams <- renderUI({
        probs <- transprobs()
        if (is.null(probs))
            return()

        item_list <- list(
            hr(),
            h4("Animation parameters"),
            sliderInput('animaxtime', 'Maximum time', min=0, max=round(max(probs$time), 2), value=round(max(probs$time), 2)),
            sliderInput('anistepsize', 'Time step', min=0, max=round(max(probs$time)/3), value=round(max(probs$time)/10)),
            sliderInput('aniupdate', 'Update frequency (ms)', min=0, max=2000, value=1000),
            actionButton("startanimation", "Start")
        )
        do.call(tagList, item_list)
    })

    animation_probs <- eventReactive(input$startanimation, {
        probs <- transprobs()

        if (is.null(probs))
            return()

        # Calculate all possible times
        times <- seq(PREDICTION_TIME, input$animaxtime, by=input$anistepsize)

        # Set initial timer value
        timer$curr_time <- PREDICTION_TIME

        # Determine the times we can use from the transition probability data frame
        ani_times <- unique(probs$time[findInterval(times, probs$time)])

        # Subset data frame to these times and include a column 'simtime' detailing the
        # time in the simulation so that the probabilities can be extracted easily
        out_long <- bind_rows(lapply(ani_times, function(t) probs[probs$time == t, ]))
        out_long$time <- as.numeric(out_long$time)
        out_long$prob <- as.numeric(out_long$prob)
        out_long
    })

    PREDICTION_TIME <- 0

    output$animation <- renderGrViz({
        probs <- animation_probs()

        if (is.null(transprobs()))
            return()

        isolate({
            times <- unique(probs$time)
            curr_time <- timer$curr_time
            this_df <- probs[probs$time == curr_time, ]

            trans_mat <- Q_prediction(split_sinks=T)

            # Create dot diagram with the buckets getting filled up
            states_dot <- paste("node [shape=circle, fixedsize=true, width=1.5, height=1.5, fontsize=12, style=filled, gradientangle=90]",
                                paste0(apply(this_df, 1, function(row) {
                                           blue_ratio <- round(as.numeric(row['prob']), 2)
                                            if (blue_ratio == 1) {
                                                fill <- "blue"
                                            } else if (blue_ratio == 0) {
                                                fill <- "white"
                                            } else {
                                                fill <- paste0("blue;", blue_ratio, ":white")
                                            }
                                            paste0("'", row['target'], "'", # Important to put the name in quotes in case of space
                                                   " [fillcolor='", fill, "']")
                                            }),
                                       collapse='\n'),
                                sep='\n')
            # Find the transitions
            trans <- which(!is.na(trans_mat), arr.ind=T)
            edges_dot <- paste(apply(trans, 1, function(row) {
                               paste0("'", row.names(trans_mat)[row[1]],
                                      "' -> '",
                                      colnames(trans_mat)[row[2]],
                                      "';")}),
                               collapse='\n')

            timer_dot <- paste0("'Current time: ", round(curr_time, 2),
                                "' [shape=plaintext, style='', fontsize=15]\n")
            full <- paste("digraph states {", states_dot, edges_dot, timer_dot, "}")

            # Have to extract these input values in isolate away from the invalidateLater call
            # As can't have that in isolate environment

            #end_time <- input$animaxtime
            #step_size <- input$anistepsize
            update_freq <- input$aniupdate
        })

        # Update loop and plot
        if ((curr_time) < max(times)) {
            timer$curr_time <<- times[which(times==curr_time)+1]
            invalidateLater(update_freq)
        } else {
            timer$curr_time <<- PREDICTION_TIME
        }
        grViz(full)

    })
}

