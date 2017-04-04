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

    # Since this takes more time to run than usual, segment into a maximum
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
    validate(
        need(length(mods) >= 1, "Please build a model before attempting to predict transition probabilities")
    )

    # Calculate all possible times
    times <- seq(0, max(proc_df()$Tstop), length.out=MAX_TRANSPROB_PREDICTION_TIME)

    mod <- mods[[input$modselectpred]]

    # Get input data
    covars <- setNames(lapply(mod$covars, function(i) input[[paste0('in', i)]]),
                       mod$covars)

    arrival_times <- setNames(lapply(mod$arrival, function (a) input[[paste0('in', a)]]),
                              mod$arrival)
    newdata <- convert_wide_to_long(covars, mod$transitions, arrival_times)
    withProgress(message="Calculating...", {
                 predict_transition(mod, newdata, times) %>%
                    filter(source == starting_state())
    })
})

timer <- reactiveValues()

output$transitioneq <- renderUI({
    item_list <- list()
    item_list[[1]] <- HTML("Estimate the probability of being in state <code>j</code> at time <code>t</code> given entering the system at time 0. I.e.:")
    item_list[[2]] <- withMathJax(
        helpText("$$P_{1j}(0, t) = P(X(t) = j | X(0) = 1)$$")
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
COLORSCHEME <- list(text="#666666", fill="#9cbbd1aa")

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
        states_dot <- paste(paste0("node [shape=circle, fixedsize=true, penwidth=2, width=1.5, height=1.5, fontsize=15, fontname=Helvetica, fontcolor='", COLORSCHEME$text,
                                   "'style=filled, gradientangle=90]"),
                            paste0(apply(this_df, 1, function(row) {
                                       blue_ratio <- round(as.numeric(row['prob']), 2)
                                        if (blue_ratio == 1) {
                                            fill <- COLORSCHEME$fill
                                        } else if (blue_ratio == 0) {
                                            fill <- "white"
                                        } else {
                                            fill <- paste0(COLORSCHEME$fill, ";", blue_ratio, ":white")
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
                                  "' [penwidth=2];")}),
                           collapse='\n')

        timer_dot <- paste0("'Current time: ", round(curr_time),
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
