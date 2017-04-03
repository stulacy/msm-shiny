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
