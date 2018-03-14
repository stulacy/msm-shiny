# msm-shiny
A Shiny web app for building, comparing, and visualising multi-state time-to-event models. 

The web app is now [hosted at shinyapps.io](https://stulacy.shinyapps.io/msm-shiny/)!

# Functionality

The app provides several advantages over manually fitting the multi-state models themselves in `mstate` or `flexsurv`:

  - Specify transitions using simple dropdowns with graphical output for feedback, rather than manually entering a transition matrix
  - The (often verbose) formula for specifying models gets automatically generated from the user's selections to a variety of input options, including:
    - The choice of semi-parametric or parametric transition hazard functions
    - Selecting which covariates have an effect on which transition hazards
    - Specifying any identical baseline hazard functions across transitions
    - Specifying the choice of time-scale, between clock-forward and clock-reset
    - Identifying if there's the possibility for the inclusion of any state-arrival times (to form a state-arrival extended semi-Markov model)
  - Models can be saved, facilitating quicker comparison
  - Provides a unified interface for predicting transition probabilities, regardless of the form of the underlying model (which require different estimation methods)
  - Transition probabilities can be displayed as an animation, facilitating quicker interpretation than reading a graph
