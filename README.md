# msm-shiny
A Shiny web app for building, comparing, and visualising multi-state time-to-event models. 

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

# Installation

The easiest way to run this app (and other Shiny apps) is to use RStudio. Create a new project and select "from git repository", passing in the link to this repository.

To run the app simply open the `app.R` file and click the `Run App` button that appears to the top right of the editor. This will create a local test server on your PC and open a web browser pointed to its location, enabling you to run the app.

Ideally the app would be running on a dedicated Shiny server, but I do not currently have any plans to create a `Shinyapps.io` account.
