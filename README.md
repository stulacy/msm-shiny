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

The easiest way to run this app (and other Shiny apps) is to use RStudio using the following steps:

  1. File -> New Project -> Version Control -> Git
  2. Enter whatever project name and location you wish, but use the following as the Repository URL: *https://github.com/stulacy/msm-shiny.git*
  3. Once the project has been pulled from github, run the app by opening the `app.R` file into the editor and press the `Run App` button that appears to the top right of the editor. This will create a local test server on your PC and open a web browser pointed to its location, enabling you to run the app.

Ideally the app would be running on a dedicated Shiny server, and I will release the app onto `shinyapps.io` once I'm happy it's gotten to a state suitable for release.
