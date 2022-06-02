# 2022 Ontario election simulation

## Summary

These files produce a dashboard predicting the results of the 2022 Ontario provincial election using publicly available polling data. The dashboard results are based on the results of 10,000 simulations.

The latest version of the dashboard is available [here](https://www.whitlow.ca/ontario_election/dashboard.html).

## Instructions

The files provided are all that are needed to reproduce the dashboard:

1) Change the directory in `setwd()` of `model.R`, `ridings.R`, `figures.R`, `simulation_figures.R`, and `dashboard.Rmd` to the directory in which they are located.

2) Running `run_model.R` will run the model and create the dashboard as `./dashboard.html`, with images and libraries in `./dashboard_files/` and `./dashboard_figures/`.

## Technical information

The forecast has four steps:

1) First, using the polling data, I estimate a Bayesian state space model which treats latent party vote intention over time as a random walk. I 10,000 draws from the posterior of the estimated vote intention on the final day to serve as a starting point for each simulation.

2) Using the estimated provincial vote intention, I estimate regional-level changes in party vote. These changes are modeled as proportional swings from the previous election's results for each individual party:
$$\text{Estimated party vote share in region} = \left( \text{Party vote share in region last election} \times \text{Estimated provincial swing} \right)$$
$$\text{Estimated provincial swing} = \left(\frac{\text{Estimated party provincial vote share today}}{\text{Party provincial vote share last election}} \right)$$
Within each simulation, I add random noise in the swings for every region that is independent across regions but takes into account the correlation structure between vote intention for the various parties estimated in the state space model and rescale the estimated vote shares to ensure they sum to 100% in each region.

3) Using the regional swings, I estimate election results at the riding level using proportional swings calculated identically to the regional swings before, except I use the simulated regional swings calculated before as a starting point.

4) I sum up estimated riding-level results to obtain provincial-level vote and seat total estimates for each simulation.
