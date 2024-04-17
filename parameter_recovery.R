
## ============================================================================================== ##
## Script:    Parameter Recovery
## ============================================================================================== ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Mar 26 13:42:43 2024
## ============================================================================================== ##
##
## ============================================================================================== ##

## Parameter ==================================================================================== ##

# @phi: prior belief of burst
# @eta: learning rate
# @rho: risk preference
# @tau: inverse temperature
# @lambda: loss aversion

## Setup ======================================================================================== ##

library(rstan); library(hBayesDM); library(bayesplot); library(gtools)

task_simulation <- function(n = 1,
                            phi = 1,
                            eta = 1,
                            rho = 1,
                            tau = 1,
                            lambda = 1) {

  # initialise dataset for saving outcome data
  sim_data <- data.frame()

  # Save simulation parameters
  simulation_parameters <- data.frame(
    "subj" = 1:n,
    "phi" = phi,
    "eta" = eta,
    "rho" = rho,
    "tau" = tau,
    "lambda" = lambda
  )

  # initialise parameters
  trials <- 20 # maximum number of trials
  u_gain <- 1 # reward for each pump (default = 1)
  u_stop <- 0 # stopping utility (default = 0)
  pump_max <- 128 # number of max pumps in experiment
  explosion_point <- c(
    64, 105, 39, 96, 88, 21, 121, 10, 64, 32,
    64, 101, 26, 34, 47, 121, 64, 95, 75, 13
  ) # fixed explosion points

  ## FOR LOOPS ---------------------------------------------------------------------------------- ##

  ### Participants ------------------------------------------------------------------------------ ##
  for (j in 1:n) {
    pumps_success <- 0 # number of successful pumps
    pumps_total <- 0 # number of total pumps
    p_burst <- phi[j] # belief of burstpoint

    # Create matrix per subject
    data_subj <-  matrix(NA, ncol = 3, nrow = trials)
    colnames(data_subj) <- c("subjID", "pumps", "explosion")

    ### Trials ---------------------------------------------------------------------------------- ##
    for (k in 1:trials[j]) {
      # set and reset parameters with each trial
      u_loss <- 0 # amount of accumulated reward that can be lost
      pump_count <- 0

      ### Pumps --------------------------------------------------------------------------------- ##
      for (l in 1:explosion_point[k]) {
        u_loss <- l - 1 # potential loss, equivalent to accumulated rewards
        u_pump <- (1 - p_burst) * u_gain - p_burst * lambda[j] * u_loss +
          rho[j] * p_burst * (1 - p_burst) * (u_gain + lambda[j] * u_loss)^2

        p_pump <- 1 / (1 + exp(-tau[j] * (0 - u_pump))) # softmax

        # Sample with computed probability whether participants pumps or transfers
        continue_pump <- sample(1:0, 1, prob = c(p_pump, 1 - p_pump))

        if (continue_pump == 0) { # break loop if participants stops pumping
          break()
        } else {
          pump_count <- pump_count + 1
        }

      } # END PUMP LOOP

      # set stopping point due to 1) stopping, or 2) reaching explosion point
      pumps_trial <- pump_count

      # calculate whether balloon exploded on trial
      explode_trial <- ifelse(pumps_trial == explosion_point[k], 1, 0)

      pumps_total <- pumps_total + pumps_trial
      pumps_success <- pumps_success + pumps_trial - explode_trial

      # update prior belief of bursting probability
      p_burst <- phi[j] + (1 - exp(-pumps_total * eta[j])) *
          ((0.0 + pumps_total - pumps_success) / pumps_total - phi[j])

      # add to matrix
      data_subj[k, "subjID"] <- j
      data_subj[k, "pumps"] <- pumps_trial
      data_subj[k, "explosion"] <- explode_trial

    } # END TRIAL LOOP

    # Add participant data to bottom of dataframe
    sim_data <- rbind(sim_data, as.data.frame(data_subj))

  } # END PARTICIPANT LOOP

  sim_data_list <- list("data" = sim_data, "parameters" = simulation_parameters)
  return(sim_data_list)

} # END FUNCTION

task_simulation(n = 1, phi = .5, eta = .2, rho = .7, tau = .3, lambda = .5)

sim_data_list$

