
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
                            lambda = 1
                            ){

  simulated_task <- data.frame()

  # Save simulation parameters
  simulation_parameters <- data.frame("subj" = 1:n,
                                      "phi" = phi,
                                      "eta" = eta,
                                      "rho" = rho,
                                      "tau" = tau,
                                      "lambda" = lambda)

  # define order of explosion points (same across participants)
  trial_order <- c(64, 105, 39, 96, 88, 21, 121, 10, 64, 32, 64, 101, 26, 34, 47, 121, 64, 95, 75, 13)

  for(subj in 1:n){

    agent_dat <- matrix(NA, ncol = 9, nrow = 64)
    colnames(agent_dat) <- c("subjID", "trial", "trial_type",
                             "pumps", "explode", "SU", "UT")
    # UT = transfer probability
    # SU = subjective utility for pumping

    for(t in 1:20){
      trial_type <- trial_order[t]

      agent_dat[t, "subjID"] <- subj
      agent_dat[t, "trial"] <- t

      # create offer
      agent_dat[t, "trial_type"] <- trial_type

      S_E <- kE[subj] * agent_dat[t, "effort_a"]^2
      S_R <- kR[subj] * agent_dat[t, "amount_a"]
      SV <- S_R - S_E
      # Choice probability
      prob_A <- 1 / (1 + exp(-1*((alpha[subj]) + SV)))


      # Sample choice
      agent_dat[t, "SV"] <- SV
      agent_dat[t, "choice"] <- sample(1:0, 1, prob = c(prob_A, 1-prob_A))

      # Update for next staircase iteration
      # Accept
      if(agent_dat[t, "choice"] == 1){
        # no updating
        if(staircases[trial_type][[1]][1] == 4
           & staircases[trial_type][[1]][2] == 1){
          staircases[trial_type][[1]] <- staircases[trial_type][[1]]

          # update reward (decrease)
        } else if(staircases[trial_type][[1]][1] == 4){
          staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]-1

          # update effort (increase)
        } else if(staircases[trial_type][[1]][2] == 1){
          staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]+1

          # random update choice (increase effort or decrease reward)
        } else {
          random_update <- sample(1:2, 1)
          if(random_update == 1){
            staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]+1
          } else {
            staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]-1
          }
        }
        # Reject
      } else if(agent_dat[t, "choice"] == 0){
        # no updating
        if(staircases[trial_type][[1]][1] == 1 & staircases[trial_type][[1]][2] == 4){
          staircases[trial_type][[1]] <- staircases[trial_type][[1]]

          # update reward (increase)
        } else if(staircases[trial_type][[1]][1] == 1){
          staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]+1

          # update effort (decrease)
        } else if(staircases[trial_type][[1]][2] == 4){
          staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]-1

          # random update choice (decrease effort or increase reward)
        } else {
          random_update <- sample(1:2, 1)
          if(random_update == 1){
            staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]-1
          } else {
            staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]+1
          }
        }
      }
    }

    agent_dat <- as.data.frame(agent_dat)
    simulated_task <- rbind(simulated_task, agent_dat)
  }

  return(list("simulation_parameters" = simulation_parameters,
              "simulated_task" = simulated_task))
}