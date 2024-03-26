## ============================================================================================= ##
## Script:    Balloon Analogue Risk hBayesDM EWMV Simulation
## ============================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Mar 19 09:35:53 2024
## ============================================================================================= ##
## Note: hBayesDM 1.2.1 is not compatible with R 4.3.3 and the following code only works when
##       reverting to R 4.1.3 (issue documented at https://github.com/CCS-Lab/hBayesDM/issues/153)
## ============================================================================================= ##

rm(list=ls())

pushoverUserKey <- "u3mn1xqabvwmrttsiknubcf96xgo3h"
pushoverAPIKey <- "a5of1hgxzko89jwq8nocn7zvr4qwk6"

## Setup --------------------------------------------------------------------------------------- ##
library(hBayesDM); library(truncnorm); library(here); library(dplyr); library(magrittr);
library(readr)
here::here()

## Simulation ---------------------------------------------------------------------------------- ##
npar <- 200 # number of participants
participants <- rep(1:npar, each = 20) # number of trials defined by 'each'
y <- round(rtruncnorm(n = npar*20, a = 0, b = 128, mean = 64, sd = 64)) # truncated normal rounded
burst <- c(64, 105, 39, 96, 88, 21, 121, 10, 64, 32, 64, 101, 26, 34, 47, 121, 64, 95, 75, 13) # bursting poinds

raw_sim_dat <- data.frame(participants, y, burst)

## Reformat for hBayesDM ----------------------------------------------------------------------- ##
# hBayesDM requires a txt file (tab-separated) with 'subjID', 'pumps', 'explosion'
# explosion is not the explosion point but whether the balloon exploded or not
colnames(raw_sim_dat) <- c("subjID", "pumps", "explosion_old")
raw_sim_dat

# mutate explosion to indicate whether explosion point was exceeded
raw_sim_dat %<>%
  mutate(explosion = ifelse(pumps < explosion_old, 0, 1))

# write tab-separated text files for hBayesDM
write.table(raw_sim_dat, file = "balloon_sim.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE, quote = TRUE)
simdat <- read.table("balloon_sim.txt", header = T, sep = "\t")

## Modeling ------------------------------------------------------------------------------------ ##

pushoverr::pushover(message = paste0("Started job 4core4chain: ",
                                     format(Sys.time(),'%B %d, %H:%M'),"."),
                    user = pushoverUserKey, app = pushoverAPIKey)

b_mod <- bart_ewmv(data = simdat, niter = 1000, nwarmup = 500, nchain = 4,
          ncore = 4, nthin = 1, inits = "vb", indPars = "mean", modelRegressor = FALSE,
          vb = FALSE, inc_postpred = FALSE, adapt_delta = .95, stepsize = 1, max_treedepth = 10)

pushoverr::pushover(message = paste0("Finished job 4core4chain: ",
                    format(Sys.time(),'%B %d, %H:%M'),"."),
                    user = pushoverUserKey, app = pushoverAPIKey)

## Visualise ----------------------------------------------------------------------------------- ##

# chains without warmup
plot(b_mod, type="trace", fontSize=11)

# chains with warmup
plot(b_mod, type="trace", inc_warmup=T)

# plot individual parameters
plotInd(b_mod, "sigma")

# compare model fit
printFit(b_mod, b_mod) # want to compare different models here ...

## Posterior Predictive ------------------------------------------------------------------------ ##

dim(b_mod$parVals$y_pred)

b_mod$parVals$sigma # 1000 (MCMC samples) x 200 (subjects) x 20 (trials)


