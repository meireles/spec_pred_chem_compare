library("spectrolab")
library("BAS")

library("foreach")
library("parallel")
library("doParallel")

########################################
# Read data
########################################
data_2016 = readRDS("data/processed/spectra/data_2016.rds")

########################################
# Fit BMA
########################################

## Register parallel backend
nclust = min(detectCores(),      ## Ok if on my computer
             length(data_2016))  ## If on msi AND asked for that many cores

registerDoParallel(nclust)
getDoParWorkers()

####################
# Parameters
####################

## MCMC
iter   = 5e7
thin   = iter / 5e5

## Model prior

# Justification for prior choice
#   The first moment of a beta binomial is: {n * alpha} / {alpha + beta}.
#   It seems reasonable that the full model includes ~ 30 bands (10nm, total 210)
#   to maybe ~ 45 at 5nm (total 420).
#   So a betabinom(2, 12) shoud work for 10nm and betabinom(2, 16) work for 5nm.

alpha_m_p0 = 1   # 2
beta_m_p0  = 1   # 12 or 16
modelprior = beta.binomial(alpha_m_p0, beta_m_p0)

####################
## Fit
####################

fit_mcmc = foreach(i = names(data_2016), .packages = "BAS") %dopar% {

    message(i)

    fit = BAS::bas.lm(data_2016[[i]]$chem ~ .,
                      data            = data_2016[[i]]$spec,
                      prior           = "ZS-null",
                      n.models        = 1e6,
                      update          = 100,
                      modelprior      = modelprior,
                      method          = "MCMC",
                      MCMC.iterations = iter,
                      thin            = thin,
                      initprobs       = "marg-eplogp")

    ## Write out full fit object
    saveRDS(fit,
            paste("data/processed/fit/bma/mcmc/fit_bma_raw_mcmc_",
                  i, ".rds", sep = ""))

    ## Write out summary w/ inclusion prob and best model spec
    l = list(init_probs = fit$probne0,
             best_model = summary(fit, n.models = 1)[fit$namesx , 2])

    saveRDS(l,
            paste("data/processed/fit/bma/mcmc_summary/fit_bma_raw_mcmc_summary_",
                  i, ".rds", sep = ""))

    message("Finished: ", i)

    ## Return fit in case of interactive use
    fit
}

# names(fit_mcmc) = names(data_2016)
