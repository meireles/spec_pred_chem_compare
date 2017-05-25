library("spectrolab")
library("BAS")

library("foreach")
library("parallel")
library("doParallel")

########################################
# Read data
########################################
data_2016 = readRDS("data/processed/spectra/data_2016.rds")

fit_paths        = dir("data/processed/fit/bma/mcmc_summary/", full.names = TRUE)
fit_bma_mcmc_sum = lapply(fit_paths, readRDS)

names(fit_bma_mcmc_sum) = gsub("fit_bma_raw_mcmc_summary_", "", basename(fit_paths))
names(fit_bma_mcmc_sum) = gsub(".rds", "", names(fit_bma_mcmc_sum))

## reorder fit elements
fit_bma_mcmc_sum = fit_bma_mcmc_sum[ names(data_2016) ]

########################################
# Fit BMA
########################################

## Register parallel backend
nclust = min(detectCores(),      ## If on my computer
             length(data_2016))  ## Of on msi AND asked for that many cores
registerDoParallel(nclust)
getDoParWorkers()

## Fit
fit = foreach(i = names(data_2016), .packages = "BAS") %dopar% {
    message(i)

    f = BAS::bas.lm(data_2016[[i]]$chem ~ .,
                    data            = data_2016[[i]]$spec,
                    prior           = "ZS-null",
                    n.models        = 3e5,
                    update          = 100,
                    modelprior      = beta.binomial(1, 1),
                    method          = "BAS",
                    initprobs       = fit_bma_mcmc_sum[[i]]$init_probs,
                    bestmodel       = fit_bma_mcmc_sum[[i]]$best_model)

    saveRDS(f,
            paste("data/processed/fit/bma/bas/fit_bma_raw_bas_", i, ".rds", sep = ""))

    n.models = 500

    cf = BAS::coef.bas(f, n.models = n.models, estimator = "BMA")
    ci = BAS::confint.coef.bas(cf)
    sm = BAS::summary.bas(f, n.models = n.models)

    message("Finished: ", i)

    list(coef_obj = cf, confint_coef_obj = ci, summary = sm)
}


names(fit) = names(data_2016)


########################################
# Write out
########################################

for(i in names(fit)){
    saveRDS(fit[[i]], paste("data/processed/fit/bma/bas/fit_bma_avgd_bas_", i, ".rds", sep = ""))
}
