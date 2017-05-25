library("spectrolab")
library("BAS")

########################################
# Read Data
########################################

data_2015 = readRDS("data/processed/spectra/data_2015.rds")
fit_paths = dir("data/processed/fit/bma/mcmc/", full.names = TRUE)
fit_bma   = lapply(fit_paths, readRDS)

names(fit_bma) = gsub("fit_bma_raw_mcmc_", "", basename(fit_paths))
names(fit_bma) = gsub(".rds", "", names(fit_bma))

## sanity check
all(names(fit_bma) %in% names(data_2015))

################################################################################
# Predict 2015 data
################################################################################

predictions = lapply(names(fit_bma), function(x){
    BAS::predict.bas(fit_bma[[x]], data_2015[[x]]$spec,
                     top        = 200,
                     estimator  = "BMA",
                     se.fit     = FALSE,
                     prediction = TRUE)
})


names(predictions) = names(data_2015)




####################
# BMA
####################

keep_n_models = 1000

predictions_bma = lapply(fit_chem_bma, function(x){
    list(
        pred_svc =
        ,
        pred_svcprime_bma = BAS::predict.bas(x,
                                             data.frame(svcprime_bma, check.names = FALSE),
                                             top        = keep_n_models,
                                             estimator  = "BMA",
                                             se.fit     = FALSE,
                                             prediction = TRUE)
        ,
        pred_svcprime_plsr = BAS::predict.bas(x,
                                              data.frame(svcprime_plsr, check.names = FALSE),
                                              top        = keep_n_models,
                                              estimator  = "BMA",
                                              se.fit     = FALSE,
                                              prediction = TRUE)
        )
})










# library("spectrolab")
# library("BAS")
# library("pls")
# library("foreach")
#
# ########################################
# # Read data
# ########################################
# # asd = readRDS("data/ASD_fiber16_clean_match_res.rds")
# svc = readRDS("data/SVC_fiber16_clean_match_res.rds")
#
# ########################################
# # Remove svc sample w/ missing chem
# ########################################
# svc = svc[complete.cases(meta(svc)), ]
#
# ####################
# # Choose chemistry
# ####################
# chem_list = c("solubles_perc", "hemicellulose_perc", "cellulose_perc",
#               "lignin_minerals_perc", "NDF_perc", "ADF_perc", "ADL_perc")
#
# # chem_list  = c("cellulose_perc")
#
# chem = lapply(chem_list, function(x){
#     meta(svc, x, simplify = TRUE)
# })
#
# names(chem) = chem_list
#
#
# ################################################################################
# # Fit Chemistry
# ################################################################################
#
# ####################
# # BMA
# ####################
#
# x = data.frame(as.matrix( resample(svc, seq(400, 2400, 20)) ),
#                check.names = FALSE)
#
# ## Fit pars
# prior           = "ZS-null"
# n.models        = 8e5 # 3e5
# update          = 100
# modelprior      = beta.binomial(1, 1)
# MCMC.iterations = 1e8
# thin            = 1e5
#
# fit_chem_bma = setNames(vector("list", length(chem_list)), chem_list)
#
#
# for(i in names(fit_chem_bma)){
#     message(i)
#     fit_chem_bma[[i]] = BAS::bas.lm(chem[[i]] ~ .,
#                                     data            = x,
#                                     prior           = prior,
#                                     n.models        = n.models,
#                                     update          = update,
#                                     modelprior      = modelprior,
#                                     method          = "MCMC+BAS",
#                                     MCMC.iterations = MCMC.iterations,
#                                     thin            = thin,
#                                     initprobs       = "marg-eplogp")
# }
#
#
# pr = predict.bas(fit_chem_bma$cellulose_perc, top = 1000, estimator = "BMA", se.fit = TRUE)
# plot(meta(svc, chem_list[[1]], simplify = TRUE), as.vector(pr$Ybma))
# points(meta(svc, chem_list[[1]], simplify = TRUE), as.vector(pr$Ybma), col = "blue")
# abline(0,1)
#
# ####################
# # PLSR
# ####################
# fit_chem_plsr = lapply(chem_list, function(x){
#     chem = meta(svc, x, simplify = TRUE)
#     plsr(chem ~ ., data = data.frame(as.matrix(svc), check.names = FALSE), ncomp = 20)
# })
# names(fit_chem_plsr) = chem_list
#
# ########################################
# # Predict chemistry
# ########################################
#
# ####################
# # BMA
# ####################
#
# keep_n_models = 1000
#
# predictions_bma = lapply(fit_chem_bma, function(x){
#     list(
#         pred_svc = BAS::predict.bas(x,
#                                     data.frame(as.matrix(svc), check.names = FALSE),
#                                     top        = keep_n_models,
#                                     estimator  = "BMA",
#                                     se.fit     = FALSE,
#                                     prediction = TRUE)
#         ,
#         pred_svcprime_bma = BAS::predict.bas(x,
#                                              data.frame(svcprime_bma, check.names = FALSE),
#                                              top        = keep_n_models,
#                                              estimator  = "BMA",
#                                              se.fit     = FALSE,
#                                              prediction = TRUE)
#         ,
#         pred_svcprime_plsr = BAS::predict.bas(x,
#                                               data.frame(svcprime_plsr, check.names = FALSE),
#                                               top        = keep_n_models,
#                                               estimator  = "BMA",
#                                               se.fit     = FALSE,
#                                               prediction = TRUE)
#         )
# })
#
#
# ####################
# # PLSR
# ####################
#
# predictions_plsr = lapply(fit_chem_plsr, function(x){
#     list(
#         pred_svc = predict(x,
#                            data.frame(as.matrix(svc), check.names = FALSE),
#                            ncomp = 10)
#         ,
#         pred_svcprime_bma = predict(x,
#                                     data.frame(svcprime_bma, check.names = FALSE),
#                                     ncomp = 10)
#         ,
#         pred_svcprime_plsr = predict(x,
#                                      data.frame(svcprime_plsr, check.names = FALSE),
#                                      ncomp = 10)
#     )
# })
#
#
# ########################################
# # Plot pred
# ########################################
#
# ####################
# # BMA
# ####################
#
# par(mfrow = c(3, 3))
#
# # NDF_perc
# plot(meta(svc, "NDF_perc", simplify = TRUE),
#      predictions_bma$NDF_perc$pred_svc$Ybma)
# abline(0, 1)
#
# plot(chem_truth$NDF_perc, predictions_bma$NDF_perc$pred_svcprime_bma$Ybma)
# abline(0, 1)
#
# plot(chem_truth$NDF_perc, predictions_bma$NDF_perc$pred_svcprime_plsr$Ybma)
# abline(0, 1)
#
# # cellulose_perc
# plot(meta(svc, "cellulose_perc", simplify = TRUE),
#      predictions_bma$cellulose_perc$pred_svc$Ybma)
# abline(0, 1)
#
# plot(chem_truth$cellulose_perc, predictions_bma$cellulose_perc$pred_svcprime_bma$Ybma)
# abline(0, 1)
#
# plot(chem_truth$cellulose_perc, predictions_bma$cellulose_perc$pred_svcprime_plsr$Ybma)
# abline(0, 1)
#
# # ADF_perc
# plot(meta(svc, "ADF_perc", simplify = TRUE),
#      predictions_bma$ADF_perc$pred_svc$Ybma)
# abline(0, 1)
#
# plot(chem_truth$ADF_perc, predictions_bma$ADF_perc$pred_svcprime_bma$Ybma)
# abline(0, 1)
#
# plot(chem_truth$ADF_perc, predictions_bma$ADF_perc$pred_svcprime_plsr$Ybma)
# abline(0, 1)
#
#
# ####################
# # PLSR
# ####################
#
# par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
#
# # NDF_perc
# plot(meta(svc, "NDF_perc", simplify = TRUE),
#      predictions_plsr$NDF_perc$pred_svc[,,1],
#      ylab = "NDF Predicted", xlab = "NDF Truth", main = "SVC true")
# abline(0, 1)
#
# plot(chem_truth$NDF_perc, predictions_plsr$NDF_perc$pred_svcprime_bma[,,1],
#      ylab = NA, xlab = NA, main = "SVC BMA")
# abline(0, 1)
#
# plot(chem_truth$NDF_perc, predictions_plsr$NDF_perc$pred_svcprime_plsr[,,1],
#      ylab = NA, xlab = NA, main = "SVC PLSR")
# abline(0, 1)
#
# # cellulose_perc
# plot(meta(svc, "cellulose_perc", simplify = TRUE),
#      predictions_plsr$cellulose_perc$pred_svc[,,1],
#      ylab = "Cellulose Predicted", xlab = "Cellulose Truth", main = "SVC true")
# abline(0, 1)
#
# plot(chem_truth$cellulose_perc, predictions_plsr$cellulose_perc$pred_svcprime_bma[,,1],
#      ylab = NA, xlab = NA, main = "SVC BMA")
# abline(0, 1)
#
# plot(chem_truth$cellulose_perc, predictions_plsr$cellulose_perc$pred_svcprime_plsr[,,1],
#      ylab = NA, xlab = NA, main = "SVC PLSR")
# abline(0, 1)
#
# # ADF_perc
# plot(meta(svc, "ADF_perc", simplify = TRUE),
#      predictions_plsr$ADF_perc$pred_svc[,,1],
#      ylab = "ADF Predicted", xlab = "ADF Truth", main = "SVC true")
# abline(0, 1)
#
# plot(chem_truth$ADF_perc, predictions_plsr$ADF_perc$pred_svcprime_bma[,,1],
#      ylab = NA, xlab = NA, main = "SVC BMA")
# abline(0, 1)
#
# plot(chem_truth$ADF_perc, predictions_plsr$ADF_perc$pred_svcprime_plsr[,,1],
#      ylab = NA, xlab = NA, main = "SVC PLSR")
# abline(0, 1)
#
