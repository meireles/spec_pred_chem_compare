library("spectrolab")
library("pls")

########################################
# Read Data
########################################

data_2015 = readRDS("data/processed/spectra/data_2015.rds")
fit_plsr  = readRDS("data/processed/fit/plsr/fit_plsr_raw.rds")

################################################################################
# Predict 2015 data
################################################################################

########################################
# Helper function
########################################

pick_ncomp_madeup = function(x){
    press  = x$validation$PRESS
    cpress = (press[length(press)] - press[1]) / length(press)
    ppress = cpress * seq(length(press) -1 ) - 1
    lpress = c(press[1], press[1] + ppress)
    wpress = abs(press - lpress)
    which(wpress == max(wpress))
}

pick_ncomp_plsdefault = function(x){
    pls::selectNcomp(x, method = "onesigma")
}

########################################
# Predict
########################################

pick_ncomp  = pick_ncomp_madeup

predictions = mapply(function(x, y){
    ncomp = pick_ncomp(x)
    as.vector(predict(x, newdata = y$spec, ncomp = ncomp)) }
    , fit_plsr, data_2015)

########################################
#
########################################



