library("spectrolab")
library("pls")

########################################
# Read data
########################################
data_2016 = readRDS("data/processed/spectra/data_2016.rds")

########################################
# Fit PLSR
########################################

max_ncomp  = 60
validation = "CV"
jackknife  = TRUE

fit_plsr = lapply(data_2016, function(y){
    y = y
    pls::plsr(y$chem ~ . ,
              data       = y$spec,
              validation = validation,
              ncomp      = min(max_ncomp, length(y$chem) - 1),
              jackknife  = jackknife)
})

########################################
# Write out
########################################

saveRDS(fit_plsr, "data/processed/fit/plsr/fit_plsr_raw.rds")
