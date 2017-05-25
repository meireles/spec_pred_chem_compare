library("spectrolab")

# I am keeping the spectral variation associated with a certain chemistry
# value instead of taking the mean spectra for each `ID_chem`.
# Aggregation may happen in `01_pick_spec_and_chem_data`

########################################
# Read data
########################################

paths     = dir("data/raw/spectra_umn/", full.names = TRUE)
spec_raw  = setNames(lapply(paths, readRDS), basename(paths))

########################################
# Process data
########################################

## Match sensors
spec = lapply(spec_raw, match_sensors, splice_at = c(990, 1900))

## 1st resample: before smoothing with moving average. Try to:
##    keep the wvl range
##    resample with minimum spline smoothing (spar = 0.05)
spec = lapply(spec, function(x){
    wlr = range(wavelengths(x))
    resample(x, seq(wlr[[1]], wlr[[2]], by = 1), spar = 0.05)
})

## Moving average smoothing. Remove high frequency variation
spec = lapply(spec, smooth,
              method = "moving_average",
              n = 8,
              save_wvls_to_meta = FALSE)

## 2nd resample
wl_min = 400
wl_max = 2500
spec   = lapply(spec, resample, new_wvls = wl_min : wl_max)

## Remove crap
spec = lapply(spec, function(x){
    x = x[ x[ , 760] > 0.3, ]
    x = x[ x[ , 550] < 0.7, ]
    x = x[ ! spectrolab::has_nir_dip(x), ]
    x
})

## Combine datasets
spec = split(spec, gsub("_.*$", "", names(spec)))
spec = lapply(spec, Reduce, f = combine)


## Remove non chemistry columns from metadata except "ID_chem"
meta_to_remove = c("year", "Notes", "abbrev", "ssp", "life_form")

spec = lapply(spec, function(x){
    meta(x, intersect(names(meta(x)), meta_to_remove)) = NULL
    x
})

########################################

## Create alternative datasets

spec_01 = spec
spec_05 = lapply(spec, resample, new_wvls = seq(wl_min, wl_max, by = 5))
spec_10 = lapply(spec, resample, new_wvls = seq(wl_min, wl_max, by = 10))
spec_20 = lapply(spec, resample, new_wvls = seq(wl_min, wl_max, by = 20))

spec_01_vn = lapply(spec_01, normalize)
spec_05_vn = lapply(spec_05, normalize)
spec_10_vn = lapply(spec_10, normalize)
spec_20_vn = lapply(spec_20, normalize)

########################################
# Write out
########################################

## Diff res

for(i in seq_along(spec_01)){
    saveRDS(spec_01[[i]],
            paste("data/processed/spectra/regular/", names(spec_01[i]), "_01nm.rds", sep = ""))
}

for(i in seq_along(spec_05)){
    saveRDS(spec_05[[i]],
            paste("data/processed/spectra/regular/", names(spec_05[i]), "_05nm.rds", sep = ""))
}

for(i in seq_along(spec_10)){
    saveRDS(spec_10[[i]],
            paste("data/processed/spectra/regular/", names(spec_10[i]), "_10nm.rds", sep = ""))
}

for(i in seq_along(spec_20)){
    saveRDS(spec_20[[i]],
            paste("data/processed/spectra/regular/", names(spec_20[i]), "_20nm.rds", sep = ""))
}

## Diff res and normalized

for(i in seq_along(spec_01_vn)){
    saveRDS(spec_01_vn[[i]],
            paste("data/processed/spectra/normalized/", names(spec_01_vn[i]), "_01nm_vn.rds", sep = ""))
}

for(i in seq_along(spec_05_vn)){
    saveRDS(spec_05_vn[[i]],
            paste("data/processed/spectra/normalized/", names(spec_05_vn[i]), "_05nm_vn.rds", sep = ""))
}

for(i in seq_along(spec_10_vn)){
    saveRDS(spec_10_vn[[i]],
            paste("data/processed/spectra/normalized/", names(spec_10_vn[i]), "_10nm_vn.rds", sep = ""))
}

for(i in seq_along(spec_20_vn)){
    saveRDS(spec_20_vn[[i]],
            paste("data/processed/spectra/normalized/", names(spec_20_vn[i]), "_20nm_vn.rds", sep = ""))
}

