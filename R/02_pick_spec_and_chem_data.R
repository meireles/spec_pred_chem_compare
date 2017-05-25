library("spectrolab")

########################################
# Read data
########################################

## Resolution: 05nm
## Normalized: FALSE

cn16    = readRDS("data/processed/spectra/regular/CN16_10nm.rds")
fiber16 = readRDS("data/processed/spectra/regular/fiber16_10nm.rds")
pig16   = readRDS("data/processed/spectra/regular/pig16_10nm.rds")

cn15    = readRDS("data/processed/spectra/regular/CN15_10nm.rds")
fiber15 = readRDS("data/processed/spectra/regular/fiber15_10nm.rds")
pig15   = readRDS("data/processed/spectra/regular/pig15_10nm.rds")


########################################
# Aggregate or not?
########################################

aggregate = FALSE

if(aggregate){
    f = function(x){
        aggregate(x = x, by = meta(x, "ID_chem", simplify = TRUE ), FUN = mean,
                  FUN_meta = try_keep_txt(mean), na.rm = TRUE)
    }

    cn16    = f(cn16)
    fiber16 = f(fiber16)
    pig16   = f(pig16)

    cn15    = f(cn15)
    fiber15 = f(fiber15)
    pig15   = f(pig15)
}


########################################
# Helper function
########################################

make_data = function(x){
    spec           = as.matrix(x)
    rownames(spec) = NULL
    predictors     = data.frame(spec, check.names = FALSE)

    rm_meta = c("ID_chem", "normalization_magnitude")
    kp_meta = as.list(meta(x))
    kp_meta = kp_meta[ setdiff(names(kp_meta), rm_meta) ]

    lapply(kp_meta, function(y){

        k = complete.cases(y) & complete.cases(predictors)
        list(chem = y[k],
             spec = predictors[k, ])
    })
}

########################################
# Create datasets
########################################

## For fitting (2016)
pigdat_16 = make_data(pig16)
pigdat_16 = pigdat_16[ grep("umol_m2", names(pigdat_16)) ]

data_2016 = c(make_data(cn16),
              make_data(fiber16),
              pigdat_16)


## For testing (2015)
pigdat_15 = make_data(pig15)
pigdat_15 = pigdat_15[ grep("umol_m2", names(pigdat_15)) ]

data_2015 = c(make_data(cn15),
              make_data(fiber15),
              pigdat_15)

########################################
# Write out
########################################

saveRDS(data_2016, "data/processed/spectra/data_2016.rds")
saveRDS(data_2015, "data/processed/spectra/data_2015.rds")
