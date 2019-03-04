coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

params <- read.table("params_synthetic_simul.txt")
for (i in 1:nrow(params)){
    X_type <- params[i, 1]
    resid_type <- params[i, 2]
    thresh <- params[i, 3]
    pi1 <- params[i, 4]
    seed <- params[i, 5]

    filename <- paste0("../data/synthetic_simul_X", X_type,
                       "_resid", resid_type,
                       "_thresh", thresh,
                       "_pi1", pi1,
                       "_seed", seed, ".RData")
    load(filename)

    coverage_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,
        seed = seed,
        res$coverage
    )
    bias_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,        
        seed = seed,
        res$bias
    )
    sdinflate_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,        
        seed = seed,
        res$sdinflate
    )
    normality_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,        
        seed = seed,
        res$normality
    )
    skewness_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,        
        seed = seed,
        res$skewness
    )
    kurtosis_list[[i]] <- data.frame(
        X = X_type,
        resid = resid_type,
        tau = thresh,
        pi = pi1,        
        seed = seed,
        res$kurtosis
    )
}

coverage <- do.call("rbind", coverage_list)
bias <- do.call("rbind", bias_list)
sdinflate <- do.call("rbind", sdinflate_list)
normality <- do.call("rbind", normality_list)
skewness <- do.call("rbind", skewness_list)
kurtosis <- do.call("rbind", kurtosis_list)
res <- list(coverage = coverage,
            bias = bias,
            sdinflate = sdinflate,
            normality = normality,
            skewness = skewness,
            kurtosis = kurtosis)

save(file = "../data/synthetic_simul_summary.RData", res)

