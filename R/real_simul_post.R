coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

params <- read.table("../bash/params_real_simul.txt")
for (i in 1:nrow(params)){
    data <- as.character(params[i, 1])
    resid_type <- params[i, 2]
    seed <- params[i, 3]

    filename <- paste0("../data/", data, "_resid", resid_type,
                       "_seed", seed, ".RData")
    load(filename)

    data <- gsub("_simul", "", data)[[1]]
    coverage_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$coverage,
        seed = seed
    )
    bias_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$bias,
        seed = seed
    )
    sdinflate_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$sdinflate,
        seed = seed
    )
    normality_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$normality,
        seed = seed
    )
    skewness_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$skewness,
        seed = seed
    )
    kurtosis_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        res$kurtosis,
        seed = seed
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

save(file = "../data/real_simul_summary.RData", res)

