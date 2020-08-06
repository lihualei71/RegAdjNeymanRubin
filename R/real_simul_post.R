expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

params <- read.table("../bash/params_real_simul.txt")
exprs <- expand.grid(rho = c(0, 1),
                     thresh = c(0, 0.025))
params <- expand.grid.df(params, exprs)

for (i in 1:nrow(params)){
    data <- as.character(params[i, 1])
    resid_type <- params[i, 2]
    seed <- params[i, 3]
    rho <- params[i, 4]
    thresh <- params[i, 5]

    filename <- paste0("../cluster_raw_data/", data,
                       "_resid", resid_type,
                       "_rho", rho,
                       "_thresh", thresh,
                       "_seed", seed,
                       ".RData")
    load(filename)

    data <- gsub("_simul", "", data)[[1]]
    coverage_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,
        res$coverage,
        seed = seed
    )
    bias_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,        
        res$bias,
        seed = seed
    )
    sdinflate_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,        
        res$sdinflate,
        seed = seed
    )
    normality_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,        
        res$normality,
        seed = seed
    )
    skewness_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,        
        res$skewness,
        seed = seed
    )
    kurtosis_list[[i]] <- data.frame(
        dataset = data,
        resid = resid_type,
        rho = rho,
        tau = thresh,        
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

