dat_list <- list()
params <- read.table("params_asym_simul.txt")
for (i in 1:nrow(params)){
    logn <- params[i, 1]
    X_type <- params[i, 2]
    resid_type <- params[i, 3]
    nreps <- params[i, 4]
    seed <- params[i, 5]

    filename <- paste0("../data/asym_simul_logn", logn,
                       "_seed", seed, "_nreps", nreps,
                       "_", X_type, "_resid", resid_type, ".RData")
    load(filename)

    max_exponent <- min(5 / 6, 1 - 2 / logn)
    exponents <- seq(0, 1, length.out = res$ngrid + 1) * max_exponent
    dat_list[[i]] <- data.frame(
        logn = logn,
        X = X_type,        
        resid = resid_type,
        seed = seed,
        exponent = rep(exponents, res$nreps),
        tauhat = as.numeric(res$tauhats),
        zscore = as.numeric(res$tauhats / res$asds)
    )
}
res <- do.call("rbind", dat_list)

save(file = "../data/asym_simul_summary.RData", res)

