source("ols_adj.R")
source("expr_func.R")

seed <- as.numeric(Sys.getenv("seed"))
X_type <- as.character(Sys.getenv("Xtype"))
resid_type <- as.character(Sys.getenv("residtype"))

filename <- paste0("../data/synthetic_simul_X", X_type,
                   "_resid", resid_type,
                   "_seed", seed, ".RData")

set.seed(seed)
n <- 500
nreps <- 5000
ngrid <- 30

X <- genX(n, X_type)
if (resid_type != "worst"){
    err <- gen_err(X, resid_type)
    Y1 <- err$err1
    Y0 <- err$err0
}
max_exponent <- 5 / 6
exponents <- seq(0, max_exponent, length.out = ngrid + 1)
coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

for (i in 1:(ngrid + 1)){
    exponent <- exponents[i]
    p <- ceiling(n^exponent)
    Xtmp <- X[, 1:p, drop = FALSE]
    if (resid_type == "worst"){
        err <- gen_err(Xtmp, resid_type)
        Y1 <- err$err1
        Y0 <- err$err0
    }
    res <- ols_adj_expr(Y1, Y0, Xtmp, nreps = nreps)
    coverage_list[[i]] <- data.frame(res$coverage,
                                     exponent = exponent)
    bias_list[[i]] <- data.frame(res$bias,
                                 exponent = exponent)
    sdinflate_list[[i]] <- data.frame(res$sdinflate,
                                      exponent = exponent)
    normality_list[[i]] <- data.frame(res$normality,
                                      exponent = exponent)
    skewness_list[[i]] <- data.frame(res$skewness,
                                     exponent = exponent)
    kurtosis_list[[i]] <- data.frame(res$kurtosis,
                                     exponent = exponent)    
    print(i)
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

save(file = filename, res)
