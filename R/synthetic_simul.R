#!/usr/bin/env Rscript

source("ols_adj.R")
source("expr_func.R")

if (!interactive()){
    suppressPackageStartupMessages(library("argparse"))

    parser <- ArgumentParser()

    parser$add_argument("--Xtype", type = "character", default = "normal", help = "Distribution of X")
    parser$add_argument("--residtype", type = "character", default = "normal", help = "Distribution of PO")
    parser$add_argument("--rho", type = "double", default = 0, help = "Correlation between Y(1) and Y(0)")
    parser$add_argument("--thresh", type = "double", default = 0, help = "quantile threshold")
    parser$add_argument("--pi1", type = "double", default = 0.1, help = "Proportion of treated")
    parser$add_argument("--seed", type = "integer", default = 1, help = "Random seed")
    
    args <- parser$parse_args()
    
    X_type <- args$Xtype
    resid_type <- args$residtype
    rho <- args$rho
    thresh <- args$thresh
    pi1 <- args$pi1
    seed <- args$seed
} else {
    X_type <- "t2"
    resid_type <- "worst"
    rho <- 1
    thresh <- 0
    pi1 <- 0.2
    seed <- 0
}

filename <- paste0("../cluster_raw_data/synthetic_simul",
                   "_X", X_type,
                   "_resid", resid_type,
                   "_rho", rho,                   
                   "_thresh", thresh,
                   "_pi1", pi1,
                   "_seed", seed,
                   ".RData")

set.seed(seed * 2018)
n <- 2000
nreps <- 5000
ngrid <- 15

X <- genX(n, X_type)
if (!resid_type %in% c("worst", "hetero")){
    err <- gen_err(X, resid_type, rho)
    Y1 <- err$err1
    Y0 <- err$err0
}
max_exponent <- 0.7
exponents <- seq(0, max_exponent, length.out = ngrid)
coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

for (i in 1:ngrid){
    exponent <- exponents[i]
    p <- ceiling(n^exponent)
    Xtmp <- X[, 1:p, drop = FALSE]
    if (resid_type %in% c("worst", "hetero")){
        err <- gen_err(Xtmp, resid_type, rho)
        Y1 <- err$err1
        Y0 <- err$err0
    }
    res <- ols_adj_expr(Y1, Y0, Xtmp, nreps = nreps,
                        pi1 = pi1, thresh = thresh)
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
