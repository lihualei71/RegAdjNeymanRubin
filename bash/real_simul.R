source("ols_adj.R")
source("expr_func.R")

data <- as.character(Sys.getenv("data"))
seed <- as.numeric(Sys.getenv("seed"))
resid_type <- as.character(Sys.getenv("residtype"))
thresh <- as.numeric(Sys.getenv("thresh"))

nreps <- 5000
output <- paste0("../data/", data, "_resid", resid_type,
                 "_thresh", thresh,
                 "_seed", seed, ".RData")
filename <- paste0("../data/", data, ".RData")
load(filename)
Y1root <- data$Y1
Y0root <- data$Y0
X <- data$X
p <- ncol(X)
pi1 <- data$pi1

coverage_list <- list()
bias_list <- list()
sdinflate_list <- list()
normality_list <- list()
skewness_list <- list()
kurtosis_list <- list()

set.seed(seed * 2018)
X <- X[, sample(p, p)]
if (resid_type != "worst"){
    err <- gen_err(X, resid_type)
    Y1 <- Y1root + data$sd1 * err$err1
    Y0 <- Y0root + data$sd0 * err$err0
}

if (p <= 49){
    kseq <- 1:p
} else {
    m <- floor(p / 49)
    kseq <- m * (1:49)
}

ind <- 0
for (k in kseq){
    ind <- ind + 1
    Xtmp <- X[, 1:k, drop = FALSE]
    if (resid_type == "worst"){
        err <- gen_err(X, resid_type)
        Y1 <- Y1root + data$sd1 * err$err1
        Y0 <- Y0root + data$sd0 * err$err0
    }
    res <- ols_adj_expr(Y1, Y0, Xtmp, pi1, nreps)
    coverage_list[[ind]] <- data.frame(res$coverage, p = k)
    bias_list[[ind]] <- data.frame(res$bias, p = k)
    sdinflate_list[[ind]] <- data.frame(res$sdinflate, p = k)
    normality_list[[ind]] <- data.frame(res$normality, p = k)
    skewness_list[[ind]] <- data.frame(res$skewness, p = k)
    kurtosis_list[[ind]] <- data.frame(res$kurtosis, p = k)    
    print(k)
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

save(file = output, res)
