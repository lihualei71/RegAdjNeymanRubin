source("ols_adj.R")
library("tidyverse")
library("moments")

thresh_entries <- function(X, tau = 0.025){
    if (tau == 0){
        return(X)
    }
    apply(X, 2, function(x){
        thresh <- quantile(x, c(tau, 1 - tau))
        x[x < thresh[1]] <- thresh[1]
        x[x > thresh[2]] <- thresh[2]
        return(x)
    })
}

ols_adj_expr <- function(Y1, Y0, X, pi1 = 1 / 2,
                         nreps = 1000,
                         thresh = 0){
    X <- thresh_entries(X, thresh)    
    n <- nrow(X)
    n1 <- floor(n * pi1)
    n0 <- n - n1
    tau <- mean(Y1) - mean(Y0)

    e1 <- resid(lm(Y1 ~ X))
    e0 <- resid(lm(Y0 ~ X))
    sigma <- sqrt(var(e1) / n1 + var(e0) / n0 - var(e1 - e0) / n)

    tauhats <- data.frame(ra = rep(NA, nreps),
                          ra_db = rep(NA, nreps))
    sigmahats <- data.frame(HC0 = rep(NA, nreps),
                            HC1 = rep(NA, nreps),
                            HC2 = rep(NA, nreps),
                            HC3 = rep(NA, nreps))
    
    for (i in 1:nreps){
        T <- sample(n, n1)
        Yobs <- Y0
        Yobs[T] <- Y1[T]
        res <- ols_adj(Yobs, T, X)

        tauhats[i, ] <- res$tauhat[1, ]
        sigmahats[i, ] <- res$sigmahat[1, ]
    }
    truth_sigma <- as.numeric(apply(tauhats, 2, sd))
    sk <- as.numeric(apply(tauhats, 2, skewness))
    sk <- data.frame(tauhat_type = c("ra", "ra_db"),
                     skewness = sk)
    ku <- as.numeric(apply(tauhats, 2, kurtosis))
    ku <- data.frame(tauhat_type = c("ra", "ra_db"),
                     kurtosis = ku)
    sigmahats <- data.frame(
        truth = truth_sigma[1],
        truth_de = truth_sigma[2],
        theoretical = rep(sigma, nreps),
        sigmahats)
    types <- expand.grid(names(tauhats), names(sigmahats))
    tstats <- apply(types, 1, function(type){
        tauhat_type <- as.character(type[1])
        sigmahat_type <- as.character(type[2])
        tauhat <- as.numeric(tauhats[[tauhat_type]])
        sigmahat <- as.numeric(sigmahats[[sigmahat_type]])
        tstat <- (tauhat - tau) / sigmahat
        data.frame(tauhat_type = tauhat_type,
                   sigmahat_type = sigmahat_type,
                   tstat = tstat)
    })
    tstats <- do.call(rbind, tstats)

    coverage <- tstats %>%
        group_by(tauhat_type, sigmahat_type) %>%
        summarize(coverage = mean(abs(tstat) <= qnorm(0.975)))
    bias <- abs(colMeans(tauhats) - tau) / sigma
    bias <- data.frame(tauhat_type = names(bias),
                       bias = as.numeric(bias))
    sdinflate <- (sigmahats[, -1] / sigmahats[, 1]) %>%
        summarize_all(mean) %>%
        gather("sigmahat_type", "sdinflate")
    normality <- tstats %>%
        group_by(tauhat_type, sigmahat_type) %>%
        summarize(shapiro = shapiro.test(tstat)$p.value)
    return(list(coverage = coverage, bias = bias,
                sdinflate = sdinflate, normality = normality,
                skewness = sk, kurtosis = ku))
}

gen_err <- function(X,
                    type = c("worst", "hetero",
                             "normal", "t1", "t2", "t3"),
                    rho = 0){
    type <- type[1]
    n <- nrow(X)    
    if (type == "worst"){
        mod <- lm(hat(X) ~ X)
        err <- as.numeric(resid(mod))
        err <- err / sd(err)
        err <- list(err1 = 2 * err, err0 = err)
    } else {
        func <- switch(type,
                       normal = rnorm,
                       t1 = function(n){rt(n, 1)},
                       t2 = function(n){rt(n, 2)},
                       t3 = function(n){rt(n, 3)})
        err1 <- func(n)
        err0 <- func(n)
        err <- list(err1 = err1,
                    err0 = err1 * rho + err0 * sqrt(1 - rho^2))
    }
    return(err)
}

center_colmeans <- function(x) {
    xcenter <- colMeans(x)
    x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

genX <- function(n, type = c("t1", "t2", "t3", "normal")){
    type <- type[1]
    dat <- switch(type,
                  t1 = rt(n^2, df = 1),
                  t2 = rt(n^2, df = 2),
                  t3 = rt(n^2, df = 3),
                  normal = rnorm(n^2))
    X <- matrix(dat, n, n)
    X <- center_colmeans(X)
    return(X)
}

