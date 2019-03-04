center_colmeans <- function(x) {
    xcenter <- colMeans(x)
    x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

ols_adj <- function(Yobs, T, X){
    if (is.logical(T)){
        T <- which(T)
    } else if (is.numeric(T) && max(T) == 1 && min(T) == 0){
        T <- which(T == 1)
    }
    n <- length(Yobs)
    n1 <- length(T)
    n0 <- n - n1
    if (is.null(X)){
        tauhat <- mean(Yobs[T]) - mean(Yobs[-T])
        s1 <- sd(Yobs[T])
        s0 <- sd(Yobs[-T])
        sigmahat <- sqrt(s1^2 / n1 + s0^2 / n0 - (s1 - s0)^2 / n)
        return(list(tauhat =
                        data.frame(ra = tauhat, ra_db = tauhat),
                    sigmahat =
                        data.frame(HC0 = sigmahat, HC1 = sigmahat,
                                   HC2 = sigmahat, HC3 = sigmahat)))
    }
    if (!is.matrix(X)){
        X <- as.matrix(X)
    }
    X <- center_colmeans(X)
    p <- ncol(X)
    if (min(n1, n0) <= p){
        stop("Insufficient sample size for OLS")
    }
    tauhat_res <- data.frame(ra = NA, ra_db = NA)
    sigmahat_res <- data.frame(
        HC0 = NA, HC1 = NA, HC2 = NA, HC3 = NA)
    
    mod1 <- lm(Yobs[T] ~ X[T, ])
    mod0 <- lm(Yobs[-T] ~ X[-T, ])
    tauhat_res$ra <- as.numeric(coef(mod1)[1] - coef(mod0)[1])

    e1 <- resid(mod1)
    e0 <- resid(mod0)
    s1_HC0 <- sd(e1)
    s0_HC0 <- sd(e0)
    sigmahat_res$HC0 <- sqrt(s1_HC0^2 / n1 + s0_HC0^2 / n0)

    s1_HC1 <- sd(e1) * sqrt((n1 - 1) / (n1 - p))
    s0_HC1 <- sd(e0) * sqrt((n0 - 1) / (n0 - p))
    sigmahat_res$HC1 <- sqrt(s1_HC1^2 / n1 + s0_HC1^2 / n0)    

    ## Using population leverage scores
    lscores <- hat(X)
    lscores1 <- lscores[T]
    lscores0 <- lscores[-T]

    Delta1 <- sum(lscores1 * e1) / n1
    Delta0 <- sum(lscores0 * e0) / n0
    bias <- -n0 / n1 * Delta1 + n1 / n0 * Delta0
    tauhat_res$ra_db <- tauhat_res$ra - bias

    ## Using sample leverage scores    
    lscores1 <- hat(X[T, ])
    lscores0 <- hat(X[-T, ])

    s1_HC2 <- sqrt(mean(e1^2 / (1 - pmin(lscores1, 0.99))))
    s0_HC2 <- sqrt(mean(e0^2 / (1 - pmin(lscores0, 0.99))))
    sigmahat_res$HC2 <- sqrt(s1_HC2^2 / n1 + s0_HC2^2 / n0)

    s1_HC3 <- sqrt(mean(e1^2 / (1 - pmin(lscores1, 0.99))^2))
    s0_HC3 <- sqrt(mean(e0^2 / (1 - pmin(lscores0, 0.99))^2))
    sigmahat_res$HC3 <- sqrt(s1_HC3^2 / n1 + s0_HC3^2 / n0)
    
    return(list(tauhat = tauhat_res,
                sigmahat = sigmahat_res))
}
