source("expr_func.R")

seed <- as.numeric(Sys.getenv("seed"))
logn <- as.numeric(Sys.getenv("logn"))
nreps <- as.numeric(Sys.getenv("nreps"))
X_type <- as.character(Sys.getenv("Xtype"))
resid_type <- as.character(Sys.getenv("residtype"))

filename <- paste0("../data/asym_simul_logn", logn,
                   "_seed", seed, "_nreps", nreps,
                   "_", X_type, "_resid", resid_type, ".RData")

ols_asd <- function(Y1, Y0, X, pi1 = 0.5){
    n <- nrow(X)
    mod1 <- lm(Y1 ~ X)
    mod0 <- lm(Y0 ~ X)
    e1 <- resid(mod1)
    e0 <- resid(mod0)
    var1 <- var(e1)
    var0 <- var(e0)
    vartau <- var(e1 - e0)
    asd <- sqrt((var1 / pi1 + var0 / (1 - pi1) - vartau) / n)
    return(asd)
}

ols_ATE_exp <- function(Y1, Y0, X,
                        n1 = floor(nrow(X) / 2),
                        nreps = 100){
    n <- nrow(X)
    tau <- mean(Y1) - mean(Y0)
    res <- rep(0, nreps)
    for (i in 1:nreps){
        T <- sample(n, n1)
        mod1 <- lm(Y1[T] ~ X[T, ])
        mod0 <- lm(Y0[-T] ~ X[-T, ])
        res[i] <- as.numeric(coef(mod1)[1] - coef(mod0)[1])
    }
    return(res)
}

set.seed(8192)
n <- 2^logn
ngrid <- 60
X <- genX(n, X_type)
beta <- rep(0, ncol(X))

err <- gen_err(X, resid_type)
Y1 <- err$err1
Y1 <- Y1 - mean(Y1)
Y0 <- err$err0
Y0 <- Y0 - mean(Y0)

max_exponent <- min(5 / 6, (logn - 2) / logn)
exponents <- seq(0, max_exponent, length.out = ngrid + 1)
tauhats <- matrix(NA, ngrid + 1, nreps)
asds <- rep(0, ngrid + 1)

set.seed(seed)
for (i in 1:(ngrid + 1)){
    exponent <- exponents[i]
    p <- ceiling(n^exponent)
    Xtmp <- X[, 1:p, drop = FALSE]
    tauhats[i, ] <- ols_ATE_exp(Y1, Y0, Xtmp, nreps = nreps)
    asds[i] <- ols_asd(Y1, Y0, Xtmp)

    res <- list(logn = logn, seed = seed, type = X_type,
                nreps = nreps, ngrid = ngrid, resid = resid_type,
                asds = asds, tauhats = tauhats)
    print(i)
    save(file = filename, res)
}
