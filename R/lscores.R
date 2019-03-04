## Compute the average kappa for Gaussian random matrices
n <- 2000
p <- ceiling(n^(2 / 3))
m <- 50
kappa_list <- rep(NA, m)
kappa_thresh_list <- rep(NA, m)
for (i in 1:m){
    set.seed(i)
    X <- matrix(rt(n * p, df = 2), n)
    kappa_list[i] <- max(hat(X))
    threshX <- apply(X, 2, function(x){
        thresh <- quantile(x, c(0.025, 0.975))
        x[x <= thresh[1]] <- thresh[1]
        x[x >= thresh[2]] <- thresh[2]
    })
    kappa_thresh_list[i] <- max(hat(threshX))
}

mean(kappa_list)
sd(kappa_list)
mean(kappa_thresh_list)
sd(kappa_thresh_list)
