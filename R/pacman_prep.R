
R version 3.3.3 (2017-03-06) -- "Another Canoe"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin13.4.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> library("dplyr")

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> select <- dplyr::select
> 
> library("Matching")
Loading required package: MASS

Attaching package: ‘MASS’

The following object is masked _by_ ‘.GlobalEnv’:

    select

The following object is masked from ‘package:dplyr’:

    select

## 
##  Matching (Version 4.9-2, Build Date: 2015-12-25)
##  See http://sekhon.berkeley.edu/matching for additional documentation.
##  Please cite software as:
##   Jasjeet S. Sekhon. 2011. ``Multivariate and Propensity Score Matching
##   Software with Automated Balance Optimization: The Matching package for R.''
##   Journal of Statistical Software, 42(7): 1-52. 
##

> data(lalonde)
> set.seed(1)
> 
> Yobs <- lalonde$re78
> T <- lalonde$treat
> X <- lalonde %>% select(-re78, -treat)
> X <- model.matrix(~ .^2, data = X) %>% as.data.frame %>%
+     select(-`(Intercept)`, -`black:hisp`,
+            -`re74:u74`, -`re75:u75`,
+            -`re74:re75`, -`re74:u75`,
+            -`re75:u74`) %>% as.matrix
> 
> ## Preparation to simulate potential outcomes
> treat <- as.logical(T)
> Y1 <- Y0 <- Yobs
> n1 <- sum(treat)
> n0 <- sum(!treat)
> n <- n1 + n0
> Y1[!treat] <- NA
> Y0[treat] <- NA
> data <- cbind(data.frame(Y = Yobs), as.data.frame(X))
> 
> mod1 <- lm(Y ~ ., data = data, subset = treat)
> sd1 <- sd(resid(mod1)) / 2
> Y1root <- predict(mod1, newdata = data) %>% as.numeric
> 
> mod0 <- lm(Y ~ ., data = data, subset = !treat)
> sd0 <- sd(resid(mod0)) / 2
> Y0root <- predict(mod0, newdata = data) %>% as.numeric
> 
> data <- list(Y1 = Y1root, Y0 = Y0root, X = X, pi1 = n1 / n,
+              sd1 = sd1, sd0 = sd0)
> save(file = "../data/lalonde_simul.RData", data)
> 
> proc.time()
   user  system elapsed 
  0.630   0.073   0.786 
