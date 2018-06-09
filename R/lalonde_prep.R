library("dplyr")
select <- dplyr::select

library("Matching")
data(lalonde)
set.seed(1)

Yobs <- lalonde$re78
T <- lalonde$treat
X <- lalonde %>% select(-re78, -treat)
X <- model.matrix(~ .^2, data = X) %>% as.data.frame %>%
    select(-`(Intercept)`, -`black:hisp`,
           -`re74:u74`, -`re75:u75`,
           -`re74:re75`, -`re74:u75`,
           -`re75:u74`) %>% as.matrix

## Preparation to simulate potential outcomes
treat <- as.logical(T)
Y1 <- Y0 <- Yobs
n1 <- sum(treat)
n0 <- sum(!treat)
n <- n1 + n0
Y1[!treat] <- NA
Y0[treat] <- NA
data <- cbind(data.frame(Y = Yobs), as.data.frame(X))

mod1 <- lm(Y ~ ., data = data, subset = treat)
sd1 <- sd(resid(mod1)) / 2
Y1root <- predict(mod1, newdata = data) %>% as.numeric

mod0 <- lm(Y ~ ., data = data, subset = !treat)
sd0 <- sd(resid(mod0)) / 2
Y0root <- predict(mod0, newdata = data) %>% as.numeric

data <- list(Y1 = Y1root, Y0 = Y0root, X = X, pi1 = n1 / n,
             sd1 = sd1, sd0 = sd0)
save(file = "../data/lalonde_simul.RData", data)
