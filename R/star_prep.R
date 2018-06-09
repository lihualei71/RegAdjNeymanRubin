library("dplyr")
select <- dplyr::select
set.seed(1)

## star data
star <- read.csv("../data/star_data.csv") %>% 
    select(GPA_year1, control, sfsp, female, gpa0, age, mtongue, hcom, chooseUTM, work1, mom1, mom2, dad1, dad2, lm_rarely, lm_never, graddeg, finish4) %>%
    na.omit %>%        
    filter(control + sfsp == 1) %>%
    select(-control) %>%
    mutate(mtongue = as.numeric(mtongue == "English"))

Yobs <- star$GPA_year1
T <- star$sfsp
X <- star %>% select(-GPA_year1, -sfsp)
formula <- paste(names(X), collapse = "+")
formula <- paste0("~ . + (age + female + gpa0) * (", formula, ")")
X <- model.matrix(as.formula(formula), data = X)[, -1] %>%
    as.data.frame %>%
    select(-`female:lm_never`) %>%
    as.matrix

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
save(file = "../data/star_simul.RData", data)
