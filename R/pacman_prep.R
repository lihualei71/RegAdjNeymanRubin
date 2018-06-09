library("dplyr")
select <- dplyr::select

data <- read.csv("../data/pacman.csv",
                 stringsAsFactors=FALSE) %>%
    filter(inrct == 1) %>%
    select(-ahsurv, -tc, -nb20, -nb30,
           -contains("miss"), -contains("cat"),
           -rate, -shanew,
           -bookletnumber, -unit, -icnno, -inrct,
           -innrsoverlapcent, -centinboth,
           -daicu, -timemonths, -timecat, -year,
           -system, -admtype, -inttemprct,
           -med, -inttemprct7, -inttemprct12,
           -IMprobcheck, -ka, -r, -kr,
           -infect24rct, -labinfect, -susinfect,
           -AP2probUK,
           -AP2aps, -benewrct3, -AP2score, -IMprobrctnew
           ) %>%
    mutate(sex = ifelse(sex == "F", 1, 0))

data1 <- filter(data, pac == 1) %>% select(-pac)
data0 <- filter(data, pac == 0) %>% select(-pac)
y1 <- data1$new_qals
y0 <- data0$new_qals
n1 <- nrow(data1)
n0 <- nrow(data0)
n <- n1 + n0

X <- rbind(data1, data0) %>% select(-new_qals)
formula <- paste(names(X), collapse = "+")
formula <- paste0("~ . + (agem + sex) * (", formula, ")")
X <- model.matrix(as.formula(formula), data = X)[, -1]

## Preparation to simulate potential outcomes
treat <- ifelse(1:n <= n1, TRUE, FALSE)
Yobs <- c(y1, y0)
data <- cbind(data.frame(Y = Yobs), as.data.frame(X))

mod1 <- lm(Y ~ ., data = data, subset = treat)
sd1 <- sd(resid(mod1)) / 2
Y1root <- predict(mod1, newdata = data) %>% as.numeric
mod0 <- lm(Y ~ ., data = data, subset = !treat)
sd0 <- sd(resid(mod0)) / 2
Y0root <- predict(mod0, newdata = data) %>% as.numeric

data <- list(Y1 = Y1root, Y0 = Y0root, X = X, pi1 = n1 / n,
             sd1 = sd1, sd0 = sd0)
save(file = "../data/pacman_simul.RData", data)
