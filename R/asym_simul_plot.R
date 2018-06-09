library("dplyr")
library("ggplot2")

load("../data/asym_simul_summary.RData")
res <- res %>% filter(!is.na(tauhat)) %>% select(-seed) %>%
    mutate(X = factor(X, levels = c("normal", "t2", "t1")),
           resid = factor(resid, levels = c("normal", "t2", "t1"))) %>%
    group_by(logn, X, resid, exponent) %>%
    summarize(cover = mean(abs(zscore) <= qnorm(0.975))) %>%
    ungroup

## Illustration
plot <- res %>%
    filter(X == "normal", resid == "normal") %>%
    select(-X, -resid) %>%
    group_by(logn, exponent) %>%
    summarize(cover = mean(cover)) %>%
    ggplot(aes(x = exponent, y = cover)) +
    geom_line() +
    scale_y_continuous(limits = c(0.75, 1)) +
    scale_x_continuous(breaks = c(0, 0.66)) +
    geom_hline(yintercept = 0.95, col = "red") + 
    facet_grid( ~ logn) +
    xlab("Exponent (log p / log n)") +
        ylab("Coverage") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

ggsave(filename = "../figs/illustration.pdf", plot, width = 7, height = 2)

## Coverage
for (X_type in c("normal", "t2", "t1")){
    if (X_type %in% c("normal", "t2")){
        ylimits <- c(0.7, 1)
        xbreaks <- c(0, 0.66)
    } else if (X_type == "t1"){
        ylimits <- c(0, 1)
        xbreaks <- c(0, 0.5)
    }
    filename <- paste0("../figs/asym_simul_X_", X_type, ".pdf")
    plot <- res %>%
    filter(X == X_type) %>%
    select(-X) %>%
    group_by(resid, logn, exponent) %>%
    summarize(cover = mean(cover)) %>%
    ungroup() %>%
    mutate(logn = factor(logn, levels = 9:13,
               labels = paste0("n = 2^", 9:13)),
           resid = factor(resid, levels = c("normal", "t2", "t1"), labels = c("normal", "t(2)", "Cauchy"))) %>%
    ggplot(aes(x = exponent, y = cover)) +
    geom_line() +
    scale_y_continuous(limits = ylimits) +
    scale_x_continuous(breaks = xbreaks) +
    geom_hline(yintercept = 0.95, col = "red") + 
    facet_grid(resid ~ logn) +
    xlab("Exponent (log p / log n)") +
        ylab("Coverage") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

    ggsave(filename = filename, plot, width = 7, height = 4)
}

## Plot of leverage scores
n <- 2^12
set.seed(20180606)

res <- list()
ind <- 0
for (exponent in c(0.66, 0.5)){
    for (i in 1:100){
        ind <- ind + 1
        p <- 2^(ceiling(12 * exponent))
        lscore_normal <-
            hat(matrix(rnorm(n * p), nrow = n)) %>%
                sort(decreasing = TRUE)
        lscore_t2 <-
            hat(matrix(rt(n * p, df = 2), nrow = n)) %>%
                sort(decreasing = TRUE)
        lscore_t1 <-
            hat(matrix(rt(n * p, df = 1), nrow = n)) %>%
                sort(decreasing = TRUE)
        res[[ind]] <- data.frame(
            ind = 1:n,
            exponent = exponent, 
            normal = lscore_normal,
            t2 = lscore_t2,
            t1 = lscore_t1)
        print(i)
    }
}
lscore_df <- do.call(rbind, res)

plot_lscore <- lscore_df %>% melt(id.vars = c("ind", "exponent"),
                   variable.name = "distribution") %>%
    group_by(ind, exponent, distribution) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    mutate(distribution = factor(distribution, levels = c("normal", "t2", "t1"), labels = c("normal", "t(2)", "Cauchy")),
           exponent = factor(exponent, levels = c("0.5", "0.66"), labels = paste0("log p / log n = ", c(0.5, 0.66)))) %>%
    ggplot(aes(x = ind, y = value, color = distribution,
               linetype = distribution)) +
    geom_line(size = 0.7) +
    facet_grid(~ exponent) +
    xlab("Index") +
    ylab("Leverage scores") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom")
ggsave(filename = "../figs/lscore.pdf", plot_lscore,
       width = 7, height = 3)
