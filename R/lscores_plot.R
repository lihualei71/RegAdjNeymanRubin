## Compute the average kappa for Gaussian random matrices
set.seed(20180610)
n <- 445
p <- 49
m <- 10000
kappa_list <- rep(NA, m)
for (i in 1:m){
    kappa_list[i] <- max(hat(matrix(rnorm(n * p), n)))
}
mean(kappa_list)

## Plot of leverage scores for random matrices

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
          legend.position = "bottom",
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 12.5),
          legend.text = element_text(size = 12.5),
          legend.title = element_text(size = 15))
ggsave(filename = "../figs/lscore.pdf", plot_lscore,
       width = 7, height = 3)
