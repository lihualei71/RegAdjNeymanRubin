library("dplyr")
library("reshape2")
library("ggplot2")

load("../data/asym_simul_summary.RData")
res <- res %>% filter(!is.na(tauhat)) %>% select(-seed) %>%
    mutate(X = factor(X, levels = c("normal", "t2", "t1")),
           resid = factor(resid, levels = c("normal", "t2", "t1"))) %>%
    group_by(logn, X, resid, exponent) %>%
    summarize(cover = mean(abs(zscore) <= qnorm(0.975))) %>%
    ungroup

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
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 12.5),
          strip.text = element_text(size = 12))

    ggsave(filename = filename, plot, width = 7, height = 4)
}
