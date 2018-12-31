library("dplyr")
library("reshape2")
library("ggplot2")

load("../data/synthetic_simul_summary.RData")

exprs <- expand.grid(Xtype = c("normal", "t1", "t2"),
                     pi1 = c(0.2, 0.5))

for (i in 1:length(res)){
    res[[i]] <- res[[i]] %>%
        filter(resid == "worst")
}

## Plots for bias
for (i in 1:nrow(exprs)){
    Xtype <- as.character(exprs[i, 1])
    pi1 <- as.numeric(exprs[i, 2])
    
    filename_root <- paste0("../figs/synthetic_simul_debiase_", Xtype, "_pi", pi1)

    ## Bias
    plot_bias <- res$bias %>% 
        filter(X == Xtype, tau == 0, pi == pi1) %>%
        mutate(tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased"))) %>%
        select(-X, -resid, -tau, -pi, -tau) %>%
        mutate(bias = pmin(bias, quantile(bias, 0.98))) %>%
        group_by(exponent, tauhat_type) %>%
        summarize(bias = median(bias)) %>%
        ggplot(aes(x = exponent, y = bias, color = tauhat_type)) +
        geom_line(size = 0.7) +
        xlab("Exponent (log p / log n)") +
        ylab("Relative Bias") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "_bias.pdf"), plot_bias, width = 4.5, height = 4.5)
    
## Coverage
    plot_coverage <- res$coverage %>%
        filter(X == Xtype, tau == 0, pi == pi1,
               sigmahat_type %in% c("HC2", "HC3")) %>%
        select(-X, -resid, -tau, -pi) %>%
        group_by(tauhat_type, sigmahat_type, exponent) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased")),
               sigmahat_type = factor(sigmahat_type, levels = c("HC2", "HC3"), labels = c("HC2", "HC3"))) %>%
        ggplot(aes(x = exponent, y = coverage,
                   color = tauhat_type,
                   linetype = tauhat_type)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0.95, alpha = 0.25) +
        facet_grid(~ sigmahat_type) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.65, 0.75, 0.85, 0.95, 1), limits = c(0, 1)) +
        xlab("Exponent (log p / log n)") +
        ylab("Coverage") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7, "line"))
    ggsave(filename = paste0(filename_root, "_coverage.pdf"), plot_coverage, width = 5, height = 3.5)
}
