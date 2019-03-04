library("dplyr")
library("reshape2")
library("ggplot2")

load("../data/real_simul_summary.RData")

exprs <- expand.grid(data = c("lalonde", "star"))

for (i in 1:length(res)){
    res[[i]] <- res[[i]] %>%
        filter(resid == "worst")
}

## Plots for bias
for (i in 1:nrow(exprs)){
    dataname <- as.character(exprs[i, 1])

    filename_root <- paste0("../figs/real_simul_thresh_", dataname)
    
## Coverage
    plot_coverage <- res$coverage %>%
        filter(dataset == dataname, 
               tauhat_type == "ra_db",
               sigmahat_type %in% c("HC2", "HC3")) %>%
        mutate(tau = factor(tau, levels = c(0, 0.025),
                            labels = c(FALSE, TRUE))) %>%
        select(-dataset, -tauhat_type) %>%
        group_by(resid, tau, sigmahat_type, p) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(sigmahat_type = factor(sigmahat_type, levels = c("HC2", "HC3"), labels = c("HC2", "HC3"))) %>%
        ggplot(aes(x = p, y = coverage,
                   color = tau,
                   linetype = tau)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0.95, alpha = 0.25) +
        facet_grid(~ sigmahat_type) +
        scale_color_discrete(name = "Regularization") + 
        scale_linetype_discrete(name = "Regularization") +
        scale_y_continuous(breaks = c(0, 0.2, 0.5, 0.65, 0.75, 0.85, 0.95, 1), limits = c(0, 1)) +
        xlab("Number of covariates") +
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
