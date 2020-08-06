library("tidyverse")
library("ggplot2")

load("../data/real_simul_summary.RData")

exprs <- expand.grid(data = c("lalonde", "star"),
                     rho = c(0, 1))

for (i in 1:length(res)){
    res[[i]] <- res[[i]] %>%
        filter(resid == "worst")
}

## Plots for bias
for (i in 1:nrow(exprs)){
    dataname <- as.character(exprs[i, 1])
    rh <- as.numeric(exprs[i, 2])
    
    filename_root <- paste0("../figs/real-simul-worst-", dataname, "-rho", rh)

    ## Bias
    plot_bias <- res$bias %>% 
        filter(dataset == dataname, rho == rh) %>%
        unite(tauhat, c("tauhat_type", "tau"), remove = FALSE) %>%
        filter(tauhat != "ra_0.025") %>%
        mutate(tauhat = recode(tauhat,
                               `ra_0` = "un-debiased",
                               `ra_db_0` = "debiased",
                               `ra_db_0.025` = "debiased (trimming)"),
               tauhat = factor(tauhat,
                               levels = c("un-debiased", "debiased", "debiased (trimming)"))) %>%
        select(-dataset, -resid, -tauhat_type, -rho, -tau) %>%
        mutate(bias = pmin(bias, quantile(bias, 0.98))) %>%
        group_by(p, tauhat) %>%
        summarize(bias = median(bias)) %>%
        ggplot(aes(x = p, y = bias,
                   color = tauhat,
                   linetype = tauhat)) +
        geom_line(size = 0.7) +
        xlab("Number of covariates") +
        ylab("Relative Bias") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 17.5),
              axis.text = element_text(size = 12.5),
              strip.text = element_text(size = 15),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "-bias.pdf"), plot_bias, width = 6, height = 5)

    ## Coverage
    plot_coverage <- res$coverage %>%
        filter(dataset == dataname, rho == rh,
               sigmahat_type %in% c("HC2", "HC3")) %>%
        unite(tauhat, c("tauhat_type", "tau"), remove = FALSE) %>%
        filter(tauhat != "ra_0.025") %>%
        mutate(tauhat = recode(tauhat,
                               `ra_0` = "un-debiased",
                               `ra_db_0` = "debiased",
                               `ra_db_0.025` = "debiased (trimming)"),
               tauhat = factor(tauhat,
                               levels = c("un-debiased", "debiased", "debiased (trimming)"))) %>%
        select(-dataset, -resid, -tauhat_type, -rho, -tau) %>%
        group_by(tauhat, sigmahat_type, p) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(sigmahat_type = factor(sigmahat_type, levels = c("HC2", "HC3"), labels = c("HC2", "HC3"))) %>%
        ggplot(aes(x = p, y = coverage,
                   color = tauhat,
                   linetype = tauhat)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0.95, alpha = 0.25) +
        facet_grid(~ sigmahat_type) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.65, 0.75, 0.85, 0.95, 1), limits = c(0, 1)) +
        xlab("Number of covariates") +
        ylab("Coverage") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 17.5),
              axis.text = element_text(size = 12.5),
              strip.text = element_text(size = 15),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7, "line"))
    ggsave(filename = paste0(filename_root, "-coverage.pdf"), plot_coverage, width = 7, height = 4)
}
