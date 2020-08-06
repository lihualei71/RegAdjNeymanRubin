library("tidyverse")
library("ggplot2")

load("../data/real_simul_summary.RData")

exprs <- expand.grid(data = c("lalonde", "star"),
                     rho = c(0, 1))

for (i in 1:length(res)){
    res[[i]] <- res[[i]] %>%
        filter(resid %in% c("normal", "t1", "t2"))
}

#### Without trimming
## Plots for bias
for (i in 1:nrow(exprs)){
    dataname <- as.character(exprs[i, 1])
    rh <- as.numeric(exprs[i, 2])
    
    filename_root <- paste0("../figs/real-simul-", dataname, "-rho", rh)

    ## Bias
    plot_bias <- res$bias %>% 
        filter(dataset == dataname, tau == 0, rho == rh) %>%
        select(-dataset, -tau, -rho) %>%
        mutate(bias = pmin(bias, quantile(bias, 0.98))) %>%
        group_by(p, resid, tauhat_type) %>%
        summarize(bias = median(bias)) %>%
        ungroup() %>%
        mutate(tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased")),
               resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst"))) %>%
        ggplot(aes(x = p, y = bias,
                   color = tauhat_type,
                   linetype = tauhat_type)) +
        geom_line(size = 0.7) +
        facet_grid( ~ resid) +
        xlab("Number of covariates") +
        ylab("Relative Bias") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "-bias.pdf"), plot_bias, width = 7, height = 3)

## Variance
    plot_sdinflate <- res$sdinflate %>%
        filter(dataset == dataname, tau == 0, rho == rh,
               sigmahat_type != "truth_de") %>%
        select(-dataset, -tau, -rho) %>%
        group_by(resid, sigmahat_type, p) %>%
        summarize(sdinflate = median(sdinflate)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               ) %>%
        ggplot(aes(x = p, y = sdinflate,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.6) +
        geom_hline(yintercept = 1, color = "black") +
        facet_grid( ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("cyan4", "magenta", "blue", "red", "darkorange")) +
        scale_linetype_manual(
            name = "type",
            values = c("dashed", "dotdash", "longdash", "solid", "twodash")) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)) + 
        xlab("Number of covariates") +
        ylab("Std. Inflated Ratio") +
        guides(color = guide_legend(nrow = 1),
               linetype = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7, "line"))
    ggsave(filename = paste0(filename_root, "-sdinflate.pdf"), plot_sdinflate, width = 7.5, height = 3.3)
    
## Coverage
    plot_coverage <- res$coverage %>%
        filter(dataset == dataname, tau == 0, rho == rh,
               sigmahat_type %in% c("theoretical", "HC2", "HC3")) %>%
        select(-dataset, -tau, -rho) %>%
        group_by(resid, tauhat_type, sigmahat_type, p) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased")),
               sigmahat_type = factor(sigmahat_type, levels = c("theoretical", "HC2", "HC3"), labels = c("theoretical", "HC2", "HC3"))) %>%
        ggplot(aes(x = p, y = coverage,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0.95, alpha = 0.25) +
        facet_grid(tauhat_type ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("darkorange", "blue", "red")) +
        scale_linetype_manual(
            name = "type",
            values = c("twodash", "longdash", "solid")) +
        scale_y_continuous(breaks = c(0, 0.2, 0.5, 0.65, 0.75, 0.85, 0.95, 1), limits = c(0.5, 1)) +
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
    ggsave(filename = paste0(filename_root, "-coverage.pdf"), plot_coverage, width = 6, height = 4.4)
}

#### With trimming
## Plots for bias
for (i in 1:nrow(exprs)){
    dataname <- as.character(exprs[i, 1])
    rh <- as.numeric(exprs[i, 2])
    
    filename_root <- paste0("../figs/real-simul-trim-", dataname, "-rho", rh)

    ## Bias
    plot_bias <- res$bias %>% 
        filter(dataset == dataname, tau == 0.025, rho == rh) %>%
        select(-dataset, -tau, -rho) %>%
        mutate(bias = pmin(bias, quantile(bias, 0.98))) %>%
        group_by(p, resid, tauhat_type) %>%
        summarize(bias = median(bias)) %>%
        ungroup() %>%
        mutate(tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased")),
               resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst"))) %>%
        ggplot(aes(x = p, y = bias,
                   color = tauhat_type,
                   linetype = tauhat_type)) +
        geom_line(size = 0.7) +
        facet_grid( ~ resid) +
        xlab("Number of covariates") +
        ylab("Relative Bias") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "-bias.pdf"), plot_bias, width = 7, height = 3)

## Variance
    plot_sdinflate <- res$sdinflate %>%
        filter(dataset == dataname, tau == 0.025, rho == rh,
               sigmahat_type != "truth_de") %>%
        select(-dataset, -tau, -rho) %>%
        group_by(resid, sigmahat_type, p) %>%
        summarize(sdinflate = median(sdinflate)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               ) %>%
        ggplot(aes(x = p, y = sdinflate,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.6) +
        geom_hline(yintercept = 1, color = "black") +
        facet_grid( ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("cyan4", "magenta", "blue", "red", "darkorange")) +
        scale_linetype_manual(
            name = "type",
            values = c("dashed", "dotdash", "longdash", "solid", "twodash")) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)) + 
        xlab("Number of covariates") +
        ylab("Std. Inflated Ratio") +
        guides(color = guide_legend(nrow = 1),
               linetype = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7, "line"))
    ggsave(filename = paste0(filename_root, "-sdinflate.pdf"), plot_sdinflate, width = 7.5, height = 3.3)
    
## Coverage
    plot_coverage <- res$coverage %>%
        filter(dataset == dataname, tau == 0.025, rho == rh,
               sigmahat_type %in% c("theoretical", "HC2", "HC3")) %>%
        select(-dataset, -tau, -rho) %>%
        group_by(resid, tauhat_type, sigmahat_type, p) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db"), labels = c("un-debiased", "debiased")),
               sigmahat_type = factor(sigmahat_type, levels = c("theoretical", "HC2", "HC3"), labels = c("theoretical", "HC2", "HC3"))) %>%
        ggplot(aes(x = p, y = coverage,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0.95, alpha = 0.25) +
        facet_grid(tauhat_type ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("darkorange", "blue", "red")) +
        scale_linetype_manual(
            name = "type",
            values = c("twodash", "longdash", "solid")) +
        scale_y_continuous(breaks = c(0, 0.2, 0.5, 0.65, 0.75, 0.85, 0.95, 1), limits = c(0.5, 1)) +
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
    ggsave(filename = paste0(filename_root, "-coverage.pdf"), plot_coverage, width = 6, height = 4.4)
}
