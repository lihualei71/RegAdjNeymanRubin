library("dplyr")
library("reshape2")
library("ggplot2")

load("../data/real_simul_summary.RData")

## Plots for bias
for (dataname in c("lalonde", "pacman", "star")){
    filename_root <- paste0("../figs/real_simul_", dataname)

    ## Bias
    plot_bias <- res$bias %>% 
        filter(dataset == dataname) %>%
        select(-dataset) %>%
        mutate(bias = pmin(bias, quantile(bias, 0.98))) %>%
        dcast(p + seed + resid ~ tauhat_type,
              value.var = "bias") %>%
        mutate(ratio_db = ra_db / ra,
               ratio_db_emp = ra_db_emp / ra) %>%
        group_by(p, resid) %>%
        summarize(db_med = median(ratio_db),
                  db_emp_med = median(ratio_db_emp)) %>%
        rename(pop. = db_med, samp. = db_emp_med) %>%
        melt(id.vars = c("p", "resid"), variable.name = "type") %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst"))) %>%
        ggplot(aes(x = p, y = value, color = type,
                   linetype = type)) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 1, color = "black") +
        facet_grid( ~ resid) +
        xlab("Number of covariates") +
        ylab("Ratio of Bias") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "_bias.pdf"), plot_bias, width = 8, height = 3)

## Variance
    plot_sdinflate <- res$sdinflate %>%
        filter(dataset == dataname,
               !sigmahat_type %in%
               c("truth_de", "truth_de_emp")) %>%
        select(-dataset) %>%
        group_by(resid, sigmahat_type, p) %>%
        summarize(sdinflate = median(sdinflate)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               sigmahat_type = factor(sigmahat_type, levels = c("theoretical", "HC0", "HC1", "HC2", "HC3", "HC2_emp", "HC3_emp"), labels = c("theoretical", "HC0", "HC1", "HC2 (pop.)", "HC3 (pop.)", "HC2 (samp.)", "HC3 (samp.)"))) %>%
        ggplot(aes(x = p, y = sdinflate,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.6) +
        geom_hline(yintercept = 1, color = "black") +
        facet_grid( ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("darkorange", 11, 8, 13, "blue", "magenta", "red")) +
        scale_linetype_manual(
            name = "type",
            values = c("twodash", "dashed", "dotted", "dotted", "longdash", "dotdash", "solid")) +
        scale_y_continuous(breaks = c(0, 0.5, 1, 1.5)) +
        xlab("Number of covariates") +
        ylab("Std. Inflated Ratio") +
        guides(color = guide_legend(nrow = 2),
               linetype = guide_legend(nrow = 2)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7,"line"))
    ggsave(filename = paste0(filename_root, "_sdinflate.pdf"), plot_sdinflate, width = 8, height = 3)
    
## Coverage
    plot_coverage <- res$coverage %>%
        filter(dataset == dataname,
               sigmahat_type %in% c("truth", "theoretical", "HC3", "HC3_emp")) %>%
        select(-dataset) %>%
        group_by(resid, tauhat_type, sigmahat_type, p) %>%
        summarize(coverage = median(coverage)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db", "ra_db_emp"), labels = c("un-debiased", "debiased (pop.)", "debiased (samp.)")),
               sigmahat_type = factor(sigmahat_type, levels = c("truth", "theoretical", "HC3", "HC3_emp"), labels = c("truth", "theoretical", "HC3 (pop.)", "HC3 (samp.)"))) %>%
        ggplot(aes(x = p, y = coverage,
                   color = sigmahat_type,
                   linetype = sigmahat_type)) +
        geom_line(size = 0.6) +
        geom_hline(yintercept = 0.95, color = "black") +
        facet_grid(tauhat_type ~ resid) +
        scale_color_manual(
            name = "type",
            values = c("cyan3", "darkorange", "blue", "red")) +
        scale_linetype_manual(
            name = "type",
            values = c("dotdash", "twodash", "longdash", "solid")) +
        xlab("Number of covariates") +
        ylab("Coverage") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15),
              legend.key.width = unit(2.7,"line"))
    ggsave(filename = paste0(filename_root, "_coverage.pdf"), plot_coverage, width = 8, height = 6.6)


## Normality
    plot_normality <- res$normality %>%
        filter(sigmahat_type == "truth",
               dataset == dataname) %>%
        select(-sigmahat_type, -dataset) %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db", "ra_db_emp"), labels = c("un-debiased", "debiased (pop.)", "debiased (samp.)"))) %>%
        group_by(resid, tauhat_type, p) %>%
        summarize(med = median(shapiro),
                  low = quantile(shapiro, 0.25),
                  high = quantile(shapiro, 0.75)) %>%
        ggplot(aes(x = p, y = med)) +
        ## geom_smooth(method = "loess") +
        geom_line(size = 0.7, color = "blue") +
        geom_ribbon(aes(x = p, ymin = low, ymax = high),
                    alpha = 0.25) +
        facet_grid(tauhat_type ~ resid) +
        scale_color_discrete(name = "type") +
        scale_linetype_discrete(name = "type") +
        xlab("Number of covariates") +
        ylab("P-value of Shapiro-Wilks Test") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "_normality.pdf"), plot_normality, width = 8, height = 6)

    ## Skewness
    plot_skewness <- res$skewness %>%
        filter(dataset == dataname) %>%
        select(-dataset) %>%
        group_by(resid, tauhat_type, p) %>%
        summarize(med = median(skewness),
                  low = quantile(skewness, 0.25),
                  high = quantile(skewness, 0.75)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db", "ra_db_emp"), labels = c("un-debiased", "debiased (pop.)", "debiased (samp.)"))) %>%
        ggplot(aes(x = p, y = med)) +
        geom_line(size = 0.7, color = "blue") +
        geom_ribbon(aes(x = p, ymin = low, ymax = high),
                    alpha = 0.25) +
        geom_hline(yintercept = 0, color = "red") +
        facet_grid(tauhat_type ~ resid) +
        scale_color_discrete(name = "type") +
        scale_linetype_discrete(name = "type") +
        xlab("Number of covariates") +
        ylab("Skewness") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "_skewness.pdf"), plot_skewness, width = 8, height = 6.6)
               
    ## Kurtosis
    plot_kurtosis <- res$kurtosis %>%
        filter(dataset == dataname) %>%
        select(-dataset) %>%
        group_by(resid, tauhat_type, p) %>%
        summarize(med = median(kurtosis),
                  low = quantile(kurtosis, 0.25),
                  high = quantile(kurtosis, 0.75)) %>%
        ungroup() %>%
        mutate(resid = factor(resid, levels = c("normal", "t2", "t1", "worst"), labels = c("normal", "t(2)", "Cauchy", "worst")),
               tauhat_type = factor(tauhat_type, levels = c("ra", "ra_db", "ra_db_emp"), labels = c("un-debiased", "debiased (pop.)", "debiased (samp.)"))) %>%
        ggplot(aes(x = p, y = med)) +
        geom_line(size = 0.7, color = "blue") +
        geom_ribbon(aes(x = p, ymin = low, ymax = high),
                    alpha = 0.25) +
        geom_hline(yintercept = 3, color = "red") +
        facet_grid(tauhat_type ~ resid) +
        scale_color_discrete(name = "type") +
        scale_linetype_discrete(name = "type") +
        xlab("Number of covariates") +
        ylab("Kurtosis") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 15),
              strip.text = element_text(size = 12.5),
              legend.text = element_text(size = 12.5),
              legend.title = element_text(size = 15))
    ggsave(filename = paste0(filename_root, "_kurtosis.pdf"), plot_kurtosis, width = 8, height = 6.6)
}
