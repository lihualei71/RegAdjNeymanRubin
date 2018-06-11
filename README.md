# Paper Repository

This repository contains all R code to replicate the results and the figures in our paper: [Regression-adjusted Estimators in Randomized Experiments With A Diverging Number of Covariates](https://arxiv.org/abs/). 

## Introduction of R files 

- `ols_adj.R` gives the function to estimate un-debiased and debiased estimators as well as four types of variance estimators;
- `expr_fun.R` gives the framework of an experiment, which returns the bias ratio, standard deviation inflation ratios, 95% coverage, skewness, kurtosis and p-values from Shapiro-Wilks tests;
- `asym_simul.R`, `synthetic_simul.R`, `real_simul.R` implement three categories of experiments: simulations for asymptotics (Fig. 1, 8, 9), simulations on synthetic datasets (Fig. 2-4, 11-16, 23-31) and simulations on real datasets (Fig. 5-7, 17-22, 32-40). All experiments (Section 5 and Supplementary Material III) should be done in a cluster since our experiments are extensive. The total workload for each category of task is 1250, 750, 1800 CPU hours, respectively. The next section gives the guide of submitting the jobs to a cluster;
- `asym_simul_post.R`, `synthetic_simul_post.R`, `real_simul_post.R` merge and post-process the patches of results from each task; 
- `asym_simul_plot.R`, `synthetic_simul_plot.R`, `real_simul_plot.R`, `lscores_plot.R` make all plots
- `lalonde_prep.R`, `pacman_prep.R`, `star_prep.R` generates the covariate matrices and the potential outcomes from real datasets.

The folder `bash/` contains all files that facilitate job submission to the cluster. 

The folder `data/`
## Replicating the Experiments
