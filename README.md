# Paper Repository

This repository contains all R code to replicate the results and the figures in our paper: [Regression-adjusted Estimators in Randomized Experiments With A Diverging Number of Covariates](https://arxiv.org/abs/1806.07585). 

## Introduction of R files 
The folder `R/` contains all R files: 

- `ols_adj.R` gives the function to estimate un-debiased and debiased estimators as well as four types of variance estimators;
- `expr_fun.R` gives the framework of an experiment, which returns the bias ratio, standard deviation inflation ratios, 95% coverage, skewness, kurtosis and p-values from Shapiro-Wilks tests;
- `synthetic_simul.R`, `real_simul.R` implement the experiments including simulations on synthetic datasets. All experiments (Section 4 and Supplementary Material III) should be done in a cluster since our experiments are extensive. The total workload for each category of task is 3600, 2000 CPU hours, respectively. The next section gives the guide of submitting the jobs to a cluster;
- `synthetic_simul_post.R`, `real_simul_post.R` merge and post-process the patches of results from each task; 
- `synthetic_simul_plot.R` and `synthetic_simul_plot_worst.R` generate Fig. 1-2, S1 - S15;
- `real_simul_plot.R` and `real_simul_plot_worst.R`generate Fig. S16 - S19;
- `lalonde_prep.R`, `star_prep.R` generates the covariate matrices and the potential outcomes from real datasets.


## Replicating the Experiments
The folder `bash/` contains all files that facilitate job submission to the cluster. 

- `synthetic_simul_jobs.sh`, `real_simul_jobs.sh` generates separate jobs to each node in the cluster. The number of jobs are 600 and 400, respectively, which equal to the number of rows in `params_synthetic_simul.txt` and `params_real_simul.txt`. Our cluster takes job submission via `sbatch` command. For other system, one can replace sbatch in each `.sh` file by other appropriate command. 

- `params_synthetic_simul.txt`, `params_real_simul.txt` gives all experimental settings. 

- Before running the experiments, make sure running the following code to generate the folders `results/`, `log/` and `cluster_raw_data/`.

```
mkdir results log cluster_raw_data
```

## Plots 
For those who are not willing to replicate all results, we provide the merged results `synthetic_simul_summary.RData` and `real_simul_summary.RData` in the folder `data/`, which are ready for making all plots. The figures will be generated in the folder `figs/` by running the following commands

```
mkdir figs
cd R
R CMD BATCH --no-save synthetic_simul_post.R
R CMD BATCH --no-save synthetic_simul_plot.R
R CMD BATCH --no-save synthetic_simul_plot_worst.R
R CMD BATCH --no-save real_simul_post.R
R CMD BATCH --no-save real_simul_plot.R
R CMD BATCH --no-save real_simul_plot_worst.R
cd ..
```

Please contact lihualei@stanford.edu for further questions.

