# Paper Repository

This repository contains all R code to replicate the results and the figures in our paper: [Regression-adjusted Estimators in Randomized Experiments With A Diverging Number of Covariates](https://arxiv.org/abs/). 

## Introduction of R files 
The folder `R/` contains all R files: 

- `ols_adj.R` gives the function to estimate un-debiased and debiased estimators as well as four types of variance estimators;
- `expr_fun.R` gives the framework of an experiment, which returns the bias ratio, standard deviation inflation ratios, 95% coverage, skewness, kurtosis and p-values from Shapiro-Wilks tests;
- `synthetic_simul.R`, `real_simul.R` implement the experiments including simulations on synthetic datasets (Fig. 1-4, S1-S4) and simulations on real datasets (Fig. S5-S8). All experiments (Section 4 and Supplementary Material III) should be done in a cluster since our experiments are extensive. The total workload for each category of task is 1800, 1000 CPU hours, respectively. The next section gives the guide of submitting the jobs to a cluster;
- `synthetic_simul_post.R`, `real_simul_post.R` merge and post-process the patches of results from each task; 
- `synthetic_simul_plot.R` generates Fig. 1-2, S1, S3; `synthetic_simul_plot_debiase.R` generates Fig. 3, S2 (upper), S4 (upper); `synthetic_simul_plot_kappa.R` generates Fig. 4, S2 (lower), S4 (lower);
- `real_simul_plot.R` generates Fig. S5, S7; `real_simul_plot_debiase.R` generates Fig. S6 (upper), S8 (upper); `real_simul_plot_kappa.R` generates Fig. S6 (lower), S8 (lower);
- `lalonde_prep.R`, `star_prep.R` generates the covariate matrices and the potential outcomes from real datasets.


## Replicating the Experiments
The folder `bash/` contains all files that facilitate job submission to the cluster. 

- `gen_jobs_synthetic_simul.sh`, `gen_jobs_real_simul.sh` generates separate jobs to each node in the cluster. The number of jobs are 1600 and 800, respectively, which equal to the number of rows in `params_synthetic_simul.txt` and `params_real_simul.txt`. Our cluster takes job submission via `sbatch` command. For other system, one can replace sbatch in each `.sh` file by other appropriate command. 

- `params_synthetic_simul.txt`, `params_real_simul.txt` gives all experimental settings. 

- Each job will generate a `.sh` file and a `.Rout` file into the folder `results/`. One can monitor the process by checking `.Rout` file.

- Each job will save output as a `.RData` file into the folder `data/`. `synthetic_simul_post.R`, `real_simul_post.R` will marge them.

- Before running the experiments, make sure running the following code to generate the folder `results/`

```
mkdir results
```

## Plots 
For those who are not willing to replicate all results, we provide the merged results `asym_simul_summary.RData`, `synthetic_simul_summary.RData` and `real_simul_summary.RData` in the folder `data/`, which are ready for making all plots. The figures will be generated in the folder `figs/` by running the following commands

```
mkdir figs
cd R
R CMD BATCH --no-save lalonde_prep.R
R CMD BATCH --no-save star_prep.R
R CMD BATCH --no-save synthetic_simul_plot.R
R CMD BATCH --no-save real_simul_plot.R
cd ..
```

Please contact lihua.lei@berkeley.edu for further questions.

