#!/bin/bash

#SBATCH --job-name=expr
#SBATCH --output=../log/real_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=2-00:00:00
#SBATCH --array=1-400

ml R

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "params_real_simul.txt")
data=$(echo $LINE | cut -d ' ' -f 1)
residtype=$(echo $LINE | cut -d ' ' -f 2)
seed=$(echo $LINE | cut -d ' ' -f 3)

output=$(echo "../results/real-data${data}-resid${residtype}-seed${seed}.out")
touch "$output"

cd ../R/

Rscript real_simul.R --data "$data" --residtype "$residtype" --rho "0" --thresh "0" --seed "$seed" >> "$output" 2>&1

Rscript real_simul.R --data "$data" --residtype "$residtype" --rho "1" --thresh "0" --seed "$seed" >> "$output" 2>&1

Rscript real_simul.R --data "$data" --residtype "$residtype" --rho "0" --thresh "0.025" --seed "$seed" >> "$output" 2>&1

Rscript real_simul.R --data "$data" --residtype "$residtype" --rho "1" --thresh "0.025" --seed "$seed" >> "$output" 2>&1
