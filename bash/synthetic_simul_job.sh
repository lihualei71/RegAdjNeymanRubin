#!/bin/bash

#SBATCH --job-name=expr
#SBATCH --output=../log/synthetic_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=2-00:00:00
#SBATCH --array=1-600

ml R

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "params_synthetic_simul.txt")
Xtype=$(echo $LINE | cut -d ' ' -f 1)
residtype=$(echo $LINE | cut -d ' ' -f 2)
seed=$(echo $LINE | cut -d ' ' -f 3)

output=$(echo "../results/synthetic-sharp-X${Xtype}-resid${residtype}-seed${seed}.out")
touch "$output"

cd ../R/

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "0" --thresh "0" --pi1 "0.2" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "1" --thresh "0" --pi1 "0.2" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "0" --thresh "0.025" --pi1 "0.2" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "1" --thresh "0.025" --pi1 "0.2" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "0" --thresh "0" --pi1 "0.5" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "1" --thresh "0" --pi1 "0.5" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "0" --thresh "0.025" --pi1 "0.5" --seed "$seed" >> "$output" 2>&1

Rscript synthetic_simul.R --Xtype "$Xtype" --residtype "$residtype" --rho "1" --thresh "0.025" --pi1 "0.5" --seed "$seed" >> "$output" 2>&1
