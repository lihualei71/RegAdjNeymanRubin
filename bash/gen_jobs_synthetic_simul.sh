#!/bin/bash

params_filename='params_synthetic_simul.txt'

while read Xtype residtype seed
do
    filename='../results/synthetic_simul_X"'$Xtype'"_resid"'$residtype'"_seed"'$seed'".sh'
    echo $filename
    Rout_filename='../results/synthetic_simul_X"'$Xtype'"_resid"'$residtype'"_seed"'$seed'".Rout'
    echo $Rout_filename
    touch $filename
    echo '#!/bin/bash' > $filename
    echo '#SBATCH --cpus-per-task 1' >> $filename
    echo 'export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK' >> $filename
    echo '' >> $filename
    echo 'export Xtype="'$Xtype'"' >> $filename
    echo 'export residtype="'$residtype'"' >> $filename
    echo 'export seed="'$seed'"' >> $filename    
    echo "R CMD BATCH --no-save ../R/synthetic_simul.R "$Rout_filename >> $filename
    chmod 755 $filename
    sbatch $filename
done < $params_filename
