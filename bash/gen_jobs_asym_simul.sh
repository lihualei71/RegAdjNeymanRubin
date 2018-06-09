#!/bin/bash

params_filename='params_asym_simul.txt'

while read logn Xtype residtype nreps seed
do
    filename='../results/asym_simul_logn"'$logn'"_seed"'$seed'"_nreps"'$nreps'"_X"'$Xtype'"_resid"'$residtype'".sh'
    echo $filename
    Rout_filename='../results/asym_simul_logn"'$logn'"_seed"'$seed'"_nreps"'$nreps'"_X"'$Xtype'"_resid"'$residtype'".Rout'
    echo $Rout_filename
    touch $filename
    echo '#!/bin/bash' > $filename
    echo '#SBATCH --cpus-per-task 1' >> $filename
    echo 'export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK' >> $filename
    echo '' >> $filename
    echo 'export logn="'$logn'"' >> $filename    
    echo 'export seed="'$seed'"' >> $filename
    echo 'export nreps="'$nreps'"' >> $filename
    echo 'export Xtype="'$Xtype'"' >> $filename
    echo 'export residtype="'$residtype'"' >> $filename    
    echo "R CMD BATCH --no-save ../R/asym_simul.R "$Rout_filename >> $filename
    chmod 755 $filename
    sbatch $filename
done < $params_filename
