#!/bin/bash

#SBATCH --nodes=1
#SBATCH --mem=100000
#SBATCH --job-name=npoly_world
#SBATCH --time=94:00:00 

module purge
module load R/4.0.2
export R_LIBS=/system/software/arcus-htc/R/4.0.2/lib64/R

Rscript /data/zool-zost/sjoh4959/projects/newproject/notebooks/cluster_scripts/npoly_world.R

