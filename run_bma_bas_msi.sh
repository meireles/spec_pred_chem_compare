#!/bin/bash -l
#PBS -l walltime=36:00:00,nodes=1:ppn=17,mem=36gb
#PBS -N j
#PBS -m abe
#PBS -M meireles@umn.edu
module load R/3.3.3
module load intel
module load ompi/intel

cd ~/projects/spec_pred_chem_compare

R --no-save -q < R/05_fit_chem_bma_bas.R
