#!/bin/bash
#$ -l h_rt=2:00:00
#$ -l h_data=20G
#$ -pe shared 4
#$ -l highp
#$ -o /u/project/xyang123/rainyliu/EHR_fuzzy_search/process_name
#$ -e /u/project/xyang123/rainyliu/EHR_fuzzy_search/process_name
#$ -M $rainyliu31@ucla.edu
#$ -m bea

. /u/local/Modules/default/init/modules.sh
module load R
cd /u/project/xyang123/rainyliu/EHR_fuzzy_search/process_name
echo "start"
Rscript process_name.R
echo "finish"