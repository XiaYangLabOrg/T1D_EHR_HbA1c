#!/bin/bash
#$ -l h_rt=1:00:00
#$ -l h_data=10G
#$ -pe shared 4
#$ -l highp
#$ -o /u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/code
#$ -e /u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/code
#$ -M $rainyliu31@ucla.edu
#$ -m bea

. /u/local/Modules/default/init/modules.sh
module load python/3.9.6
cd /u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/
python3 ./code/slow_search_top10.py ./insulin/insulin.json ./EHR_drug_list/drug_list_UCLA.csv