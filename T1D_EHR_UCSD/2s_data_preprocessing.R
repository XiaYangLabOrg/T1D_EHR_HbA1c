
library(dplyr)
library(data.table)
library(readr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/")
# 
# path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"
# T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
# 
# diagnosis <- read.csv(paste0(path, "dx.csv"), sep = "\t")
# T1D_diagnosis <- subset(diagnosis, (icd_type == 9 & grepl("^250\\.[0-9][1234]$", icd_code)) | (icd_type == 10 & grepl("^E10|^11", icd_code)))
# 
# diagnosis.new <- read.csv(paste0(path, "Max=2106-UCLA-V3/MAX-2016-UCLA-dx.V3.csv"), sep = "\t")
# T1D_diagnosis.new <- subset(diagnosis.new, (icd_type == 9 & grepl("^250\\.[0-9][1234]$", icd_code)) | (icd_type == 10 & grepl("^E10|^11", icd_code)))
# 
# T1D_diagnosis <- rbind(T1D_diagnosis, T1D_diagnosis.new)
# T1D_diagnosis$DIABETES <- ifelse((grepl("^250\\.[0-9][13]", T1D_diagnosis$icd_code) & T1D_diagnosis$icd_type == "09") |
#                                    (grepl("^E10", T1D_diagnosis$icd_code) & T1D_diagnosis$icd_type == "10"), "T1D", "T2D")
# T1D_diagnosis <- unique(T1D_diagnosis)
# T1D_diagnosis <- subset(T1D_diagnosis, ip_patient_id %in% T1D_patients)
# 
# summary_table <- T1D_diagnosis %>%
#   group_by(ip_patient_id) %>%
#   summarise(
#     numT2D = sum(DIABETES == "T2D", na.rm = TRUE),
#     numT1D = sum(DIABETES == "T1D", na.rm = TRUE)
#   )
# summary_table$ratio <- ifelse(summary_table$numT2D == 0, Inf, summary_table$numT1D / summary_table$numT2D)
# summary_table_subset <- subset(summary_table, ratio >= 0.5)
# summary_table_subset <- subset(summary_table, ratio >= 0.5 | numT1D >= 10)
# 
# saveRDS(summary_table_subset$ip_patient_id, "./intermediate_data/processed_data/T1D_patients_ratio_gtet0.5.RDS")
# saveRDS(summary_table_subset$ip_patient_id, "./intermediate_data/processed_data/T1D_patients_ratio_gtet0.5_OR_T1D_gtet10.RDS")
# 
# write.csv(summary_table, "./intermediate_data/processed_data/T1D_diagnosis_summary.csv", row.names = FALSE)



path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")

diagnosis <- read.csv(paste0(path, "dx.csv"), sep = "\t")
T1D_diagnosis <- subset(diagnosis, ip_patient_id %in% T1D_patients)

diagnosis.new <- read.csv(paste0(path, "Max=2106-UCLA-V3/MAX-2016-UCLA-dx.V3.csv"), sep = "\t")
T1D_diagnosis.new <- subset(diagnosis.new, ip_patient_id %in% T1D_patients)

T1D_diagnosis <- rbind(T1D_diagnosis, T1D_diagnosis.new)
T1D_diagnosis <- subset(T1D_diagnosis, ip_patient_id %in% T1D_patients)

T1D_DKD_patient <- subset(T1D_diagnosis, (grepl("^250\\.4[13]", icd_code) & icd_type == "09") |
                            (grepl("^E10\\.2[129]", icd_code) & icd_type == "10"))
saveRDS(unique(T1D_DKD_patient$ip_patient_id), "./intermediate_data/processed_data/T1D_DKD_patients.RDS")

T1D_DKD_patient <- T1D_DKD_patient %>%
  group_by(ip_patient_id) %>%
  slice_min(Diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_DKD_patient <- T1D_DKD_patient[,c(1,3)]
colnames(T1D_DKD_patient) <- c("patient_id", "diagnosis_date")

T1D_DKA_patient <- subset(T1D_diagnosis, (grepl("^250\\.1[13]", icd_code) & icd_type == "09") |
                            (grepl("^E10\\.1[01]", icd_code) & icd_type == "10"))
saveRDS(unique(T1D_DKA_patient$ip_patient_id), "./intermediate_data/processed_data/T1D_DKA_patients.RDS")


T1D_DKA_patient <- T1D_DKA_patient %>%
  group_by(ip_patient_id) %>%
  slice_min(Diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_DKA_patient <- T1D_DKA_patient[,c(1,3)]
colnames(T1D_DKA_patient) <- c("patient_id", "diagnosis_date")


T1D_diagnosis <- T1D_diagnosis %>%
  group_by(ip_patient_id) %>%
  slice_min(Diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_diagnosis <- T1D_diagnosis[,c(1,3)]
colnames(T1D_diagnosis) <- c("patient_id", "diagnosis_date")

write.csv(T1D_DKD_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKD_1st.csv", row.names = FALSE)
write.csv(T1D_DKA_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKA_1st.csv", row.names = FALSE)
write.csv(T1D_diagnosis, "./intermediate_data/processed_data/T1D_diagnosis_T1D_1st.csv", row.names = FALSE)

