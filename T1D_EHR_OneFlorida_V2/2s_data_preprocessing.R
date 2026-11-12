
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V2/")

## check how many patients remain with T1D/T2D > 0.5
T1D_patients_ICD <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V2/"

diagnosis <- list()
for(i in 1:4){
  print(i)
  diagnosis_df <- read.csv(paste0(path, "DIAGNOSIS_YANG", i, "_V2.csv"))
  diagnosis_df <- subset(diagnosis_df, ID %in% T1D_patients_ICD)
  diagnosis_df$DX_TYPE <- as.character(diagnosis_df$DX_TYPE)
  diagnosis_df <- subset(diagnosis_df, (grepl("^250\\.[0-9]", DX) & DX_TYPE == "09") |
                           (grepl("^E10|^E11", DX) & DX_TYPE == "10"))
  diagnosis[[i]] <- diagnosis_df
  rm(diagnosis_df)
  gc()
}
gc()

T1D_diagnosis <- do.call(rbind, diagnosis)

T1D_diagnosis$DIABETES <- ifelse((grepl("^250\\.[0-9][13]", T1D_diagnosis$DX) & T1D_diagnosis$DX_TYPE == "09") |
                                   (grepl("^E10", T1D_diagnosis$DX) & T1D_diagnosis$DX_TYPE == "10"), "T1D", "T2D")

summary_table <- T1D_diagnosis %>%
  group_by(ID) %>%
  summarise(
    numT2D = sum(DIABETES == "T2D", na.rm = TRUE),
    numT1D = sum(DIABETES == "T1D", na.rm = TRUE)
  )
summary_table$ratio <- ifelse(summary_table$numT2D == 0, Inf, summary_table$numT1D / summary_table$numT2D)
summary_table_subset <- subset(summary_table, ratio >= 0.5)
summary_table_subset <- subset(summary_table, ratio >= 0.5 | numT1D >= 5)

write.csv(summary_table, "./intermediate_data/processed_data/T1D_diagnosis_summary.csv", row.names = FALSE)

saveRDS(summary_table_subset$ID, "./intermediate_data/processed_data/T1D_patients_ratio_gtet0.5.RDS")
saveRDS(summary_table_subset$ID, "./intermediate_data/processed_data/T1D_patients_ratio_gtet0.5_OR_T1D_gtet5.RDS")


## check how many patients remain with T1D/T2D > 0.5
T1D_patients_ICD <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V2/"

diagnosis_keto <- list()
diagnosis_kidney <- list()
for(i in 1:4){
  print(i)
  diagnosis_df <- read.csv(paste0(path, "DIAGNOSIS_YANG", i, "_V2.csv"))
  diagnosis_df <- subset(diagnosis_df, ID %in% T1D_patients_ICD)
  diagnosis_df$DX_TYPE <- as.character(diagnosis_df$DX_TYPE)
  diagnosis_kidney[[i]] <- subset(diagnosis_df, (grepl("^250\\.4[13]", DX) & DX_TYPE == "09") |
                                    (grepl("^E10\\.2[129]", DX) & DX_TYPE == "10"))
  diagnosis_keto[[i]] <- subset(diagnosis_df, (grepl("^250\\.1[13]", DX) & DX_TYPE == "09") |
                                  (grepl("^E10\\.1[01]", DX) & DX_TYPE == "10"))
  rm(diagnosis_df)
  gc()
}
gc()

reference_date <- as.Date("2012-01-01")
T1D_DKA_patient <- do.call(rbind, diagnosis_keto)
T1D_DKA_patient$ADMIT_DATE_OFFSET <- as.Date(T1D_DKA_patient$ADMIT_DATE_OFFSET, format = "%d%b%Y")
T1D_DKA_patient$DX_DATE_OFFSET <- as.Date(T1D_DKA_patient$DX_DATE_OFFSET, format = "%d%b%Y")
T1D_DKA_patient <- T1D_DKA_patient %>%
  mutate(ADMIT_DATE_OFFSET_RELATIVE = as.numeric(ADMIT_DATE_OFFSET - reference_date),
         DX_DATE_OFFSET_RELATIVE = as.numeric(DX_DATE_OFFSET - reference_date),
         diagnosis_date = pmin(as.numeric(ADMIT_DATE_OFFSET - reference_date), as.numeric(DX_DATE_OFFSET - reference_date), na.rm = TRUE))
T1D_DKA_patient <- T1D_DKA_patient %>%
  group_by(ID) %>%
  slice_min(diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_DKA_patient <- T1D_DKA_patient[,c("ID", "diagnosis_date")]
colnames(T1D_DKA_patient) <- c("patient_id", "diagnosis_date")
write.csv(T1D_DKA_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKA_1st.csv", row.names = FALSE)
saveRDS(unique(T1D_DKA_patient$patient_id), "./intermediate_data/processed_data/T1D_DKA_patients.RDS")

T1D_DKD_patient <- do.call(rbind, diagnosis_kidney)
T1D_DKD_patient$ADMIT_DATE_OFFSET <- as.Date(T1D_DKD_patient$ADMIT_DATE_OFFSET, format = "%d%b%Y")
T1D_DKD_patient$DX_DATE_OFFSET <- as.Date(T1D_DKD_patient$DX_DATE_OFFSET, format = "%d%b%Y")
T1D_DKD_patient <- T1D_DKD_patient %>%
  mutate(ADMIT_DATE_OFFSET_RELATIVE = as.numeric(ADMIT_DATE_OFFSET - reference_date),
         DX_DATE_OFFSET_RELATIVE = as.numeric(DX_DATE_OFFSET - reference_date),
         diagnosis_date = pmin(as.numeric(ADMIT_DATE_OFFSET - reference_date), as.numeric(DX_DATE_OFFSET - reference_date), na.rm = TRUE))
T1D_DKD_patient <- T1D_DKD_patient %>%
  group_by(ID) %>%
  slice_min(diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_DKD_patient <- T1D_DKD_patient[,c("ID", "diagnosis_date")]
colnames(T1D_DKD_patient) <- c("patient_id", "diagnosis_date")
write.csv(T1D_DKD_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKD_1st.csv", row.names = FALSE)
saveRDS(unique(T1D_DKD_patient$patient_id), "./intermediate_data/processed_data/T1D_DKD_patients.RDS")

# reference_date <- as.Date("2012-01-01")
# T1D_DKA_patient <- do.call(rbind, diagnosis_keto)
# T1D_DKA_patient$ADMIT_DATE_OFFSET <- as.Date(T1D_DKA_patient$ADMIT_DATE_OFFSET, format = "%d%b%Y")
# T1D_DKA_patient <- T1D_DKA_patient %>%
#   mutate(diagnosis_date = as.numeric(ADMIT_DATE_OFFSET - reference_date))
# saveRDS(unique(T1D_DKA_patient$ID), "./intermediate_data/processed_data/T1D_DKA_patients.RDS")
# 
# T1D_DKA_patient <- T1D_DKA_patient %>%
#   group_by(ID) %>%
#   slice_min(ADMIT_DATE_OFFSET, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# T1D_DKA_patient <- T1D_DKA_patient[,c("ID", "diagnosis_date")]
# colnames(T1D_DKA_patient) <- c("patient_id", "diagnosis_date")
# write.csv(T1D_DKA_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKA_1st.csv", row.names = FALSE)
# 
# T1D_DKD_patient <- do.call(rbind, diagnosis_kidney)
# T1D_DKD_patient$ADMIT_DATE_OFFSET <- as.Date(T1D_DKD_patient$ADMIT_DATE_OFFSET, format = "%d%b%Y")
# T1D_DKD_patient <- T1D_DKD_patient %>%
#   mutate(diagnosis_date = as.numeric(ADMIT_DATE_OFFSET - reference_date))
# saveRDS(unique(T1D_DKD_patient$ID), "./intermediate_data/processed_data/T1D_DKD_patients.RDS")
# 
# T1D_DKD_patient <- T1D_DKD_patient %>%
#   group_by(ID) %>%
#   slice_min(ADMIT_DATE_OFFSET, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# T1D_DKD_patient <- T1D_DKD_patient[,c("ID", "diagnosis_date")]
# colnames(T1D_DKD_patient) <- c("patient_id", "diagnosis_date")
# write.csv(T1D_DKD_patient, "./intermediate_data/processed_data/T1D_diagnosis_DKD_1st.csv", row.names = FALSE)


## new code




