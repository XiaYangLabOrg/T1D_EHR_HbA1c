
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V2/")

old_dir <- "./intermediate_data/processed_data/"
new_dir <- "./intermediate_data/processed_data_stringent/"

## filter files for stringency condition
T1D_patients_stringent <- readRDS(paste0(old_dir, "T1D_patients_ratio_gtet0.5_OR_T1D_gtet10.RDS"))
saveRDS(T1D_patients_stringent, paste0(new_dir, "T1D_patients.RDS"))

## T1D_demo.csv
T1D_demo <- read.csv(paste0(old_dir, "T1D_demo.csv"))
T1D_demo <- subset(T1D_demo, patient_id %in% T1D_patients_stringent)
write.csv(T1D_demo, paste0(new_dir, "T1D_demo.csv"), row.names = FALSE)

## T1D_DKA_patients.RDS
T1D_DKA_patients <- readRDS(paste0(old_dir, "T1D_DKA_patients.RDS"))
T1D_DKA_patients <- T1D_DKA_patients[T1D_DKA_patients %in% T1D_patients_stringent]
saveRDS(T1D_DKA_patients, paste0(new_dir, "T1D_DKA_patients.RDS"))

## T1D_DKD_patients.RDS
T1D_DKD_patients <- readRDS(paste0(old_dir, "T1D_DKD_patients.RDS"))
T1D_DKD_patients <- T1D_DKD_patients[T1D_DKD_patients %in% T1D_patients_stringent]
saveRDS(T1D_DKD_patients, paste0(new_dir, "T1D_DKD_patients.RDS"))

## T1D_diagnosis_DKA_1st.csv
T1D_diagnosis_DKA_1st <- read.csv(paste0(old_dir, "T1D_diagnosis_DKA_1st.csv"))
T1D_diagnosis_DKA_1st <- subset(T1D_diagnosis_DKA_1st, patient_id %in% T1D_patients_stringent)
write.csv(T1D_diagnosis_DKA_1st, paste0(new_dir, "T1D_diagnosis_DKA_1st.csv"), row.names = FALSE)

## T1D_diagnosis_DKD_1st.csv
T1D_diagnosis_DKD_1st <- read.csv(paste0(old_dir, "T1D_diagnosis_DKD_1st.csv"))
T1D_diagnosis_DKD_1st <- subset(T1D_diagnosis_DKD_1st, patient_id %in% T1D_patients_stringent)
write.csv(T1D_diagnosis_DKD_1st, paste0(new_dir, "T1D_diagnosis_DKD_1st.csv"), row.names = FALSE)

## T1D_lab_hba1c_percent.csv
T1D_lab_hba1c_percent <- read.csv(paste0(old_dir, "T1D_lab_hba1c_percent.csv"))
T1D_lab_hba1c_percent <- subset(T1D_lab_hba1c_percent, patient_id %in% T1D_patients_stringent)
write.csv(T1D_lab_hba1c_percent, paste0(new_dir, "T1D_lab_hba1c_percent.csv"), row.names = FALSE)

## T1D_medication_insulin.csv
T1D_medication_insulin <- read.csv(paste0(old_dir, "T1D_medication_insulin.csv"))
T1D_medication_insulin <- subset(T1D_medication_insulin, patient_id %in% T1D_patients_stringent)
write.csv(T1D_medication_insulin, paste0(new_dir, "T1D_medication_insulin.csv"), row.names = FALSE)

## T1D_medication_antihypertensive.csv
T1D_medication_antihypertensive <- read.csv(paste0(old_dir, "T1D_medication_antihypertensive.csv"))
T1D_medication_antihypertensive <- subset(T1D_medication_antihypertensive, patient_id %in% T1D_patients_stringent)
write.csv(T1D_medication_antihypertensive, paste0(new_dir, "T1D_medication_antihypertensive.csv"), row.names = FALSE)

## T1D_medication_lipid_modifying.csv
T1D_medication_lipid_modifying <- read.csv(paste0(old_dir, "T1D_medication_lipid_modifying.csv"))
T1D_medication_lipid_modifying <- subset(T1D_medication_lipid_modifying, patient_id %in% T1D_patients_stringent)
write.csv(T1D_medication_lipid_modifying, paste0(new_dir, "T1D_medication_lipid_modifying.csv"), row.names = FALSE)

## T1D_medication_T2D.csv
T1D_medication_T2D <- read.csv(paste0(old_dir, "T1D_medication_T2D.csv"))
T1D_medication_T2D <- subset(T1D_medication_T2D, patient_id %in% T1D_patients_stringent)
write.csv(T1D_medication_T2D, paste0(new_dir, "T1D_medication_T2D.csv"), row.names = FALSE)

## add DKA and DKD into T1D_demo
T1D_demo <- read.csv(paste0(new_dir, "T1D_demo.csv"))
T1D_demo$kidney_disease <- NULL
T1D_demo$ketoacidosis <- NULL

T1D_DKD_patients <- readRDS(paste0(new_dir, "T1D_DKD_patients.RDS"))
T1D_DKA_patients <- readRDS(paste0(new_dir, "T1D_DKA_patients.RDS"))
T1D_demo$DKA <- ifelse(T1D_demo$patient_id %in% T1D_DKA_patients, 1, 0)
T1D_demo$DKD <- ifelse(T1D_demo$patient_id %in% T1D_DKD_patients, 1, 0)

write.csv(T1D_demo, paste0(new_dir, "T1D_demo.csv"), row.names = FALSE)

