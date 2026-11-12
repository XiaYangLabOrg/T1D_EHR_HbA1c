
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/")

T1D_patients <- readRDS("./intermediate_data/processed_data_stringent/T1D_patients.RDS")

path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"

################################################################################
####################### process diagnosis file #################################
################################################################################

diagnosis <- read.csv(paste0(path, "dx.csv"), sep = "\t")
diagnosis <- subset(diagnosis, ip_patient_id %in% T1D_patients)

diagnosis.new <- read.csv(paste0(path, "Max=2106-UCLA-V3/MAX-2016-UCLA-dx.V3.csv"), sep = "\t")
diagnosis.new <- subset(diagnosis.new, ip_patient_id %in% T1D_patients)

diagnosis <- rbind(diagnosis, diagnosis.new)
rm(diagnosis.new)

T1D_diagnosis <- subset(diagnosis, ! is.na(Diagnosis_date))
T1D_diagnosis <- T1D_diagnosis %>%
  group_by(ip_patient_id) %>%
  slice_min(Diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_diagnosis <- T1D_diagnosis[,c("ip_patient_id", "Diagnosis_date")]
colnames(T1D_diagnosis) <- c("patient_id", "earliest_date")

################################################################################
####################### subsetting medication/prescribing file #################
################################################################################

T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")
T1D_medication <- subset(T1D_medication, patient_id %in% T1D_patients)
T1D_medication <- subset(T1D_medication, start_date > -1000)
T1D_medication <- T1D_medication %>%
  group_by(patient_id) %>%
  slice_min(start_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_medication <- T1D_medication[,c("patient_id", "start_date")]
colnames(T1D_medication) <- c("patient_id", "earliest_date")

################################################################################
####################### subsetting lab file ####################################
################################################################################

T1D_lab <- read.csv("./intermediate_data/processed_data/T1D_lab.csv", row.names = 1)
T1D_lab <- subset(T1D_lab, ip_patient_id %in% T1D_patients)
T1D_lab$earliest_date <- pmin(T1D_lab$specimen_taken_time, T1D_lab$result_time, na.rm = TRUE)
T1D_lab$earliest_date <- as.numeric(T1D_lab$earliest_date)
T1D_lab <- T1D_lab %>%
  group_by(ip_patient_id) %>%
  slice_min(earliest_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()
T1D_lab <- T1D_lab[,c("ip_patient_id", "earliest_date")]
colnames(T1D_lab) <- c("patient_id", "earliest_date")

T1D_history <- rbind(T1D_diagnosis, T1D_medication, T1D_lab)
T1D_history <- T1D_history %>%
  group_by(patient_id) %>%
  slice_min(earliest_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()

print(nrow(T1D_history))

write.csv(T1D_history, "./intermediate_data/processed_data_stringent/T1D_history_1st.csv", row.names = FALSE)

T1D_diagnosis_1st <- read.csv("./intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st.csv")
T1D_diagnosis_1st <- merge(T1D_history, T1D_diagnosis_1st, by = "patient_id")
T1D_diagnosis_1st <- T1D_diagnosis_1st[T1D_diagnosis_1st$diagnosis_date - T1D_diagnosis_1st$earliest_date >= 365,]
write.csv(T1D_diagnosis_1st, "./intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st_oneyear_record.csv", row.names = FALSE)
