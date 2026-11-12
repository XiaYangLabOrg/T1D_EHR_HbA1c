
library(dplyr)
library(data.table)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCLA/")

############### rain cloud plot ################################################

T1D_lab_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_lab_hba1c_percent <- T1D_lab_hba1c_percent[!is.na(T1D_lab_hba1c_percent$specimen_taken_time),]
T1D_lab_hba1c_percent$specimen_taken_time <- as.numeric(T1D_lab_hba1c_percent$specimen_taken_time)
T1D_lab_hba1c_percent <- subset(T1D_lab_hba1c_percent, !is.na(specimen_taken_time) & result > 5)

results <- T1D_lab_hba1c_percent %>%
  group_by(patient_id) %>%
  filter(n() > 1) %>%
  mutate(
    start = min(specimen_taken_time),
    end   = max(specimen_taken_time),
    mid   = start + (end - start)/2
  ) %>%
  summarise(
    BEFORE = mean(result[specimen_taken_time < mid], na.rm = TRUE),
    AFTER  = mean(result[specimen_taken_time > mid], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(BEFORE), !is.na(AFTER)) %>%
  mutate(DIFF = AFTER - BEFORE)

write.csv(results, "./intermediate_data/graph_additional/T1D_lab_hba1c_percent_difference.csv", row.names = FALSE)

############### diabetic ketoacidosis information ################################################

T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
diagnosis <- read.csv("./intermediate_data/downloaded_data/T1D_diagnosis.csv", row.names = 1)
diagnosis <- subset(diagnosis, IP_PATIENT_ID %in% T1D_patients)
keto_diag <- subset(diagnosis, (grepl("(^250.1[01])", ICD_CODE) & ICD_TYPE == 9) | 
                      (grepl("(^E1[013]\\.1[01])", ICD_CODE) & ICD_TYPE == 10))
keto_diag <- keto_diag[,c("IP_PATIENT_ID", "DIAGNOSIS_DATE")]
keto_diag_min <- keto_diag %>%
  group_by(IP_PATIENT_ID) %>%
  slice_min(DIAGNOSIS_DATE, n = 1, with_ties = FALSE) %>%
  ungroup()
colnames(keto_diag_min) <- c("patient_id", "diagnosis_date")
write.csv(keto_diag_min, "./intermediate_data/graph_additional/T1D_diagnosis_DKA_1st.csv", row.names = FALSE)

T1D_diag <- subset(diagnosis, (grepl("(^250.0[13])", ICD_CODE) & ICD_TYPE == 9) | 
                     (grepl("(^E10)", ICD_CODE) & ICD_TYPE == 10))
T1D_diag <- T1D_diag[,c("IP_PATIENT_ID", "DIAGNOSIS_DATE")]
T1D_diag_min <- T1D_diag %>%
  group_by(IP_PATIENT_ID) %>%
  slice_min(DIAGNOSIS_DATE, n = 1, with_ties = FALSE) %>%
  ungroup()
colnames(T1D_diag_min) <- c("patient_id", "diagnosis_date")
write.csv(T1D_diag_min, "./intermediate_data/graph_additional/T1D_diagnosis_T1D_1st.csv", row.names = FALSE)


T1D_lab_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_lab_hba1c_percent <- T1D_lab_hba1c_percent[!is.na(T1D_lab_hba1c_percent$specimen_taken_time),]

drug_classification <- c("T2D")[2]
T1D_medication <- read.csv(paste0("./intermediate_data/processed_data/T1D_medication_", drug_classification, ".csv"))
T1D_medication$start_date <- as.numeric(T1D_medication$start_date)
T1D_medication <- T1D_medication[!is.na(T1D_medication$start_date),]

T1D_medication_summary <- T1D_medication %>%
  group_by(patient_id, ATC_drug) %>%
  summarise(
    Earliest_start_date = min(start_date, na.rm=TRUE),
    Latest_end_date = max(start_date, end_date, na.rm=TRUE),
    .groups = "drop"
  )
T1D_medication_summary <- T1D_medication_summary[! apply(T1D_medication_summary, 1, function(row) any(grepl("NULL", row))), ]
T1D_medication_summary <- T1D_medication_summary[T1D_medication_summary$Latest_end_date - T1D_medication_summary$Earliest_start_date > 30,] ## at least 30 days

T1D_drug_time <- data.frame(ATC_drug = character(0), patient_id = character(0), 
                            start_date = numeric(0), before_drug = numeric(0), 
                            end_date = numeric(0), after_drug = numeric(0), drug_period = numeric(0))
colnames(T1D_drug_time) <- c("ATC_drug", "patient_id", "start_date", "before_drug", "end_date", "after_drug", "drug_period")
for (drug in unique(T1D_medication_summary$ATC_drug)) {
  print(drug)
  T1D_drug <- T1D_medication_summary[T1D_medication_summary$ATC_drug == drug, ]
  patient_id <- unique(T1D_drug$patient_id)
  for (patient in patient_id) {
    start_date <- T1D_drug[T1D_drug$patient_id == patient, "Earliest_start_date", drop = TRUE]
    end_date <- T1D_drug[T1D_drug$patient_id == patient, "Latest_end_date", drop = TRUE]
    T1D_hba1c_patient <- T1D_lab_hba1c_percent[T1D_lab_hba1c_percent$patient_id == patient,]
    drug_period <- end_date - start_date
    num_measure_before_start <- sum(T1D_hba1c_patient$specimen_taken_time < start_date & T1D_hba1c_patient$specimen_taken_time > start_date - 365)
    num_measure_after_end <- sum(T1D_hba1c_patient$specimen_taken_time > end_date & T1D_hba1c_patient$specimen_taken_time < end_date + 365)
    if (num_measure_before_start >= 1 && num_measure_after_end >= 1) {
      mean_before <- mean(T1D_hba1c_patient[T1D_hba1c_patient$specimen_taken_time < start_date & T1D_hba1c_patient$specimen_taken_time > start_date - 365, "result"])
      mean_after <- mean(T1D_hba1c_patient[T1D_hba1c_patient$specimen_taken_time > end_date & T1D_hba1c_patient$specimen_taken_time < end_date + 365, "result"])
      summary <- setNames(c(drug, patient, start_date, mean_before, end_date, mean_after, drug_period), colnames(T1D_drug_time))
      T1D_drug_time <- rbind(T1D_drug_time, summary)
    }
  }
}
colnames(T1D_drug_time) <- c("ATC_drug", "patient_id", "start_date", "before_drug", "end_date", "after_drug", "drug_period")
T1D_drug_time$start_date <- as.numeric(T1D_drug_time$start_date)
T1D_drug_time$before_drug <- as.numeric(T1D_drug_time$before_drug)
T1D_drug_time$end_date <- as.numeric(T1D_drug_time$end_date)
T1D_drug_time$after_drug <- as.numeric(T1D_drug_time$after_drug)
T1D_drug_time$diff <- T1D_drug_time$after_drug - T1D_drug_time$before_drug

write.csv(T1D_drug_time, paste0("./intermediate_data/graph_additional/", drug_classification, 
                                "_30day.period_365day.range_1measure_drug_time_ATC_drug.csv"), row.names = FALSE)



drug_classification <- c("insulin")
T1D_medication <- read.csv(paste0("./intermediate_data/processed_data/T1D_medication_", drug_classification, ".csv"))
T1D_medication$start_date <- as.numeric(T1D_medication$start_date)
T1D_medication <- T1D_medication[!is.na(T1D_medication$start_date),]

T1D_medication_summary <- T1D_medication %>%
  group_by(patient_id, ATC_drug) %>%
  summarise(
    start_date = min(start_date, na.rm=TRUE),
    end_date = max(start_date, end_date, na.rm=TRUE),
    .groups = "drop"
  )
T1D_medication_summary <- T1D_medication_summary[! apply(T1D_medication_summary, 1, function(row) any(grepl("NULL", row))), ]
T1D_medication_summary <- T1D_medication_summary[T1D_medication_summary$end_date - T1D_medication_summary$start_date > 30,] ## at least 30 day
write.csv(T1D_medication_summary, paste0("./intermediate_data/graph_additional/", drug_classification, 
                                "_30day.period_365day.range_1measure_drug_time_ATC_drug.csv"), row.names = FALSE)

T2D_drug_patient <- read.csv(paste0("./intermediate_data/graph_additional/T2D_30day.period_365day.range_1measure_drug_time_ATC_drug.csv"))
T1D_medication_summary <- subset(T1D_medication_summary, ! patient_id %in% T2D_drug_patient$patient_id)
write.csv(T1D_medication_summary, paste0("./intermediate_data/graph_additional/", drug_classification, 
                                         "_xT2D_30day.period_365day.range_1measure_drug_time_ATC_drug.csv"), row.names = FALSE)


