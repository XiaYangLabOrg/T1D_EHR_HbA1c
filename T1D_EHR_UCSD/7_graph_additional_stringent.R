
library(dplyr)
library(data.table)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/")

## used in raincloud plot
T1D_lab_hba1c_percent <- read.csv("./intermediate_data/processed_data_stringent/T1D_lab_hba1c_percent.csv")
T1D_lab_hba1c_percent <- T1D_lab_hba1c_percent[!is.na(T1D_lab_hba1c_percent$specimen_taken_time),]
T1D_lab_hba1c_percent$specimen_taken_time <- as.numeric(T1D_lab_hba1c_percent$specimen_taken_time)
T1D_lab_hba1c_percent <- subset(T1D_lab_hba1c_percent, !is.na(specimen_taken_time) & result > 5)

results <- T1D_lab_hba1c_percent %>%
  group_by(patient_id) %>%
  filter(n() >= 3) %>%
  summarise(
    TIME = (max(specimen_taken_time) - min(specimen_taken_time)) / 365,
    n_measurements = n(),
    slope = coef(lm(result ~ specimen_taken_time))[2],
    DIFF_timeadjusted = slope * 365,
    .groups = "drop"
  ) %>%
  filter(!is.na(slope))
write.csv(results, "./intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference_slope.csv", row.names = FALSE)

patients_cardiometabolic <- rbind(read.csv("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted/T2D_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"),
                                  read.csv("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted/lipid_modifying_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"),
                                  read.csv("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted/antihypertensive_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"))
results <- subset(results, ! patient_id %in% patients_cardiometabolic$patient_id)
write.csv(results, "./intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference_slope_xcardiometabolic.csv", row.names = FALSE)


# results <- T1D_lab_hba1c_percent %>%
#   group_by(patient_id) %>%
#   filter(n() > 1) %>%
#   mutate(
#     start = min(specimen_taken_time),
#     end   = max(specimen_taken_time),
#     mid   = start + (end - start)/2
#   ) %>%
#   summarise(
#     TIME = (first(end) - first(start))/365, 
#     BEFORE = mean(result[specimen_taken_time < mid], na.rm = TRUE),
#     AFTER  = mean(result[specimen_taken_time > mid], na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(!is.na(BEFORE), !is.na(AFTER)) %>%
#   mutate(DIFF = AFTER - BEFORE) %>%
#   mutate(DIFF_timeadjusted = DIFF/ TIME)
# 
# write.csv(results, "./intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference.csv", row.names = FALSE)

## used in survival plot

T1D_lab_hba1c_percent <- read.csv("./intermediate_data/processed_data_stringent/T1D_lab_hba1c_percent.csv")
T1D_lab_hba1c_percent <- T1D_lab_hba1c_percent[!is.na(T1D_lab_hba1c_percent$specimen_taken_time),]

### find all the patients that are taking T2D drugs or insulin for at least 30 days and record the periods
for(drug_classification in c("T2D", "insulin")){
  T1D_medication <- read.csv(paste0("./intermediate_data/processed_data_stringent/T1D_medication_", drug_classification, ".csv"))
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
                              start_date = numeric(0), 
                              end_date = numeric(0), drug_period = numeric(0))
  colnames(T1D_drug_time) <- c("ATC_drug", "patient_id", "start_date", "end_date", "drug_period")
  for (drug in unique(T1D_medication_summary$ATC_drug)) {
    print(drug)
    T1D_drug <- T1D_medication_summary[T1D_medication_summary$ATC_drug == drug, ]
    patient_id <- unique(T1D_drug$patient_id)
    for (patient in patient_id) {
      start_date <- T1D_drug[T1D_drug$patient_id == patient, "Earliest_start_date", drop = TRUE]
      end_date <- T1D_drug[T1D_drug$patient_id == patient, "Latest_end_date", drop = TRUE]
      T1D_hba1c_patient <- T1D_lab_hba1c_percent[T1D_lab_hba1c_percent$patient_id == patient,]
      drug_period <- end_date - start_date
      summary <- setNames(c(drug, patient, start_date, end_date, drug_period), colnames(T1D_drug_time))
      T1D_drug_time <- rbind(T1D_drug_time, summary)
    }
  }
  colnames(T1D_drug_time) <- c("ATC_drug", "patient_id", "start_date", "end_date", "drug_period")
  T1D_drug_time$start_date <- as.numeric(T1D_drug_time$start_date)
  T1D_drug_time$end_date <- as.numeric(T1D_drug_time$end_date)
  
  write.csv(T1D_drug_time, paste0("./intermediate_data/graph_additional_stringent/", drug_classification, 
                                  "_30day.period_drug_time_ATC_drug.csv"), row.names = FALSE)
}
gc()

### filter for insulin-only patients by excluding patients with T2D history
T1D_drug_time_T2D <- read.csv(paste0("./intermediate_data/graph_additional_stringent/T2D_30day.period_drug_time_ATC_drug.csv"))
T1D_drug_time_insulin <- read.csv(paste0("./intermediate_data/graph_additional_stringent/insulin_30day.period_drug_time_ATC_drug.csv"))
T1D_drug_time_insulin <- subset(T1D_drug_time_insulin, ! patient_id %in% T1D_drug_time_T2D$patient_id)
write.csv(T1D_drug_time_insulin, paste0("./intermediate_data/graph_additional_stringent/", drug_classification, 
                                        "_xT2D_30day.period_drug_time_ATC_drug.csv"), row.names = FALSE)
rm(ls())
gc()
