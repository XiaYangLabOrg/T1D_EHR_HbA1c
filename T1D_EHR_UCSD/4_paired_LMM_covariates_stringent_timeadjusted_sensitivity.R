
library(dplyr)
library(data.table)
library(lmerTest)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/")

num_drug <- 1

################################################################################
#### Preprocessing paired LMM ##################################################
################################################################################
T1D_patients <- readRDS("./intermediate_data/processed_data_stringent/T1D_patients.RDS")

T1D_lab_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_lab_hba1c_percent <- subset(T1D_lab_hba1c_percent, patient_id %in% T1D_patients)
T1D_lab_hba1c_percent <- T1D_lab_hba1c_percent[!is.na(T1D_lab_hba1c_percent$specimen_taken_time),]

drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[num_drug]
T1D_medication <- read.csv(paste0("./intermediate_data/processed_data/T1D_medication_", drug_classification, ".csv"))
T1D_medication <- T1D_medication[!is.na(T1D_medication$start_date),]
T1D_medication <- subset(T1D_medication, patient_id %in% T1D_patients)

T1D_DKA_patient <- read.csv("./intermediate_data/processed_data/T1D_diagnosis_DKA_1st.csv")
T1D_DKA_patient <- subset(T1D_DKA_patient, patient_id %in% T1D_patients)
T1D_DKD_patient <- read.csv("./intermediate_data/processed_data/T1D_diagnosis_DKD_1st.csv")
T1D_DKD_patient <- subset(T1D_DKD_patient, patient_id %in% T1D_patients)

## ATC_class
T1D_medication_summary <- T1D_medication %>%
  group_by(patient_id, ATC_class) %>%
  summarise(
    Earliest_start_date = min(start_date, na.rm=TRUE),
    Latest_end_date = max(start_date, end_date, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Latest_end_date))
T1D_medication_summary <- T1D_medication_summary[! apply(T1D_medication_summary, 1, function(row) any(grepl("NULL", row))), ]
T1D_medication_summary <- T1D_medication_summary[T1D_medication_summary$Latest_end_date - T1D_medication_summary$Earliest_start_date > 30,] ## at least 30 days

system.time({
  meds <- as.data.table(T1D_medication_summary)
  labs <- as.data.table(T1D_lab_hba1c_percent)
  setkey(meds, patient_id); setkey(labs, patient_id)
  all_drug_time <- list()
  for (drug_name in unique(meds$ATC_class)) {
    cat("Processing:", drug_name, "\n")
    meds_drug <- meds[ATC_class == drug_name]
    labs_joined <- labs[meds_drug, nomatch = 0, allow.cartesian = TRUE]
    
    if (nrow(labs_joined) == 0) next
    labs_joined[, before := specimen_taken_time < Earliest_start_date & specimen_taken_time > (Earliest_start_date - 365 / 2)]
    labs_joined[, after := specimen_taken_time > Latest_end_date & specimen_taken_time < (Latest_end_date + 365 / 2)]
    labs_joined[, count_before := sum(before), by = patient_id]
    labs_joined[, count_after := sum(after), by = patient_id]
    valid_patients <- labs_joined[count_before >= 1 & count_after >= 1, unique(patient_id)]
    
    if (length(valid_patients) == 0) next
    valid_data <- labs_joined[patient_id %in% valid_patients]
    drug_time <- valid_data[
      before == TRUE | after == TRUE,
      .(
        ATC_class = drug_name,
        patient_id = patient_id,
        Earliest_start_date = Earliest_start_date,
        Latest_end_date = Latest_end_date,
        specimen_taken_time = specimen_taken_time,
        timepoint = ifelse(before, 0, (Latest_end_date - Earliest_start_date)/365),
        result = result
      )
    ]
    all_drug_time[[drug_name]] <- drug_time
  }
  rm(drug_time)
  T1D_drug_time <- rbindlist(all_drug_time, use.names = TRUE, fill = TRUE)
  T1D_drug_time <- as.data.frame(T1D_drug_time)
})
T1D_drug_time$kidney_disease <- 0
T1D_drug_time$ketoacidosis <- 0
for(i in 1:nrow(T1D_drug_time)){
  latest_date <- T1D_drug_time[i,"Latest_end_date"]
  patient_id <- T1D_drug_time[i,"patient_id"]
  if(patient_id %in% T1D_DKA_patient$patient_id && T1D_DKA_patient[T1D_DKA_patient$patient_id == patient_id, "diagnosis_date"] <= latest_date){
    T1D_drug_time[i, "ketoacidosis"] <- 1
  }
  if(patient_id %in% T1D_DKD_patient$patient_id && T1D_DKD_patient[T1D_DKD_patient$patient_id == patient_id, "diagnosis_date"] <= latest_date){
    T1D_drug_time[i, "kidney_disease"] <- 1
  }
}

T1D_demo <- read.csv("./intermediate_data/processed_data/T1D_demo.csv")
T1D_demo <- subset(T1D_demo, patient_id %in% T1D_patients)
T1D_demo$age_group <- cut(
  T1D_demo$age,
  breaks = c(-Inf, 18, 30, 40, 50, 65, Inf),
  labels = c("<18", "18-29", "30-39", "40-49", "50-64", ">64"),
  right = FALSE
)
T1D_demo$age_group <- factor(T1D_demo$age_group,
                             levels = c("40-49", "<18", "18-29", "30-39", "50-64", ">64"))
T1D_demo <- subset(T1D_demo, sex %in% c("Female", "Male"))
T1D_demo$sex <- factor(T1D_demo$sex, levels = c("Femal", "Male"))
levels(T1D_demo$sex) <- c("F", "M")
T1D_demo <- T1D_demo[,c("patient_id", "sex", "age_group")]
T1D_drug_time <- merge(T1D_drug_time, T1D_demo, by.x = "patient_id", by.y = "patient_id", all = FALSE)

write.csv(T1D_drug_time, paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                "_30day.period_halfyr.range_1measure_drug_time_ATC_class.csv"), row.names = FALSE)

## ATC_drug
T1D_medication_summary <- T1D_medication %>%
  group_by(patient_id, ATC_drug) %>%
  summarise(
    Earliest_start_date = min(start_date, na.rm=TRUE),
    Latest_end_date = max(start_date, end_date, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Latest_end_date))
T1D_medication_summary <- T1D_medication_summary[! apply(T1D_medication_summary, 1, function(row) any(grepl("NULL", row))), ]
T1D_medication_summary <- T1D_medication_summary[T1D_medication_summary$Latest_end_date - T1D_medication_summary$Earliest_start_date > 30,] ## at least 30 days

system.time({
  meds <- as.data.table(T1D_medication_summary)
  labs <- as.data.table(T1D_lab_hba1c_percent)
  setkey(meds, patient_id); setkey(labs, patient_id)
  all_drug_time <- list()
  for (drug_name in unique(meds$ATC_drug)) {
    cat("Processing:", drug_name, "\n")
    meds_drug <- meds[ATC_drug == drug_name]
    labs_joined <- labs[meds_drug, nomatch = 0, allow.cartesian = TRUE]
    
    if (nrow(labs_joined) == 0) next
    labs_joined[, before := specimen_taken_time < Earliest_start_date & specimen_taken_time > (Earliest_start_date - 365 / 2)]
    labs_joined[, after := specimen_taken_time > Latest_end_date & specimen_taken_time < (Latest_end_date + 365 / 2)]
    labs_joined[, count_before := sum(before), by = patient_id]
    labs_joined[, count_after := sum(after), by = patient_id]
    valid_patients <- labs_joined[count_before >= 1 & count_after >= 1, unique(patient_id)]
    
    if (length(valid_patients) == 0) next
    valid_data <- labs_joined[patient_id %in% valid_patients]
    drug_time <- valid_data[
      before == TRUE | after == TRUE,
      .(
        ATC_drug = drug_name,
        patient_id = patient_id,
        Earliest_start_date = Earliest_start_date,
        Latest_end_date = Latest_end_date,
        specimen_taken_time = specimen_taken_time,
        timepoint = ifelse(before, 0, (Latest_end_date - Earliest_start_date)/365),
        result = result
      )
    ]
    all_drug_time[[drug_name]] <- drug_time
  }
  rm(drug_time)
  T1D_drug_time <- rbindlist(all_drug_time, use.names = TRUE, fill = TRUE)
  T1D_drug_time <- as.data.frame(T1D_drug_time)
})

T1D_demo <- read.csv("./intermediate_data/processed_data/T1D_demo.csv")
T1D_demo <- subset(T1D_demo, patient_id %in% T1D_patients)
T1D_drug_time$kidney_disease <- 0
T1D_drug_time$ketoacidosis <- 0
for(i in 1:nrow(T1D_drug_time)){
  latest_date <- T1D_drug_time[i,"Latest_end_date"]
  patient_id <- T1D_drug_time[i,"patient_id"]
  if(patient_id %in% T1D_DKA_patient$patient_id && T1D_DKA_patient[T1D_DKA_patient$patient_id == patient_id, "diagnosis_date"] <= latest_date){
    T1D_drug_time[i, "ketoacidosis"] <- 1
  }
  if(patient_id %in% T1D_DKD_patient$patient_id && T1D_DKD_patient[T1D_DKD_patient$patient_id == patient_id, "diagnosis_date"] <= latest_date){
    T1D_drug_time[i, "kidney_disease"] <- 1
  }
}
T1D_demo$age_group <- cut(
  T1D_demo$age,
  breaks = c(-Inf, 18, 30, 40, 50, 65, Inf),
  labels = c("<18", "18-29", "30-39", "40-49", "50-64", ">64"),
  right = FALSE
)
T1D_demo$age_group <- factor(T1D_demo$age_group,
                             levels = c("40-49", "<18", "18-29", "30-39", "50-64", ">64"))
T1D_demo <- subset(T1D_demo, sex %in% c("Female", "Male"))
T1D_demo$sex <- factor(T1D_demo$sex, levels = c("Femal", "Male"))
levels(T1D_demo$sex) <- c("F", "M")
T1D_demo <- T1D_demo[,c("patient_id", "sex", "age_group")]
T1D_drug_time <- merge(T1D_drug_time, T1D_demo, by.x = "patient_id", by.y = "patient_id", all = FALSE)

write.csv(T1D_drug_time, paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                "_30day.period_halfyr.range_1measure_drug_time_ATC_drug.csv"), row.names = FALSE)

################################################################################
#### Running paired LMM ########################################################
################################################################################

drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[num_drug]

## ATC_class
T1D_drug_time <- read.csv(paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                 "_30day.period_halfyr.range_1measure_drug_time_ATC_class.csv"))

paired_LMM_Result <- data.frame(ATC_class = character(0), t_score = numeric(0), estimate = numeric(0),
                                p_value = numeric(0), p_adj = numeric(0), ci_lower = numeric(0), ci_upper = numeric(0),
                                num_on_drug = numeric(0))
for (drug in unique(T1D_drug_time$ATC_class)) {
  print(drug)
  T1D_drug_time_selected <- T1D_drug_time[T1D_drug_time$ATC_class == drug,]
  if (length(unique(T1D_drug_time_selected$patient_id)) >= 10){
    vars <- c("sex", "age_group", "kidney_disease", "ketoacidosis")
    valid_vars <- vars[sapply(T1D_drug_time_selected[, vars],
                              function(x) length(unique(na.omit(x))) > 1)]
    formula_str <- paste(
      "result ~ timepoint +",
      paste(valid_vars, collapse = " + "),
      "+ (1 | patient_id)"
    )
    model <- lmer(as.formula(formula_str),
                  data = T1D_drug_time_selected)
    
    coefficient <- summary(model)$coefficients
    estimate <- coefficient["timepoint", "Estimate"]
    SE <- coefficient["timepoint", "Std. Error"]
    t_score <- coefficient["timepoint", "t value"]
    p_value   <- coefficient["timepoint", "Pr(>|t|)"]
    df <- coefficient["timepoint", "df"]
    num_on_drug <- length(unique(T1D_drug_time_selected$patient_id))
    
    if (estimate < 0) {
      p_value <- p_value / 2
    } else {
      p_value <- 1 - p_value / 2
    }
    
    tcrit <- qt(0.95, df)
    lower_ci <- -Inf
    upper_ci <- estimate + tcrit * SE
    
    summary <- setNames(c(drug, t_score, estimate, p_value, NA, lower_ci, upper_ci, num_on_drug),
                        c("ATC_class", "t_score", "estimate", "p_value", "p_adj", "ci_lower", "ci_upper", "num_on_drug"))
    paired_LMM_Result <- rbind(paired_LMM_Result, summary)
  }
}
colnames(paired_LMM_Result) <- c("ATC_class", "t_score", "estimate", "p_value", "p_adj", "ci_lower", "ci_upper", "num_on_drug")
for(column in 2:ncol(paired_LMM_Result)){
  paired_LMM_Result[, column] <- as.numeric(paired_LMM_Result[, column])
}
paired_LMM_Result$p_adj <- p.adjust(paired_LMM_Result$p_value, method = "bonferroni")
T1D_medication <- read.csv(paste0("./intermediate_data/processed_data/T1D_medication_", drug_classification, ".csv"))
T1D_medication <- unique(T1D_medication[,c("ATC_class", "Drug_class_name")])
paired_LMM_Result <- merge(T1D_medication, paired_LMM_Result, by = "ATC_class", all.y = TRUE)
paired_LMM_Result <- paired_LMM_Result[order(paired_LMM_Result$p_value), ]
write.csv(paired_LMM_Result, paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                    "_30day.period_halfyr.range_1measure_results_ATC_class.csv"), row.names = FALSE)

## ATC_drug
T1D_drug_time <- read.csv(paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                 "_30day.period_halfyr.range_1measure_drug_time_ATC_drug.csv"))

paired_LMM_Result <- data.frame(ATC_drug = character(0), t_score = numeric(0), estimate = numeric(0),
                                p_value = numeric(0), p_adj = numeric(0), ci_lower = numeric(0), ci_upper = numeric(0),
                                num_on_drug = numeric(0))
for (drug in unique(T1D_drug_time$ATC_drug)) {
  print(drug)
  T1D_drug_time_selected <- T1D_drug_time[T1D_drug_time$ATC_drug == drug,]
  if (length(unique(T1D_drug_time_selected$patient_id)) >= 10){
    vars <- c("sex", "age_group", "kidney_disease", "ketoacidosis")
    valid_vars <- vars[sapply(T1D_drug_time_selected[, vars],
                              function(x) length(unique(na.omit(x))) > 1)]
    formula_str <- paste(
      "result ~ timepoint +",
      paste(valid_vars, collapse = " + "),
      "+ (1 | patient_id)"
    )
    model <- lmer(as.formula(formula_str),
                  data = T1D_drug_time_selected)
    
    coefficient <- summary(model)$coefficients
    estimate <- coefficient["timepoint", "Estimate"]
    SE <- coefficient["timepoint", "Std. Error"]
    t_score <- coefficient["timepoint", "t value"]
    p_value   <- coefficient["timepoint", "Pr(>|t|)"]
    df <- coefficient["timepoint", "df"]
    num_on_drug <- length(unique(T1D_drug_time_selected$patient_id))
    
    if (estimate < 0) {
      p_value <- p_value / 2
    } else {
      p_value <- 1 - p_value / 2
    }
    
    tcrit <- qt(0.95, df)
    lower_ci <- -Inf
    upper_ci <- estimate + tcrit * SE
    
    summary <- setNames(c(drug, t_score, estimate, p_value, NA, lower_ci, upper_ci, num_on_drug),
                        c("ATC_drug", "t_score", "estimate", "p_value", "p_adj", "ci_lower", "ci_upper", "num_on_drug"))
    paired_LMM_Result <- rbind(paired_LMM_Result, summary)
  }
}
colnames(paired_LMM_Result) <- c("ATC_drug", "t_score", "estimate", "p_value", "p_adj", "ci_lower", "ci_upper", "num_on_drug")
for(column in 2:ncol(paired_LMM_Result)){
  paired_LMM_Result[, column] <- as.numeric(paired_LMM_Result[, column])
}
paired_LMM_Result$p_adj <- p.adjust(paired_LMM_Result$p_value, method = "bonferroni")
T1D_medication <- read.csv(paste0("./intermediate_data/processed_data/T1D_medication_", drug_classification, ".csv"))
T1D_medication <- unique(T1D_medication[,c("ATC_drug", "Drug_name")])
paired_LMM_Result <- merge(T1D_medication, paired_LMM_Result, by = "ATC_drug", all.y = TRUE)
paired_LMM_Result <- paired_LMM_Result[order(paired_LMM_Result$p_value), ]
write.csv(paired_LMM_Result, paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                    "_30day.period_halfyr.range_1measure_results_ATC_drug.csv"), row.names = FALSE)

################################################################################
#### Evaluating significance ###################################################
################################################################################

classification <- c("drug", "class")[1]

num_drug <- 2
drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[num_drug]

paired_LMM_Result_1yr <- read.csv(paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", drug_classification,
                                         "_30day.period_365day.range_1measure_results_ATC_", classification, ".csv"))

paired_LMM_Result_2yr <- read.csv(paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                         "_30day.period_2yr.range_1measure_results_ATC_", classification, ".csv"))
paired_LMM_Result_halfyr <- read.csv(paste0("./intermediate_data/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                                            "_30day.period_halfyr.range_1measure_results_ATC_", classification, ".csv"))

paired_LMM_Result_1yr <- paired_LMM_Result_1yr[paired_LMM_Result_1yr$p_value < 0.05, paste0("ATC_", classification)]
paired_LMM_Result_2yr <- paired_LMM_Result_2yr[paired_LMM_Result_2yr$p_value < 0.05, paste0("ATC_", classification)]
paired_LMM_Result_halfyr <- paired_LMM_Result_halfyr[paired_LMM_Result_halfyr$p_value < 0.05, paste0("ATC_", classification)]

sum(paired_LMM_Result_2yr %in% paired_LMM_Result_1yr) / length(paired_LMM_Result_1yr)
sum(paired_LMM_Result_halfyr %in% paired_LMM_Result_1yr) / length(paired_LMM_Result_1yr)
