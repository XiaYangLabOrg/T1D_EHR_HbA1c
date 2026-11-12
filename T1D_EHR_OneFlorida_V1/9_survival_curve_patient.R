
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/")

drug_classification <- "T2D"
drug_selection <- 2
if(drug_classification == "T2D"){
  drug <- c("A10BJ05", "A10BK03", "A10BA02", "A10BH01", "A10BG03", "A10BB12")[drug_selection]
}
drug_reference <- read.csv(file = paste0("../T1D_EMR_fuzzy_search/", drug_classification, "/", drug_classification, "_drugs.csv"))
drug_name <- drug_reference[drug_reference$ATC_code == drug, "Drug_name", drop = TRUE][1]

DKA_diag_min <- read.csv(paste0("./intermediate_data/processed_data_stringent/T1D_diagnosis_DKA_1st.csv"))
T1D_medication_T2D <- read.csv("./intermediate_data/processed_data_stringent/T1D_medication_T2D.csv")

for(time in c("0_2", "2_5")){
  corrected <- c()
  patients_drug <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient_all/", 
                                  drug, "_", drug_name, "_patients_drug_", time, ".RDS"))
  for(patient in patients_drug){
    DKA_diag_temp <- DKA_diag_min[DKA_diag_min$patient_id == patient, "diagnosis_date"]
    T1D_medication_temp <- subset(T1D_medication_T2D, patient_id == patient & ATC_drug == drug)
    if(length(DKA_diag_temp) != 0){
      if (min(T1D_medication_temp$start_date, na.rm = TRUE) < DKA_diag_temp){
        corrected <- c(corrected, patient)
      }
    } else {
      corrected <- c(corrected, patient)
    }
  }
  saveRDS(corrected, paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
                            drug, "_", drug_name, "_patients_drug_", time, ".RDS"))
  print(length(corrected))
}

# 
# library(dplyr)
# setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/")
# 
# drug_classification <- "T2D"
# drug_selection <- 1
# if(drug_classification == "T2D"){
#   drug <- c("A10BJ05", "A10BK03", "A10BA02", "A10BH01", "A10BG03", "A10BB12")[drug_selection]
# }
# drug_reference <- read.csv(file = paste0("../T1D_EMR_fuzzy_search/", drug_classification, "/", drug_classification, "_drugs.csv"))
# drug_name <- drug_reference[drug_reference$ATC_code == drug, "Drug_name", drop = TRUE][1]
# 
# summary <- as.data.frame(matrix(nrow = 3, ncol = 2))
# colnames(summary) <- c("drug_2_5", "drug_0_2")
# rownames(summary) <- c("sex", "age", "baseline_HbA1c")
# 
# T1D_demo <- read.csv("./intermediate_data/processed_data_stringent/T1D_demo.csv")
# T1D_demo$age <- 2023 - T1D_demo$birth_year
# T1D_demo <- subset(T1D_demo, sex %in% c("F", "M") & age != 123)
# 
# T1D_demo$age_group <- cut(
#   T1D_demo$age,
#   breaks = c(-Inf, 30, 40, 50, 65, Inf),
#   labels = c("18-29", "30-39", "40-49", "50-64", ">64"),
#   right = FALSE
# )
# T1D_demo$age_group <- as.character(T1D_demo$age_group)
# 
# patients_drug_2_5 <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
#                                     drug, "_", drug_name, "_patients_drug_2_5.RDS"))
# patients_drug_0_2 <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
#                                     drug, "_", drug_name, "_patients_drug_0_2.RDS"))
# # patients_only_insulin <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
# #                                         drug, "_", drug_name, "_patients_insulin.RDS"))
# 
# patients_only_insulin <- read.csv(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/",
#                                          drug, "_", drug_name, "_balanced_insulin_subset.csv"))
# patients_only_insulin <- unique(patients_only_insulin$patient_id)
# 
# patients_drug_2_5 <- subset(T1D_demo, patient_id %in% patients_drug_2_5)
# patients_drug_0_2 <- subset(T1D_demo, patient_id %in% patients_drug_0_2)
# patients_only_insulin <- subset(T1D_demo, patient_id %in% patients_only_insulin)
# 
# ## test for sex
# test <- chisq.test(table(c(rep("drug", nrow(patients_drug_2_5)), rep("insulin", nrow(patients_only_insulin))), 
#                          c(patients_drug_2_5$sex, patients_only_insulin$sex)),
#                    simulate.p.value = FALSE)
# summary[1, 1] <- test$p.value
# 
# test <- chisq.test(table(c(rep("drug", nrow(patients_drug_0_2)), rep("insulin", nrow(patients_only_insulin))), 
#                          c(patients_drug_0_2$sex, patients_only_insulin$sex)),
#                    simulate.p.value = FALSE)
# summary[1, 2] <- test$p.value
# 
# ## test for age
# # test <- chisq.test(table(c(rep("drug", nrow(patients_drug_2_5)), rep("insulin", nrow(patients_only_insulin))),
# #                          c(patients_drug_2_5$age_group, patients_only_insulin$age_group)),
# #                    simulate.p.value = FALSE)
# # summary[2, 1] <- test$p.value
# 
# test <- t.test(patients_drug_2_5$age, patients_only_insulin$age)
# summary[2, 1] <- test$p.value
# 
# # test <- chisq.test(table(c(rep("drug", nrow(patients_drug_0_2)), rep("insulin", nrow(patients_only_insulin))),
# #                          c(patients_drug_0_2$age_group, patients_only_insulin$age_group)),
# #                    simulate.p.value = FALSE)
# # summary[2, 2] <- test$p.value
# 
# test <- t.test(patients_drug_0_2$age, patients_only_insulin$age)
# summary[2, 2] <- test$p.value
# 
# ## test for baseline HbA1c
# T1D_hba1c <- read.csv("./intermediate_data/processed_data_stringent/T1D_lab_hba1c_percent.csv")
# T1D_hba1c <- T1D_hba1c %>%
#   group_by(patient_id) %>%
#   slice_min(pmin(specimen_taken_time, result_time), n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# patients_drug_2_5 <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
#                                     drug, "_", drug_name, "_patients_drug_2_5.RDS"))
# patients_drug_0_2 <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
#                                     drug, "_", drug_name, "_patients_drug_0_2.RDS"))
# patients_only_insulin <- readRDS(paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/", 
#                                         drug, "_", drug_name, "_patients_insulin.RDS"))
# 
# patients_drug_2_5 <- subset(T1D_hba1c, patient_id %in% patients_drug_2_5)
# patients_drug_0_2 <- subset(T1D_hba1c, patient_id %in% patients_drug_0_2)
# patients_only_insulin <- subset(T1D_hba1c, patient_id %in% patients_only_insulin)
# 
# test <- t.test(patients_drug_2_5$result, patients_only_insulin$result)
# summary[3, 1] <- test$p.value
# 
# test <- t.test(patients_drug_0_2$result, patients_only_insulin$result)
# summary[3, 2] <- test$p.value
# 
# write.csv(summary, paste0("./intermediate_data/graph_additional_stringent/survival_curve_patient/",
#                           drug, "_", drug_name, "_summary.csv"))
# 
