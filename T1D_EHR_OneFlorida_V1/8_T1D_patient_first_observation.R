
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/")

reference_date <- as.Date("2012-01-01")

T1D_patients <- readRDS("./intermediate_data/processed_data_stringent/T1D_patients.RDS")

path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V1/"

################################################################################
####################### process diagnosis file #################################
################################################################################

# diagnosis <- list()
# for(i in 1:4){
#   print(i)
#   diagnosis_df <- read.csv(paste0(path, "DIAGNOSIS_YANG", i, "_V1.csv"))
#   diagnosis_df <- subset(diagnosis_df, ID %in% T1D_patients)
#   diagnosis[[i]] <- diagnosis_df
#   rm(diagnosis_df)
#   gc()
# }
# gc()
# T1D_diagnosis <- do.call(rbind, diagnosis)
# T1D_diagnosis$ADMIT_DATE_OFFSET <- as.Date(T1D_diagnosis$ADMIT_DATE_OFFSET, format = "%d%b%Y")
# T1D_diagnosis$DX_DATE_OFFSET <- as.Date(T1D_diagnosis$DX_DATE_OFFSET, format = "%d%b%Y")
# T1D_diagnosis <- T1D_diagnosis %>%
#   mutate(ADMIT_DATE_OFFSET_RELATIVE = as.numeric(ADMIT_DATE_OFFSET - reference_date),
#          DX_DATE_OFFSET_RELATIVE = as.numeric(DX_DATE_OFFSET - reference_date),
#          diagnosis_date = pmin(as.numeric(ADMIT_DATE_OFFSET - reference_date), as.numeric(DX_DATE_OFFSET - reference_date), na.rm = TRUE))
# T1D_diagnosis <- subset(T1D_diagnosis, ! is.na(diagnosis_date))
# T1D_diagnosis <- T1D_diagnosis %>%
#   group_by(ID) %>%
#   slice_min(diagnosis_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# T1D_diagnosis <- T1D_diagnosis[,c("ID", "diagnosis_date")]
# colnames(T1D_diagnosis) <- c("patient_id", "earliest_date")
# 
# write.csv(T1D_diagnosis, "./intermediate_data/processed_data_stringent/all_observations/diagnosis_1st.csv")

# ################################################################################
# ####################### subsetting medication/prescribing file #################
# ################################################################################
# 
# T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")
# T1D_medication <- subset(T1D_medication, patient_id %in% T1D_patients)
# T1D_medication <- T1D_medication %>%
#   group_by(patient_id) %>%
#   slice_min(start_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# T1D_medication <- T1D_medication[,c("patient_id", "start_date")]
# colnames(T1D_medication) <- c("patient_id", "earliest_date")
# 
# write.csv(T1D_medication, "./intermediate_data/processed_data_stringent/all_observations/medication_1st.csv")

################################################################################
####################### subsetting lab file ####################################
################################################################################

# lab_hba1c <- list()
# for(i in 1:12){
#   print(i)
#   T1D_lab <- read.csv(paste0("./intermediate_data/processed_data/T1D_lab_", i, ".csv"))
#   T1D_lab <- subset(T1D_lab, id %in% T1D_patients)
#   lab_hba1c[[i]] <- T1D_lab
#   rm(T1D_lab)
# }
# T1D_lab <- do.call(rbind, lab_hba1c)
# T1D_lab$earliest_date <- pmin(T1D_lab$lab_order_date_offset_relative, T1D_lab$specimen_date_offset_relative, T1D_lab$result_date_offset_relative, na.rm = TRUE)
# T1D_lab <- T1D_lab %>%
#   group_by(id) %>%
#   slice_min(earliest_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
#   ungroup()
# T1D_lab <- T1D_lab[,c("id", "earliest_date")]
# colnames(T1D_lab) <- c("patient_id", "earliest_date")
# write.csv(T1D_lab, "./intermediate_data/processed_data_stringent/all_observations/lab_1st.csv")
# 

T1D_lab <- read.csv("./intermediate_data/processed_data_stringent/all_observations/lab_1st.csv")
T1D_medication <- read.csv("./intermediate_data/processed_data_stringent/all_observations/medication_1st.csv")
T1D_diagnosis <- read.csv("./intermediate_data/processed_data_stringent/all_observations/diagnosis_1st.csv")

T1D_history <- rbind(T1D_diagnosis, T1D_medication, T1D_lab)
T1D_history <- T1D_history %>%
  group_by(patient_id) %>%
  slice_min(earliest_date, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
  ungroup()

print(nrow(T1D_history))

write.csv(T1D_history, "./intermediate_data/processed_data_stringent/T1D_history_1st.csv", row.names = FALSE)

T1D_history <- read.csv("./intermediate_data/processed_data_stringent/T1D_history_1st.csv")
T1D_diagnosis_1st <- read.csv("./intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st.csv")
T1D_diagnosis_1st <- merge(T1D_history, T1D_diagnosis_1st, by = "patient_id")
T1D_diagnosis_1st <- T1D_diagnosis_1st[T1D_diagnosis_1st$diagnosis_date - T1D_diagnosis_1st$earliest_date >= 365,]
write.csv(T1D_diagnosis_1st, "./intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st_oneyear_record.csv", row.names = FALSE)

