
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/")

reference_date <- as.Date("2012-01-01")

# data preprocessing
# 1. filter T1D patients from medication file and process medication names
# 2. filter T1D patients and hba1c measurements from lab file
# 3. filter T1D patients from demographics file
# 4. find T1D patients with all medication, lab, demographics information
# 5. create a T1D patients hba1c average file

################################################################################
####################### process diagnosis file #################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V1/"

diagnosis <- list()
for(i in 1:4){
  print(i)
  diagnosis_df <- read.csv(paste0(path, "DIAGNOSIS_YANG", i, "_V1.csv"))
  diagnosis_df$DX_TYPE <- as.character(diagnosis_df$DX_TYPE)
  diagnosis_df <- subset(diagnosis_df, (grepl("^250\\.[0-9][13]", DX) & DX_TYPE == "09") |
                           (grepl("^E10", DX) & DX_TYPE == "10"))
  diagnosis[[i]] <- diagnosis_df
  rm(diagnosis_df)
  gc()
}
gc()

T1D_patients <- unique(unlist(lapply(diagnosis, function(df) df$ID)))
print(length(T1D_patients))

T1D_patients<-readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
diagnosis_kidney <- list()
diagnosis_keto <- list()
for(i in 1:4){
  print(i)
  diagnosis_df <- read.csv(paste0(path, "DIAGNOSIS_YANG", i, "_V1.csv"))
  diagnosis_df$DX_TYPE <- as.character(diagnosis_df$DX_TYPE)
  diagnosis_df <- subset(diagnosis_df, ID %in% T1D_patients)
  diagnosis_kidney[[i]] <- subset(diagnosis_df, (grepl("^250\\.4[13]", DX) & DX_TYPE == "09") |
                                    (grepl("^E10\\.2[129]", DX) & DX_TYPE == "10"))
  diagnosis_keto[[i]] <- subset(diagnosis_df, (grepl("^250\\.1[13]", DX) & DX_TYPE == "09") |
                                  (grepl("^E10\\.1[01]", DX) & DX_TYPE == "10"))
  rm(diagnosis_df)
  gc()
}
gc()

kidney <- unique(unlist(lapply(diagnosis_kidney, function(df) df$ID)))
ketoacidosis <- unique(unlist(lapply(diagnosis_keto, function(df) df$ID)))

saveRDS(T1D_patients, "./intermediate_data/processed_data/T1D_patients.RDS")
saveRDS(kidney, "./intermediate_data/processed_data/T1D_DKD_patients.RDS")
saveRDS(ketoacidosis, "./intermediate_data/processed_data/T1D_DKA_patients.RDS")
rm(list = ls())
gc()

################################################################################
####################### subsetting medication/prescribing file #################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V1/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")

## med
med <- list()
for(i in 1:4){
  print(i)
  med_df <- read.csv(paste0(path, "MED_ADMIN_YANG", i, "_V1.csv"))
  med_df <- med_df[med_df$ID %in% T1D_patients, ]
  med[[i]] <- med_df
  rm(med_df)
}
T1D_medication <- do.call(rbind, med)
rm(med)

T1D_medication$MEDADMIN_START_DATE_OFFSET <- as.Date(T1D_medication$MEDADMIN_START_DATE_OFFSET, format = "%d%b%Y")
T1D_medication$MEDADMIN_STOP_DATE_OFFSET <- as.Date(T1D_medication$MEDADMIN_STOP_DATE_OFFSET, format = "%d%b%Y")
T1D_medication <- T1D_medication %>%
  mutate(MEDADMIN_START_DATE_OFFSET_RELATIVE = as.numeric(MEDADMIN_START_DATE_OFFSET - reference_date)) %>%
  mutate(MEDADMIN_STOP_DATE_OFFSET_RELATIVE = as.numeric(MEDADMIN_STOP_DATE_OFFSET - reference_date))
T1D_medication <- T1D_medication[,c("ID", "MEDADMIN_START_DATE_OFFSET_RELATIVE", "MEDADMIN_STOP_DATE_OFFSET_RELATIVE",
                                    "RAW_MEDADMIN_MED_NAME")]
colnames(T1D_medication)[1:4] <- c("patient_id", "start_date", "end_date", "RAW_MEDADMIN_MED_NAME")

## prescribing
prescribing <- list()
for(i in 1:4){
  print(i)
  prescribing_df <- read.csv(paste0(path, "PRESCRIBING_YANG", i, "_V1.csv"))
  prescribing_df <- prescribing_df[prescribing_df$ID %in% T1D_patients, ]
  prescribing[[i]] <- prescribing_df
  rm(prescribing_df)
}
T1D_prescribing <- do.call(rbind, prescribing)
rm(prescribing)

T1D_prescribing$RX_START_DATE_OFFSET <- as.Date(T1D_prescribing$RX_START_DATE_OFFSET, format = "%d%b%Y")
T1D_prescribing$RX_END_DATE_OFFSET <- as.Date(T1D_prescribing$RX_END_DATE_OFFSET, format = "%d%b%Y")
T1D_prescribing <- T1D_prescribing %>%
  mutate(RX_START_DATE_OFFSET_RELATIVE = as.numeric(RX_START_DATE_OFFSET - reference_date)) %>%
  mutate(RX_END_DATE_OFFSET_RELATIVE = as.numeric(RX_END_DATE_OFFSET - reference_date))
T1D_prescribing <- T1D_prescribing[,c("ID", "RX_START_DATE_OFFSET_RELATIVE", "RX_END_DATE_OFFSET_RELATIVE",
                                      "RAW_RX_MED_NAME")]
colnames(T1D_prescribing)[1:4] <- c("patient_id", "start_date", "end_date", "RAW_MEDADMIN_MED_NAME")

T1D_medication <- rbind(T1D_medication, T1D_prescribing)
rm(T1D_prescribing)
write.csv(T1D_medication, "./intermediate_data/processed_data/T1D_medication.csv", row.names = FALSE)

T1D_medication$RAW_MEDADMIN_MED_NAME <- toupper(T1D_medication$RAW_MEDADMIN_MED_NAME)
T1D_medication$RAW_MEDADMIN_MED_NAME_general <- sub("([0-9]+.*)", "", T1D_medication$RAW_MEDADMIN_MED_NAME)
T1D_medication$RAW_MEDADMIN_MED_NAME_general <- ifelse(grepl("^[^A-Za-z]", T1D_medication$RAW_MEDADMIN_MED_NAME_general) & grepl("[A-Za-z]", T1D_medication$RAW_MEDADMIN_MED_NAME_general),
                                            T1D_medication$RAW_MEDADMIN_MED_NAME_general,
                                            sub("\\s\\d+.*", "", T1D_medication$RAW_MEDADMIN_MED_NAME_general))
T1D_medication <- T1D_medication[!grepl("^\\d+(\\D?\\d+)*$", T1D_medication$RAW_MEDADMIN_MED_NAME_general), ]
T1D_medication$RAW_MEDADMIN_MED_NAME_general <- trimws(T1D_medication$RAW_MEDADMIN_MED_NAME_general)
T1D_medication$RAW_MEDADMIN_MED_NAME_general <- toupper(T1D_medication$RAW_MEDADMIN_MED_NAME_general)

T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ATORVASTATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general) &
                                              !grepl("ATORVASTATIN[-]|[-]ATORVASTATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ATORVASTATIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METFORMIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general) &
                                              !grepl("METFORMIN[-]|[-]METFORMIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METFORMIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FLUCONAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FLUCONAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FLUTICASONE PROPIONATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FLUTICASONE PROPIONATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PANTOPRAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PANTOPRAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VITAMIN D", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VITAMIN D"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VITAMIN B", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VITAMIN B"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MIDAZOLAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MIDAZOLAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFAZOLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFAZOLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMLODIPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMLODIPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LIDOCAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LIDOCAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FUROSEMIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FUROSEMIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PREDNISONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PREDNISONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OMEPRAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OMEPRAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TACROLIMUS", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TACROLIMUS"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROPOFOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROPOFOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CLONIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CLONIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DIAZEPAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DIAZEPAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MELATONIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MELATONIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FLUOXETINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FLUOXETINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ARIPIPRAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ARIPIPRAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ACETAMINOPHEN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ACETAMINOPHEN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SEMAGLUTIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SEMAGLUTIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ASPIRIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ASPIRIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TYLENOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TYLENOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ERGOCALCIFEROL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ERGOCALCIFEROL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFEPIME", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFEPIME"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GABAPENTIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GABAPENTIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LOPERAMIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LOPERAMIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LABETALOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LABETALOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FENTANYL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FENTANYL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALBUMIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general) & grepl("HUMAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <-
  "ALBUMIN HUMAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ACETAZOLAMIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ACETAZOLAMIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ACETYLCYSTEINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ACETYLCYSTEINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ACYCLOVIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ACYCLOVIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ADENOSINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ADENOSINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALBUTEROL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ALBUTEROL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALCOHOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ALCOHOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALLOPURINOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ALLOPURINOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALPRAZOLAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ALPRAZOLAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ALTEPLASE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ALTEPLASE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMINOCAPROIC ACID", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMINOCAPROIC ACID"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMIODARONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMIODARONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMITRIPTYLINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMITRIPTYLINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMOXICILLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMOXICILLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMPHOTERICIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMPHOTERICIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AMPICILLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AMPICILLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ASCORBIC ACID", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ASCORBIC ACID"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ATROPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ATROPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AZITHROMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AZITHROMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("AZTREONAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "AZTREONAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BACITRACIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BACITRACIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BACLOFEN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BACLOFEN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BENAZEPRIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BENAZEPRIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BENZOCAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BENZOCAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BETAMETHASONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BETAMETHASONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BEVACIZUMAB", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BEVACIZUMAB"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BISACODYL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BISACODYL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BRIMONIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BRIMONIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUDESONIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUDESONIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUMETANIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUMETANIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUPIVACAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUPIVACAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUPRENORPHINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUPRENORPHINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUPROPION", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUPROPION"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("BUSPIRONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "BUSPIRONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CAFFEINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CAFFEINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CALCIUM ACETATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CALCIUM ACETATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CALCIUM CARBONATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CALCIUM CARBONATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CALCIUM CHLORIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CALCIUM CHLORIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CALCIUM GLUCONATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CALCIUM GLUCONATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CAPSAICIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CAPSAICIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CARBIDOPA", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CARBIDOPA"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CARBOPLATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CARBOPLATIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CARVEDILOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CARVEDILOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFOTETAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFOTETAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFOXITIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFOXITIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFTAROLINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFTAROLINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFTAZIDIME", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFTAZIDIME"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFTRIAXONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFTRIAXONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CEFUROXIME", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CEFUROXIME"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CETIRIZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CETIRIZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CHLORHEXIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CHLORHEXIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CHLOROTHIAZIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CHLOROTHIAZIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CHLORPROMAZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CHLORPROMAZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CHOLECALCIFEROL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CHOLECALCIFEROL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CIPROFLOXACIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CIPROFLOXACIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CISATRACURIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CISATRACURIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CLINDAMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CLINDAMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CLONAZEPAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CLONAZEPAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CLOPIDOGREL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CLOPIDOGREL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CLOTRIMAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CLOTRIMAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CODEINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CODEINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("COLLAGENASE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "COLLAGENASE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CYANOCOBALAMIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CYANOCOBALAMIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CYCLOBENZAPRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CYCLOBENZAPRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("CYCLOSPORINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "CYCLOSPORINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DABIGATRAN ETEXILATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DABIGATRAN ETEXILATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DAPTOMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DAPTOMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DARBEPOETIN ALFA", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DARBEPOETIN ALFA"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DESMOPRESSIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DESMOPRESSIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DEXAMETHASONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DEXAMETHASONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DEXMEDETOMIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DEXMEDETOMIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DEXTROSE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DEXTROSE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DICLOFENAC", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DICLOFENAC"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DICYCLOMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DICYCLOMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DILTIAZEM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DILTIAZEM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DIPHENHYDRAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DIPHENHYDRAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DIVALPROEX", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DIVALPROEX"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DOBUTAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DOBUTAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DOCUSATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DOCUSATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DORZOLAMIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DORZOLAMIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DOXYCYCLINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DOXYCYCLINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("DULOXETINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "DULOXETINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ENALAPRIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ENALAPRIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ENOXAPARIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ENOXAPARIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ENALAPRILAT", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ENALAPRILAT"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("EPHEDRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "EPHEDRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("EPINEPHRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "EPINEPHRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("EPOETIN ALFA", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "EPOETIN ALFA"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("EPOPROSTENOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "EPOPROSTENOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ERTAPENEM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ERTAPENEM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ERYTHROMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ERYTHROMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ESTROGENS", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ESTROGENS"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("EZETIMIBE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "EZETIMIBE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FAMOTIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FAMOTIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FENOFIBRATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FENOFIBRATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FENTANYL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FENTANYL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FEXOFENADINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FEXOFENADINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FILGRASTIM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FILGRASTIM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FLUOROURACIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FLUOROURACIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FLUTICASONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FLUTICASONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FORMOTEROL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FORMOTEROL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("FOSPHENYTOIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "FOSPHENYTOIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GANCICLOVIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GANCICLOVIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GENTAMICIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GENTAMICIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GLUCAGON", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GLUCAGON"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GLUCOSE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GLUCOSE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GLYCERIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GLYCERIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("GUAIFENESIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "GUAIFENESIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HALOPERIDOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HALOPERIDOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HEPARIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HEPARIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HUMALOG", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HUMALOG"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDRALAZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDRALAZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDROCHLOROTHIAZIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDROCHLOROTHIAZIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDROCORTISONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDROCORTISONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDROMORPHONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDROMORPHONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDROXYCHLOROQUINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDROXYCHLOROQUINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("HYDROXYZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "HYDROXYZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("IBUPROFEN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "IBUPROFEN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INFLUENZA", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INFLUENZA VACC"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN ASPART", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN ASPART"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN DETEMIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN DETEMIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN GLARGINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN GLARGINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN HUMAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN HUMAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN ISOPHANE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN ISOPHANE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN LISPRO", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN LISPRO"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN NPH", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN NPH"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("INSULIN REGULAR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "INSULIN REGULAR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("IPRATROPIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "IPRATROPIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ISOSORBIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ISOSORBIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("KETAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "KETAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("KETOCONAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "KETOCONAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("KETOROLAC", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "KETOROLAC"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LACTATED RINGERS", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LACTATED RINGERS"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LACTOBACILLUS RHAMNOSUS", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LACTOBACILLUS RHAMNOSUS"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LACTULOSE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LACTULOSE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LANTUS", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LANTUS"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LEVEMIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LEVEMIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LEVETIRACETAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LEVETIRACETAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LEVOFLOXACIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LEVOFLOXACIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LEVONORGESTREL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LEVONORGESTREL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LEVOTHYROXINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LEVOTHYROXINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LISINOPRIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LISINOPRIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LORAZEPAM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LORAZEPAM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("LOSARTAN POTASSIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "LOSARTAN POTASSIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MAGNESIUM CITRATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MAGNESIUM CITRATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MAGNESIUM OXIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MAGNESIUM OXIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MAGNESIUM SULFATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MAGNESIUM SULFATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MECLIZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MECLIZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MEPERIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MEPERIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MEPIVACAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MEPIVACAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MEROPENEM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MEROPENEM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METHOCARBAMOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METHOCARBAMOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METHOTREXATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METHOTREXATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METHYLENE BLUE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METHYLENE BLUE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METHYLPHENIDATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METHYLPHENIDATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METHYLPREDNISOLONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METHYLPREDNISOLONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METOCLOPRAMIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METOCLOPRAMIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METOLAZONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METOLAZONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METOPROLOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METOPROLOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("METRONIDAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "METRONIDAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MILRINONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MILRINONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MORPHINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MORPHINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MULTIVITAMIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MULTIVITAMIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MUPIROCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MUPIROCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("MYCOPHENOLATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "MYCOPHENOLATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NAFCILLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NAFCILLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NALBUPHINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NALBUPHINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NALOXONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NALOXONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NEOMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NEOMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NICARDIPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NICARDIPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NICOTINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NICOTINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NIFEDIPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NIFEDIPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NITROFURANTOIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NITROFURANTOIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NITROGLYCERIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NITROGLYCERIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NOREPINEPHRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NOREPINEPHRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NORETHINDRONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NORETHINDRONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NOVOLOG", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NOVOLOG"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NOVOLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NOVOLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("NYSTATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "NYSTATIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OCTREOTIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OCTREOTIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OFLOXACIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OFLOXACIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OLANZAPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OLANZAPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OLMESARTAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OLMESARTAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OLOPATADINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OLOPATADINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ONDANSETRON", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ONDANSETRON"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ORPHENADRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ORPHENADRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OXYCODONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OXYCODONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OXYMETAZOLINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OXYMETAZOLINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("OXYTOCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "OXYTOCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PANCRELIPASE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PANCRELIPASE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PAPAVERINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PAPAVERINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PAROXETINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PAROXETINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PENICILLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PENICILLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENAZOPYRIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENAZOPYRIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENOBARBITAL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENOBARBITAL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENTERMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENTERMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENYLEPHRINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENYLEPHRINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PHENYTOIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PHENYTOIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PIPERACILLIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PIPERACILLIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PNEUMOCOCCAL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PNEUMOCOCCAL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POLYETHYLENE GLYCOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POLYETHYLENE GLYCOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POLYMYXIN B", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POLYMYXIN B"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POLYVINYL ALCOHOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POLYVINYL ALCOHOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POTASSIUM ACETATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POTASSIUM ACETATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POTASSIUM CHLORIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POTASSIUM CHLORIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("POTASSIUM PHOSPHATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "POTASSIUM PHOSPHATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PRAVASTATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PRAVASTATIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PREDNISOLONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PREDNISOLONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROCHLORPERAZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROCHLORPERAZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROGESTERONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROGESTERONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROMETHAZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROMETHAZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROPRANOLOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROPRANOLOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("PROTAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "PROTAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("QUETIAPINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "QUETIAPINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("RANOLAZINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "RANOLAZINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("REMDESIVIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "REMDESIVIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("RITUXIMAB", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "RITUXIMAB"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ROCURONIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ROCURONIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ROPINIROLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ROPINIROLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ROPIVACAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ROPIVACAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ROSUVASTATIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ROSUVASTATIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SACUBITRIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SACUBITRIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SARS-COV-2 (COVID-19)", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SARS-COV-2 (COVID-19)"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SAXAGLIPTIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SAXAGLIPTIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SCOPOLAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SCOPOLAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SENNOSIDES", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SENNOSIDES"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SERTRALINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SERTRALINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SEVELAMER", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SEVELAMER"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SIMETHICONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SIMETHICONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SITAGLIPTIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SITAGLIPTIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SODIUM BICARBONATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SODIUM BICARBONATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SODIUM CHLORIDE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SODIUM CHLORIDE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SODIUM CITRATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SODIUM CITRATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SODIUM HYALURONATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SODIUM HYALURONATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SODIUM PHOSPHATE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SODIUM PHOSPHATE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SOLIFENACIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SOLIFENACIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SPIRONOLACTONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SPIRONOLACTONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SUCCINYLCHOLINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SUCCINYLCHOLINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SUFENTANIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SUFENTANIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SULFAMETHOXAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SULFAMETHOXAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("SUMATRIPTAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "SUMATRIPTAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TAMSULOSIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TAMSULOSIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TERBUTALINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TERBUTALINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TESTOSTERONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TESTOSTERONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TETRACAINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TETRACAINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("THIAMINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "THIAMINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("THROMBIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "THROMBIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TIMOLOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TIMOLOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TIOTROPIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TIOTROPIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TIZANIDINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TIZANIDINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TOBRAMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TOBRAMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TOLTERODINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TOLTERODINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TRAMADOL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TRAMADOL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TRANEXAMIC ACID", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TRANEXAMIC ACID"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TRAVOPROST", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TRAVOPROST"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TRAZODONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TRAZODONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TRIAMCINOLONE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TRIAMCINOLONE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("TUBERCULIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "TUBERCULIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VALACYCLOVIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VALACYCLOVIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VALGANCICLOVIR", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VALGANCICLOVIR"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VALSARTAN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VALSARTAN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VANCOMYCIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VANCOMYCIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VASOPRESSIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VASOPRESSIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VECURONIUM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VECURONIUM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VENLAFAXINE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VENLAFAXINE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VERAPAMIL", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VERAPAMIL"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VITAMIN C", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VITAMIN C"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VITAMIN E", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VITAMIN E"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("VORICONAZOLE", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "VORICONAZOLE"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("WARFARIN", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "WARFARIN"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ZOLEDRONIC ACID", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ZOLEDRONIC ACID"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ZOLPIDEM", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ZOLPIDEM"
T1D_medication$RAW_MEDADMIN_MED_NAME_general[grepl("ZOSTER", T1D_medication$RAW_MEDADMIN_MED_NAME_general)] <- "ZOSTER VACC"
T1D_medication$RAW_MEDADMIN_MED_NAME_general <- trimws(T1D_medication$RAW_MEDADMIN_MED_NAME_general)

colnames(T1D_medication)[1:5] <- c("patient_id", "start_date", "end_date", "medication_name", "medication_name_general")
# T1D_medication <- T1D_medication[! T1D_medication$end_date %in% "2041-11-22",]
# T1D_medication <- T1D_medication[as.numeric(substr(T1D_medication$MEDADMIN_START_DATE_OFFSET, 1, 4)) >= 2011, ]
# T1D_medication$end_date <-
#   ifelse(T1D_medication$end_date < combined_med_df$start_date,
#          NA, T1D_medication$end_date)
T1D_medication <- T1D_medication[T1D_medication$medication_name != "", ]
write.csv(T1D_medication, "./intermediate_data/processed_data/T1D_medication.csv", row.names = FALSE)

################################################################################
####################### subsetting lab file ####################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V1/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")

for(i in 10:12){
  print(i)
  lab_df <- read.csv(paste0(path, "LAB_RESULT_YANG", i, "_V1.csv"))
  T1D_lab <- lab_df[lab_df$ID %in% T1D_patients, ]
  rm(lab_df)
  T1D_lab$LAB_ORDER_DATE_OFFSET <- as.Date(T1D_lab$LAB_ORDER_DATE_OFFSET, format = "%d%b%Y")
  T1D_lab$SPECIMEN_DATE_OFFSET <- as.Date(T1D_lab$SPECIMEN_DATE_OFFSET, format = "%d%b%Y")
  T1D_lab$RESULT_DATE_OFFSET <- as.Date(T1D_lab$RESULT_DATE_OFFSET, format = "%d%b%Y")
  T1D_lab <- T1D_lab %>%
    mutate(LAB_ORDER_DATE_OFFSET_RELATIVE = as.numeric(LAB_ORDER_DATE_OFFSET - reference_date)) %>%
    mutate(SPECIMEN_DATE_OFFSET_RELATIVE = as.numeric(SPECIMEN_DATE_OFFSET - reference_date)) %>%
    mutate(RESULT_DATE_OFFSET_RELATIVE = as.numeric(RESULT_DATE_OFFSET - reference_date))
  colnames(T1D_lab) <- tolower(colnames(T1D_lab))
  write.csv(T1D_lab, paste0("./intermediate_data/processed_data/T1D_lab_", i, ".csv"), row.names = FALSE)

  T1D_lab <- T1D_lab[T1D_lab$lab_loinc %in% c("55454-3", "43150-2", "41995-2", "86910-7",
                                              "4548-4", "71875-9", "17855-8", "96595-4",
                                              "4549-2", "17856-6", "62388-4","59261-8"), ]
  write.csv(T1D_lab, paste0("./intermediate_data/processed_data/T1D_lab_hba1c_", i, ".csv"), row.names = FALSE)
  gc()
}

lab_hba1c <- list()
for(i in 1:12){
  T1D_lab_hba1c <- read.csv(paste0("./intermediate_data/processed_data/T1D_lab_hba1c_", i, ".csv"))
  lab_hba1c[[i]] <- T1D_lab_hba1c
  rm(T1D_lab_hba1c)
}
T1D_hba1c_percent <- do.call(rbind, lab_hba1c) ## 527430 lines
T1D_hba1c_percent <- subset(T1D_hba1c_percent, result_qual != "ABNORMAL") ## 517643 lines
T1D_hba1c_percent <- subset(T1D_hba1c_percent, ! result_unit %in% c("mg/dL", "g/dL", "umol/L")) ## 517613 lines

T1D_hba1c_percent$result_num <- ifelse(is.na(T1D_hba1c_percent$result_num),
                                       T1D_hba1c_percent$raw_result,
                                       T1D_hba1c_percent$result_num)
T1D_hba1c_percent <- subset(T1D_hba1c_percent, result_num != "") ## 510479 lines

T1D_hba1c_percent$result_num <- sub("%.*", "", T1D_hba1c_percent$result_num)
T1D_hba1c_percent$result_num <- sub("\\+.*", "", T1D_hba1c_percent$result_num)
T1D_hba1c_percent$result_num <- trimws(T1D_hba1c_percent$result_num)
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 18.5", "result_num"] <- "18.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 14.8", "result_num"] <- "14.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 14.6", "result_num"] <- "14.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 15.6", "result_num"] <- "15.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "greater than 15", "result_num"] <- "15.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "greater than 14", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 14", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "Greater than 15.6", "result_num"] <- "15.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">14", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "10.1.", "result_num"] <- "10.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<3.8", "result_num"] <- "3.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.9", "result_num"] <- "17.0"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">18.1", "result_num"] <- "18.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">=14", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">>14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "=>14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">=14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">18.6", "result_num"] <- "18.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "> 14.", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "5'1", "result_num"] <- "5.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "8.5.", "result_num"] <- "8.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">18.0", "result_num"] <- "18.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">>14.00", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<14", "result_num"] <- "13.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<=14.0", "result_num"] <- "13.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<3.4", "result_num"] <- "3.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "4.3.", "result_num"] <- "4.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "7.7.", "result_num"] <- "7.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">18.5", "result_num"] <- "18.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">20.1", "result_num"] <- "20.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "> 14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "6..3", "result_num"] <- "6.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<3.7", "result_num"] <- "3.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">17.0", "result_num"] <- "17.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.3", "result_num"] <- "4.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.8", "result_num"] <- "16.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.2", "result_num"] <- "4.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.9", "result_num"] <- "4.8"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.2", "result_num"] <- "5.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.6", "result_num"] <- "4.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.8", "result_num"] <- "4.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.2", "result_num"] <- "5.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.7", "result_num"] <- "4.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.1", "result_num"] <- "5.0"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">14.00", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">15.5", "result_num"] <- "15.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "=>14", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "=> 14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<17.4", "result_num"] <- "17.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "over 14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.7", "result_num"] <- "5.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<6.0", "result_num"] <- "5.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<14.0", "result_num"] <- "13.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.0", "result_num"] <- "4.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.3", "result_num"] <- "5.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">6.9", "result_num"] <- "7.0"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "10..7", "result_num"] <- "10.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "14.0 >", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "assumed 14 or greater", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "8..5", "result_num"] <- "8.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">15.0", "result_num"] <- "15.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "= >14.0", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.0'", "result_num"] <- "3.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.6'", "result_num"] <- "5.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<6.8'", "result_num"] <- "6.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.4'", "result_num"] <- "16.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "13..2'", "result_num"] <- "13.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ".7.4", "result_num"] <- "7.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.0", "result_num"] <- "16.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "7..3", "result_num"] <- "7.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.8", "result_num"] <- "5.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.5'", "result_num"] <- "4.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "9.6.", "result_num"] <- "9.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<3.5", "result_num"] <- "3.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "7.9.", "result_num"] <- "7.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.5", "result_num"] <- "16.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "6/8", "result_num"] <- "6.8"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ".7.5", "result_num"] <- "7.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "11:4", "result_num"] <- "11.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "11.", "result_num"] <- "11.0"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.4", "result_num"] <- "5.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.5", "result_num"] <- "5.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.0", "result_num"] <- "3.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<5.6", "result_num"] <- "5.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<6.8", "result_num"] <- "6.7"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">16.4", "result_num"] <- "16.5"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">15", "result_num"] <- "15.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "13..2", "result_num"] <- "13.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "<4.5", "result_num"] <- "4.4"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == ">14.0t", "result_num"] <- "14.1"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "8.2t", "result_num"] <- "8.2"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "6.3f", "result_num"] <- "6.3"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "6.9t", "result_num"] <- "6.9"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "9.6t", "result_num"] <- "9.6"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "6.0t", "result_num"] <- "6.0"
T1D_hba1c_percent[T1D_hba1c_percent$result_num == "7.0t", "result_num"] <- "7.0"
T1D_hba1c_percent <- subset(T1D_hba1c_percent, result_num != "") ## 510478 lines
T1D_hba1c_percent$result_num <- as.numeric(T1D_hba1c_percent$result_num)
T1D_hba1c_percent <- subset(T1D_hba1c_percent, ! is.na(result_num) & result_num <= 20 & result_num >= 2) ## 510319 lines

T1D_hba1c_percent <- T1D_hba1c_percent[, c("id", "specimen_date_offset_relative", "result_date_offset_relative", "result_num")]
colnames(T1D_hba1c_percent) <- c("patient_id", "specimen_taken_time", "result_time", "result")
write.csv(T1D_hba1c_percent, "./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv", row.names = FALSE)

################################################################################
####################### subsetting demo file ###################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/OneFlorida/V1/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
kidney_demo <- readRDS("./intermediate_data/processed_data/T1D_kidney_patients.RDS")
keto_demo <- readRDS("./intermediate_data/processed_data/T1D_ketoacidosis_patients.RDS")

demo <- read.csv(paste0(path, "DEMOGRAPHIC_YANG_V1.csv"))
T1D_demo <- demo[demo$ID %in% T1D_patients, ]
rm(demo)

T1D_demo$BIRTH_DATE_OFFSET <- as.Date(T1D_demo$BIRTH_DATE_OFFSET, format = "%d%b%Y")
T1D_demo$BIRTH_YEAR <- as.numeric(format(T1D_demo$BIRTH_DATE_OFFSET, "%Y"))
colnames(T1D_demo) <- tolower(colnames(T1D_demo))
T1D_demo$masked_source <- NULL
colnames(T1D_demo)[1] <- "patient_id"
T1D_demo$kidney_disease <- ifelse(T1D_demo$patient_id %in% kidney_demo, 1, 0)
T1D_demo$ketoacidosis <- ifelse(T1D_demo$patient_id %in% keto_demo, 1, 0)
write.csv(T1D_demo, "./intermediate_data/processed_data/T1D_demo.csv", row.names = FALSE)

# sex_percentages <- T1D_demo %>%
#   group_by(sex)%>%
#   summarise(Percentage =n() / nrow(T1D_demo)*100)
# print(sex_percentages)
#
# png("/u/project/xyang123/rainyliu/T1D_EMR_UCLA/output_plot/demographics/histogram.png",
#     width = 8, height = 6, units = "in", res = 300)
# hist(T1D_demo$age, breaks =seq(0, max(T1D_demo$age)+5, by=5),
#      col="skyblue3", border="white",
#      xlab="Age",ylab="Frequency",
#      main= "Ages of UCLA T1D Patients")
# axis(side=1, at=seq(0, max(T1D_demo$age)+10, by=10))
# dev.off()

################################################################################
####################### subset patients with all files #########################
################################################################################

T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")
T1D_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_demo <- read.csv("./intermediate_data/processed_data/T1D_demo.csv")

T1D_patients <- T1D_patients[T1D_patients %in% T1D_medication$patient_id &
                               T1D_patients %in% T1D_hba1c_percent$patient_id &
                               T1D_patients %in% T1D_demo$patient_id]
saveRDS(T1D_patients, "./intermediate_data/processed_data/T1D_patients.RDS")

T1D_medication <- T1D_medication[T1D_medication$patient_id %in% T1D_patients, ]
write.csv(T1D_medication, "./intermediate_data/processed_data/T1D_medication.csv", row.names = FALSE)

T1D_hba1c_percent <- T1D_hba1c_percent[T1D_hba1c_percent$patient_id %in% T1D_patients, ]
write.csv(T1D_hba1c_percent, "./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv", row.names = FALSE)

T1D_demo <- T1D_demo[T1D_demo$patient_id %in% T1D_patients, ]
write.csv(T1D_demo, "./intermediate_data/processed_data/T1D_demo.csv", row.names = FALSE)

################################################################################
####################### create T1D with info ###################################
################################################################################

T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
T1D_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_demo <- read.csv("./intermediate_data/processed_data/T1D_demo.csv")

# T1D_info <- data.frame("patient_id" = T1D_patients, "mean_hba1c" = rep(NA, length(T1D_patients)),
#                        "num_measurement" = rep(NA, length(T1D_patients)))
# for (patient in T1D_info$patient_id) {
#   T1D_info[T1D_info$patient_id == patient, "mean_hba1c"] <- mean(T1D_hba1c_percent[T1D_hba1c_percent$patient_id == patient, "result"])
#   T1D_info[T1D_info$patient_id == patient, "num_measurement"] <- length(T1D_hba1c_percent[T1D_hba1c_percent$patient_id == patient, "result"])
# }

library(dplyr)

T1D_info <- T1D_hba1c_percent %>%
  group_by(patient_id) %>%
  summarise(
    mean_hba1c = mean(result, na.rm = TRUE),
    num_measurement = n()
  ) %>%
  filter(patient_id %in% T1D_patients)

T1D_info <- merge(x = T1D_demo, y = T1D_info, by = "patient_id")
write.csv(T1D_info, "./intermediate_data/processed_data/T1D_lab_hba1c_average.csv", row.names = FALSE)

################################################################################
####################### subsetting medication file based on classification #####
################################################################################

T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")
T1D_medication$medication_name <- trimws(T1D_medication$medication_name)

## T2D
drug_ref_T2D <- read.csv("./intermediate_data/fuzzy_search_reference/OF_T2D_drugs.csv")
drug_ref_T2D <- drug_ref_T2D[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_T2D <- merge(x = T1D_medication, y = drug_ref_T2D, by.x = "medication_name", by.y = "QueryName")
drug_ref_T2D <- drug_ref_T2D[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_T2D)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_T2D, "./intermediate_data/processed_data/T1D_medication_T2D.csv", row.names = FALSE)

## antihypertensive
drug_ref_BP <- read.csv("./intermediate_data/fuzzy_search_reference/OF_antihypertensive_drugs.csv")
drug_ref_BP <- drug_ref_BP[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_BP <- merge(x = T1D_medication, y = drug_ref_BP, by.x = "medication_name", by.y = "QueryName")
drug_ref_BP <- drug_ref_BP[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_BP)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_BP, "./intermediate_data/processed_data/T1D_medication_antihypertensive.csv", row.names = FALSE)

## lipid lowering
drug_ref_lipid <- read.csv("./intermediate_data/fuzzy_search_reference/OF_lipid_modifying_drugs.csv")
drug_ref_lipid <- drug_ref_lipid[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_lipid <- merge(x = T1D_medication, y = drug_ref_lipid, by.x = "medication_name", by.y = "QueryName")
drug_ref_lipid <- drug_ref_lipid[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_lipid)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_lipid, "./intermediate_data/processed_data/T1D_medication_lipid_modifying.csv", row.names = FALSE)

## insulin
drug_ref_insulin <- read.csv("./intermediate_data/fuzzy_search_reference/OF_insulin.csv")
drug_ref_insulin <- drug_ref_insulin[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_insulin <- merge(x = T1D_medication, y = drug_ref_insulin, by.x = "medication_name", by.y = "QueryName")
drug_ref_insulin <- drug_ref_insulin[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_insulin)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_insulin, "./intermediate_data/processed_data/T1D_medication_insulin.csv", row.names = FALSE)


