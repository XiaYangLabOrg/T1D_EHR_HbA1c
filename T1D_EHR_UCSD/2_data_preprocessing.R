
library(dplyr)
library(data.table)
library(readr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/")

# data preprocessing
# 1. filter T1D patients from medication file and process medication names
# 2. filter T1D patients and hba1c measurements from lab file
# 3. filter T1D patients from demographics file
# 4. find T1D patients with all medication, lab, demographics information
# 5. create a T1D patients hba1c average file

################################################################################
#### Subsetting diagnosis file #################################################
################################################################################
path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"

diagnosis <- read.csv(paste0(path, "dx.csv"), sep = "\t")
T1D <- subset(diagnosis, (icd_type == 9 & grepl("^250\\.[0-9][13]$", icd_code)) | (icd_type == 10 & grepl("^E10", icd_code)))
T1D_patients <- unique(T1D$ip_patient_id)

diagnosis.new <- read.csv(paste0(path, "Max=2106-UCLA-V3/MAX-2016-UCLA-dx.V3.csv"), sep = "\t")
T1D.new <- subset(diagnosis.new, (icd_type == 9 & grepl("^250\\.[0-9][13]$", icd_code)) | (icd_type == 10 & grepl("^E10", icd_code)))
T1D_patients.new <- unique(T1D.new$ip_patient_id)

diagnosis <- rbind(diagnosis, diagnosis.new)
T1D_patients <- unique(c(T1D_patients, T1D_patients.new))
saveRDS(T1D_patients, "./intermediate_data/processed_data/T1D_patients.RDS")

rm(list = ls())
gc()

################################################################################
#### Subsetting medication file ################################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")
medication <- fread(paste0(path, "medication.csv"), sep = "\t", header = TRUE)
colnames(medication) <- tolower(colnames(medication))
T1D_medication <- medication[medication$ip_patient_id %in% T1D_patients,]
rm(medication);gc()

T1D_medication$epic_medication_name_general <- sub("([0-9]+.*)", "", T1D_medication$epic_medication_name)
T1D_medication$epic_medication_name_general <- ifelse(grepl("^[^A-Za-z]", T1D_medication$epic_medication_name_general) & grepl("[A-Za-z]", T1D_medication$epic_medication_name_general), 
                                                      T1D_medication$epic_medication_name_general, 
                                                      sub("\\s\\d+.*", "", T1D_medication$epic_medication_name_general))
T1D_medication <- T1D_medication[!grepl("^\\d+(\\D?\\d+)*$", T1D_medication$epic_medication_name_general), ]
T1D_medication$epic_medication_name_general <- trimws(T1D_medication$epic_medication_name_general)
T1D_medication$epic_medication_name_general <- toupper(T1D_medication$epic_medication_name_general)

T1D_medication$epic_medication_name_general[grepl("ATORVASTATIN", T1D_medication$epic_medication_name_general) &
                                              !grepl("ATORVASTATIN[-]|[-]ATORVASTATIN", T1D_medication$epic_medication_name_general)] <- "ATORVASTATIN"
T1D_medication$epic_medication_name_general[grepl("METFORMIN", T1D_medication$epic_medication_name_general) &
                                              !grepl("METFORMIN[-]|[-]METFORMIN", T1D_medication$epic_medication_name_general)] <- "METFORMIN"
T1D_medication$epic_medication_name_general[grepl("FLUCONAZOLE", T1D_medication$epic_medication_name_general)] <- "FLUCONAZOLE"
T1D_medication$epic_medication_name_general[grepl("FLUTICASONE PROPIONATE", T1D_medication$epic_medication_name_general)] <- "FLUTICASONE PROPIONATE"
T1D_medication$epic_medication_name_general[grepl("PANTOPRAZOLE", T1D_medication$epic_medication_name_general)] <- "PANTOPRAZOLE"
T1D_medication$epic_medication_name_general[grepl("VITAMIN D", T1D_medication$epic_medication_name_general)] <- "VITAMIN D"
T1D_medication$epic_medication_name_general[grepl("VITAMIN B", T1D_medication$epic_medication_name_general)] <- "VITAMIN B"
T1D_medication$epic_medication_name_general[grepl("MIDAZOLAM", T1D_medication$epic_medication_name_general)] <- "MIDAZOLAM"
T1D_medication$epic_medication_name_general[grepl("CEFAZOLIN", T1D_medication$epic_medication_name_general)] <- "CEFAZOLIN"
T1D_medication$epic_medication_name_general[grepl("AMLODIPINE", T1D_medication$epic_medication_name_general)] <- "AMLODIPINE"
T1D_medication$epic_medication_name_general[grepl("LIDOCAINE", T1D_medication$epic_medication_name_general)] <- "LIDOCAINE"
T1D_medication$epic_medication_name_general[grepl("FUROSEMIDE", T1D_medication$epic_medication_name_general)] <- "FUROSEMIDE"
T1D_medication$epic_medication_name_general[grepl("PREDNISONE", T1D_medication$epic_medication_name_general)] <- "PREDNISONE"
T1D_medication$epic_medication_name_general[grepl("OMEPRAZOLE", T1D_medication$epic_medication_name_general)] <- "OMEPRAZOLE"
T1D_medication$epic_medication_name_general[grepl("TACROLIMUS", T1D_medication$epic_medication_name_general)] <- "TACROLIMUS"
T1D_medication$epic_medication_name_general[grepl("PROPOFOL", T1D_medication$epic_medication_name_general)] <- "PROPOFOL"
T1D_medication$epic_medication_name_general[grepl("CLONIDINE", T1D_medication$epic_medication_name_general)] <- "CLONIDINE"
T1D_medication$epic_medication_name_general[grepl("DIAZEPAM", T1D_medication$epic_medication_name_general)] <- "DIAZEPAM"
T1D_medication$epic_medication_name_general[grepl("MELATONIN", T1D_medication$epic_medication_name_general)] <- "MELATONIN"
T1D_medication$epic_medication_name_general[grepl("FLUOXETINE", T1D_medication$epic_medication_name_general)] <- "FLUOXETINE"
T1D_medication$epic_medication_name_general[grepl("ARIPIPRAZOLE", T1D_medication$epic_medication_name_general)] <- "ARIPIPRAZOLE"
T1D_medication$epic_medication_name_general[grepl("ACETAMINOPHEN", T1D_medication$epic_medication_name_general)] <- "ACETAMINOPHEN"
T1D_medication$epic_medication_name_general[grepl("SEMAGLUTIDE", T1D_medication$epic_medication_name_general)] <- "SEMAGLUTIDE"
T1D_medication$epic_medication_name_general[grepl("ASPIRIN", T1D_medication$epic_medication_name_general)] <- "ASPIRIN"
T1D_medication$epic_medication_name_general[grepl("TYLENOL", T1D_medication$epic_medication_name_general)] <- "TYLENOL"
T1D_medication$epic_medication_name_general[grepl("ERGOCALCIFEROL", T1D_medication$epic_medication_name_general)] <- "ERGOCALCIFEROL"
T1D_medication$epic_medication_name_general[grepl("CEFEPIME", T1D_medication$epic_medication_name_general)] <- "CEFEPIME"
T1D_medication$epic_medication_name_general[grepl("GABAPENTIN", T1D_medication$epic_medication_name_general)] <- "GABAPENTIN"
T1D_medication$epic_medication_name_general[grepl("LOPERAMIDE", T1D_medication$epic_medication_name_general)] <- "LOPERAMIDE"
T1D_medication$epic_medication_name_general[grepl("LABETALOL", T1D_medication$epic_medication_name_general)] <- "LABETALOL"
T1D_medication$epic_medication_name_general[grepl("FENTANYL", T1D_medication$epic_medication_name_general)] <- "FENTANYL"
T1D_medication$epic_medication_name_general[grepl("ALBUMIN", T1D_medication$epic_medication_name_general) & grepl("HUMAN", T1D_medication$epic_medication_name_general)] <- 
  "ALBUMIN HUMAN"
T1D_medication$epic_medication_name_general[grepl("ACETAZOLAMIDE", T1D_medication$epic_medication_name_general)] <- "ACETAZOLAMIDE"
T1D_medication$epic_medication_name_general[grepl("ACETYLCYSTEINE", T1D_medication$epic_medication_name_general)] <- "ACETYLCYSTEINE"
T1D_medication$epic_medication_name_general[grepl("ACYCLOVIR", T1D_medication$epic_medication_name_general)] <- "ACYCLOVIR"
T1D_medication$epic_medication_name_general[grepl("ADENOSINE", T1D_medication$epic_medication_name_general)] <- "ADENOSINE"
T1D_medication$epic_medication_name_general[grepl("ALBUTEROL", T1D_medication$epic_medication_name_general)] <- "ALBUTEROL"
T1D_medication$epic_medication_name_general[grepl("ALCOHOL", T1D_medication$epic_medication_name_general)] <- "ALCOHOL"
T1D_medication$epic_medication_name_general[grepl("ALLOPURINOL", T1D_medication$epic_medication_name_general)] <- "ALLOPURINOL"
T1D_medication$epic_medication_name_general[grepl("ALPRAZOLAM", T1D_medication$epic_medication_name_general)] <- "ALPRAZOLAM"
T1D_medication$epic_medication_name_general[grepl("ALTEPLASE", T1D_medication$epic_medication_name_general)] <- "ALTEPLASE"
T1D_medication$epic_medication_name_general[grepl("AMINOCAPROIC ACID", T1D_medication$epic_medication_name_general)] <- "AMINOCAPROIC ACID"
T1D_medication$epic_medication_name_general[grepl("AMIODARONE", T1D_medication$epic_medication_name_general)] <- "AMIODARONE"
T1D_medication$epic_medication_name_general[grepl("AMITRIPTYLINE", T1D_medication$epic_medication_name_general)] <- "AMITRIPTYLINE"
T1D_medication$epic_medication_name_general[grepl("AMOXICILLIN", T1D_medication$epic_medication_name_general)] <- "AMOXICILLIN"
T1D_medication$epic_medication_name_general[grepl("AMPHOTERICIN", T1D_medication$epic_medication_name_general)] <- "AMPHOTERICIN"
T1D_medication$epic_medication_name_general[grepl("AMPICILLIN", T1D_medication$epic_medication_name_general)] <- "AMPICILLIN"
T1D_medication$epic_medication_name_general[grepl("ASCORBIC ACID", T1D_medication$epic_medication_name_general)] <- "ASCORBIC ACID"
T1D_medication$epic_medication_name_general[grepl("ATROPINE", T1D_medication$epic_medication_name_general)] <- "ATROPINE"
T1D_medication$epic_medication_name_general[grepl("AZITHROMYCIN", T1D_medication$epic_medication_name_general)] <- "AZITHROMYCIN"
T1D_medication$epic_medication_name_general[grepl("AZTREONAM", T1D_medication$epic_medication_name_general)] <- "AZTREONAM"
T1D_medication$epic_medication_name_general[grepl("BACITRACIN", T1D_medication$epic_medication_name_general)] <- "BACITRACIN"
T1D_medication$epic_medication_name_general[grepl("BACLOFEN", T1D_medication$epic_medication_name_general)] <- "BACLOFEN"
T1D_medication$epic_medication_name_general[grepl("BENAZEPRIL", T1D_medication$epic_medication_name_general)] <- "BENAZEPRIL"
T1D_medication$epic_medication_name_general[grepl("BENZOCAINE", T1D_medication$epic_medication_name_general)] <- "BENZOCAINE"
T1D_medication$epic_medication_name_general[grepl("BETAMETHASONE", T1D_medication$epic_medication_name_general)] <- "BETAMETHASONE"
T1D_medication$epic_medication_name_general[grepl("BEVACIZUMAB", T1D_medication$epic_medication_name_general)] <- "BEVACIZUMAB"
T1D_medication$epic_medication_name_general[grepl("BISACODYL", T1D_medication$epic_medication_name_general)] <- "BISACODYL"
T1D_medication$epic_medication_name_general[grepl("BRIMONIDINE", T1D_medication$epic_medication_name_general)] <- "BRIMONIDINE"
T1D_medication$epic_medication_name_general[grepl("BUDESONIDE", T1D_medication$epic_medication_name_general)] <- "BUDESONIDE"
T1D_medication$epic_medication_name_general[grepl("BUMETANIDE", T1D_medication$epic_medication_name_general)] <- "BUMETANIDE"
T1D_medication$epic_medication_name_general[grepl("BUPIVACAINE", T1D_medication$epic_medication_name_general)] <- "BUPIVACAINE"
T1D_medication$epic_medication_name_general[grepl("BUPRENORPHINE", T1D_medication$epic_medication_name_general)] <- "BUPRENORPHINE"
T1D_medication$epic_medication_name_general[grepl("BUPROPION", T1D_medication$epic_medication_name_general)] <- "BUPROPION"
T1D_medication$epic_medication_name_general[grepl("BUSPIRONE", T1D_medication$epic_medication_name_general)] <- "BUSPIRONE"
T1D_medication$epic_medication_name_general[grepl("CAFFEINE", T1D_medication$epic_medication_name_general)] <- "CAFFEINE"
T1D_medication$epic_medication_name_general[grepl("CALCIUM ACETATE", T1D_medication$epic_medication_name_general)] <- "CALCIUM ACETATE"
T1D_medication$epic_medication_name_general[grepl("CALCIUM CARBONATE", T1D_medication$epic_medication_name_general)] <- "CALCIUM CARBONATE"
T1D_medication$epic_medication_name_general[grepl("CALCIUM CHLORIDE", T1D_medication$epic_medication_name_general)] <- "CALCIUM CHLORIDE"
T1D_medication$epic_medication_name_general[grepl("CALCIUM GLUCONATE", T1D_medication$epic_medication_name_general)] <- "CALCIUM GLUCONATE"
T1D_medication$epic_medication_name_general[grepl("CAPSAICIN", T1D_medication$epic_medication_name_general)] <- "CAPSAICIN"
T1D_medication$epic_medication_name_general[grepl("CARBIDOPA", T1D_medication$epic_medication_name_general)] <- "CARBIDOPA"
T1D_medication$epic_medication_name_general[grepl("CARBOPLATIN", T1D_medication$epic_medication_name_general)] <- "CARBOPLATIN"
T1D_medication$epic_medication_name_general[grepl("CARVEDILOL", T1D_medication$epic_medication_name_general)] <- "CARVEDILOL"
T1D_medication$epic_medication_name_general[grepl("CEFOTETAN", T1D_medication$epic_medication_name_general)] <- "CEFOTETAN"
T1D_medication$epic_medication_name_general[grepl("CEFOXITIN", T1D_medication$epic_medication_name_general)] <- "CEFOXITIN"
T1D_medication$epic_medication_name_general[grepl("CEFTAROLINE", T1D_medication$epic_medication_name_general)] <- "CEFTAROLINE"
T1D_medication$epic_medication_name_general[grepl("CEFTAZIDIME", T1D_medication$epic_medication_name_general)] <- "CEFTAZIDIME"
T1D_medication$epic_medication_name_general[grepl("CEFTRIAXONE", T1D_medication$epic_medication_name_general)] <- "CEFTRIAXONE"
T1D_medication$epic_medication_name_general[grepl("CEFUROXIME", T1D_medication$epic_medication_name_general)] <- "CEFUROXIME"
T1D_medication$epic_medication_name_general[grepl("CETIRIZINE", T1D_medication$epic_medication_name_general)] <- "CETIRIZINE"
T1D_medication$epic_medication_name_general[grepl("CHLORHEXIDINE", T1D_medication$epic_medication_name_general)] <- "CHLORHEXIDINE"
T1D_medication$epic_medication_name_general[grepl("CHLOROTHIAZIDE", T1D_medication$epic_medication_name_general)] <- "CHLOROTHIAZIDE"
T1D_medication$epic_medication_name_general[grepl("CHLORPROMAZINE", T1D_medication$epic_medication_name_general)] <- "CHLORPROMAZINE"
T1D_medication$epic_medication_name_general[grepl("CHOLECALCIFEROL", T1D_medication$epic_medication_name_general)] <- "CHOLECALCIFEROL"
T1D_medication$epic_medication_name_general[grepl("CIPROFLOXACIN", T1D_medication$epic_medication_name_general)] <- "CIPROFLOXACIN"
T1D_medication$epic_medication_name_general[grepl("CISATRACURIUM", T1D_medication$epic_medication_name_general)] <- "CISATRACURIUM"
T1D_medication$epic_medication_name_general[grepl("CLINDAMYCIN", T1D_medication$epic_medication_name_general)] <- "CLINDAMYCIN"
T1D_medication$epic_medication_name_general[grepl("CLONAZEPAM", T1D_medication$epic_medication_name_general)] <- "CLONAZEPAM"
T1D_medication$epic_medication_name_general[grepl("CLOPIDOGREL", T1D_medication$epic_medication_name_general)] <- "CLOPIDOGREL"
T1D_medication$epic_medication_name_general[grepl("CLOTRIMAZOLE", T1D_medication$epic_medication_name_general)] <- "CLOTRIMAZOLE"
T1D_medication$epic_medication_name_general[grepl("CODEINE", T1D_medication$epic_medication_name_general)] <- "CODEINE"
T1D_medication$epic_medication_name_general[grepl("COLLAGENASE", T1D_medication$epic_medication_name_general)] <- "COLLAGENASE"
T1D_medication$epic_medication_name_general[grepl("CYANOCOBALAMIN", T1D_medication$epic_medication_name_general)] <- "CYANOCOBALAMIN"
T1D_medication$epic_medication_name_general[grepl("CYCLOBENZAPRINE", T1D_medication$epic_medication_name_general)] <- "CYCLOBENZAPRINE"
T1D_medication$epic_medication_name_general[grepl("CYCLOSPORINE", T1D_medication$epic_medication_name_general)] <- "CYCLOSPORINE"
T1D_medication$epic_medication_name_general[grepl("DABIGATRAN ETEXILATE", T1D_medication$epic_medication_name_general)] <- "DABIGATRAN ETEXILATE"
T1D_medication$epic_medication_name_general[grepl("DAPTOMYCIN", T1D_medication$epic_medication_name_general)] <- "DAPTOMYCIN"
T1D_medication$epic_medication_name_general[grepl("DARBEPOETIN ALFA", T1D_medication$epic_medication_name_general)] <- "DARBEPOETIN ALFA"
T1D_medication$epic_medication_name_general[grepl("DESMOPRESSIN", T1D_medication$epic_medication_name_general)] <- "DESMOPRESSIN"
T1D_medication$epic_medication_name_general[grepl("DEXAMETHASONE", T1D_medication$epic_medication_name_general)] <- "DEXAMETHASONE"
T1D_medication$epic_medication_name_general[grepl("DEXMEDETOMIDINE", T1D_medication$epic_medication_name_general)] <- "DEXMEDETOMIDINE"
T1D_medication$epic_medication_name_general[grepl("DEXTROSE", T1D_medication$epic_medication_name_general)] <- "DEXTROSE"
T1D_medication$epic_medication_name_general[grepl("DICLOFENAC", T1D_medication$epic_medication_name_general)] <- "DICLOFENAC"
T1D_medication$epic_medication_name_general[grepl("DICYCLOMINE", T1D_medication$epic_medication_name_general)] <- "DICYCLOMINE"
T1D_medication$epic_medication_name_general[grepl("DILTIAZEM", T1D_medication$epic_medication_name_general)] <- "DILTIAZEM"
T1D_medication$epic_medication_name_general[grepl("DIPHENHYDRAMINE", T1D_medication$epic_medication_name_general)] <- "DIPHENHYDRAMINE"
T1D_medication$epic_medication_name_general[grepl("DIVALPROEX", T1D_medication$epic_medication_name_general)] <- "DIVALPROEX"
T1D_medication$epic_medication_name_general[grepl("DOBUTAMINE", T1D_medication$epic_medication_name_general)] <- "DOBUTAMINE"
T1D_medication$epic_medication_name_general[grepl("DOCUSATE", T1D_medication$epic_medication_name_general)] <- "DOCUSATE"
T1D_medication$epic_medication_name_general[grepl("DORZOLAMIDE", T1D_medication$epic_medication_name_general)] <- "DORZOLAMIDE"
T1D_medication$epic_medication_name_general[grepl("DOXYCYCLINE", T1D_medication$epic_medication_name_general)] <- "DOXYCYCLINE"
T1D_medication$epic_medication_name_general[grepl("DULOXETINE", T1D_medication$epic_medication_name_general)] <- "DULOXETINE"
T1D_medication$epic_medication_name_general[grepl("ENALAPRIL", T1D_medication$epic_medication_name_general)] <- "ENALAPRIL"
T1D_medication$epic_medication_name_general[grepl("ENOXAPARIN", T1D_medication$epic_medication_name_general)] <- "ENOXAPARIN"
T1D_medication$epic_medication_name_general[grepl("ENALAPRILAT", T1D_medication$epic_medication_name_general)] <- "ENALAPRILAT"
T1D_medication$epic_medication_name_general[grepl("EPHEDRINE", T1D_medication$epic_medication_name_general)] <- "EPHEDRINE"
T1D_medication$epic_medication_name_general[grepl("EPINEPHRINE", T1D_medication$epic_medication_name_general)] <- "EPINEPHRINE"
T1D_medication$epic_medication_name_general[grepl("EPOETIN ALFA", T1D_medication$epic_medication_name_general)] <- "EPOETIN ALFA"
T1D_medication$epic_medication_name_general[grepl("EPOPROSTENOL", T1D_medication$epic_medication_name_general)] <- "EPOPROSTENOL"
T1D_medication$epic_medication_name_general[grepl("ERTAPENEM", T1D_medication$epic_medication_name_general)] <- "ERTAPENEM"
T1D_medication$epic_medication_name_general[grepl("ERYTHROMYCIN", T1D_medication$epic_medication_name_general)] <- "ERYTHROMYCIN"
T1D_medication$epic_medication_name_general[grepl("ESTROGENS", T1D_medication$epic_medication_name_general)] <- "ESTROGENS"
T1D_medication$epic_medication_name_general[grepl("EZETIMIBE", T1D_medication$epic_medication_name_general)] <- "EZETIMIBE"
T1D_medication$epic_medication_name_general[grepl("FAMOTIDINE", T1D_medication$epic_medication_name_general)] <- "FAMOTIDINE"
T1D_medication$epic_medication_name_general[grepl("FENOFIBRATE", T1D_medication$epic_medication_name_general)] <- "FENOFIBRATE"
T1D_medication$epic_medication_name_general[grepl("FENTANYL", T1D_medication$epic_medication_name_general)] <- "FENTANYL"
T1D_medication$epic_medication_name_general[grepl("FEXOFENADINE", T1D_medication$epic_medication_name_general)] <- "FEXOFENADINE"
T1D_medication$epic_medication_name_general[grepl("FILGRASTIM", T1D_medication$epic_medication_name_general)] <- "FILGRASTIM"
T1D_medication$epic_medication_name_general[grepl("FLUOROURACIL", T1D_medication$epic_medication_name_general)] <- "FLUOROURACIL"
T1D_medication$epic_medication_name_general[grepl("FLUTICASONE", T1D_medication$epic_medication_name_general)] <- "FLUTICASONE"
T1D_medication$epic_medication_name_general[grepl("FORMOTEROL", T1D_medication$epic_medication_name_general)] <- "FORMOTEROL"
T1D_medication$epic_medication_name_general[grepl("FOSPHENYTOIN", T1D_medication$epic_medication_name_general)] <- "FOSPHENYTOIN"
T1D_medication$epic_medication_name_general[grepl("GANCICLOVIR", T1D_medication$epic_medication_name_general)] <- "GANCICLOVIR"
T1D_medication$epic_medication_name_general[grepl("GENTAMICIN", T1D_medication$epic_medication_name_general)] <- "GENTAMICIN"
T1D_medication$epic_medication_name_general[grepl("GLUCAGON", T1D_medication$epic_medication_name_general)] <- "GLUCAGON"
T1D_medication$epic_medication_name_general[grepl("GLUCOSE", T1D_medication$epic_medication_name_general)] <- "GLUCOSE"
T1D_medication$epic_medication_name_general[grepl("GLYCERIN", T1D_medication$epic_medication_name_general)] <- "GLYCERIN"
T1D_medication$epic_medication_name_general[grepl("GUAIFENESIN", T1D_medication$epic_medication_name_general)] <- "GUAIFENESIN"
T1D_medication$epic_medication_name_general[grepl("HALOPERIDOL", T1D_medication$epic_medication_name_general)] <- "HALOPERIDOL"
T1D_medication$epic_medication_name_general[grepl("HEPARIN", T1D_medication$epic_medication_name_general)] <- "HEPARIN"
T1D_medication$epic_medication_name_general[grepl("HUMALOG", T1D_medication$epic_medication_name_general)] <- "HUMALOG"
T1D_medication$epic_medication_name_general[grepl("HYDRALAZINE", T1D_medication$epic_medication_name_general)] <- "HYDRALAZINE"
T1D_medication$epic_medication_name_general[grepl("HYDROCHLOROTHIAZIDE", T1D_medication$epic_medication_name_general)] <- "HYDROCHLOROTHIAZIDE"
T1D_medication$epic_medication_name_general[grepl("HYDROCORTISONE", T1D_medication$epic_medication_name_general)] <- "HYDROCORTISONE"
T1D_medication$epic_medication_name_general[grepl("HYDROMORPHONE", T1D_medication$epic_medication_name_general)] <- "HYDROMORPHONE"
T1D_medication$epic_medication_name_general[grepl("HYDROXYCHLOROQUINE", T1D_medication$epic_medication_name_general)] <- "HYDROXYCHLOROQUINE"
T1D_medication$epic_medication_name_general[grepl("HYDROXYZINE", T1D_medication$epic_medication_name_general)] <- "HYDROXYZINE"
T1D_medication$epic_medication_name_general[grepl("IBUPROFEN", T1D_medication$epic_medication_name_general)] <- "IBUPROFEN"
T1D_medication$epic_medication_name_general[grepl("INFLUENZA", T1D_medication$epic_medication_name_general)] <- "INFLUENZA VACC"
T1D_medication$epic_medication_name_general[grepl("INSULIN ASPART", T1D_medication$epic_medication_name_general)] <- "INSULIN ASPART"
T1D_medication$epic_medication_name_general[grepl("INSULIN DETEMIR", T1D_medication$epic_medication_name_general)] <- "INSULIN DETEMIR"
T1D_medication$epic_medication_name_general[grepl("INSULIN GLARGINE", T1D_medication$epic_medication_name_general)] <- "INSULIN GLARGINE"
T1D_medication$epic_medication_name_general[grepl("INSULIN HUMAN", T1D_medication$epic_medication_name_general)] <- "INSULIN HUMAN"
T1D_medication$epic_medication_name_general[grepl("INSULIN ISOPHANE", T1D_medication$epic_medication_name_general)] <- "INSULIN ISOPHANE"
T1D_medication$epic_medication_name_general[grepl("INSULIN LISPRO", T1D_medication$epic_medication_name_general)] <- "INSULIN LISPRO"
T1D_medication$epic_medication_name_general[grepl("INSULIN NPH", T1D_medication$epic_medication_name_general)] <- "INSULIN NPH"
T1D_medication$epic_medication_name_general[grepl("INSULIN REGULAR", T1D_medication$epic_medication_name_general)] <- "INSULIN REGULAR"
T1D_medication$epic_medication_name_general[grepl("IPRATROPIUM", T1D_medication$epic_medication_name_general)] <- "IPRATROPIUM"
T1D_medication$epic_medication_name_general[grepl("ISOSORBIDE", T1D_medication$epic_medication_name_general)] <- "ISOSORBIDE"
T1D_medication$epic_medication_name_general[grepl("KETAMINE", T1D_medication$epic_medication_name_general)] <- "KETAMINE"
T1D_medication$epic_medication_name_general[grepl("KETOCONAZOLE", T1D_medication$epic_medication_name_general)] <- "KETOCONAZOLE"
T1D_medication$epic_medication_name_general[grepl("KETOROLAC", T1D_medication$epic_medication_name_general)] <- "KETOROLAC"
T1D_medication$epic_medication_name_general[grepl("LACTATED RINGERS", T1D_medication$epic_medication_name_general)] <- "LACTATED RINGERS"
T1D_medication$epic_medication_name_general[grepl("LACTOBACILLUS RHAMNOSUS", T1D_medication$epic_medication_name_general)] <- "LACTOBACILLUS RHAMNOSUS"
T1D_medication$epic_medication_name_general[grepl("LACTULOSE", T1D_medication$epic_medication_name_general)] <- "LACTULOSE"
T1D_medication$epic_medication_name_general[grepl("LANTUS", T1D_medication$epic_medication_name_general)] <- "LANTUS"
T1D_medication$epic_medication_name_general[grepl("LEVEMIR", T1D_medication$epic_medication_name_general)] <- "LEVEMIR"
T1D_medication$epic_medication_name_general[grepl("LEVETIRACETAM", T1D_medication$epic_medication_name_general)] <- "LEVETIRACETAM"
T1D_medication$epic_medication_name_general[grepl("LEVOFLOXACIN", T1D_medication$epic_medication_name_general)] <- "LEVOFLOXACIN"
T1D_medication$epic_medication_name_general[grepl("LEVONORGESTREL", T1D_medication$epic_medication_name_general)] <- "LEVONORGESTREL"
T1D_medication$epic_medication_name_general[grepl("LEVOTHYROXINE", T1D_medication$epic_medication_name_general)] <- "LEVOTHYROXINE"
T1D_medication$epic_medication_name_general[grepl("LISINOPRIL", T1D_medication$epic_medication_name_general)] <- "LISINOPRIL"
T1D_medication$epic_medication_name_general[grepl("LORAZEPAM", T1D_medication$epic_medication_name_general)] <- "LORAZEPAM"
T1D_medication$epic_medication_name_general[grepl("LOSARTAN POTASSIUM", T1D_medication$epic_medication_name_general)] <- "LOSARTAN POTASSIUM"
T1D_medication$epic_medication_name_general[grepl("MAGNESIUM CITRATE", T1D_medication$epic_medication_name_general)] <- "MAGNESIUM CITRATE"
T1D_medication$epic_medication_name_general[grepl("MAGNESIUM OXIDE", T1D_medication$epic_medication_name_general)] <- "MAGNESIUM OXIDE"
T1D_medication$epic_medication_name_general[grepl("MAGNESIUM SULFATE", T1D_medication$epic_medication_name_general)] <- "MAGNESIUM SULFATE"
T1D_medication$epic_medication_name_general[grepl("MECLIZINE", T1D_medication$epic_medication_name_general)] <- "MECLIZINE"
T1D_medication$epic_medication_name_general[grepl("MEPERIDINE", T1D_medication$epic_medication_name_general)] <- "MEPERIDINE"
T1D_medication$epic_medication_name_general[grepl("MEPIVACAINE", T1D_medication$epic_medication_name_general)] <- "MEPIVACAINE"
T1D_medication$epic_medication_name_general[grepl("MEROPENEM", T1D_medication$epic_medication_name_general)] <- "MEROPENEM"
T1D_medication$epic_medication_name_general[grepl("METHOCARBAMOL", T1D_medication$epic_medication_name_general)] <- "METHOCARBAMOL"
T1D_medication$epic_medication_name_general[grepl("METHOTREXATE", T1D_medication$epic_medication_name_general)] <- "METHOTREXATE"
T1D_medication$epic_medication_name_general[grepl("METHYLENE BLUE", T1D_medication$epic_medication_name_general)] <- "METHYLENE BLUE"
T1D_medication$epic_medication_name_general[grepl("METHYLPHENIDATE", T1D_medication$epic_medication_name_general)] <- "METHYLPHENIDATE"
T1D_medication$epic_medication_name_general[grepl("METHYLPREDNISOLONE", T1D_medication$epic_medication_name_general)] <- "METHYLPREDNISOLONE"
T1D_medication$epic_medication_name_general[grepl("METOCLOPRAMIDE", T1D_medication$epic_medication_name_general)] <- "METOCLOPRAMIDE"
T1D_medication$epic_medication_name_general[grepl("METOLAZONE", T1D_medication$epic_medication_name_general)] <- "METOLAZONE"
T1D_medication$epic_medication_name_general[grepl("METOPROLOL", T1D_medication$epic_medication_name_general)] <- "METOPROLOL"
T1D_medication$epic_medication_name_general[grepl("METRONIDAZOLE", T1D_medication$epic_medication_name_general)] <- "METRONIDAZOLE"
T1D_medication$epic_medication_name_general[grepl("MILRINONE", T1D_medication$epic_medication_name_general)] <- "MILRINONE"
T1D_medication$epic_medication_name_general[grepl("MORPHINE", T1D_medication$epic_medication_name_general)] <- "MORPHINE"
T1D_medication$epic_medication_name_general[grepl("MULTIVITAMIN", T1D_medication$epic_medication_name_general)] <- "MULTIVITAMIN"
T1D_medication$epic_medication_name_general[grepl("MUPIROCIN", T1D_medication$epic_medication_name_general)] <- "MUPIROCIN"
T1D_medication$epic_medication_name_general[grepl("MYCOPHENOLATE", T1D_medication$epic_medication_name_general)] <- "MYCOPHENOLATE"
T1D_medication$epic_medication_name_general[grepl("NAFCILLIN", T1D_medication$epic_medication_name_general)] <- "NAFCILLIN"
T1D_medication$epic_medication_name_general[grepl("NALBUPHINE", T1D_medication$epic_medication_name_general)] <- "NALBUPHINE"
T1D_medication$epic_medication_name_general[grepl("NALOXONE", T1D_medication$epic_medication_name_general)] <- "NALOXONE"
T1D_medication$epic_medication_name_general[grepl("NEOMYCIN", T1D_medication$epic_medication_name_general)] <- "NEOMYCIN"
T1D_medication$epic_medication_name_general[grepl("NICARDIPINE", T1D_medication$epic_medication_name_general)] <- "NICARDIPINE"
T1D_medication$epic_medication_name_general[grepl("NICOTINE", T1D_medication$epic_medication_name_general)] <- "NICOTINE"
T1D_medication$epic_medication_name_general[grepl("NIFEDIPINE", T1D_medication$epic_medication_name_general)] <- "NIFEDIPINE"
T1D_medication$epic_medication_name_general[grepl("NITROFURANTOIN", T1D_medication$epic_medication_name_general)] <- "NITROFURANTOIN"
T1D_medication$epic_medication_name_general[grepl("NITROGLYCERIN", T1D_medication$epic_medication_name_general)] <- "NITROGLYCERIN"
T1D_medication$epic_medication_name_general[grepl("NOREPINEPHRINE", T1D_medication$epic_medication_name_general)] <- "NOREPINEPHRINE"
T1D_medication$epic_medication_name_general[grepl("NORETHINDRONE", T1D_medication$epic_medication_name_general)] <- "NORETHINDRONE"
T1D_medication$epic_medication_name_general[grepl("NOVOLOG", T1D_medication$epic_medication_name_general)] <- "NOVOLOG"
T1D_medication$epic_medication_name_general[grepl("NOVOLIN", T1D_medication$epic_medication_name_general)] <- "NOVOLIN"
T1D_medication$epic_medication_name_general[grepl("NYSTATIN", T1D_medication$epic_medication_name_general)] <- "NYSTATIN"
T1D_medication$epic_medication_name_general[grepl("OCTREOTIDE", T1D_medication$epic_medication_name_general)] <- "OCTREOTIDE"
T1D_medication$epic_medication_name_general[grepl("OFLOXACIN", T1D_medication$epic_medication_name_general)] <- "OFLOXACIN"
T1D_medication$epic_medication_name_general[grepl("OLANZAPINE", T1D_medication$epic_medication_name_general)] <- "OLANZAPINE"
T1D_medication$epic_medication_name_general[grepl("OLMESARTAN", T1D_medication$epic_medication_name_general)] <- "OLMESARTAN"
T1D_medication$epic_medication_name_general[grepl("OLOPATADINE", T1D_medication$epic_medication_name_general)] <- "OLOPATADINE"
T1D_medication$epic_medication_name_general[grepl("ONDANSETRON", T1D_medication$epic_medication_name_general)] <- "ONDANSETRON"
T1D_medication$epic_medication_name_general[grepl("ORPHENADRINE", T1D_medication$epic_medication_name_general)] <- "ORPHENADRINE"
T1D_medication$epic_medication_name_general[grepl("OXYCODONE", T1D_medication$epic_medication_name_general)] <- "OXYCODONE"
T1D_medication$epic_medication_name_general[grepl("OXYMETAZOLINE", T1D_medication$epic_medication_name_general)] <- "OXYMETAZOLINE"
T1D_medication$epic_medication_name_general[grepl("OXYTOCIN", T1D_medication$epic_medication_name_general)] <- "OXYTOCIN"
T1D_medication$epic_medication_name_general[grepl("PANCRELIPASE", T1D_medication$epic_medication_name_general)] <- "PANCRELIPASE"
T1D_medication$epic_medication_name_general[grepl("PAPAVERINE", T1D_medication$epic_medication_name_general)] <- "PAPAVERINE"
T1D_medication$epic_medication_name_general[grepl("PAROXETINE", T1D_medication$epic_medication_name_general)] <- "PAROXETINE"
T1D_medication$epic_medication_name_general[grepl("PENICILLIN", T1D_medication$epic_medication_name_general)] <- "PENICILLIN"
T1D_medication$epic_medication_name_general[grepl("PHENAZOPYRIDINE", T1D_medication$epic_medication_name_general)] <- "PHENAZOPYRIDINE"
T1D_medication$epic_medication_name_general[grepl("PHENOBARBITAL", T1D_medication$epic_medication_name_general)] <- "PHENOBARBITAL"
T1D_medication$epic_medication_name_general[grepl("PHENOL", T1D_medication$epic_medication_name_general)] <- "PHENOL"
T1D_medication$epic_medication_name_general[grepl("PHENTERMINE", T1D_medication$epic_medication_name_general)] <- "PHENTERMINE"
T1D_medication$epic_medication_name_general[grepl("PHENYLEPHRINE", T1D_medication$epic_medication_name_general)] <- "PHENYLEPHRINE"
T1D_medication$epic_medication_name_general[grepl("PHENYTOIN", T1D_medication$epic_medication_name_general)] <- "PHENYTOIN"
T1D_medication$epic_medication_name_general[grepl("PIPERACILLIN", T1D_medication$epic_medication_name_general)] <- "PIPERACILLIN"
T1D_medication$epic_medication_name_general[grepl("PNEUMOCOCCAL", T1D_medication$epic_medication_name_general)] <- "PNEUMOCOCCAL"
T1D_medication$epic_medication_name_general[grepl("POLYETHYLENE GLYCOL", T1D_medication$epic_medication_name_general)] <- "POLYETHYLENE GLYCOL"
T1D_medication$epic_medication_name_general[grepl("POLYMYXIN B", T1D_medication$epic_medication_name_general)] <- "POLYMYXIN B"
T1D_medication$epic_medication_name_general[grepl("POLYVINYL ALCOHOL", T1D_medication$epic_medication_name_general)] <- "POLYVINYL ALCOHOL"
T1D_medication$epic_medication_name_general[grepl("POTASSIUM ACETATE", T1D_medication$epic_medication_name_general)] <- "POTASSIUM ACETATE"
T1D_medication$epic_medication_name_general[grepl("POTASSIUM CHLORIDE", T1D_medication$epic_medication_name_general)] <- "POTASSIUM CHLORIDE"
T1D_medication$epic_medication_name_general[grepl("POTASSIUM PHOSPHATE", T1D_medication$epic_medication_name_general)] <- "POTASSIUM PHOSPHATE"
T1D_medication$epic_medication_name_general[grepl("PRAVASTATIN", T1D_medication$epic_medication_name_general)] <- "PRAVASTATIN"
T1D_medication$epic_medication_name_general[grepl("PREDNISOLONE", T1D_medication$epic_medication_name_general)] <- "PREDNISOLONE"
T1D_medication$epic_medication_name_general[grepl("PROCHLORPERAZINE", T1D_medication$epic_medication_name_general)] <- "PROCHLORPERAZINE"
T1D_medication$epic_medication_name_general[grepl("PROGESTERONE", T1D_medication$epic_medication_name_general)] <- "PROGESTERONE"
T1D_medication$epic_medication_name_general[grepl("PROMETHAZINE", T1D_medication$epic_medication_name_general)] <- "PROMETHAZINE"
T1D_medication$epic_medication_name_general[grepl("PROPRANOLOL", T1D_medication$epic_medication_name_general)] <- "PROPRANOLOL"
T1D_medication$epic_medication_name_general[grepl("PROTAMINE", T1D_medication$epic_medication_name_general)] <- "PROTAMINE"
T1D_medication$epic_medication_name_general[grepl("QUETIAPINE", T1D_medication$epic_medication_name_general)] <- "QUETIAPINE"
T1D_medication$epic_medication_name_general[grepl("RANOLAZINE", T1D_medication$epic_medication_name_general)] <- "RANOLAZINE"
T1D_medication$epic_medication_name_general[grepl("REMDESIVIR", T1D_medication$epic_medication_name_general)] <- "REMDESIVIR"
T1D_medication$epic_medication_name_general[grepl("RITUXIMAB", T1D_medication$epic_medication_name_general)] <- "RITUXIMAB"
T1D_medication$epic_medication_name_general[grepl("ROCURONIUM", T1D_medication$epic_medication_name_general)] <- "ROCURONIUM"
T1D_medication$epic_medication_name_general[grepl("ROPINIROLE", T1D_medication$epic_medication_name_general)] <- "ROPINIROLE"
T1D_medication$epic_medication_name_general[grepl("ROPIVACAINE", T1D_medication$epic_medication_name_general)] <- "ROPIVACAINE"
T1D_medication$epic_medication_name_general[grepl("ROSUVASTATIN", T1D_medication$epic_medication_name_general)] <- "ROSUVASTATIN"
T1D_medication$epic_medication_name_general[grepl("SACUBITRIL", T1D_medication$epic_medication_name_general)] <- "SACUBITRIL"
T1D_medication$epic_medication_name_general[grepl("SARS-COV-2 (COVID-19)", T1D_medication$epic_medication_name_general)] <- "SARS-COV-2 (COVID-19)"
T1D_medication$epic_medication_name_general[grepl("SAXAGLIPTIN", T1D_medication$epic_medication_name_general)] <- "SAXAGLIPTIN"
T1D_medication$epic_medication_name_general[grepl("SCOPOLAMINE", T1D_medication$epic_medication_name_general)] <- "SCOPOLAMINE"
T1D_medication$epic_medication_name_general[grepl("SENNOSIDES", T1D_medication$epic_medication_name_general)] <- "SENNOSIDES"
T1D_medication$epic_medication_name_general[grepl("SERTRALINE", T1D_medication$epic_medication_name_general)] <- "SERTRALINE"
T1D_medication$epic_medication_name_general[grepl("SEVELAMER", T1D_medication$epic_medication_name_general)] <- "SEVELAMER"
T1D_medication$epic_medication_name_general[grepl("SIMETHICONE", T1D_medication$epic_medication_name_general)] <- "SIMETHICONE"
T1D_medication$epic_medication_name_general[grepl("SITAGLIPTIN", T1D_medication$epic_medication_name_general)] <- "SITAGLIPTIN"
T1D_medication$epic_medication_name_general[grepl("SODIUM BICARBONATE", T1D_medication$epic_medication_name_general)] <- "SODIUM BICARBONATE"
T1D_medication$epic_medication_name_general[grepl("SODIUM CHLORIDE", T1D_medication$epic_medication_name_general)] <- "SODIUM CHLORIDE"
T1D_medication$epic_medication_name_general[grepl("SODIUM CITRATE", T1D_medication$epic_medication_name_general)] <- "SODIUM CITRATE"
T1D_medication$epic_medication_name_general[grepl("SODIUM HYALURONATE", T1D_medication$epic_medication_name_general)] <- "SODIUM HYALURONATE"
T1D_medication$epic_medication_name_general[grepl("SODIUM PHOSPHATE", T1D_medication$epic_medication_name_general)] <- "SODIUM PHOSPHATE"
T1D_medication$epic_medication_name_general[grepl("SOLIFENACIN", T1D_medication$epic_medication_name_general)] <- "SOLIFENACIN"
T1D_medication$epic_medication_name_general[grepl("SPIRONOLACTONE", T1D_medication$epic_medication_name_general)] <- "SPIRONOLACTONE"
T1D_medication$epic_medication_name_general[grepl("SUCCINYLCHOLINE", T1D_medication$epic_medication_name_general)] <- "SUCCINYLCHOLINE"
T1D_medication$epic_medication_name_general[grepl("SUFENTANIL", T1D_medication$epic_medication_name_general)] <- "SUFENTANIL"
T1D_medication$epic_medication_name_general[grepl("SULFAMETHOXAZOLE", T1D_medication$epic_medication_name_general)] <- "SULFAMETHOXAZOLE"
T1D_medication$epic_medication_name_general[grepl("SUMATRIPTAN", T1D_medication$epic_medication_name_general)] <- "SUMATRIPTAN"
T1D_medication$epic_medication_name_general[grepl("TAMSULOSIN", T1D_medication$epic_medication_name_general)] <- "TAMSULOSIN"
T1D_medication$epic_medication_name_general[grepl("TERBUTALINE", T1D_medication$epic_medication_name_general)] <- "TERBUTALINE"
T1D_medication$epic_medication_name_general[grepl("TESTOSTERONE", T1D_medication$epic_medication_name_general)] <- "TESTOSTERONE"
T1D_medication$epic_medication_name_general[grepl("TETRACAINE", T1D_medication$epic_medication_name_general)] <- "TETRACAINE"
T1D_medication$epic_medication_name_general[grepl("THIAMINE", T1D_medication$epic_medication_name_general)] <- "THIAMINE"
T1D_medication$epic_medication_name_general[grepl("THROMBIN", T1D_medication$epic_medication_name_general)] <- "THROMBIN"
T1D_medication$epic_medication_name_general[grepl("TIMOLOL", T1D_medication$epic_medication_name_general)] <- "TIMOLOL"
T1D_medication$epic_medication_name_general[grepl("TIOTROPIUM", T1D_medication$epic_medication_name_general)] <- "TIOTROPIUM"
T1D_medication$epic_medication_name_general[grepl("TIZANIDINE", T1D_medication$epic_medication_name_general)] <- "TIZANIDINE"
T1D_medication$epic_medication_name_general[grepl("TOBRAMYCIN", T1D_medication$epic_medication_name_general)] <- "TOBRAMYCIN"
T1D_medication$epic_medication_name_general[grepl("TOLTERODINE", T1D_medication$epic_medication_name_general)] <- "TOLTERODINE"
T1D_medication$epic_medication_name_general[grepl("TRAMADOL", T1D_medication$epic_medication_name_general)] <- "TRAMADOL"
T1D_medication$epic_medication_name_general[grepl("TRANEXAMIC ACID", T1D_medication$epic_medication_name_general)] <- "TRANEXAMIC ACID"
T1D_medication$epic_medication_name_general[grepl("TRAVOPROST", T1D_medication$epic_medication_name_general)] <- "TRAVOPROST"
T1D_medication$epic_medication_name_general[grepl("TRAZODONE", T1D_medication$epic_medication_name_general)] <- "TRAZODONE"
T1D_medication$epic_medication_name_general[grepl("TRIAMCINOLONE", T1D_medication$epic_medication_name_general)] <- "TRIAMCINOLONE"
T1D_medication$epic_medication_name_general[grepl("TUBERCULIN", T1D_medication$epic_medication_name_general)] <- "TUBERCULIN"
T1D_medication$epic_medication_name_general[grepl("VALACYCLOVIR", T1D_medication$epic_medication_name_general)] <- "VALACYCLOVIR"
T1D_medication$epic_medication_name_general[grepl("VALGANCICLOVIR", T1D_medication$epic_medication_name_general)] <- "VALGANCICLOVIR"
T1D_medication$epic_medication_name_general[grepl("VALSARTAN", T1D_medication$epic_medication_name_general)] <- "VALSARTAN"
T1D_medication$epic_medication_name_general[grepl("VANCOMYCIN", T1D_medication$epic_medication_name_general)] <- "VANCOMYCIN"
T1D_medication$epic_medication_name_general[grepl("VASOPRESSIN", T1D_medication$epic_medication_name_general)] <- "VASOPRESSIN"
T1D_medication$epic_medication_name_general[grepl("VECURONIUM", T1D_medication$epic_medication_name_general)] <- "VECURONIUM"
T1D_medication$epic_medication_name_general[grepl("VENLAFAXINE", T1D_medication$epic_medication_name_general)] <- "VENLAFAXINE"
T1D_medication$epic_medication_name_general[grepl("VERAPAMIL", T1D_medication$epic_medication_name_general)] <- "VERAPAMIL"
T1D_medication$epic_medication_name_general[grepl("VITAMIN C", T1D_medication$epic_medication_name_general)] <- "VITAMIN C"
T1D_medication$epic_medication_name_general[grepl("VITAMIN E", T1D_medication$epic_medication_name_general)] <- "VITAMIN E"
T1D_medication$epic_medication_name_general[grepl("VORICONAZOLE", T1D_medication$epic_medication_name_general)] <- "VORICONAZOLE"
T1D_medication$epic_medication_name_general[grepl("WARFARIN", T1D_medication$epic_medication_name_general)] <- "WARFARIN"
T1D_medication$epic_medication_name_general[grepl("ZOLEDRONIC ACID", T1D_medication$epic_medication_name_general)] <- "ZOLEDRONIC ACID"
T1D_medication$epic_medication_name_general[grepl("ZOLPIDEM", T1D_medication$epic_medication_name_general)] <- "ZOLPIDEM"
T1D_medication$epic_medication_name_general[grepl("ZOSTER", T1D_medication$epic_medication_name_general)] <- "ZOSTER VACC"
T1D_medication$epic_medication_name_general <- trimws(T1D_medication$epic_medication_name_general)

T1D_medication <- T1D_medication[,c("ip_patient_id", "start_date", "end_date", 
                                    "epic_medication_name", "epic_medication_name_general", 
                                    "medispan_class_name", "medispan_subclass_name")]
colnames(T1D_medication)[1:5] <- c("patient_id", "start_date", "end_date", "medication_name", "medication_name_general")
T1D_medication$end_date <- ifelse(T1D_medication$end_date == "NULL", NA, T1D_medication$end_date)
T1D_medication$end_date <- as.numeric(T1D_medication$end_date)

write.csv(T1D_medication, "./intermediate_data/processed_data/T1D_medication.csv", row.names = FALSE)

################################################################################
#### Subsetting lab file #######################################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")

out_dir <- "./intermediate_data/processed_data/T1D_lab_chunks"
read_tsv_chunked(
  paste0(path, "lab.csv"),
  callback = DataFrameCallback$new(function(chunk, pos) {
    print(pos)
    chunk <- chunk[chunk$ip_patient_id %in% T1D_patients, ]
    if (nrow(chunk) == 0) return(NULL)
    write.csv(
      chunk,
      file.path(out_dir, paste0("chunk_", (pos%/%10000000 + 1), ".tsv")),
      row.names = FALSE
    )
  }),
  chunk_size = 10000000
)

T1D_lab <- read.csv("./intermediate_data/processed_data/T1D_lab_chunks/chunk_1.tsv")
for(i in 2:15){
  print(i)
  temp <- read.csv(paste0("./intermediate_data/processed_data/T1D_lab_chunks/chunk_", i, ".tsv"))
  T1D_lab <- rbind(T1D_lab, temp)
  rm(temp)
}
colnames(T1D_lab) <- tolower(colnames(T1D_lab))
write.csv(T1D_lab, "./intermediate_data/processed_data/T1D_lab.csv")

T1D_hba1c_percent <- T1D_lab[grepl("A1C", T1D_lab$component_name, ignore.case = TRUE),]
T1D_hba1c_percent <- subset(T1D_hba1c_percent, !is.na(result))
T1D_hba1c_percent <- subset(T1D_hba1c_percent, ! reference_unit %in% c("mg/dL", "mg/dL (calc)"))
T1D_hba1c_percent <- subset(T1D_hba1c_percent, ! result %in% c("QNS", "NOT AVAILABLE", "CANCELED", "CANCELLED", "See Note", "INVALID",
                                                               "We are unable to determine hemoglobin A1C because of the presence of an abnormal hemoglobin variant. This specimen has been sent to an outside reference laboratory for determination of total glycated hemoglobin. See a separate report."))
T1D_hba1c_percent$result <- gsub("%", "", T1D_hba1c_percent$result)
T1D_hba1c_percent$result <- gsub("\\+", "", T1D_hba1c_percent$result)
T1D_hba1c_percent$result <- gsub("h|H", "", T1D_hba1c_percent$result)
T1D_hba1c_percent$result <- gsub("\\*", "", T1D_hba1c_percent$result)
T1D_hba1c_percent$result <- trimws(T1D_hba1c_percent$result)
T1D_hba1c_percent <- T1D_hba1c_percent[!is.na(T1D_hba1c_percent$result),]
T1D_hba1c_percent[T1D_hba1c_percent$result=="<3.6", "result"] <- 3.5
T1D_hba1c_percent[T1D_hba1c_percent$result=="<4.2", "result"] <- 4.1
T1D_hba1c_percent[T1D_hba1c_percent$result==">14", "result"] <- 14.1
T1D_hba1c_percent[T1D_hba1c_percent$result==">14.0", "result"] <- 14.1
T1D_hba1c_percent[T1D_hba1c_percent$result==">16.0", "result"] <- 16.1
T1D_hba1c_percent[T1D_hba1c_percent$result==">16.5", "result"] <- 16.6
T1D_hba1c_percent[T1D_hba1c_percent$result==">18.9", "result"] <- 19.0
T1D_hba1c_percent[T1D_hba1c_percent$result==">20.1", "result"] <- 20.2
T1D_hba1c_percent[T1D_hba1c_percent$result==">15.0", "result"] <- 15.1
T1D_hba1c_percent[T1D_hba1c_percent$result==">15.5", "result"] <- 15.6
T1D_hba1c_percent[T1D_hba1c_percent$result==">25.0", "result"] <- 25.1
T1D_hba1c_percent$result <- as.numeric(T1D_hba1c_percent$result)
T1D_hba1c_percent <- subset(T1D_hba1c_percent, !is.na(result) & result <= 20)

T1D_hba1c_percent <- T1D_hba1c_percent[, c("ip_patient_id", "specimen_taken_time", "result_time", "result")]
colnames(T1D_hba1c_percent) <- c("patient_id", "specimen_taken_time", "result_time", "result")
write.csv(T1D_hba1c_percent, "./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv", row.names = FALSE)

################################################################################
#### Subsetting demo file ######################################################
################################################################################

path <- "/u/project/xyang123/shared/datasets/EHR_data/UCSD/"
T1D_patients <- readRDS("./intermediate_data/processed_data/T1D_patients.RDS")

demo <- read.csv(paste0(path, "demo.csv"), sep = "\t")
demo_new <- read.csv(paste0(path, "Max=2106-UCLA-V3/MAX-2016-UCLA-demo.V3.csv"), sep = "\t")
demo <- unique(rbind(demo, demo_new))
colnames(demo) <- tolower(colnames(demo))

T1D_demo <- demo[demo$ip_patient_id %in% T1D_patients,]
T1D_demo[T1D_demo$age == ">89", "age"] <- 90
T1D_demo$age <- as.numeric(T1D_demo$age)
T1D_demo <- T1D_demo[order(T1D_demo$ip_patient_id, T1D_demo$age, decreasing = TRUE),]
T1D_demo <- T1D_demo %>%
  group_by(ip_patient_id) %>%
  slice(1) %>%
  ungroup()
rm(demo, demo_new)

## add ketoacidosis and kidney disease
kidney_demo <- diagnosis[grepl("(^250.4[13]|^E10\\.2[129])", diagnosis$icd_code), , drop = FALSE]
kidney_demo <- unique(kidney_demo$ip_patient_id)
T1D_demo$kidney_disease <- ifelse(T1D_demo$ip_patient_id %in% kidney_demo, 1, 0)

keto_demo <- diagnosis[grepl("(^250.1[13]|^E10\\.1[01])", diagnosis$icd_code),, 
                       drop = FALSE]
keto_demo <- unique(keto_demo$ip_patient_id)
T1D_demo$ketoacidosis <- ifelse(T1D_demo$ip_patient_id %in% keto_demo, 1, 0)

colnames(T1D_demo)[1] <- "patient_id"
write.csv(T1D_demo, "./intermediate_data/processed_data/T1D_demo.csv", row.names = FALSE)

sex_percentages <- T1D_demo %>%
  group_by(sex)%>%
  summarise(Percentage =n() / nrow(T1D_demo)*100)
print(sex_percentages)

png("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/output_plot/demographics/histogram.png", 
    width = 8, height = 6, units = "in", res = 300)
hist(T1D_demo$age, breaks =seq(0, max(T1D_demo$age)+5, by=5),
     col="skyblue3", border="white",
     xlab="Age",ylab="Frequency",
     main= "Ages of UCLA T1D Patients")
axis(side=1, at=seq(0, max(T1D_demo$age)+10, by=10))
dev.off()

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
T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")
T1D_hba1c_percent <- read.csv("./intermediate_data/processed_data/T1D_lab_hba1c_percent.csv")
T1D_demo <- read.csv("./intermediate_data/processed_data/T1D_demo.csv")

T1D_info <- data.frame("patient_id" = T1D_patients, "mean_hba1c" = rep(NA, length(T1D_patients)), 
                       "num_measurement" = rep(NA, length(T1D_patients)))
for (patient in T1D_info$patient_id) {
  T1D_info[T1D_info$patient_id == patient, "mean_hba1c"] <- mean(T1D_hba1c_percent[T1D_hba1c_percent$patient_id == patient, "result"])
  T1D_info[T1D_info$patient_id == patient, "num_measurement"] <- length(T1D_hba1c_percent[T1D_hba1c_percent$patient_id == patient, "result"])
}

T1D_info <- merge(x = T1D_demo, y = T1D_info, by = "patient_id")
write.csv(T1D_info, "./intermediate_data/processed_data/T1D_lab_hba1c_average.csv", row.names = FALSE)

################################################################################
####################### subsetting medication file based on classification #####
################################################################################

T1D_medication <- read.csv("./intermediate_data/processed_data/T1D_medication.csv")

## T2D
drug_ref_T2D <- read.csv("./intermediate_data/fuzzy_search_reference/UCSD_T2D_drugs.csv")
drug_ref_T2D <- drug_ref_T2D[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_T2D <- merge(x = T1D_medication, y = drug_ref_T2D, by.x = "medication_name", by.y = "QueryName")
drug_ref_T2D <- drug_ref_T2D[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_T2D)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_T2D, "./intermediate_data/processed_data/T1D_medication_T2D.csv", row.names = FALSE)

## antihypertensive
drug_ref_BP <- read.csv("./intermediate_data/fuzzy_search_reference/UCSD_antihypertensive_drugs.csv")
drug_ref_BP <- drug_ref_BP[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_BP <- merge(x = T1D_medication, y = drug_ref_BP, by.x = "medication_name", by.y = "QueryName")
drug_ref_BP <- drug_ref_BP[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_BP)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_BP, "./intermediate_data/processed_data/T1D_medication_antihypertensive.csv", row.names = FALSE)

## lipid lowering
drug_ref_lipid <- read.csv("./intermediate_data/fuzzy_search_reference/UCSD_lipid_modifying_drugs.csv")
drug_ref_lipid <- drug_ref_lipid[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_lipid <- merge(x = T1D_medication, y = drug_ref_lipid, by.x = "medication_name", by.y = "QueryName")
drug_ref_lipid <- drug_ref_lipid[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_lipid)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_lipid, "./intermediate_data/processed_data/T1D_medication_lipid_modifying.csv", row.names = FALSE)

## insulin
drug_ref_insulin <- read.csv("./intermediate_data/fuzzy_search_reference/UCSD_insulin.csv")
drug_ref_insulin <- drug_ref_insulin[, c("QueryName", "Drug_name", "ATC_code", "Drug_class", "Drug_class_name")]
drug_ref_insulin <- merge(x = T1D_medication, y = drug_ref_insulin, by.x = "medication_name", by.y = "QueryName")
drug_ref_insulin <- drug_ref_insulin[,c("patient_id", "start_date", "end_date", "medication_name", "ATC_code", "Drug_name", "Drug_class", "Drug_class_name")]
colnames(drug_ref_insulin)[c(5, 7)] <- c("ATC_drug", "ATC_class")
write.csv(drug_ref_insulin, "./intermediate_data/processed_data/T1D_medication_insulin.csv", row.names = FALSE)
