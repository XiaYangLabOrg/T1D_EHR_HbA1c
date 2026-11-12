
## OF
setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V2/intermediate_data/processed_data")
data_V2 <-  read.csv("T1D_medication.csv")
data_V2 <- unique(data_V2$medication_name)

setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/intermediate_data/processed_data")
data_V1 <-  read.csv("T1D_medication.csv")
data_V1 <- unique(data_V1$medication_name)

data <- unique(c(data_V2, data_V1))
data <- trimws(data)
data <- as.data.frame(data)
write.table(data,"/u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/EHR_drug_list/drug_list_OF.csv",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
## UCLA
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCLA/intermediate_data/processed_data/")
data <- read.csv("T1D_medication.csv")
data <- unique(data$medication_name)
data <- as.data.frame(data)
write.table(data,"/u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/EHR_drug_list/drug_list_UCLA.csv",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## UCSD
setwd("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/intermediate_data/processed_data/")
data <- read.csv("T1D_medication.csv")
data <- unique(data$medication_name)
data <- as.data.frame(data)
write.table(data,"/u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/EHR_drug_list/drug_list_UCSD.csv",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
