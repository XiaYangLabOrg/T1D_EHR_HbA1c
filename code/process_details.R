
library(dplyr)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search/")

# datasets <- c("OF", "UCLA", "UCSD")
# for(dataset in datasets){
#   details <- read.csv(paste0("./insulin/insulin_", dataset, "_top10/", dataset,"_details.csv"))
#   
#   details <- details %>%
#     group_by(QueryName) %>%
#     mutate(max_FuzzyScore = max(FuzzyScore)) %>%
#     ungroup() %>%
#     arrange(desc(max_FuzzyScore), QueryName, desc(FuzzyScore)) %>%
#     dplyr::select(-max_FuzzyScore)
#   details <- unique(details)
#   
#   write.csv(details, paste0("./insulin/", dataset, "_details.csv"),
#             row.names = FALSE, quote = TRUE)
# }


#### after manually picking

reference <- read.csv("./insulin/insulin.csv")
reference$ATC_code <- trimws(reference$ATC_code)
reference$Drug_name <- trimws(reference$Drug_name)
reference$Drug_class <- trimws(reference$Drug_class)
reference$Drug_class_name <- trimws(reference$Drug_class_name)
reference$Drug_name <- gsub("<ca>", "", reference$Drug_name)

drugs <- read.csv("./insulin/UCSD_details.csv")
drugs$MatchedName <- trimws(drugs$MatchedName)
drugs$BestMatched <- trimws(drugs$BestMatched)
drugs$accession <- trimws(drugs$accession)
drugs$QueryName <- trimws(drugs$QueryName)

drugs <- merge(drugs, reference, by.x = c("accession", "BestMatched"),
               by.y = c("ATC_code", "Drug_name"), all.x = TRUE)
drugs <- drugs[,c(3,5,4,2,1,7,8)]
colnames(drugs)[4:5] <- c("Drug_name","ATC_code")
drugs$ATC_code <- drugs$Drug_class_name
drugs$Drug_name <- drugs$Drug_class

write.csv(drugs, "./insulin/UCSD_insulin_details.csv", row.names = FALSE, quote = TRUE)

# # #### after manually picking
# 
# reference <- read.csv("./immune/immune_drugs.csv")
# reference$ATC_code <- trimws(reference$ATC_code)
# reference$Drug_name <- trimws(reference$Drug_name)
# reference$Drug_class <- trimws(reference$Drug_class)
# reference$Drug_class_name <- trimws(reference$Drug_class_name)
# 
# drugs <- read.csv("./immune/UCLA_immune_drugs.csv")
# drugs$MatchedName <- trimws(drugs$MatchedName)
# drugs$BestMatched <- trimws(drugs$BestMatched)
# drugs$accession <- trimws(drugs$accession)
# drugs$QueryName <- trimws(drugs$QueryName)
# 
# drugs <- merge(drugs, reference, by.x = c("accession", "BestMatched"),
#                by.y = c("ATC_code", "Drug_name"), all.x = TRUE)
# drugs <- drugs[,c(3,5,4,2,1,7,8)]
# colnames(drugs)[4:5] <- c("Drug_name","ATC_code")
# write.csv(drugs, "./immune/UCLA_immune_drugs.csv", row.names = FALSE, quote = TRUE)
# 
# # #### after manually picking
# 
# reference <- read.csv("./immune/immune_drugs.csv")
# reference$ATC_code <- trimws(reference$ATC_code)
# reference$Drug_name <- trimws(reference$Drug_name)
# reference$Drug_class <- trimws(reference$Drug_class)
# reference$Drug_class_name <- trimws(reference$Drug_class_name)
# 
# drugs <- read.csv("./immune/UCSD_immune_drugs.csv")
# drugs$MatchedName <- trimws(drugs$MatchedName)
# drugs$BestMatched <- trimws(drugs$BestMatched)
# drugs$accession <- trimws(drugs$accession)
# drugs$QueryName <- trimws(drugs$QueryName)
# 
# drugs <- merge(drugs, reference, by.x = c("accession", "BestMatched"),
#                by.y = c("ATC_code", "Drug_name"), all.x = TRUE)
# drugs <- drugs[,c(3,5,4,2,1,7,8)]
# colnames(drugs)[4:5] <- c("Drug_name","ATC_code")
# write.csv(drugs, "./immune/UCSD_immune_drugs.csv", row.names = FALSE, quote = TRUE)
