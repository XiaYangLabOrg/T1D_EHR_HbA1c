
library(jsonlite)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_fuzzy_search")

df <- read.csv("./insulin/insulin.csv")
df$Drug_class <- trimws(df$Drug_class)
df$Drug_class_name <- trimws(df$Drug_class_name)
df$ATC_code <- trimws(df$ATC_code)
df$Drug_name <- trimws(df$Drug_name)

result <- lapply(split(df, df$ATC_code), function(group) {
  list(
    accession = unique(group$ATC_code),
    kegg_id = "insulin",
    name = group$Drug_name[1],
    synonyms = list(group$Drug_name)
  )
})

# Save to JSON file
# write_json(result, path = "./Desktop/drug_data.json", 
#            pretty = TRUE, auto_unbox = TRUE)

formatted_json <- paste0("[", paste(sapply(result, function(x) jsonlite::toJSON(x, auto_unbox = TRUE)), collapse = ", "), "]")
# Remove single square brackets and reduce double square brackets
formatted_json <- gsub("\\[\\[", "[", formatted_json) # Reduce double open brackets
formatted_json <- gsub("\\]\\]", "]", formatted_json) # Reduce double close brackets
formatted_json <- gsub("\\[\\]", "", formatted_json)  # Remove standalone brackets
cat(formatted_json)

print("JSON file created successfully.")

