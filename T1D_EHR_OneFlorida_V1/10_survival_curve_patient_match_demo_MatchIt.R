library(dplyr)
library(MatchIt)
library(cobalt)

setwd("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/")
# set.seed(345)

args <- commandArgs(trailingOnly = TRUE)

drug_classification <- "T2D"
# drug_selection <- as.numeric(args[1])
drug_selection <- 2

if(drug_classification == "T2D"){
  drug <- c("A10BJ05", "A10BK03", "A10BA02", "A10BH01", "A10BG03", "A10BB12")[drug_selection]
}

drug_reference <- read.csv(file = paste0("../T1D_EMR_fuzzy_search/", drug_classification, "/", drug_classification, "_drugs.csv"))
drug_name <- drug_reference[drug_reference$ATC_code == drug, "Drug_name", drop = TRUE][1]

print(paste(drug, drug_name))

## ------------------------------------------------------------
## 1. Build one patient-level covariate table
## ------------------------------------------------------------

T1D_demo <- read.csv("./intermediate_data/processed_data_stringent/T1D_demo.csv")

T1D_demo <- T1D_demo %>%
  mutate(
    age = 2023 - birth_year,
    age_group = cut(
      age,
      breaks = c(-Inf, 30, 40, 50, 65, Inf),
      labels = c("18-29", "30-39", "40-49", "50-64", ">64"),
      right = FALSE
    ),
    age_group = as.character(age_group)
  ) %>%
  filter(
    sex %in% c("F", "M"),
    age != 123
  ) %>%
  select(patient_id, sex, age, age_group)

## Use the first available HbA1c per patient
T1D_hba1c <- read.csv("./intermediate_data/processed_data_stringent/T1D_lab_hba1c_percent.csv") %>%
  mutate(
    hba1c_time = pmin(specimen_taken_time, result_time, na.rm = TRUE)
  ) %>%
  filter(is.finite(hba1c_time)) %>%
  group_by(patient_id) %>%
  slice_min(hba1c_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    patient_id,
    baseline_HbA1c = result
  )

patient_covariates <- T1D_demo %>%
  inner_join(T1D_hba1c, by = "patient_id") %>%
  filter(!is.na(sex), !is.na(age), !is.na(age_group), !is.na(baseline_HbA1c)) %>%
  distinct(patient_id, .keep_all = TRUE)

## ------------------------------------------------------------
## 2. Load cohort IDs
## ------------------------------------------------------------

patients_drug_2_5_ids <- readRDS(
  paste0(
    "./intermediate_data/graph_additional_stringent/survival_curve_patient/",
    drug, "_", drug_name, "_patients_drug_2_5.RDS"
  )
)

patients_drug_0_2_ids <- readRDS(
  paste0(
    "./intermediate_data/graph_additional_stringent/survival_curve_patient/",
    drug, "_", drug_name, "_patients_drug_0_2.RDS"
  )
)

patients_insulin_0_2_ids <- readRDS(
  paste0(
    "./intermediate_data/graph_additional_stringent/survival_curve_patient/",
    drug, "_", drug_name, "_patients_insulin_0_2.RDS"
  )
)

patients_insulin_2_5_ids <- readRDS(
  paste0(
    "./intermediate_data/graph_additional_stringent/survival_curve_patient/",
    drug, "_", drug_name, "_patients_insulin_2_5.RDS"
  )
)

drug_0_2_cov <- patient_covariates %>%
  filter(patient_id %in% patients_drug_0_2_ids) %>%
  distinct(patient_id, .keep_all = TRUE)

drug_2_5_cov <- patient_covariates %>%
  filter(patient_id %in% patients_drug_2_5_ids) %>%
  distinct(patient_id, .keep_all = TRUE)

insulin_0_2_pool_cov <- patient_covariates %>%
  filter(patient_id %in% patients_insulin_0_2_ids) %>%
  distinct(patient_id, .keep_all = TRUE)

insulin_2_5_pool_cov <- patient_covariates %>%
  filter(patient_id %in% patients_insulin_2_5_ids) %>%
  distinct(patient_id, .keep_all = TRUE)

all_drug_ids <- union(
  patients_drug_0_2_ids,
  patients_drug_2_5_ids
)

insulin_0_2_pool_cov <- insulin_0_2_pool_cov %>%
  filter(!patient_id %in% all_drug_ids)

insulin_2_5_pool_cov <- insulin_2_5_pool_cov %>%
  filter(!patient_id %in% all_drug_ids)

cat("Drug 0-2 cohort:", nrow(drug_0_2_cov), "\n")
cat("Available insulin 0-2 reference pool:", nrow(insulin_0_2_pool_cov), "\n")

cat("Drug 2-5 cohort:", nrow(drug_2_5_cov), "\n")
cat("Available insulin 2-5 reference pool:", nrow(insulin_2_5_pool_cov), "\n")

## ------------------------------------------------------------
## 3. Matching function
## ------------------------------------------------------------

run_matching <- function(
    insulin_pool,
    drug_cov,
    ratio = 20,
    caliper = 0.2
) {
  
  if (nrow(drug_cov) == 0) {
    stop("The drug cohort is empty.")
  }
  
  if (nrow(insulin_pool) == 0) {
    stop("The insulin reference pool is empty.")
  }
  
  drug_match <- drug_cov %>%
    mutate(treatment = 1)
  
  insulin_match <- insulin_pool %>%
    mutate(treatment = 0)
  
  match_input <- bind_rows(
    drug_match,
    insulin_match
  ) %>%
    mutate(sex = factor(sex))
  
  m.out <- matchit(
    treatment ~ sex + age + baseline_HbA1c,
    data = match_input,
    method = "nearest",
    distance = "glm",
    mahvars = ~ age + baseline_HbA1c,
    exact = ~ sex,
    ratio = ratio,
    caliper = caliper,
    std.caliper = TRUE,
    replace = FALSE,
    estimand = "ATT"
  )
  
  matched_data <- match.data(m.out)
  
  matched_drug <- matched_data %>%
    filter(treatment == 1)
  
  matched_insulin <- matched_data %>%
    filter(treatment == 0)
  
  balance <- bal.tab(
    m.out,
    un = TRUE
  )
  
  balance_table <- balance$Balance
  balance_table$variable <- rownames(balance_table)
  rownames(balance_table) <- NULL
  
  max_smd <- max(
    abs(balance_table$Diff.Adj),
    na.rm = TRUE
  )
  
  return(
    list(
      match_object = m.out,
      matched_data = matched_data,
      matched_drug = matched_drug,
      matched_insulin = matched_insulin,
      balance = balance,
      balance_table = balance_table,
      max_smd = max_smd,
      ratio = ratio,
      caliper = caliper
    )
  )
}

## ------------------------------------------------------------
## 4. Try different matching ratios
## ------------------------------------------------------------

find_best_matching_ratio <- function(
    insulin_pool,
    drug_cov,
    ratios = c(20, 10, 8, 5, 4, 3, 2, 1),
    caliper = 0.2,
    max_smd_allowed = 0.10
) {
  
  results <- list()
  
  for (ratio in ratios) {
    
    cat("\n============================================================\n")
    cat("Trying matching ratio 1:", ratio, "\n")
    cat("============================================================\n")
    
    result <- tryCatch(
      run_matching(
        insulin_pool = insulin_pool,
        drug_cov = drug_cov,
        ratio = ratio,
        caliper = caliper
      ),
      error = function(e) {
        message("Matching failed for ratio ", ratio, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(result)) {
      next
    }
    
    results[[as.character(ratio)]] <- result
    
    cat("Drug patients retained:", nrow(result$matched_drug), "/", nrow(drug_cov), "\n")
    cat("Insulin controls matched:", nrow(result$matched_insulin), "\n")
    cat("Maximum adjusted SMD:", round(result$max_smd, 4), "\n")
    
    print(
      result$balance_table[
        ,
        c("variable", "Diff.Un", "Diff.Adj")
      ]
    )
    
    all_drug_retained <- nrow(result$matched_drug) == nrow(drug_cov)
    balanced <- result$max_smd < max_smd_allowed
    
    if (all_drug_retained && balanced) {
      
      cat("\nAcceptable balance found.\n")
      cat("Selected ratio = 1:", ratio, "\n")
      
      return(result)
    }
  }
  
  if (length(results) == 0) {
    message("No matching solution could be generated.")
    return(NULL)
  }
  
  max_smd_values <- sapply(
    results,
    function(x) {
      x$max_smd
    }
  )
  
  best_index <- which.min(max_smd_values)
  best_result <- results[[best_index]]
  
  message("\nNo matching ratio achieved maximum SMD < ", max_smd_allowed, ".")
  message("Returning the solution with the smallest maximum SMD.")
  message("Selected ratio = 1:", best_result$ratio)
  message("Maximum SMD = ", round(best_result$max_smd, 4))
  
  return(best_result)
}

## ------------------------------------------------------------
## 5. Run the two matching searches separately
## ------------------------------------------------------------

balanced_result_0_2 <- find_best_matching_ratio(
  insulin_pool = insulin_0_2_pool_cov,
  drug_cov = drug_0_2_cov,
  ratios = c(40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10),
  caliper = 0.2,
  max_smd_allowed = 0.10
)

balanced_result_2_5 <- find_best_matching_ratio(
  insulin_pool = insulin_2_5_pool_cov,
  drug_cov = drug_2_5_cov,
  ratios = c(40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10),
  caliper = 0.2,
  max_smd_allowed = 0.10
)

## ------------------------------------------------------------
## 6. Save one comparison
## ------------------------------------------------------------

save_balanced_result <- function(balanced_result, comparison_name, output_dir, drug, drug_name) {
  
  if (is.null(balanced_result)) {
    message("No matched insulin population found for ", comparison_name)
    return(invisible(NULL))
  }
  
  file_prefix <- paste0(drug, "_", drug_name, "_", comparison_name)
  
  ## Save complete result object
  saveRDS(
    balanced_result,
    file.path(
      output_dir,
      paste0(file_prefix, "_balanced_result.RDS")
    )
  )
  
  ## Save MatchIt object
  saveRDS(
    balanced_result$match_object,
    file.path(
      output_dir,
      paste0(file_prefix, "_matchit_object.RDS")
    )
  )
  
  ## Save only matched insulin patient IDs
  saveRDS(
    unique(balanced_result$matched_insulin$patient_id),
    file.path(
      output_dir,
      paste0(file_prefix, "_balanced_insulin_patient_ids.RDS")
    )
  )
  
  ## Save matched insulin patients with covariates
  write.csv(
    balanced_result$matched_insulin,
    file.path(
      output_dir,
      paste0(file_prefix, "_balanced_insulin_subset.csv")
    ),
    row.names = FALSE
  )
  
  ## Save matched drug patients
  write.csv(
    balanced_result$matched_drug,
    file.path(
      output_dir,
      paste0(file_prefix, "_matched_drug_subset.csv")
    ),
    row.names = FALSE
  )
  
  ## Save balance statistics
  write.csv(
    balanced_result$balance_table,
    file.path(
      output_dir,
      paste0(file_prefix, "_balance.csv")
    ),
    row.names = FALSE
  )
  
  matching_summary <- data.frame(
    comparison = comparison_name,
    drug = drug,
    drug_name = drug_name,
    matching_ratio = balanced_result$ratio,
    caliper = balanced_result$caliper,
    matched_drug_n = nrow(balanced_result$matched_drug),
    matched_insulin_n = nrow(balanced_result$matched_insulin),
    max_adjusted_SMD = balanced_result$max_smd
  )
  
  write.csv(
    matching_summary,
    file.path(
      output_dir,
      paste0(file_prefix, "_matching_summary.csv")
    ),
    row.names = FALSE
  )
  
  message("Matching completed for ", comparison_name)
  message("Selected matching ratio: 1:", balanced_result$ratio)
  message("Matched drug patients: ", nrow(balanced_result$matched_drug))
  message("Matched insulin patients: ", nrow(balanced_result$matched_insulin))
  message("Maximum adjusted SMD: ", round(balanced_result$max_smd, 4))
  
  print(balanced_result$balance_table)
  
  invisible(NULL)
}

output_dir <- "./intermediate_data/graph_additional_stringent/survival_curve_patient/"

save_balanced_result(
  balanced_result = balanced_result_0_2,
  comparison_name = "0_2",
  output_dir = output_dir,
  drug = drug,
  drug_name = drug_name
)

save_balanced_result(
  balanced_result = balanced_result_2_5,
  comparison_name = "2_5",
  output_dir = output_dir,
  drug = drug,
  drug_name = drug_name
)

## ------------------------------------------------------------
## 7. Love plots
## ------------------------------------------------------------

if (!is.null(balanced_result_0_2)) {
  
  pdf(
    file.path(
      output_dir,
      paste0(drug, "_", drug_name, "_0_2_balance_loveplot.pdf")
    ),
    width = 7,
    height = 5
  )
  
  print(
    love.plot(
      balanced_result_0_2$match_object,
      stats = "mean.diffs",
      abs = TRUE,
      threshold = 0.10,
      var.order = "unadjusted"
    )
  )
  
  dev.off()
}

if (!is.null(balanced_result_2_5)) {
  
  pdf(
    file.path(
      output_dir,
      paste0(drug, "_", drug_name, "_2_5_balance_loveplot.pdf")
    ),
    width = 7,
    height = 5
  )
  
  print(
    love.plot(
      balanced_result_2_5$match_object,
      stats = "mean.diffs",
      abs = TRUE,
      threshold = 0.10,
      var.order = "unadjusted"
    )
  )
  
  dev.off()
}

## ------------------------------------------------------------
## 8. Final summary
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("Matching analysis finished.\n")
cat("Drug:", drug, drug_name, "\n")

if (!is.null(balanced_result_0_2)) {
  cat(
    "0-2 years:",
    nrow(balanced_result_0_2$matched_drug),
    "drug patients +",
    nrow(balanced_result_0_2$matched_insulin),
    "insulin controls",
    "| ratio = 1:", balanced_result_0_2$ratio,
    "| max SMD =", round(balanced_result_0_2$max_smd, 4),
    "\n"
  )
}

if (!is.null(balanced_result_2_5)) {
  cat(
    "2-5 years:",
    nrow(balanced_result_2_5$matched_drug),
    "drug patients +",
    nrow(balanced_result_2_5$matched_insulin),
    "insulin controls",
    "| ratio = 1:", balanced_result_2_5$ratio,
    "| max SMD =", round(balanced_result_2_5$max_smd, 4),
    "\n"
  )
}

cat("============================================================\n")