
library(survival)
library(survminer)
library(grid)
library(ggplot2)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_Meta/")

drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[1]
drug_selection <- 2
drug_reference <- read.csv(file = paste0("../T1D_EMR_fuzzy_search/", drug_classification, "/", drug_classification, "_drugs.csv"))
if(drug_classification == "T2D"){
  drug <- c("A10BJ05", "A10BK03", "A10BA02", "A10BH01", "A10BG03", "A10BB12")[drug_selection]
}
drug_name <- drug_reference[drug_reference$ATC_code == drug, "Drug_name", drop = TRUE][1]


summary <- data.frame(group = character(0), patient_id = character(0), time = numeric(0), status = numeric(0))
for(i in c(1)){
  cohort <- c("OneFlorida_V1", "UCLA", "UCSD")[i]
  cohort_name <- c("OF", "UCLA", "UCSD")[i]
  
  print(paste(cohort, drug_classification))
  DKA_diag_min <- read.csv(paste0("../T1D_EMR_", cohort, "/intermediate_data/processed_data_stringent/T1D_diagnosis_DKA_1st.csv"))
  T1D_diag_min <- read.csv(paste0("../T1D_EMR_", cohort, "/intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st_halfyear_record.csv"))
  # T1D_diag_min <- read.csv(paste0("../T1D_EMR_", cohort, "/intermediate_data/processed_data_stringent/T1D_diagnosis_T1D_1st.csv"))
  
  insulin_patient_drug_matched_2_5 <- read.csv(paste0("../T1D_EMR_", cohort, 
                                                  "/intermediate_data/graph_additional_stringent/survival_curve_patient/"
                                                  , drug, "_", drug_name, "_2_5_balanced_insulin_subset.csv"))
  insulin_patient_drug_matched_2_5 <- unique(insulin_patient_drug_matched_2_5$patient_id)
  
  insulin_patient_drug_matched_0_2 <- read.csv(paste0("../T1D_EMR_", cohort, 
                                                      "/intermediate_data/graph_additional_stringent/survival_curve_patient/"
                                                      , drug, "_", drug_name, "_0_2_balanced_insulin_subset.csv"))
  insulin_patient_drug_matched_0_2 <- unique(insulin_patient_drug_matched_0_2$patient_id)
  
  drug_time_drug <- read.csv(paste0("../T1D_EMR_", cohort, "/intermediate_data/graph_additional_stringent/", drug_classification,
                                    "_30day.period_drug_time_ATC_drug.csv"))
  drug_time_drug <- subset(drug_time_drug, ATC_drug == drug)
  # drug_time_drug <- subset(drug_time_drug, grepl("^A10BK", ATC_drug))
  DKA_diag_min_drug <- subset(DKA_diag_min, patient_id %in% drug_time_drug$patient_id)
  T1D_diag_min_drug <- subset(T1D_diag_min, patient_id %in% drug_time_drug$patient_id)
  
  if(nrow(T1D_diag_min_drug) > 0){
    summary_drug <- data.frame(group = cohort_name, patient_id = unique(T1D_diag_min_drug$patient_id), time = NA, status = NA)
    for(patient in unique(T1D_diag_min_drug$patient_id)){
      start_date <- min(drug_time_drug[drug_time_drug$patient_id == patient, "start_date"], na.rm = TRUE)
      diag_date <- min(T1D_diag_min_drug[T1D_diag_min_drug$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
      ## starting drug 0-2 years
      if(!is.infinite(diag_date) & start_date >= diag_date & start_date < diag_date + 365*2){
        if(!patient %in% DKA_diag_min_drug$patient_id){
          summary_drug[summary_drug$patient_id == patient, "group"] <- 
            paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +0-2 yrs")
          summary_drug[summary_drug$patient_id == patient, "time"] <- 5
          summary_drug[summary_drug$patient_id == patient, "status"] <- 0
        } else {
          DKA_date <- min(DKA_diag_min_drug[DKA_diag_min_drug$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
          if(DKA_date > diag_date + 365*5){
            summary_drug[summary_drug$patient_id == patient, "group"] <- 
              paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +0-2 yrs")
            summary_drug[summary_drug$patient_id == patient, "time"] <- 5
            summary_drug[summary_drug$patient_id == patient, "status"] <- 0
          } else if(DKA_date > diag_date & DKA_date < diag_date + 365*5) {
            summary_drug[summary_drug$patient_id == patient, "group"] <- 
              paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +0-2 yrs")
            summary_drug[summary_drug$patient_id == patient, "time"] <- (DKA_date - diag_date)/(365)
            summary_drug[summary_drug$patient_id == patient, "status"] <- 1
          }
        }
        ## starting drug 2-5 years
      } else if(!is.infinite(diag_date) & start_date >= diag_date + 365*2 & start_date < diag_date + 365*5){
        if(!patient %in% DKA_diag_min_drug$patient_id){
          summary_drug[summary_drug$patient_id == patient, "group"] <- 
            paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +2-5 yrs")
          summary_drug[summary_drug$patient_id == patient, "time"] <- 5
          summary_drug[summary_drug$patient_id == patient, "status"] <- 0
        } else {
          DKA_date <- min(DKA_diag_min_drug[DKA_diag_min_drug$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
          if(DKA_date > diag_date + 365*5){
            summary_drug[summary_drug$patient_id == patient, "group"] <- 
              paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +2-5 yrs")
            summary_drug[summary_drug$patient_id == patient, "time"] <- 5
            summary_drug[summary_drug$patient_id == patient, "status"] <- 0
          } else if(DKA_date > diag_date & DKA_date < diag_date + 365*5) {
            summary_drug[summary_drug$patient_id == patient, "group"] <- 
              paste0(summary_drug[summary_drug$patient_id == patient, "group"], " drug start +2-5 yrs")
            summary_drug[summary_drug$patient_id == patient, "time"] <- (DKA_date - diag_date)/(365)
            summary_drug[summary_drug$patient_id == patient, "status"] <- 1
          }
        }
      }
    }
    summary <- rbind(summary, summary_drug)
    rm(summary_drug)
  }
  
  print(paste(cohort, "insulin"))
  drug_time_insulin <- read.csv(paste0("../T1D_EMR_", cohort, 
                                       "/intermediate_data/graph_additional_stringent/insulin_xT2D_30day.period_drug_time_ATC_drug.csv"))
  DKA_diag_min_insulin <- subset(DKA_diag_min, patient_id %in% drug_time_insulin$patient_id)
  T1D_diag_min_insulin <- subset(T1D_diag_min, patient_id %in% drug_time_insulin$patient_id)
  
  if (nrow(T1D_diag_min_insulin) > 0) {
    summary_drug <- data.frame(
      group = paste0(cohort_name, " only insulin"),
      patient_id = unique(T1D_diag_min_insulin$patient_id),
      time = NA,
      status = NA
    )
    for (patient in unique(T1D_diag_min_insulin$patient_id)) {
      start_date <- min(drug_time_insulin[drug_time_insulin$patient_id == patient,"start_date"], na.rm = TRUE)
      diag_date <- min(T1D_diag_min_insulin[T1D_diag_min_insulin$patient_id == patient,"diagnosis_date"], na.rm = TRUE)
      if (!patient %in% DKA_diag_min_insulin$patient_id) {
        summary_drug[summary_drug$patient_id == patient, "time"] <- 5
        summary_drug[summary_drug$patient_id == patient, "status"] <- 0
      } else {
        DKA_date <- min(DKA_diag_min_insulin[DKA_diag_min_insulin$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
        if(DKA_date > diag_date + 365*5){
          summary_drug[summary_drug$patient_id == patient, "time"] <- 5
          summary_drug[summary_drug$patient_id == patient, "status"] <- 0
        } else if(DKA_date > diag_date & DKA_date < diag_date + 365*5) {
          summary_drug[summary_drug$patient_id == patient, "time"] <- (DKA_date - diag_date)/(365)
          summary_drug[summary_drug$patient_id == patient, "status"] <- 1
        }
      }
    }
    summary <- rbind(summary, summary_drug)
  }
  
  if(length(insulin_patient_drug_matched_2_5) > 0){
    summary_drug <- data.frame(
      group = paste0(cohort_name, " only insulin (balanced +2-5 yrs)"),
      patient_id = unique(T1D_diag_min_insulin$patient_id),
      time = NA,
      status = NA
    )
    for (patient in insulin_patient_drug_matched_2_5) {
      start_date <- min(drug_time_insulin[drug_time_insulin$patient_id == patient,"start_date"], na.rm = TRUE)
      diag_date <- min(T1D_diag_min_insulin[T1D_diag_min_insulin$patient_id == patient,"diagnosis_date"], na.rm = TRUE)
      if (!patient %in% DKA_diag_min_insulin$patient_id) {
        summary_drug[summary_drug$patient_id == patient, "time"] <- 5
        summary_drug[summary_drug$patient_id == patient, "status"] <- 0
      } else {
        DKA_date <- min(DKA_diag_min_insulin[DKA_diag_min_insulin$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
        if(DKA_date > diag_date + 365*5){
          summary_drug[summary_drug$patient_id == patient, "time"] <- 5
          summary_drug[summary_drug$patient_id == patient, "status"] <- 0
        } else if(DKA_date > diag_date & DKA_date < diag_date + 365*5) {
          summary_drug[summary_drug$patient_id == patient, "time"] <- (DKA_date - diag_date)/(365)
          summary_drug[summary_drug$patient_id == patient, "status"] <- 1
        }
      }
    }
    summary <- rbind(summary, summary_drug)
  }
  
  if(length(insulin_patient_drug_matched_0_2) > 0){
    summary_drug <- data.frame(
      group = paste0(cohort_name, " only insulin (balanced +0-2 yrs)"),
      patient_id = unique(T1D_diag_min_insulin$patient_id),
      time = NA,
      status = NA
    )
    for (patient in insulin_patient_drug_matched_0_2) {
      start_date <- min(drug_time_insulin[drug_time_insulin$patient_id == patient,"start_date"], na.rm = TRUE)
      diag_date <- min(T1D_diag_min_insulin[T1D_diag_min_insulin$patient_id == patient,"diagnosis_date"], na.rm = TRUE)
      if (!patient %in% DKA_diag_min_insulin$patient_id) {
        summary_drug[summary_drug$patient_id == patient, "time"] <- 5
        summary_drug[summary_drug$patient_id == patient, "status"] <- 0
      } else {
        DKA_date <- min(DKA_diag_min_insulin[DKA_diag_min_insulin$patient_id == patient, "diagnosis_date"], na.rm = TRUE)
        if(DKA_date > diag_date + 365*5){
          summary_drug[summary_drug$patient_id == patient, "time"] <- 5
          summary_drug[summary_drug$patient_id == patient, "status"] <- 0
        } else if(DKA_date > diag_date & DKA_date < diag_date + 365*5) {
          summary_drug[summary_drug$patient_id == patient, "time"] <- (DKA_date - diag_date)/(365)
          summary_drug[summary_drug$patient_id == patient, "status"] <- 1
        }
      }
    }
    summary <- rbind(summary, summary_drug)
  }
}
summary <- subset(summary, !is.na(time))

group_n <- table(summary$group)
summary <- subset(summary, group %in% names(group_n[group_n >= 20]))
summary$group <- paste0(
  summary$group,
  " (n=", group_n[summary$group], ")"
)

############################# plot #############################################

palette <- c(
  "OF only insulin (balanced +0-2 yrs)" = "lavenderblush4", 
  "OF only insulin (balanced +2-5 yrs)" = "#4F494B", 
  "OF drug start +0-2 yrs"   = "lightcoral",
  "OF drug start +2-5 yrs"   = "brown"
)
names(palette) <- paste0(
  names(palette),
  " (n=", group_n[names(palette)], ")"
)
palette <- palette[names(palette) %in% summary$group]

summary <- subset(summary, !is.na(time))
summary$group <- factor(summary$group, levels = names(palette))
fit_group <- survfit(Surv(time, status) ~ group, data = summary)


p <- ggsurvplot(
  fit_group,
  data = summary,
  conf.int = FALSE,
  risk.table = FALSE,
  palette = palette,
  legend.title = "",
  legend.labs  = names(palette),
  ggtheme = theme_classic(),
  xlab = "Diagnosis-aligned Time",
  ylab = "DKA-free Probability"
)

p$plot <- p$plot +
  
  coord_cartesian(xlim = c(0, 5), ylim = c(ifelse(drug == "A10BB12", 0.75, 0.8), 1)) +
  scale_x_continuous(
    breaks = c(0, 2, 5),
    labels = c("diagnosis", "+2 yrs", "+5 yrs")
  ) +
  
  labs(
    title = paste(drug, "-", drug_name)
  ) +
  
  theme(
    legend.position = c(0.28, 0.3),
    legend.background = element_rect(
      fill = NA,
      color = NA
    ),
    legend.key = element_blank(),
    plot.title = element_text(
      size = 12,
      hjust = 0.5)
  )

print(p)
ggsave(paste0("../T1D_EMR_figures/figures_stringent/survivalcurve_", drug_classification, "_", drug, "_OneFlorida_V1.png"),
       width = 6, height = 3.5, units = "in", dpi = 600)

############################# cox model ########################################

library(dplyr)
library(survival)

summary_for_cox <- summary %>%
  filter(!is.na(time), !is.na(status)) %>%
  mutate(
    group_clean = sub(" \\(n=.*\\)$", "", as.character(group)),
    time = as.numeric(time),
    status = as.numeric(status)
  )

## Check groups
print(table(summary_for_cox$group))
print(table(summary_for_cox$group, summary_for_cox$status))


## ------------------------------------------------------------
## Function for pairwise Cox model
## ------------------------------------------------------------

run_pairwise_cox <- function(data, insulin_group, drug_group) {

  cox_data <- data %>%
    filter(group_clean %in% c(insulin_group, drug_group)) %>%
    mutate(
      exposure = ifelse(group_clean == drug_group, "drug", "insulin"),
      exposure = factor(exposure, levels = c("insulin", "drug"))
    )

  cat("\nComparison:", drug_group, "vs", insulin_group, "\n")
  print(table(cox_data$exposure))
  print(table(cox_data$exposure, cox_data$status))

  fit <- coxph(
    Surv(time, status) ~ exposure,
    data = cox_data
  )

  fit_sum <- summary(fit)

  result <- data.frame(
    comparison = paste0(drug_group, " vs ", insulin_group),
    n_insulin = sum(cox_data$exposure == "insulin"),
    n_drug = sum(cox_data$exposure == "drug"),
    events_insulin = sum(cox_data$status[cox_data$exposure == "insulin"] == 1),
    events_drug = sum(cox_data$status[cox_data$exposure == "drug"] == 1),
    HR = fit_sum$conf.int["exposuredrug", "exp(coef)"],
    HR_lower_95 = fit_sum$conf.int["exposuredrug", "lower .95"],
    HR_upper_95 = fit_sum$conf.int["exposuredrug", "upper .95"],
    cox_p_value = fit_sum$coefficients["exposuredrug", "Pr(>|z|)"]
  )

  return(result)
}


## ------------------------------------------------------------
## Run pairwise Cox models
## ------------------------------------------------------------

insulin_group_0_2 <- "OF only insulin (balanced +0-2 yrs)"
insulin_group_2_5 <- "OF only insulin (balanced +2-5 yrs)"

drug_group_0_2 <- "OF drug start +0-2 yrs"
drug_group_2_5 <- "OF drug start +2-5 yrs"

cox_result_0_2 <- run_pairwise_cox(
  data = summary_for_cox,
  insulin_group = insulin_group_0_2,
  drug_group = drug_group_0_2
)

cox_result_2_5 <- run_pairwise_cox(
  data = summary_for_cox,
  insulin_group = insulin_group_2_5,
  drug_group = drug_group_2_5
)

cox_results <- bind_rows(
  cox_result_0_2,
  cox_result_2_5
)

cat("\nPairwise Cox model results:\n")
print(cox_results)

print(run_pairwise_cox(
  data = summary_for_cox,
  insulin_group = insulin_group_2_5,
  drug_group = insulin_group_0_2
))

drug_compare <- summary_for_cox %>%
  filter(group_clean %in% c("OF drug start +0-2 yrs", "OF drug start +2-5 yrs")) %>%
  mutate(
    drug_group = factor(
      group_clean,
      levels = c("OF drug start +0-2 yrs", "OF drug start +2-5 yrs")
    )
  )

fit_drug_compare <- coxph(
  Surv(time, status) ~ drug_group,
  data = drug_compare
)

fit_sum <- summary(fit_drug_compare)

drug_group_comparison <- data.frame(
  comparison = "drug start +2-5 yrs vs +0-2 yrs",
  n_0_2 = sum(drug_compare$drug_group == "OF drug start +0-2 yrs"),
  n_2_5 = sum(drug_compare$drug_group == "OF drug start +2-5 yrs"),
  events_0_2 = sum(drug_compare$status[drug_compare$drug_group == "OF drug start +0-2 yrs"] == 1),
  events_2_5 = sum(drug_compare$status[drug_compare$drug_group == "OF drug start +2-5 yrs"] == 1),
  HR = fit_sum$conf.int["drug_groupOF drug start +2-5 yrs", "exp(coef)"],
  HR_lower_95 = fit_sum$conf.int["drug_groupOF drug start +2-5 yrs", "lower .95"],
  HR_upper_95 = fit_sum$conf.int["drug_groupOF drug start +2-5 yrs", "upper .95"],
  p_value = fit_sum$coefficients["drug_groupOF drug start +2-5 yrs", "Pr(>|z|)"]
)

print(drug_group_comparison)

## ------------------------------------------------------------
## Save results
## ------------------------------------------------------------

# write.csv(
#   cox_results,
#   paste0(
#     "../T1D_EMR_figures/figures_stringent/cox_pvalues_",
#     drug_classification, "_", drug, "_OneFlorida_V1.csv"
#   ),
#   row.names = FALSE
# )
