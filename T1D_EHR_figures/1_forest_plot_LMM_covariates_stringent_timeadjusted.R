
library(tibble)
library(forestploter)
library(dplyr)
library(stringr)
library(Cairo)
library(grid)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_Meta/")

format_pval_parse <- function(p) {
  sapply(p, function(x) {
    if (x >= 0.01) {
      return(sprintf("%.2f", x))
    }
    exp <- floor(log10(x))
    base <- x / (10^exp)
    base_str <- sprintf("%.1f", base)
    
    paste0(base_str, " %*% 10^{", exp, "}")
  })
}

num_level <- 1
level <- c("ATC_class", "ATC_drug")[num_level]
level_drug_name <- c("Drug_class_name", "Drug_name")[num_level]
drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[2]

ucla <- read.csv(paste0("/u/project/xyang123/rainyliu/T1D_EMR_UCLA/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", 
                        drug_classification, "_30day.period_365day.range_1measure_results_", level, ".csv"))
ucla <- ucla[,c(level, level_drug_name, "estimate", "ci_upper", "p_value", "num_on_drug")]
ucla$lower <- -2
ucla$p_value <- format_pval_parse(ucla$p_value)
colnames(ucla)[3:4] <- c("mean", "upper")
colnames(ucla)[3:7] <- paste0(colnames(ucla)[3:7])
ucla <- subset(ucla, num_on_drug >= 10)
ucla <- as_tibble(ucla)

ucsd <- read.csv(paste0("/u/project/xyang123/rainyliu/T1D_EMR_UCSD/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", 
                        drug_classification, "_30day.period_365day.range_1measure_results_", level, ".csv"))
ucsd <- ucsd[,c(level, level_drug_name, "estimate", "ci_upper", "p_value", "num_on_drug")]
ucsd$lower <- -2
ucsd$p_value <- format_pval_parse(ucsd$p_value)
colnames(ucsd)[3:4] <- c("mean", "upper")
colnames(ucsd)[3:7] <- paste0(colnames(ucsd)[3:7])
ucsd <- subset(ucsd, num_on_drug >= 10)
ucsd <- as_tibble(ucsd)

of <- read.csv(paste0("/u/project/xyang123/rainyliu/T1D_EMR_OneFlorida_V1/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", 
                      drug_classification, "_30day.period_365day.range_1measure_results_", level, ".csv"))
of <- of[,c(level, level_drug_name, "estimate", "ci_upper", "p_value", "num_on_drug")]
of$lower <- -2
of$p_value <- format_pval_parse(of$p_value)
colnames(of)[3:4] <- c("mean", "upper")
colnames(of)[3:7] <- paste0(colnames(of)[3:7])
of <- subset(of, num_on_drug >= 10)
of <- as_tibble(of)

summary <- ucla %>%
  rename_with(~ paste0(., "_ucla"), -all_of(c(level, level_drug_name))) %>%
  full_join(
    ucsd %>% rename_with(~ paste0(., "_ucsd"), -all_of(c(level, level_drug_name))),
    by = c(level, level_drug_name)
  ) %>%
  full_join(
    of %>% rename_with(~ paste0(., "_of"), -all_of(c(level, level_drug_name))),
    by = c(level, level_drug_name)
  )
summary$Hba1c_result <- paste(rep(" ", ifelse(level == "ATC_class", 25, 30)), collapse = " ")
summary$num_on_drug_ucla <- ifelse(is.na(summary$num_on_drug_ucla), "", summary$num_on_drug_ucla)
summary$num_on_drug_ucsd <- ifelse(is.na(summary$num_on_drug_ucsd), "", summary$num_on_drug_ucsd)
summary$num_on_drug_of <- ifelse(is.na(summary$num_on_drug_of), "", summary$num_on_drug_of)

summary <- summary %>%
  mutate(
    LEVEL = .data[[level]],
    DRUG_NAME = .data[[level_drug_name]]
  )

meta_result <- read.csv(paste0("./data_meta_p/paired_LMM_covariates_stringent_timeadjusted_sensitivity/", drug_classification,
                               "_30day.period_halfyr.range_1measure_results_", level, "_metap.csv"))
meta_result <- meta_result[,c(level, "meta_fdr")]
meta_result$meta_fdr <- format_pval_parse(meta_result$meta_fdr)
summary <- merge(summary, meta_result, by = level)

if(level == "ATC_class" & drug_classification == "T2D"){
  order_vec <- c("A10BJ", "A10BK", "A10BA", "A10BH", "A10BG",
                 "A10BD", "A10BB", "A10BX", "A10BF")
  summary$Drug_class_name <- ifelse(summary$LEVEL == "A10BJ", 
                                    "Glucagon-like peptide-1\n(GLP-1) analogues", 
                                    summary$Drug_class_name)
  summary$Drug_class_name <- ifelse(summary$LEVEL == "A10BK", 
                                    "Sodium-glucose co-transporter 2\n(SGLT2) inhibitors", 
                                    summary$Drug_class_name)
  summary$Drug_class_name <- ifelse(summary$LEVEL == "A10BD", 
                                    "Combinations of oral\nblood glucose lowering drugs", 
                                    summary$Drug_class_name)
  summary$Drug_class_name <- ifelse(summary$LEVEL == "A10BX", 
                                    "Other blood glucose lowering drugs,\nexcl. insulins", 
                                    summary$Drug_class_name)
}
if(level == "ATC_drug" & drug_classification == "T2D"){
  order_vec <- c("A10BJ05", "A10BJ01", "A10BK03", "A10BA02", "A10BJ06",
                 "A10BJ02", "A10BH01", "A10BK01", "A10BK02", "A10BG03")
}
if(level == "ATC_class" & drug_classification == "antihypertensive"){
  order_vec <- c("C02AC", "C02DB", "C07AB", "C07AG", "C03CA", 
                 "C09AA", "C08CA", "C03AA", "C09CA", "C03BA")
  summary$Drug_class_name <- ifelse(summary$LEVEL == "C09CA", 
                                    "Angiotensin II receptor blockers\n(ARBs), plain", 
                                    summary$Drug_class_name)
}
if(level == "ATC_drug" & drug_classification == "antihypertensive"){
  order_vec <- c("C07AG01", "C02DB02", "C02AC01", "C08CA01",  
                 "C03CA01", "C09AA03", "C02DB03", "C07AB02",
                 "C07AG02", "C03AA03", "C09CA01")
}
if(level == "ATC_class" & drug_classification == "lipid_modifying"){
  order_vec <- c("C10AC", "C10AA", "C10AB", "C10AX", "C10AD", "C10BX")
  summary$Drug_class_name <- ifelse(summary$LEVEL == "C10BX", 
                                    "Lipid modifying agents\nin combination with other drugs", 
                                    summary$Drug_class_name)
}
if(level == "ATC_drug" & drug_classification == "lipid_modifying"){
  order_vec <- c("C10AC01", "C10AX13", "C10BX03", "C10AA05", "C10AA07", "C10AB05", 
                 "C10AA01", "C10AX06", "C10AD02")
  summary$Drug_name <- ifelse(summary$LEVEL == "C10AX06", 
                              "omega-3-triglycerides\nincl. other esters and acids", 
                              summary$Drug_name)
}
summary <- summary %>%
  filter(LEVEL %in% order_vec) %>%
  mutate(LEVEL = factor(LEVEL, levels = order_vec)) %>%
  arrange(LEVEL)

fig_dt <- summary[, c(1,2,16,15,6,5,11,10,21,18)]
colnames(fig_dt)[1:2] <- if (level == "ATC_class") {
  c("ATC Class", "Drug Class\nName")
} else {
  c("ATC Drug", "Drug Name")
}
colnames(fig_dt)[3:10] <- c("OF\nNo. patient", "OF\nP value",
                            "UCLA\nNo. patient", "UCLA\nP value",
                            "UCSD\nNo. patient", "UCSD\nP value", "META\nP adjusted",
                            "bold(Delta*HbA*\"1\"*c*\"(%) / Year\"~\"(95% CI)\")")
parse_mat <- matrix(FALSE, 
                    nrow = nrow(fig_dt),
                    ncol = ncol(fig_dt))
parse_mat[, c(4,6,8,9)] <- TRUE

tm <- forest_theme(base_size = 9,
                   ci_pch = 15,
                   ci_col = c("darkred", "darkblue", "darkgreen"),
                   ci_fill = c("brown2", "royalblue", "seagreen"),
                   ci_lwd = 1,
                   ci_Theight = unit(1.7, "mm"),
                   refline_gp = gpar(lwd = 1, col = "gray70"),
                   xaxis_gp = gpar(lwd = 1),
                   
                   legend_position = "none",
                   colhead = list(fg_params = list(parse = TRUE, hjust = 0.5, x = 0.5, fontface = "bold")), 
                   core = list(padding = unit(c(2, 1.5), "mm"),
                               fg_params = list(parse = parse_mat, hjust = 0.5, x = 0.5),
                               bg_params = list(fill = c("#EFEFEF", "#FFFFFF"))),
                   legend_name = "Cohort",
                   legend_value = c("OneFlorida", "UCLA", "UCSD"))

xlim_range <- list("antihypertensive_ATC_class" = c(-0.4,0.4), "antihypertensive_ATC_drug" = c(-0.35,0.35),
                   "lipid_modifying_ATC_class" = c(-0.6,0.6), "lipid_modifying_ATC_drug" = c(-1.5,1.5))[[paste0(drug_classification, "_", level)]]

p <- forest(fig_dt,
            est = list(summary$mean_of,
                       summary$mean_ucla,
                       summary$mean_ucsd),
            lower = list(summary$lower_of,
                         summary$lower_ucla,
                         summary$lower_ucsd), 
            upper = list(summary$upper_of,
                         summary$upper_ucla,
                         summary$upper_ucsd),
            sizes = 0.9,
            ci_column = 10,
            ref_line = 0,
            nudge_y = 0.3,
            theme = tm) |>
  set_xaxis(xlim = xlim_range)
plot(p)
print(p)
convertHeight(p$heights, "mm", valueOnly = TRUE) 
p$heights <- rep(unit(ifelse(level == "ATC_class", 9, 8.5), "mm"), nrow(p))

convertWidth(p$widths, "mm", valueOnly = TRUE)
if(level == "ATC_class"){
  p$widths <- c(5,20,45,rep(18, 7), 60, 5)
}
if(level == "ATC_drug"){
  p$widths <- c(5,20,35,rep(20*6/7, 7), 60, 5)
}


## T2D atc_class 10, 4
## T2D atc_code 10, 7.5
## antihypertensive atc_class 10, 8
## antihypertensive atc_code 10, 8
## lipid_modifying atc_class 10, 3
## lipid_modifying atc_class 10, 10

# ggplot2::ggsave(paste0("../T1D_EMR_figures/figures/forestplot_paired_LMM_", drug_classification, "_", level, ".pdf"),
#                 plot = p,
#                 width = 10, height = height)


height <- list("T2D_ATC_class" = 4, "T2D_ATC_drug" = 4.5,
               "antihypertensive_ATC_class" = 5, "antihypertensive_ATC_drug" = 5,
               "lipid_modifying_ATC_class" = 4, "lipid_modifying_ATC_drug" = 4.5)[[paste0(drug_classification, "_", level)]]

pdf(paste0("../T1D_EMR_figures/figures_stringent/forestplot_paired_LMM_covariates_stringent_timeadjusted_", 
           drug_classification, "_", level, ".pdf"),
    width = 10.5, height = height)
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(p)

dev.off()
