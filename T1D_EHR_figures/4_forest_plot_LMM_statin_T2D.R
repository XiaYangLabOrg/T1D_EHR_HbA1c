
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
    if(x == 0){
      return(x)
    }
    exp <- floor(log10(x))
    base <- x / (10^exp)
    base_str <- sprintf("%.1f", base)
    
    paste0(base_str, " %*% 10^{", exp, "}")
  })
}

num_level <- 2
level <- c("ATC_class", "ATC_drug")[num_level]
level_drug_name <- c("Drug_class_name", "Drug_name")[num_level]
drug_classification <- "lipid_modifying"

data_address <- "/u/project/xyang123/rzhang/T1D_EHR/T2D_statin/intermediate_data/T2D_final_newuser_post40_no180/results/"

below40 <- read.csv(paste0(data_address, "post_T2D_age_lt40_", level, ".csv"))
below40 <- below40[,c("drug_code", "annotation", "estimate", "ci_upper", "p_value", "num_on_drug", "ci_lower")]
colnames(below40)[1:2] <- c(level, level_drug_name)
below40$p_value <- format_pval_parse(below40$p_value)
colnames(below40)[c(3:4,7)] <- c("mean", "upper", "lower")
below40 <- subset(below40, num_on_drug >= 10)
below40 <- as_tibble(below40)

above40 <- read.csv(paste0(data_address, "post_T2D_age_ge40_", level, ".csv"))
above40 <- above40[,c("drug_code", "annotation", "estimate", "ci_upper", "p_value", "num_on_drug", "ci_lower")]
colnames(above40)[1:2] <- c(level, level_drug_name)
above40$p_value <- format_pval_parse(above40$p_value)
colnames(above40)[c(3:4,7)] <- c("mean", "upper", "lower")
above40 <- subset(above40, num_on_drug >= 10)
above40 <- as_tibble(above40)

statininduced <- read.csv(paste0(data_address, "pre_T2D_365_", level, ".csv"))
statininduced <- statininduced[,c("drug_code", "annotation", "estimate", "ci_upper", "p_value", "num_on_drug", "ci_lower")]
colnames(statininduced)[1:2] <- c(level, level_drug_name)
statininduced$p_value <- format_pval_parse(statininduced$p_value)
colnames(statininduced)[c(3:4,7)] <- c("mean", "upper", "lower")
statininduced <- subset(statininduced, num_on_drug >= 10)
statininduced <- as_tibble(statininduced)

summary <- below40 %>%
  rename_with(~ paste0(., "_below40"), -all_of(c(level, level_drug_name))) %>%
  full_join(
    above40 %>% rename_with(~ paste0(., "_above40"), -all_of(c(level, level_drug_name))),
    by = c(level, level_drug_name)
  ) %>%
  full_join(
    statininduced %>% rename_with(~ paste0(., "_statininduced"), -all_of(c(level, level_drug_name))),
    by = c(level, level_drug_name)
  )
summary$Hba1c_result <- paste(rep(" ", ifelse(level == "ATC_class", 25, 30)), collapse = " ")
summary$num_on_drug_above40 <- ifelse(is.na(summary$num_on_drug_above40), "", summary$num_on_drug_above40)
summary$num_on_drug_below40 <- ifelse(is.na(summary$num_on_drug_below40), "", summary$num_on_drug_below40)
summary$num_on_drug_statininduced <- ifelse(is.na(summary$num_on_drug_statininduced), "", summary$num_on_drug_statininduced)

summary <- summary %>%
  mutate(
    LEVEL = .data[[level]],
    DRUG_NAME = .data[[level_drug_name]]
  )

if(level == "ATC_class" & drug_classification == "lipid_modifying"){
  order_vec <- c("C10AA", "C10AB", "C10BX", "C10AX", "C10AC", "C10AD", "C10BA")
  summary$Drug_class_name <- ifelse(summary$LEVEL == "C10BX",
                                    "Lipid modifying agents\nin combination with other drugs",
                                    summary$Drug_class_name)
  summary$Drug_class_name <- ifelse(summary$LEVEL == "C10BA",
                                    "Combinations of various\nlipid modifying agents",
                                    summary$Drug_class_name)
}
if(level == "ATC_drug" & drug_classification == "lipid_modifying"){
  order_vec <- c("C10AA05", "C10BX03", "C10AA07", "C10AB05", "C10AA03",
                 "C10AX06", "C10AC01", "C10AD02", "C10AA01", "C10AA02", 
                 "C10AC04", "C10AX09", "C10AX13")
  summary$Drug_name <- ifelse(summary$LEVEL == "C10AX06",
                              "omega-3-triglycerides\nincl. other esters and acids",
                              summary$Drug_name)
}
summary <- summary %>%
  filter(LEVEL %in% order_vec) %>%
  mutate(LEVEL = factor(LEVEL, levels = order_vec)) %>%
  arrange(LEVEL)

fig_dt <- summary[, c(1,2,16,15,6,5,11,10,18)]
colnames(fig_dt)[1:2] <- if (level == "ATC_class") {
  c("ATC Class", "Drug Class\nName")
} else {
  c("ATC Drug", "Drug Name")
}
colnames(fig_dt)[3:9] <- c("Pre\nNo. patient", "Pre\nP value",
                           "Post <40\nNo. patient", "Post <40\nP value",
                           # "Post \u226540\nNo. patient", "Post \u226540\nP value",
                           "Post >40\nNo. patient", "Post >40\nP value",
                           "bold(Delta*HbA*\"1\"*c*\"(%) / Year\"~\"(95% CI)\")")
parse_mat <- matrix(FALSE, 
                    nrow = nrow(fig_dt),
                    ncol = ncol(fig_dt))
parse_mat[, c(4,6,8)] <- TRUE


tm <- forest_theme(base_size = 9,
                   ci_pch = 15,
                   ci_col = c("darkcyan", "#C2185B", "deeppink4"),
                   ci_fill = c("cyan3", "pink2", "deeppink3"),
                   ci_lwd = 1,
                   ci_Theight = unit(1.7, "mm"),
                   refline_gp = gpar(lwd = 1, col = "gray70"),
                   xaxis_gp = gpar(lwd = 1),
                   
                   legend_position = "none",
                   colhead = list(fg_params = list(parse = c(rep(FALSE, 8), TRUE), hjust = 0.5, x = 0.5, fontface = "bold")), 
                   core = list(padding = unit(c(2, 1.5), "mm"),
                               fg_params = list(parse = parse_mat, hjust = 0.5, x = 0.5),
                               bg_params = list(fill = c("#EFEFEF", "#FFFFFF"))),
                   legend_name = "Cohort",
                   legend_value = c("Pre-T2D statin-induced", "Post-T2D <40", "Post-T2D >40"))

xlim_range <- list("lipid_modifying_ATC_class" = c(-0.5,0.5), "lipid_modifying_ATC_drug" = c(-0.5,0.4))[[paste0(drug_classification, "_", level)]]

p <- forest(fig_dt,
            est = list(summary$mean_statininduced,
                       summary$mean_below40,
                       summary$mean_above40),
            lower = list(summary$lower_statininduced,
                         summary$lower_below40,
                         summary$lower_above40), 
            upper = list(summary$upper_statininduced,
                         summary$upper_below40,
                         summary$upper_above40),
            sizes = 0.9,
            ci_column = 9,
            ref_line = 0,
            nudge_y = 0.3,
            style = tm) |>
  set_xaxis(xlim = xlim_range)

plot(p)
convertHeight(p$heights, "mm", valueOnly = TRUE)
p$heights <- rep(unit(ifelse(level == "ATC_class", 9, 8.5), "mm"), nrow(p))

convertWidth(p$widths, "mm", valueOnly = TRUE)
if(level == "ATC_class"){
  p$widths <- c(5,20,50,rep(22, 6), 60, 5)
}
if(level == "ATC_drug"){
  p$widths <- c(5,20,35,rep(22, 6), 60, 5)
}


# ggplot2::ggsave(paste0("../T1D_EMR_figures/figures/forestplot_paired_LMM_", drug_classification, "_", level, ".pdf"),
#                 plot = p,
#                 width = 10, height = height)


height <- list("lipid_modifying_ATC_class" = 4, "lipid_modifying_ATC_drug" = 6)[[paste0(drug_classification, "_", level)]]

pdf(paste0("../T1D_EMR_figures/figures_stringent/forestplot_paired_LMM_covariates_stringent_timeadjusted_",
           drug_classification, "_", level, "_T2D_patients.pdf"),
    width = 10.5, height = height)
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(p)

dev.off()

## need to create the greater than or equal to sign with Adobe Illustrator
