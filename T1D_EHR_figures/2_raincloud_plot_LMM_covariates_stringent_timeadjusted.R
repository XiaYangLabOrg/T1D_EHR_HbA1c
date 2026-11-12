
library(ggrain)
library(ggplot2)
library(dplyr)
library(rjson)
library(colorspace)
setwd("/u/project/xyang123/rainyliu/T1D_EMR_Meta/")

drug_classification <- c("T2D", "antihypertensive", "lipid_modifying")[1]
drug_reference <- read.csv(file = paste0("../T1D_EMR_fuzzy_search/", drug_classification, "/", drug_classification, "_drugs.csv"))

### Plot difference between two time points
OFV1 <- read.csv(paste0("../T1D_EMR_OneFlorida_V1/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", drug_classification,
                        "_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"))
OFV1$Cohort <- "OF"
OFV1$Class <- "OF"
UCLA <- read.csv(paste0("../T1D_EMR_UCLA/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", drug_classification,
                        "_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"))
UCLA$Cohort <- "UCLA"
UCLA$Class <- "UCLA"
UCSD <- read.csv(paste0("../T1D_EMR_UCSD/intermediate_data/paired_LMM_covariates_stringent_timeadjusted/", drug_classification,
                        "_30day.period_365day.range_1measure_drug_time_summary_ATC_drug.csv"))
UCSD$Cohort <- "UCSD"
UCSD$Class <- "UCSD"

### Add differences
OFV1_ref <- read.csv(paste0("../T1D_EMR_OneFlorida_V1/intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference.csv"))
OFV1_ref$Cohort <- "OF_ref"
OFV1_ref$Class <- "OF"
UCLA_ref <- read.csv(paste0("../T1D_EMR_UCLA/intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference.csv"))
UCLA_ref$Cohort <- "UCLA_ref"
UCLA_ref$Class <- "UCLA"
UCSD_ref <- read.csv(paste0("../T1D_EMR_UCSD/intermediate_data/graph_additional_stringent/T1D_lab_hba1c_percent_difference.csv"))
UCSD_ref$Cohort <- "UCSD_ref"
UCSD_ref$Class <- "UCSD"
references <- rbind(OFV1_ref, UCLA_ref, UCSD_ref)
references <- references[,c("patient_id", "DIFF_timeadjusted", "Cohort", "Class")]
colnames(references) <- c("patient_id", "diff", "Cohort", "Class")

if(drug_classification == "T2D"){
  drug <- c("A10BJ05", "A10BK03", "A10BA02", "A10BH01", "A10BG03", "A10BB12", "A10BK01")[7]
} else if(drug_classification == "antihypertensive"){
  drug <- c("C03CA01", "C08CA01", "C09AA03", "C02DB02", "C07AB02", "C07AG02", "C09CA01", "C03AA03", "C07AG01", "C02AC01")[10]
} else {
  drug <- c("C10AA05", "C10AB05", "C10AC01", "C10AX13")[4]
}
drug_name <- drug_reference[drug_reference$ATC_code == drug, "Drug_name", drop = TRUE][1]
OFV1_sub <- subset(OFV1, ATC_drug == drug)
UCLA_sub <- subset(UCLA, ATC_drug == drug)
UCSD_sub <- subset(UCSD, ATC_drug == drug)
if(nrow(UCSD_sub) <= 15){
  full_sub <- rbind(OFV1_sub, UCLA_sub)
  full_sub <- full_sub[,c("patient_id", "DIFF_timeadjusted", "Cohort", "Class")]
  colnames(full_sub)[2] <- "diff"
  references_sub <- subset(references, Cohort != "UCSD_ref" & ! patient_id %in% full_sub$patient_id)
  full_sub <- rbind(full_sub, references_sub)
} else{
  full_sub <- rbind(OFV1_sub, rbind(UCLA_sub, UCSD_sub))
  full_sub <- full_sub[,c("patient_id", "DIFF_timeadjusted", "Cohort", "Class")]
  colnames(full_sub)[2] <- "diff"
  references_sub <- subset(references, ! patient_id %in% ! patient_id %in% full_sub$patient_id)
  full_sub <- rbind(references_sub, full_sub)
}
full_sub$is_ref <- grepl("_ref$", full_sub$Cohort)
full_sub <- subset(full_sub, abs(diff) <= 2)


cohort_color <- c(OF_ref = "lightpink1", OF = "brown3", 
                  UCLA_ref = "lightskyblue1", UCLA = "royalblue3", 
                  UCSD_ref = "olivedrab2", UCSD = "seagreen")
full_sub$Cohort <- factor(full_sub$Cohort, 
                          levels = c("UCSD_ref", "UCSD", "UCLA_ref", "UCLA", "OF_ref", "OF"))
full_sub$Class <- factor(full_sub$Class, 
                         levels = c("UCSD", "UCLA", "OF"))

ggplot(full_sub, aes(Class, diff, fill = Cohort)) +
  
  ## ---- TREATMENT rainclouds (saturated, on top) ----
geom_rain(
  data = subset(full_sub, !is_ref),
  rain.side = "r",
  alpha = 0.5,
  boxplot.args = list(color = "grey10", outlier.shape = NA, alpha = 0.4),
  boxplot.args.pos = list(
    position = ggpp::position_dodgenudge(x = .05, width = 0.2),
    width = 0.1
  ),
  violin.args = list(aes(color = Cohort), alpha = 0.5),
  point.args = list(alpha = 0)
) +
  
  ## ---- REF rainclouds (lighter, behind) ----
geom_rain(
  data = subset(full_sub, is_ref),
  rain.side = "r",
  alpha = 0.35,
  boxplot.args = list(color = "grey10", outlier.shape = NA, alpha = 0.6),
  boxplot.args.pos = list(
    position = ggpp::position_dodgenudge(x = .05, width = 0.2),
    width = 0.1
  ),
  violin.args = list(aes(color = Cohort),alpha = 0.35),
  point.args = list(alpha = 0)
) +
  scale_color_manual(values = cohort_color) + 
  guides(color = "none") +
  ggnewscale::new_scale_color() +
  
  ## ---- Treatment dots only ----
geom_point(
  data = subset(full_sub, !is_ref & Class == "UCLA"),
  aes(x = Class, y = diff, color = diff),
  position = ggpp::position_dodgenudge(x = -0.3, width = ifelse(drug_classification == "T2D", 0.4, 0.35)),
  alpha = 0.8,
  size = 1.6,
  shape = 16,
  stroke = 0
) +
  scale_color_gradientn(
    # colors = c("royalblue3", "grey70", "grey80"),
    # values = scales::rescale(c(-5, 0, 5)),
    colors = c("royalblue3", "grey85"),
    limits = c(-2,2)
  ) +
  guides(color  = "none") + 
  ggnewscale::new_scale_color() +
  
  geom_point(
    data = subset(full_sub, !is_ref & Class == "OF"),
    aes(x = Class, y = diff, color = diff),
    position = ggpp::position_dodgenudge(x = -0.3, width = ifelse(drug_classification == "T2D", 0.4, 0.35)),
    alpha = 0.8,
    size = 1.6,
    shape = 16,
    stroke = 0
  ) +
  scale_color_gradientn(
    colors = c("brown3", "grey85"),
    # values = scales::rescale(c(-5, 0, 5)),
    limits = c(-2,2)
  ) + 
  guides(color  = "none") + 
  ggnewscale::new_scale_color() +
  
  geom_point(
    data = subset(full_sub, !is_ref & Class == "UCSD"),
    aes(x = Class, y = diff, color = diff),
    position = ggpp::position_dodgenudge(x = -0.3, width = ifelse(drug_classification == "T2D", 0.4, 0.35)),
    alpha = 0.8,
    size = 1.6,
    shape = 16,
    stroke = 0
  ) +
  scale_color_gradientn(
    colors = c("seagreen4", "grey85"),
    limits = c(-2,2)
  ) + 
  guides(color  = "none") + 
  
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey40", alpha = 0.5) +
  theme_classic() +
  scale_fill_manual(values = cohort_color) +
  # scale_color_manual(values = cohort_color) +
  guides(
    fill  = guide_legend(reverse = TRUE)
  ) + 
  coord_flip() +
  ylim(-2,2) +
  labs(
    title = paste(drug, "-", drug_name),
    x = "Cohort",
    # y = "Difference in HbA1c(%) / Year"
    y = expression(Delta*" HbA1c(%) / Year")
  ) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.text.y = element_text(size = 10))

ggsave(paste0("../T1D_EMR_figures/figures_stringent/raincloudplot_", drug_classification, "_", drug, "_difference.png"), width = 6, height = 3.5, units = "in", dpi = 600)

