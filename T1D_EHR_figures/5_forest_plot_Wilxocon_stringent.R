
## Use 
library(tibble)
library(forestploter) ## use 1.2.0
library(dplyr)
library(stringr)
library(Cairo)
library(grid)

setwd("/u/project/xyang123/rainyliu/T1D_EMR_FinnDiane/")

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

finndiane <- read.csv("finndiane_results.csv", check.names = FALSE)
finndiane <- finndiane %>%
  mutate(
    p_value_plot = paste0(format_pval_parse(p_value)),
    p_adj_plot = paste0(format_pval_parse(p_adj)),
    Box = paste(rep(" ", 35), collapse = " "),
    num_on_drug = as.character(num_on_drug)
  )
finndiane <- finndiane %>%
  mutate(
    ATC_drug = factor(
      ATC_drug,
      levels = c("A10BK03", "A10BK01", "A10BA02",
                 "C10AA05", "C10AA07", "C10AA01", "C10AA03", "C10AA02",
                 "C08CA01", "C09CA01", "C09CA07", "C09CA03", "C09DA03", "C09DA01", 
                 "C03CA01", "C03AA03", "C09AA02", "C03BA11"))
  ) %>%
  arrange(ATC_drug)

finndiane <- finndiane %>%
  mutate(
    ATC_drug = factor(
      ATC_drug,
      levels = unique(ATC_drug)
    )
  ) %>%
  arrange(ATC_drug)

finndiane$drug_class <- ifelse(grepl("A10B", finndiane$ATC_drug), "Glucose-lowering", 
                               ifelse(grepl("C10", finndiane$ATC_drug), "Lipid-modifying", "Antihypertensive"))

finndiane <- finndiane %>%
  mutate(
    drug_class = factor(
      drug_class,
      levels = c(
        "Glucose-lowering",
        "Lipid-modifying",
        "Antihypertensive"
      )
    )
  ) %>%
  arrange(drug_class, ATC_drug) %>%
  group_by(drug_class) %>%
  group_modify(~ bind_rows(
    .x,
    tibble(
      ATC_drug = "",
      Drug_name = "",
      num_on_drug = "",
      p_value = NA,
      p_adj = NA,
      median = NA,
      Q1 = NA,
      Q3 = NA,
      p_value_plot = "",
      Box = ""
    )
  )) %>%
  ungroup()

# Remove blank row after the final drug class
finndiane <- finndiane[-nrow(finndiane), ]
finndiane$drug_class <- as.character(finndiane$drug_class)
finndiane$ATC_drug <- as.character(finndiane$ATC_drug)
finndiane[c(4,10), "drug_class"] <- ""

fig_dt <- finndiane[, c("drug_class", "ATC_drug", "Drug_name",
                        "num_on_drug", "p_value_plot", "p_adj_plot", "Box")]
colnames(fig_dt) <- c("Drug Class", "ATC Drug", "Drug Name", 
                      "No. patient", "P value", "P adjusted", 
                      'bold(Delta*HbA*"1"*c*" (%)")')

parse_mat <- matrix(FALSE, nrow = nrow(fig_dt), ncol = ncol(fig_dt))
parse_mat[, c(5,6)] <- TRUE

my_ci <- function(est, lower, upper, sizes, xlim, pch, gp, t_height, nudge_y,
                  lowhinge, uphinge, hinge_height = 0.25, gp_box = gpar(), ...) {
  grobTree(
    # Q1-Q3 box
    rectGrob(x = unit((lowhinge + uphinge) / 2, "native"), y = unit(0.5 + nudge_y, "npc"),
             width = unit(uphinge - lowhinge, "native"), height = unit(hinge_height, "npc"),
             gp = gp_box),
    # Median vertical line
    segmentsGrob(x0 = unit(est, "native"), x1 = unit(est, "native"),
                 y0 = unit(0.5 + nudge_y - hinge_height / 2, "npc"), y1 = unit(0.5 + nudge_y + hinge_height / 2, "npc"),
                 gp = gpar(col = "mediumpurple4", lwd = 2, lineend = "butt")),
    # IMPORTANT: establish native x coordinates
    vp = viewport(xscale = xlim)
  )
}
tm <- forest_theme(base_size = 9,
                   ci_Theight = 0.2,
                   ci_col = "mediumpurple4",
                   ci_lwd = 0,
                   refline_gp = gpar(lwd = 1, col = "gray70"),
                   xaxis_gp = gpar(lwd = 1),
                   
                   legend_position = "none",
                   colhead = list(fg_params = list(parse = TRUE, hjust = 0.5, x = 0.5, fontface = "bold")),
                   core = list(padding = unit(c(2, 1.5), "mm"),
                               fg_params = list(parse = parse_mat, hjust = 0.5,x = 0.5),
                               bg_params = list(fill = c("#EFEFEF", "#FFFFFF"))))
p <- forestploter::forest(fig_dt, 
            est = finndiane$median, 
            lower = finndiane$Q1,
            upper = finndiane$Q3,
            # fn_ci = make_boxplot,
            fn_ci = my_ci,
            ci_column = 7,
            sizes = 1.3,
            
            # hinge
            lowhinge = finndiane$Q1,
            uphinge = finndiane$Q3,
            hinge_height = 0.35,
            index_args = c("lowhinge", "uphinge"),
            
            ## Orange/yellow box
            gp_box = gpar(fill = adjustcolor("#C5B0E3", alpha.f = 0.1), col = "mediumpurple4",  lwd = 1),
            ref_line = 0,
            style = tm
) |>
  set_xaxis(c(-1.25, 0.5), ticks_at = seq(from = -1, to = 0.5, by = 0.5))
plot(p)

# p$heights <- c(rep(unit(8.5, "mm"), 5), unit(7, "mm"), rep(unit(8.5, "mm"), 5), unit(7, "mm"), rep(unit(8.5, "mm"), 12))
p$heights <- rep(unit(8.5, "mm"), nrow(p))
convertWidth(p$widths, "mm", valueOnly = TRUE)
p$widths <- c(5, 45, 25, 35, rep(30, 3), 65, 5)

pdf(
  "../T1D_EMR_figures/figures_stringent/forestplot_Wilcoxon_FinnDiane_HbA1c_ATC_drug.pdf",
  width = 10.5,
  height = 7.5
)
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(p)
dev.off()

