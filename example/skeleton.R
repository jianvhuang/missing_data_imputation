params <-
list(performance_path = "agg_results_all.csv", cor_plot_all = "correlation_heatmap_all.png", 
    cor_plot_cont = "correlation_heatmap_continuous.png", selected_features_path = "selected_features.csv", 
    summary_path = "summary_stats.csv", missing_path = "missing_summary.csv", 
    boxplot_path = "boxplots_before_after.png", density_path = "density_plots_before_after.png", 
    barplot_path = "barplots_cat_before_after.png", raw_missing_rate = 0, 
    data_file = "unknown.csv", performance_plot = "lineplot_MSE_Accuracy_combined.png")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = params$dir_output)
library(ggplot2)
library(readr)
library(knitr)
library(dplyr)
library(tinytex)

## ----dataset-info, echo=FALSE, results='asis'---------------------------------
cat(paste0("**Data file used:** ", params$data_file, "  \n",
           "**Overall missing rate:** ", sprintf("%.2f%%", params$raw_missing_rate * 100), "  \n\n"))

# ---- Variables used for imputation ----
features_file <- file.path(params$dir_output, params$selected_features_path)
if (file.exists(features_file)) {
  features <- read_csv(features_file, show_col_types = FALSE)
  cat("**Variables used for imputation:**  \n\n")
  for (v in features$selected_features) {
    cat(paste0("- ", v, "  \n"))
  }
  cat("\n")
} else {
  cat("**Variables used for imputation:** File not found.**  \n\n")
}

# ---- Best model info ----
best_model_info <- read_csv(file.path(params$dir_output, params$performance_path)) %>%
  arrange(Average_MSE, desc(Average_Accuracy)) %>%
  slice(1)
model_text <- paste0("**Best model selected: **", params$best_model_method)
if (params$best_model_method == "KNN" && !is.na(params$best_model_k)) {
  model_text <- paste0(model_text, " (K = ", params$best_model_k, ")")
}
cat(paste0(model_text, "  \n"))

cat(paste0("**Imputed dataset saved as: **", params$output_file_name, "  \n"))

## ----load-image-paths, echo=FALSE---------------------------------------------
cor_all_path <- file.path(params$dir_output, params$cor_plot_all)
cor_cont_path <- file.path(params$dir_output, params$cor_plot_cont)
boxplot_path <- file.path(params$dir_output, params$boxplot_path)
density_path <- file.path(params$dir_output, params$density_path)
barplot_path <- file.path(params$dir_output, params$barplot_path)

## ----show-cor-cont, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Correlation Heatmap - Continuous Variables", fig.pos="H"----
knitr::include_graphics(cor_cont_path)

## ----show-cor-all, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Correlation Heatmap - All Variables", fig.pos="H"----
knitr::include_graphics(cor_all_path)

## ----imputation-params, echo=FALSE, results='asis'----------------------------

cat("**Imputation Configuration:**  \n")

cat(paste0("- Missing rate levels (`list_noNA`): ", paste(params$list_noNA, collapse = ", "), "  \n"))

cat(paste0("- Number of Trees (`ntree`): ", paste(params$ntree, collapse = ", "), "  \n"))

cat(paste0("- Maximum Number of Iterations in RandomForest (`maxiter`): ", paste(params$maxiter, collapse = ", "), "  \n"))

cat(paste0("- K values for KNN (`k_values`): ", paste(params$k_values, collapse = ", "), "  \n"))

cat(paste0("- MICE iterations (`mice_m`): ", params$mice_m, "  \n"))

cat(paste0("- MICE maxit (`mice_maxit`): ", params$mice_maxit, "  \n"))

cat(paste0("- Number of decision trees in MiceRanger  (`micer_num_trees`): ", params$micer_num_trees, "  \n"))

cat(paste0("- Random seed (`seed`): ", params$seed, "  \n"))

cat(paste0("- Number of iterations (`niter`): ", params$niter, "  \n"))

cat(paste0("- Methods used to simulate imputation (`methods`): ",
           paste(params$methods_used, collapse = ", "), "\n"))

## ----load-performance---------------------------------------------------------
perf_path <- file.path(params$dir_output, params$performance_path)
agg_results <- read_csv(perf_path)
kable(agg_results, caption = "Model Performance Across Missing Rates")

## ----show-performance-lineplot, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Performance Performance Comparison Plot Across Missing Rates", fig.pos="H"----
performance_plot <- file.path(params$dir_output, params$performance_plot)

if (file.exists(performance_plot)) {
  knitr::include_graphics(performance_plot)
} else {
  cat("**Performance plot not found.**  \n")
}

## ----compare-before-after-----------------------------------------------------
missing_summary <- read_csv(file.path(params$dir_output, params$missing_path))
kable(missing_summary, caption = "Missing Values Before vs. After Imputation")

summary_stats <- read_csv(file.path(params$dir_output, params$summary_path))
kable(summary_stats, caption = "Summary Statistics Before vs. After Imputation", format = "latex", booktabs = TRUE, longtable = TRUE)

## ----show-boxplot, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Boxplots (Before vs After)", fig.pos="H"----
knitr::include_graphics(boxplot_path)

## ----show-densityplot, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Density Plots (Before vs After)", fig.pos="H"----
knitr::include_graphics(density_path)

## ----show-barplot, echo=FALSE, out.width="90%", fig.align="center", fig.cap="Bar Plots (Before vs After)", fig.pos="H"----
knitr::include_graphics(barplot_path)

