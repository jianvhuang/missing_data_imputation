#' Run Imputation Analysis
#'
#' @description Performs a complete imputation analysis workflow on a dataset,
#' including data preparation, missing value analysis, imputation method comparison,
#' and report generation.
#'
#' @param data A data frame to analyze
#' @param continuous_vars Vector of continuous variable names
#' @param ordinal_vars Vector of ordinal variable names
#' @param nominal_vars Vector of nominal variable names
#' @param info_vars Vector of identifier variable names (default: NULL)
#' @param dir_output Directory where output files will be saved
#' @param data_file_name Name of the dataset for the report (default: "data")
#' @param output_file_name Name for the output report file (default: "imputation_report.pdf")
#' @param list_noNA Vector of missing rates to test (default: 0.05 to 0.5)
#' @param seed Random seed for reproducibility (default: 123)
#' @param niter Number of iterations (default: 10)
#' @param ntree Number of trees for Random Forest (default: 100)
#' @param maxiter Maximum iterations for Random Forest (default: 10)
#' @param k_values Vector of K values for KNN (default: c(3, 5, 7))
#' @param mice_m Number of imputations for MICE (default: 5)
#' @param mice_maxit Maximum iterations for MICE (default: 10)
#' @param micer_num_trees Number of trees for miceRanger (default: 100)
#' @param user_defined_features Additional features to include in imputation (default: NULL)
#'
#' @return A list containing imputation results
#' 
#' @details
#' This function executes a complete imputation analysis workflow:
#' \itemize{
#'   \item Prepares the dataset with proper variable types
#'   \item Performs missing value analysis
#'   \item Analyzes correlations between variables
#'   \item Compares multiple imputation methods (Random Forest, KNN, MICE, miceRanger)
#'   \item Selects the best-performing method
#'   \item Applies the selected method to the dataset
#'   \item Generates a comprehensive report
#' }
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(mtcars)
#' 
#' # Add some missing values
#' set.seed(123)
#' mtcars_missing <- mtcars
#' mtcars_missing[sample(1:nrow(mtcars) * ncol(mtcars), 30)] <- NA
#' 
#' # Run imputation analysis
#' result <- run_imputation_analysis(
#'   data = mtcars_missing,
#'   continuous_vars = c("mpg", "disp", "hp", "drat", "wt", "qsec"),
#'   ordinal_vars = c("cyl", "gear", "carb"),
#'   nominal_vars = c("vs", "am"),
#'   dir_output = "imputation_results/",
#'   data_file_name = "mtcars"
#' )
#' }
#'
#' @importFrom dplyr select mutate across
#' @importFrom rmarkdown render
#' @export
run_imputation_analysis <- function(
    data,
    continuous_vars,
    ordinal_vars,
    nominal_vars,
    info_vars = NULL,
    dir_output,
    dir_sourcecode = NULL,
    data_file_name = "data",
    output_file_name = "imputation_report.pdf",
    list_noNA = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
    seed = 123,
    niter = 10,
    ntree = 100,
    maxiter = 10,
    k_values = c(3, 5, 7),
    mice_m = 5,
    mice_maxit = 10,
    micer_num_trees = 100,
    user_defined_features = NULL
) {
  # Ensure required packages are loaded
  required_packages <- c("tidyr", "dplyr", "mice", "caret", "DMwR2", "polycor", 
                         "ggplot2", "VIM", "patchwork", "reshape2", "missForest", 
                         "miceRanger", "rmarkdown")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is needed for this function to work. Please install it."))
    }
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(dir_output)) {
    dir.create(dir_output, recursive = TRUE)
  }
  
  # Define variable types
  categorical_vars <- c(ordinal_vars, nominal_vars)
  
  # Extract information variables if provided
  if (!is.null(info_vars) && length(info_vars) > 0) {
    info_data <- data %>% select(all_of(info_vars))
  } else {
    info_data <- NULL
  }
  
  # Select relevant variables
  selected_vars <- c(continuous_vars, categorical_vars)
  data_selected <- data %>% select(all_of(selected_vars))
  
  # Convert data types
  data_selected <- data_selected %>%
    mutate(across(all_of(ordinal_vars), ~ factor(., ordered = TRUE)), 
           across(all_of(nominal_vars), as.factor))
  
  lapply(data_selected[ordinal_vars], levels)
  
  data_selected[categorical_vars] <- lapply(data_selected[categorical_vars], as.factor)
  data_selected[continuous_vars] <- lapply(data_selected[continuous_vars], as.numeric)
  
  # Missing value and correlation analysis
  raw_missing_rate <- check_missing_values(data_selected, dir_output)
  cor_vars <- check_correlation(data_selected, continuous_vars, dir_output)
  
  # Variables used to impute
  if (is.null(user_defined_features)) {
    user_defined_features <- c()
  }
  selected_features <- unique(c(cor_vars, user_defined_features))
  
  if (length(selected_features) == 0) {
    # If no correlated variables are found, use all variables
    selected_features <- selected_vars
    message("No strongly correlated variables found. Using all variables for imputation.")
  }
  
  message("Features used to impute: ", paste(selected_features, collapse = ", "))
  
  # Save selected features to CSV for report
  write.csv(data.frame(selected_features = selected_features),
            file = file.path(dir_output, "selected_features.csv"),
            row.names = FALSE)
  
  # Start imputation
  result <- MI_func(
    data_selected = data_selected, 
    dir_output = dir_output, 
    info_vars = info_data,
    list_noNA = list_noNA, 
    ntree = ntree, 
    maxiter = maxiter,
    k_values = k_values, 
    mice_m = mice_m,
    mice_maxit = mice_maxit,
    micer_num_trees = micer_num_trees,
    seed = seed,
    niter = niter,
    selected_features = selected_features
  )
  
  # Generate imputation report
  template_src <- system.file("rmarkdown/templates/imputation_report/skeleton/skeleton.Rmd", package = "ImputationR")
  template_dst <- file.path(dir_output, "skeleton.Rmd")
  
  file.copy(from = template_src, to = template_dst, overwrite = TRUE)
  
  
  rmarkdown::render(
    input = template_dst,
    output_file = output_file_name,
    output_dir = dir_output,
    knit_root_dir = dir_output,
    params = list(
      dir_output = dir_output,
      performance_path = "agg_results_all.csv",
      cor_plot_all = "correlation_heatmap_all.png",
      cor_plot_cont = "correlation_heatmap_continuous.png",
      selected_features_path = "selected_features.csv",
      summary_path = "summary_stats.csv",
      missing_path = "missing_summary.csv",
      boxplot_path = "boxplots_before_after.png",
      density_path = "density_plots_before_after.png",
      barplot_path = "barplots_cat_before_after.png",
      raw_missing_rate = raw_missing_rate,
      data_file = data_file_name,
      best_model_method = result$best_model$Method,
      best_model_k = result$best_model$Best_K_Value,
      output_file_name = result$output_file_name,
      performance_plot = "lineplot_MSE_Accuracy_combined.png",
      list_noNA = list_noNA,
      ntree = ntree, 
      maxiter = maxiter,
      k_values = k_values,
      mice_m = mice_m,
      mice_maxit = mice_maxit,
      micer_num_trees = micer_num_trees,
      seed = seed,
      niter = niter
    ))

  # Provide a summary message
  message("Imputation analysis completed successfully.")
  message("Best imputation method: ", result$best_model$Method)
  message("Output report saved to: ", file.path(dir_output, output_file_name))
  
  return(result)
}