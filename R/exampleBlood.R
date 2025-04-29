rm(list = ls())

library(ImputationR)
library(dplyr)

remotes::install_github("higgi13425/medicaldata")
library(medicaldata)
data("blood_storage", package = "medicaldata")
data <- blood_storage

# Create temporary directory
temp_dir <- tempdir()
dir_output <- file.path(temp_dir, "imputation-blood")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
cat("Output will be saved to:", dir_output, "\n")

# Because the raw missing rate is only 0.5%, we want to simulate higher missings.
simulate_missing_by_rows <- function(data, additional_rate = 0.1, max_missing_per_row = 2, seed = 123) {
  set.seed(seed)

  original_missing_count <- sum(is.na(data))
  total_cells <- prod(dim(data))
  original_missing_rate <- original_missing_count / total_cells

  cat(sprintf("Original missing rate: %.4f\n", original_missing_rate))

  target_missing_rate <- original_missing_rate + additional_rate
  target_missing_count <- floor(total_cells * target_missing_rate)
  additional_missing_needed <- target_missing_count - original_missing_count

  if (additional_missing_needed <= 0) {
    warning("The missing rate of the original dataset has reached or exceeded the target missing rate.")
    return(data)
  }

  cat(sprintf("The number of missing values that need to be added additionally: %d\n", additional_missing_needed))

  result <- data

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  missing_per_row <- rowSums(is.na(data))

  max_additional_per_row <- pmin(max_missing_per_row - missing_per_row, n_cols - missing_per_row)
  max_additional_per_row[max_additional_per_row < 0] <- 0

  max_possible_to_add <- sum(max_additional_per_row)

  if (max_possible_to_add < additional_missing_needed) {
    cat(sprintf("Warning: According to the limit of a maximum of %d missing values per line, a maximum of %d missing values can be added (Target: %d）\n",
                max_missing_per_row, max_possible_to_add, additional_missing_needed))
    additional_missing_needed <- max_possible_to_add
  }

  if (sum(max_additional_per_row > 0) == 0) {
    warning("No row can add more missing values")
    return(data)
  }

  actual_added_missing <- 0
  remaining_to_add <- additional_missing_needed

  while (remaining_to_add > 0 && sum(max_additional_per_row > 0) > 0) {
    eligible_rows <- which(max_additional_per_row > 0)
    row_idx <- sample(eligible_rows, 1)

    to_add_this_row <- min(remaining_to_add, max_additional_per_row[row_idx],
                           sample(1:max_additional_per_row[row_idx], 1))

    if (to_add_this_row > 0) {
      non_na_cols <- which(!is.na(result[row_idx, ]))
      cols_to_na <- sample(non_na_cols, to_add_this_row)

      result[row_idx, cols_to_na] <- NA

      actual_added_missing <- actual_added_missing + to_add_this_row
      remaining_to_add <- remaining_to_add - to_add_this_row
      max_additional_per_row[row_idx] <- max_additional_per_row[row_idx] - to_add_this_row
    }
  }

  final_missing_rate <- sum(is.na(result)) / total_cells
  cat(sprintf("Original missing rate: %.4f\n", original_missing_rate))
  cat(sprintf("Target missing rate: %.4f\n", target_missing_rate))
  cat(sprintf("Final missing rate: %.4f\n", final_missing_rate))
  cat(sprintf("The actual number of missing values added: %d\n", actual_added_missing))

  final_complete <- na.omit(result)
  cat(sprintf("The number of rows after completely deleting NA: %d (Original data: %d rows)\n",
              nrow(final_complete), nrow(data)))

  if (nrow(final_complete) > 0) {
    constant_cols_final <- names(which(sapply(final_complete, function(x) length(unique(x)) <= 1)))

    if (length(constant_cols_final) > 0) {
      warning("After deleting NA, a constant column was found: ", paste(constant_cols_final, collapse=", "))
    } else {
      cat("No constant columns were found after NA was deleted\n")
    }
  }

  return(result)
}

data <- blood_storage
n_cols <- ncol(data)
# Simulate 15% missing rate
data_with_more_missing <- simulate_missing_by_rows(data, additional_rate = 0.15, max_missing_per_row = 0.5*n_cols)
complete_data <- na.omit(data_with_more_missing)

all_na_cols_complete <- names(which(colSums(!is.na(complete_data)) == 0))
constant_cols_complete <- names(which(sapply(complete_data, function(x) length(unique(na.omit(x)))) <= 1))
problematic_cols_complete <- unique(c(all_na_cols_complete, constant_cols_complete))

print(problematic_cols_complete)

n_cols <- ncol(data)
data_with_more_missing <- simulate_missing_by_rows(data, additional_rate = 0.15, max_missing_per_row = n_cols)

# Define variable types
# Store information column (demographic/identifier variables)
info_vars <- NULL
info_data <- data_with_more_missing %>% select(all_of(info_vars))

continuous_vars <- c("Median.RBC.Age", "Age", "PVol", "PreopPSA","Units", "TimeToRecurrence")
ordinal_vars <- c("RBC.Age.Group", "TVol", "T.Stage", "bGS", "sGS")
nominal_vars <- c("AA", "FamHx", "BN+", "OrganConfined", "PreopTherapy", "AnyAdjTherapy", "AdjRadTherapy", "Recurrence", "Censor")
categorical_vars <- c(ordinal_vars, nominal_vars)

# Select relevant variables
selected_vars <- c(continuous_vars, categorical_vars)

data_selected <- data_with_more_missing %>% select(all_of(selected_vars))


# convert data types
data_selected <- data_selected %>%
  mutate(across(all_of(ordinal_vars), ~ factor(., ordered = TRUE)),
         across(all_of(nominal_vars), as.factor))

lapply(data_selected[ordinal_vars], levels)


data_selected[categorical_vars] <- lapply(data_selected[categorical_vars], as.factor)

data_selected[continuous_vars] <- lapply(data_selected[continuous_vars], as.numeric)

# Check data types
str(data_selected)
dir.create(file.path(dir_output, "imputed dataset"), recursive = TRUE, showWarnings = FALSE)

cat("\nTesting missing value analysis and correlation functions...\n")

raw_missing_rate <- check_missing_values(data_selected, dir_output)
cat("Missing value analysis completed. Overall missing rate:", raw_missing_rate, "\n")

cor_vars <- check_correlation(data_selected, continuous_vars, dir_output)
cat("Correlation analysis completed. Correlated variables:", paste(cor_vars, collapse=", "), "\n")

# Variables used to impute
user_defined_features <- c()
selected_features <- unique(c(cor_vars, user_defined_features))
print(paste("Features used to impute: ", selected_features))
# Save selected features to CSV for report
write.csv(data.frame(selected_features = selected_features),
          file = file.path(dir_output, "selected_features.csv"),
          row.names = FALSE)
complete_data <- na.omit(data_selected)

cat("\n--- Starting Imputation Analysis ---\n")
tryCatch({
  # A list of missing data proportions to simulate (e.g., 5%, 10%, ..., 50%)
  list_noNA = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)

  # Random seed to ensure reproducibility
  seed = 123

  # Number of iterations to repeat the entire simulation process
  niter = 10

  # number of trees to grow in each forest.
  ntree = 100

  # maximum number of iterations (in random forest) to be performed given the stopping criterion is not met beforehand.
  maxiter = 10

  # A vector of K values to test for KNN imputation (e.g., K = 3, 5, 7)
  k_values = c(3, 5, 7)

  # Number of multiple imputations to perform in MICE
  mice_m = 5

  # A scalar giving the number of iterations
  mice_maxit = 10

  # The number of decision trees in miceForest
  micer_num_trees = 100


  cat("\nUsing run_imputation_analysis from package\n")

  result <- run_imputation_analysis(
    data = data_with_more_missing,
    continuous_vars = continuous_vars,
    ordinal_vars = ordinal_vars,
    nominal_vars = nominal_vars,
    info_vars = NULL,
    dir_output = dir_output,
    data_file_name = "blood_storage",
    output_file_name = "blood_imputation_report.pdf",
    list_noNA = list_noNA,
    niter = niter,
    ntree = ntree,
    maxiter = maxiter,
    k_values = k_values,
    mice_m = mice_m,
    mice_maxit = mice_maxit,
    micer_num_trees = micer_num_trees,
    seed = seed
  )

  cat("run_imputation_analysis completed successfully!\n")

  cat("\n--- Test Completed Successfully! ---\n")
  cat("Best model:", result$best_model$Method, "\n")

  cat("\nFiles in output directory:\n")
  list.files(dir_output, recursive = TRUE)

  # open genterated report
  report_path <- file.path(dir_output, "blood_imputation_report.pdf")
  if (file.exists(report_path)) {
    cat("\nTest completed successfully!\n")
    cat("You can view the report at:", report_path, "\n")

    if (interactive()) {
      os <- Sys.info()["sysname"]
      if (os == "Windows") {
        shell.exec(report_path)
      } else if (os == "Darwin") {  # macOS
        system2("open", report_path)
      } else if (os == "Linux") {
        system2("xdg-open", report_path)
      }
    }
  }

}, error = function(e) {
  cat("\n--- ERROR ENCOUNTERED ---\n")
  cat("Error message:", e$message, "\n\n")
  cat("Call stack:\n")
  print(sys.calls())
  cat("\n--- Debug Information ---\n")
  cat("Working directory:", getwd(), "\n")
  cat("Output directory exists:", dir.exists(dir_output), "\n")
})
