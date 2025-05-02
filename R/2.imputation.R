#' Multiple Imputation Function
#'
#' @description This function performs multiple imputation using various methods
#' (Random Forest, KNN, MICE, and miceRanger) and compares their performance.
#'
#' @param data_selected A data frame containing the data to be imputed.
#' @param dir_output Directory where output files will be saved.
#' @param info_vars Optional data frame with identifier variables.
#' @param list_noNA A vector of missing rates to simulate.
#' @param ntree Number of trees for random forest methods.
#' @param maxiter Maximum number of iterations.
#' @param k_values A vector of K values to test for KNN imputation.
#' @param mice_m Number of multiple imputations for MICE.
#' @param mice_maxit Maximum iterations for MICE.
#' @param micer_num_trees Number of trees for miceRanger.
#' @param seed Random seed for reproducibility.
#' @param niter Number of iterations for each missing rate simulation.
#' @param selected_features Optional vector of feature names to use for imputation.
#' @param methods Methods used to simulate imputation.
#'
#' @importFrom dplyr %>% filter arrange slice group_by summarise ungroup distinct select
#' @importFrom missForest missForest prodNA
#' @importFrom DMwR2 knnImputation
#' @importFrom caret createFolds
#' @importFrom mice mice make.method complete
#' @importFrom miceRanger miceRanger completeData
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal ggsave
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#'
#' @return A list containing:
#' \itemize{
#'   \item imputed_data: The final imputed dataset.
#'   \item all_imputations: A list of all imputed datasets (if using multiple imputation).
#'   \item performance: Performance metrics for each method.
#'   \item combined_plot: Combined line plots of model performance.
#'   \item best_model: Information about the best-performing model.
#'   \item best_model_text: Name of best-performing model.
#'   \item missing_summary: Missing data counts comparison before and after imputation.
#'   \item summary_stats: Statistical comparison before and after imputation.
#'   \item final_boxplot: Box plots of continuous variables before and after imputation.
#'   \item final_density_plot: Density plots of continuous variables before and after imputation.
#'   \item final_barplot: Bar plots of categorical variables before and after imputation.
#'   \item output_file_name: The name of the output file where imputed data is saved.
#'   \item methods_used: The name of used methods.
#'
#' @examples
#' \dontrun{
#' result <- MI_func(
#'   data_selected = your_data,
#'   dir_output = "output/",
#'   list_noNA = c(0.1, 0.2, 0.3),
#'   ntree = 100,
#'   maxiter = 10,
#'   k_values = c(3, 5, 7),
#'   mice_m = 5,
#'   mice_maxit = 10,
#'   micer_num_trees = 100,
#'   seed = 123,
#'   niter = 5,
#'   selected_features = selected_features,
#'   methods = c("rf", "knn", "mice", "mice_rf")
#' )
#' }
#'
#' @export

MI_func <- function(data_selected,
                    dir_output,
                    info_vars = NULL,
                    list_noNA = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                    ntree = 100,
                    maxiter = 10,
                    k_values = c(3, 5, 7),
                    mice_m = 5,
                    mice_maxit = 10,
                    micer_num_trees = 100,
                    seed = 123,
                    niter = 10,
                    selected_features = NULL,
                    methods = c("rf", "knn", "mice", "mice_rf")) {
  library(dplyr)
  library(missForest)
  library(DMwR2)
  library(caret)
  library(mice)
  library(miceRanger)
  library(ggplot2)
  library(tidyr)
  library(patchwork)

  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
  dir_imputed <- file.path(dir_output, "imputed dataset")
  if (!dir.exists(dir_imputed)) dir.create(dir_imputed, recursive = TRUE)

  if (is.null(selected_features)) {
    selected_features <- colnames(data_selected)
  }

  data_selected <- data_selected[, selected_features]

  # Check for problematic columns in the original data
  cat("Checking for problematic columns in the original data...\n")
  all_na_cols <- names(which(colSums(!is.na(data_selected)) == 0))
  constant_cols <- names(which(sapply(data_selected, function(x) length(unique(na.omit(x)))) <= 1))

  problematic_cols <- unique(c(all_na_cols, constant_cols))

  # If problematic columns are found, print information and exit the function
  if(length(problematic_cols) > 0) {
    cat("\n========== ERROR: PROBLEMATIC COLUMNS DETECTED ==========\n")

    if(length(all_na_cols) > 0) {
      cat("Columns with all NA values:", paste(all_na_cols, collapse=", "), "\n")
    }

    if(length(constant_cols) > 0) {
      cat("Constant columns (only one unique value):", paste(constant_cols, collapse=", "), "\n")
    }

    cat("\nPlease remove these problematic columns before running imputation.\n")
    cat("You can use: data_selected <- data_selected[, !colnames(data_selected) %in% c(",
        paste(sapply(problematic_cols, function(x) paste0("'", x, "'")), collapse=", "), ")]\n")
    cat("==========================================================\n")

    # Return NULL or an error message to indicate the function ended prematurely
    return(list(
      error = TRUE,
      message = "Problematic columns detected",
      problematic_cols = problematic_cols
    ))
  }

  # After creating data_selected (this is just for diagnostics)
  cat("Original data check:\n")
  print(sapply(data_selected, function(x) c(NA_count = sum(is.na(x)),
                                            unique_count = length(unique(na.omit(x))))))

  complete_data <- na.omit(data_selected)

  # Make sure we have enough data after removing rows with NAs
  if(nrow(complete_data) < 5) {
    cat("\n========== ERROR: INSUFFICIENT COMPLETE CASES ==========\n")
    cat("After removing rows with missing values, only", nrow(complete_data), "complete cases remain.\n")
    cat("This is not enough data for reliable imputation testing.\n")
    cat("Consider removing columns with high missingness.\n")
    cat("========================================================\n")

    return(list(
      error = TRUE,
      message = "Insufficient complete cases",
      complete_cases = nrow(complete_data)
    ))
  }

  continuous_vars <- names(which(sapply(complete_data, is.numeric)))
  complete_data[continuous_vars] <- scale(complete_data[continuous_vars])

  # After creating complete_data
  cat("Complete data check:\n")
  print(sapply(complete_data, function(x) c(NA_count = sum(is.na(x)),
                                            unique_count = length(unique(na.omit(x))))))

  # Check for problematic columns in the complete data
  all_na_cols_complete <- names(which(colSums(!is.na(complete_data)) == 0))
  constant_cols_complete <- names(which(sapply(complete_data, function(x) length(unique(na.omit(x)))) <= 1))
  problematic_cols_complete <- unique(c(all_na_cols_complete, constant_cols_complete))

  if(length(problematic_cols_complete) > 0) {
    cat("\n========== ERROR: PROBLEMATIC COLUMNS IN COMPLETE DATA ==========\n")

    if(length(all_na_cols_complete) > 0) {
      cat("Columns with all NA values after na.omit():", paste(all_na_cols_complete, collapse=", "), "\n")
    }

    if(length(constant_cols_complete) > 0) {
      cat("Constant columns after na.omit():", paste(constant_cols_complete, collapse=", "), "\n")
    }

    cat("==================================================================\n")

    return(list(
      error = TRUE,
      message = "Problematic columns in complete data",
      problematic_cols_complete = problematic_cols_complete
    ))
  }

  raw_missing_rate <- sum(is.na(data_selected)) / prod(dim(data_selected))
  selected_features <- colnames(data_selected)

  # Validate the methods parameter
  valid_methods <- c("rf", "knn", "mice", "mice_rf")
  invalid_methods <- setdiff(methods, valid_methods)

  if (length(invalid_methods) > 0) {
    warning("Invalid methods specified: ", paste(invalid_methods, collapse=", "),
            ". Using only valid methods.")
    methods <- intersect(methods, valid_methods)
  }

  if (length(methods) == 0) {
    stop("No valid imputation methods specified. Please choose from: 'rf', 'knn', 'mice', 'mice_rf'")
  }

  # Map method codes to full names for results
  method_names <- c(
    "rf" = "RandomForest",
    "knn" = "KNN",
    "mice" = "MICE",
    "mice_rf" = "miceRanger"
  )

  selected_method_names <- method_names[methods]

  agg_results_all <- data.frame()

  for (missing_rate in list_noNA) {
    cat(sprintf("\nRunning experiments with missing rate: %.2f\n", missing_rate))

    set.seed(seed)

    results <- data.frame(
      Method = character(),
      Iteration = integer(),
      MSE = numeric(),
      Categorical_Accuracy = numeric(),
      K_Value = integer(),
      Missing_Rate = numeric(),
      stringsAsFactors = FALSE
    )

    for (i in 1:niter) {
      cat("\nRunning Iteration:", i, "\n")

      train_data.mis <- prodNA(complete_data, noNA = missing_rate)
      subset_train_data <- train_data.mis[, selected_features]
      subset_train_data <- as.data.frame(subset_train_data)

      num_vars <- names(which(sapply(complete_data, is.numeric)))
      cat_vars <- names(which(sapply(complete_data, is.factor)))

      ## Random Forest
      if("rf" %in% methods) {
        cat("\nRunning missForest...\n")
        imputed_data_rf <- missForest(subset_train_data, ntree = ntree, maxiter = maxiter, xtrue = complete_data, verbose = TRUE)
        imputed_rf <- imputed_data_rf$ximp

        mse_rf <- mean((as.matrix(imputed_rf[, num_vars]) - as.matrix(complete_data[, num_vars]))^2, na.rm = TRUE)
        acc_rf <- mean(sapply(cat_vars, function(var) mean(imputed_rf[[var]] == complete_data[[var]], na.rm = TRUE)))

        results <- rbind(results, data.frame(Method = "RandomForest", Iteration = i, MSE = mse_rf, Categorical_Accuracy = acc_rf, K_Value = NA, Missing_Rate = missing_rate))
      }


      ## KNN
      if("knn" %in% methods) {
        knn_results <- data.frame(K = k_values, MSE = NA, Accuracy = NA)

        for (k in k_values) {
          cat(sprintf("\nRunning KNN, K = %d...\n", k))

          if (sum(is.na(subset_train_data)) == 0) {
            imputed_knn <- subset_train_data
          } else {
            complete_cases <- subset_train_data[complete.cases(subset_train_data), ]
            if (nrow(complete_cases) < k || ncol(subset_train_data) < 2) {
              next
            } else {
              subset_train_data_temp <- as.data.frame(lapply(subset_train_data, function(x) if (is.factor(x)) as.numeric(as.character(x)) else x))
              imputed_knn <- tryCatch({
                knnImputation(subset_train_data_temp, k = k)
              }, error = function(e) subset_train_data_temp)
            }
          }

          available_num_vars <- intersect(num_vars, colnames(imputed_knn))
          if (length(available_num_vars) > 0) {
            mse_knn <- mean((as.matrix(imputed_knn[, available_num_vars]) - as.matrix(complete_data[, available_num_vars]))^2, na.rm = TRUE)
          } else {
            mse_knn <- NA
          }

          available_cat_vars <- intersect(cat_vars, colnames(imputed_knn))
          if (length(available_cat_vars) > 0) {
            acc_values <- sapply(available_cat_vars, function(var) {
              if (var %in% colnames(imputed_knn) && var %in% colnames(complete_data)) {
                mean(imputed_knn[[var]] == complete_data[[var]], na.rm = TRUE)
              } else {
                NA
              }
            })
            acc_knn <- mean(acc_values, na.rm = TRUE)
          } else {
            acc_knn <- NA
          }

          if (mse_knn == 0 || acc_knn == 1) {
            cat("WARNING: KNN has suspicious perfect results (MSE=0, Acc=1). This may indicate evaluation issues.\n")
            mse_knn <- NA
            acc_knn <- NA
          }

          knn_results[knn_results$K == k, "MSE"] <- mse_knn
          knn_results[knn_results$K == k, "Accuracy"] <- acc_knn
        }

        if (!all(is.na(knn_results$MSE))) {
          best_knn <- knn_results[which.min(knn_results$MSE), ]
          results <- rbind(results, data.frame(
            Method = "KNN",
            Iteration = i,
            MSE = best_knn$MSE,
            Categorical_Accuracy = best_knn$Accuracy,
            K_Value = best_knn$K,
            Missing_Rate = missing_rate
          ))
        } else {
          stop(paste0(
            "\n========== ERROR: KNN FAILED ==========\n",
            "All KNN imputation attempts returned NA at missing rate = ", missing_rate, ".\n",
            "This likely indicates insufficient complete cases to find ", max(k_values), " neighbors.\n",
            "Consider lowering the missing rate or removing 'knn' from methods.\n",
            "=========================================\n"
          ))
        }
      }


      ## MICE
      if("mice" %in% methods) {
        cat("\nRunning MICE...\n")
        mice_methods <- make.method(subset_train_data)
        mice_methods[sapply(subset_train_data, is.numeric)] <- "pmm"
        mice_methods[sapply(subset_train_data, function(x) is.factor(x) & nlevels(x) == 2)] <- "logreg"
        mice_methods[sapply(subset_train_data, is.ordered)] <- "polr"
        mice_methods[sapply(subset_train_data, function(x) is.factor(x) & nlevels(x) > 2)] <- "polyreg"

        mice_model <- tryCatch({
          mice(subset_train_data, method = mice_methods, m = mice_m, maxit = mice_maxit, seed = seed)
        }, error = function(e) {
          cat("MICE error:", e$message, "\n")
          return(NULL)
        })

        if (!is.null(mice_model)) {
          imputed_mice <- tryCatch({
            complete(mice_model, 1)
          }, error = function(e) {
            cat("Error completing MICE data:", e$message, "\n")
            return(NULL)
          })

          if (!is.null(imputed_mice)) {
            available_num_vars <- intersect(num_vars, colnames(imputed_mice))
            available_cat_vars <- intersect(cat_vars, colnames(imputed_mice))

            mse_mice <- NA
            if (length(available_num_vars) > 0) {
              mse_mice <- mean((as.matrix(imputed_mice[, available_num_vars]) - as.matrix(complete_data[, available_num_vars]))^2, na.rm = TRUE)
            }

            acc_mice <- NA
            if (length(available_cat_vars) > 0) {
              acc_values <- sapply(available_cat_vars, function(var) {
                if (var %in% colnames(imputed_mice) && var %in% colnames(complete_data)) {
                  mean(imputed_mice[[var]] == complete_data[[var]], na.rm = TRUE)
                } else {
                  NA
                }
              })
              acc_mice <- mean(acc_values, na.rm = TRUE)
            }

            results <- rbind(results, data.frame(Method = "MICE", Iteration = i, MSE = mse_mice, Categorical_Accuracy = acc_mice, K_Value = NA, Missing_Rate = missing_rate))
          }
        }
      }

      ## miceRanger
      if("mice_rf" %in% methods) {
        cat("\nRunning miceRanger...\n")
        micerf_success <- FALSE

        tryCatch({
          if (sum(!is.na(subset_train_data)) < nrow(subset_train_data) * 0.5 || ncol(subset_train_data) < 2) {
            cat("Insufficient data for miceRanger imputation\n")
          } else {
            data_for_imputation <- subset_train_data

            cat("Analyzing variable types for miceRanger...\n")

            var_types <- sapply(data_for_imputation, class)
            cat_vars_in_data <- names(which(sapply(data_for_imputation, is.factor)))
            ordered_cat_vars <- names(which(sapply(data_for_imputation, is.ordered)))
            unordered_cat_vars <- setdiff(cat_vars_in_data, ordered_cat_vars)
            numeric_vars <- names(which(sapply(data_for_imputation, is.numeric)))

            cat("Found", length(cat_vars_in_data), "categorical variables:",
                length(ordered_cat_vars), "ordered and",
                length(unordered_cat_vars), "unordered\n")
            cat("Found", length(numeric_vars), "numeric variables\n")

            data_for_imputation_prep <- data_for_imputation

            orig_data_info <- list(
              ordered_vars = ordered_cat_vars,
              unordered_vars = unordered_cat_vars,
              numeric_vars = numeric_vars
            )

            orig_levels <- list()
            for (var in cat_vars_in_data) {
              orig_levels[[var]] <- levels(data_for_imputation[[var]])
            }
            orig_data_info$levels <- orig_levels

            # Handle ordered variables - convert to numeric values to maintain order
            if (length(ordered_cat_vars) > 0) {
              for (var in ordered_cat_vars) {
                data_for_imputation_prep[[var]] <- as.numeric(data_for_imputation[[var]])
              }
              cat("Converted", length(ordered_cat_vars), "ordered categorical variables to numeric\n")
            }

            # Handling unordered variables - Try to create dummy variables
            if (length(unordered_cat_vars) > 0) {
              tryCatch({
                cat("Attempting to create dummy variables for unordered categories...\n")

                for (var in unordered_cat_vars) {
                  levels_var <- levels(data_for_imputation[[var]])

                  data_for_imputation_prep[[var]] <- NULL

                  # Create dummy variables for each level
                  for (level in levels_var) {
                    dummy_name <- paste0(var, "_", make.names(level))
                    data_for_imputation_prep[[dummy_name]] <- as.numeric(data_for_imputation[[var]] == level)
                  }

                  if (!exists("dummy_var_mapping", where = orig_data_info)) {
                    orig_data_info$dummy_var_mapping <- list()
                  }

                  dummy_vars <- c()
                  for (level in levels_var) {
                    dummy_vars <- c(dummy_vars, paste0(var, "_", make.names(level)))
                  }
                  orig_data_info$dummy_var_mapping[[var]] <- dummy_vars
                }

                cat("Successfully created dummy variables for", length(unordered_cat_vars),
                    "unordered categorical variables\n")

              }, error = function(e) {
                cat("Error creating dummy variables:", e$message, "\n")
                cat("Falling back to direct numeric conversion for unordered variables\n")

                for (var in unordered_cat_vars) {
                  data_for_imputation_prep[[var]] <- as.numeric(data_for_imputation[[var]])
                }

                orig_data_info$used_fallback <- TRUE
              })
            }

            cat("Original data dimensions:", dim(data_for_imputation), "\n")
            cat("Preprocessed data dimensions:", dim(data_for_imputation_prep), "\n")

            cat("Running miceRanger with preprocessed data...\n")
            micer_model <- tryCatch(
              miceRanger(
                data = data_for_imputation_prep,
                m = mice_m,
                maxiter = mice_maxit,
                num.trees = micer_num_trees,
                verbose = TRUE,
                returnModels = TRUE,
                seed = seed + i
              ),
              error = function(e) {
                cat("miceRanger execution error:", e$message, "\n")
                return(NULL)
              }
            )

            if (!is.null(micer_model)) {
              imputed_i_num <- tryCatch(completeData(micer_model)[[1]], error = function(e) {
                cat("completeData error:", e$message, "\n")
                return(NULL)
              })

              if (!is.null(imputed_i_num)) {
                imputed_i <- data.frame(matrix(ncol = 0, nrow = nrow(imputed_i_num)))

                for (var in numeric_vars) {
                  if (var %in% colnames(imputed_i_num)) {
                    imputed_i[[var]] <- imputed_i_num[[var]]
                  }
                }

                # Rebuild ordered categorical variables
                for (var in ordered_cat_vars) {
                  if (var %in% colnames(imputed_i_num)) {
                    orig_levels_var <- orig_data_info$levels[[var]]
                    max_level <- length(orig_levels_var)

                    imputed_values <- imputed_i_num[[var]]

                    # Round and limit the range
                    imputed_values <- round(imputed_values)
                    imputed_values[imputed_values < 1] <- 1
                    imputed_values[imputed_values > max_level] <- max_level

                    # convert to ordered factor
                    imputed_i[[var]] <- factor(
                      orig_levels_var[imputed_values],
                      levels = orig_levels_var,
                      ordered = TRUE
                    )
                  }
                }

                # Rebuild unordered categorical variables
                if (length(unordered_cat_vars) > 0) {
                  if (exists("used_fallback", where = orig_data_info) && orig_data_info$used_fallback) {
                    for (var in unordered_cat_vars) {
                      if (var %in% colnames(imputed_i_num)) {
                        orig_levels_var <- orig_data_info$levels[[var]]
                        max_level <- length(orig_levels_var)
                        imputed_values <- imputed_i_num[[var]]

                        # Round and limit the range
                        imputed_values <- round(imputed_values)
                        imputed_values[imputed_values < 1] <- 1
                        imputed_values[imputed_values > max_level] <- max_level

                        # convert to factor
                        imputed_i[[var]] <- factor(
                          orig_levels_var[imputed_values],
                          levels = orig_levels_var
                        )
                      }
                    }
                  } else if (exists("dummy_var_mapping", where = orig_data_info)) {
                    for (var in unordered_cat_vars) {
                      var_dummy_cols <- orig_data_info$dummy_var_mapping[[var]]

                      if (all(var_dummy_cols %in% colnames(imputed_i_num))) {
                        dummy_matrix <- matrix(nrow = nrow(imputed_i_num), ncol = length(var_dummy_cols))

                        for (j in 1:length(var_dummy_cols)) {
                          dummy_matrix[, j] <- imputed_i_num[[var_dummy_cols[j]]]
                        }

                        max_index <- apply(dummy_matrix, 1, which.max)

                        orig_levels_var <- orig_data_info$levels[[var]]

                        imputed_i[[var]] <- factor(
                          orig_levels_var[max_index],
                          levels = orig_levels_var
                        )
                      } else {
                        cat("Warning: Not all dummy variables found for", var, "\n")
                        imputed_i[[var]] <- data_for_imputation[[var]]
                      }
                    }
                  }
                }

                missing_cols <- setdiff(colnames(data_for_imputation), colnames(imputed_i))
                if (length(missing_cols) > 0) {
                  for (col in missing_cols) {
                    imputed_i[[col]] <- data_for_imputation[[col]]
                  }
                }

                available_num_vars <- intersect(num_vars, colnames(imputed_i))
                available_cat_vars <- intersect(cat_vars, colnames(imputed_i))

                mse_micer <- NA
                if (length(available_num_vars) > 0) {
                  mse_values <- c()
                  for (var in available_num_vars) {
                    var_mse <- tryCatch({
                      mean((imputed_i[[var]] - complete_data[[var]])^2, na.rm = TRUE)
                    }, error = function(e) NA)
                    mse_values <- c(mse_values, var_mse)
                  }
                  mse_micer <- mean(mse_values, na.rm = TRUE)
                }

                acc_micer <- NA
                if (length(available_cat_vars) > 0) {
                  acc_values <- c()
                  for (var in available_cat_vars) {
                    var_acc <- tryCatch({
                      mean(imputed_i[[var]] == complete_data[[var]], na.rm = TRUE)
                    }, error = function(e) NA)
                    acc_values <- c(acc_values, var_acc)
                  }
                  acc_micer <- mean(acc_values, na.rm = TRUE)
                }

                cat("Adding miceRanger results: MSE =", mse_micer, ", Accuracy =", acc_micer, "\n")
                results <- rbind(results, data.frame(
                  Method = "miceRanger",
                  Iteration = i,
                  MSE = mse_micer,
                  Categorical_Accuracy = acc_micer,
                  K_Value = NA,
                  Missing_Rate = missing_rate
                ))

                micerf_success <- TRUE
              } else {
                cat("Failed to get completed data from miceRanger\n")
              }
            } else {
              cat("Failed to create miceRanger model\n")
            }
          }
        }, error = function(e) {
          cat("General error in miceRanger section:", e$message, "\n")
        })

        if (!micerf_success && "mice_rf" %in% methods) {
          cat("miceRanger failed completely, adding NA result for tracking\n")
          results <- rbind(results, data.frame(
            Method = "miceRanger",
            Iteration = i,
            MSE = NA,
            Categorical_Accuracy = NA,
            K_Value = NA,
            Missing_Rate = missing_rate
          ))
        }
      }
    }

    agg_results <- results %>%
      group_by(Method, Missing_Rate) %>%
      summarise(
        Average_MSE = mean(MSE, na.rm = TRUE),
        Average_Accuracy = mean(Categorical_Accuracy, na.rm = TRUE),
        Best_K_Value = ifelse(Method == "KNN",
                              as.numeric(names(sort(table(K_Value), decreasing = TRUE)[1])),
                              NA),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      distinct()

    agg_results_all <- rbind(agg_results_all, agg_results)
  }

  write.csv(agg_results_all, file.path(dir_output, "agg_results_all.csv"), row.names = FALSE)

  if (nrow(agg_results_all) == 0) {
    stop("No model results found — check if any method successfully ran.")
  }

  # MSE & Accuracy Plot across Missing Rates
  p_mse <- ggplot(agg_results_all, aes(x = Missing_Rate, y = Average_MSE, color = Method)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "MSE across Missing Rates", x = "Missing Rate", y = "Average MSE") +
    theme_minimal()

  p_acc <- ggplot(agg_results_all, aes(x = Missing_Rate, y = Average_Accuracy, color = Method)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "Accuracy across Missing Rates", x = "Missing Rate", y = "Average Accuracy") +
    theme_minimal()

  combined_lineplot <- p_mse + p_acc + plot_layout(ncol = 2) +
    plot_annotation(title = "Model Performance across Missing Rates")

  ggsave(file.path(dir_output, "lineplot_MSE_Accuracy_combined.png"),
         plot = combined_lineplot, width = 12, height = 5, dpi = 300)

  # ============ Start Imputation ============
  selected_missing_rate <- list_noNA[which.min(abs(list_noNA - raw_missing_rate))]


  best_model <- agg_results_all %>%
    filter(Missing_Rate == selected_missing_rate) %>%
    arrange(Average_MSE, desc(Average_Accuracy)) %>%
    slice(1)

  # If the model is not selected at all, directly suspend the process
  if (nrow(best_model) == 0 || is.null(best_model$Method) || is.na(best_model$Method)) {
    stop(paste0(
      "\n========== ERROR: BEST MODEL NOT FOUND ==========\n",
      "No model could be selected for missing rate = ", selected_missing_rate, ".\n",
      "This often happens when all models failed (e.g., KNN failure under high missing rate).\n",
      "Please check `agg_results_all.csv` to see which methods succeeded.\n",
      "==================================================\n"
    ))
  }

  # If the best model isn't one of the selected methods, choose the best among selected methods
  if (!(best_model$Method %in% selected_method_names)) {
    best_model <- agg_results_all %>%
      filter(Missing_Rate == selected_missing_rate, Method %in% selected_method_names) %>%
      arrange(Average_MSE, desc(Average_Accuracy)) %>%
      slice(1)

    if (nrow(best_model) == 0) {
      stop("No valid models found in the selected methods. Please check your method selection.")
    }

    cat("\nNote: Best overall model not in selected methods. Using best model from selected methods instead.\n")
  }

  cat(sprintf("\nBest model selected: %s", best_model$Method))
  if (best_model$Method == "KNN" && !is.na(best_model$Best_K_Value)) {
    cat(sprintf(" (K = %d)", best_model$Best_K_Value))
  }
  cat("\n")

  if (is.null(best_model) || is.null(best_model$Method)) {
    stop("Best model could not be determined. Please check simulation results.")
  }

  data_selected_subset <- data_selected[, selected_features]
  data_selected_subset <- as.data.frame(data_selected_subset)

  if (best_model$Method == "RandomForest") {
    final_imputed_data <- missForest(data_selected_subset, ntree = ntree, maxiter = maxiter)$ximp
  } else if (best_model$Method == "KNN") {
    best_k <- best_model$Best_K_Value
    if (is.na(best_k) || best_k <= 0) best_k <- 3
    final_imputed_data <- knnImputation(data_selected_subset, k = best_k)
  } else if (best_model$Method == "MICE") {
    mice_methods <- make.method(data_selected_subset)
    mice_methods[sapply(data_selected_subset, is.numeric)] <- "pmm"
    mice_methods[sapply(data_selected_subset, function(x) is.factor(x) & nlevels(x) == 2)] <- "logreg"
    mice_methods[sapply(data_selected_subset, is.ordered)] <- "polr"
    mice_methods[sapply(data_selected_subset, function(x) is.factor(x) & nlevels(x) > 2)] <- "polyreg"
    mice_model_full <- mice(data_selected_subset, method = mice_methods, m = mice_m, maxit = mice_maxit, seed = seed)
    final_imputed_data <- complete(mice_model_full, 1)
  } else if (best_model$Method == "miceRanger") {
    cat("\nUsing miceRanger for final imputation...\n")

    data_for_imputation <- data_selected_subset

    cat("Analyzing variable types for miceRanger...\n")

    var_types <- sapply(data_for_imputation, class)
    cat_vars_in_data <- names(which(sapply(data_for_imputation, is.factor)))
    ordered_cat_vars <- names(which(sapply(data_for_imputation, is.ordered)))
    unordered_cat_vars <- setdiff(cat_vars_in_data, ordered_cat_vars)
    numeric_vars <- names(which(sapply(data_for_imputation, is.numeric)))

    cat("Found", length(cat_vars_in_data), "categorical variables:",
        length(ordered_cat_vars), "ordered and",
        length(unordered_cat_vars), "unordered\n")
    cat("Found", length(numeric_vars), "numeric variables\n")

    data_for_imputation_prep <- data_for_imputation

    orig_data_info <- list(
      ordered_vars = ordered_cat_vars,
      unordered_vars = unordered_cat_vars,
      numeric_vars = numeric_vars
    )

    orig_levels <- list()
    for (var in cat_vars_in_data) {
      orig_levels[[var]] <- levels(data_for_imputation[[var]])
    }
    orig_data_info$levels <- orig_levels

    # Handle ordered variables - convert to numeric values to maintain order
    if (length(ordered_cat_vars) > 0) {
      for (var in ordered_cat_vars) {
        data_for_imputation_prep[[var]] <- as.numeric(data_for_imputation[[var]])
      }
      cat("Converted", length(ordered_cat_vars), "ordered categorical variables to numeric\n")
    }

    # Handling unordered variables - Try to create dummy variables
    if (length(unordered_cat_vars) > 0) {
      tryCatch({
        cat("Attempting to create dummy variables for unordered categories...\n")

        for (var in unordered_cat_vars) {
          levels_var <- levels(data_for_imputation[[var]])

          data_for_imputation_prep[[var]] <- NULL

          # Create dummy variables for each level
          for (level in levels_var) {
            dummy_name <- paste0(var, "_", make.names(level))
            data_for_imputation_prep[[dummy_name]] <- as.numeric(data_for_imputation[[var]] == level)
          }

          if (!exists("dummy_var_mapping", where = orig_data_info)) {
            orig_data_info$dummy_var_mapping <- list()
          }

          dummy_vars <- c()
          for (level in levels_var) {
            dummy_vars <- c(dummy_vars, paste0(var, "_", make.names(level)))
          }
          orig_data_info$dummy_var_mapping[[var]] <- dummy_vars
        }

        cat("Successfully created dummy variables for", length(unordered_cat_vars),
            "unordered categorical variables\n")

      }, error = function(e) {
        cat("Error creating dummy variables:", e$message, "\n")
        cat("Falling back to direct numeric conversion for unordered variables\n")

        for (var in unordered_cat_vars) {
          data_for_imputation_prep[[var]] <- as.numeric(data_for_imputation[[var]])
        }

        orig_data_info$used_fallback <- TRUE
      })
    }

    cat("Original data dimensions:", dim(data_for_imputation), "\n")
    cat("Preprocessed data dimensions:", dim(data_for_imputation_prep), "\n")

    micer_model_full <- tryCatch({
      miceRanger(
        data = data_for_imputation_prep,
        m = mice_m,
        maxiter = mice_maxit,
        num.trees = micer_num_trees,
        verbose = TRUE,
        returnModels = TRUE,
        seed = seed
      )
    }, error = function(e) {
      cat("miceRanger error:", e$message, "\n")
      cat("Falling back to missForest...\n")
      return(NULL)
    })

    if (is.null(micer_model_full)) {
      # If miceRanger fails, use missForest as an alternative
      final_imputed_data <- missForest(data_for_imputation)$ximp
      all_imputations <- list(final_imputed_data)
      names(all_imputations) <- "final_imputed_data_missforest"
    } else {
      all_imputations <- list()

      ordered_cat_vars <- orig_data_info$ordered_vars
      unordered_cat_vars <- orig_data_info$unordered_vars

      for (i in 1:mice_m) {
        tryCatch({
          imputed_i_num <- completeData(micer_model_full)[[i]]

          imputed_i <- data.frame(matrix(ncol = 0, nrow = nrow(imputed_i_num)))

          for (var in numeric_vars) {
            if (var %in% colnames(imputed_i_num)) {
              imputed_i[[var]] <- imputed_i_num[[var]]
            }
          }

          # Rebuild ordered categorical variables
          for (var in ordered_cat_vars) {
            if (var %in% colnames(imputed_i_num)) {
              orig_levels_var <- orig_data_info$levels[[var]]
              max_level <- length(orig_levels_var)

              imputed_values <- imputed_i_num[[var]]

              # Round and limit the range
              imputed_values <- round(imputed_values)
              imputed_values[imputed_values < 1] <- 1
              imputed_values[imputed_values > max_level] <- max_level

              # convert to ordered factor
              imputed_i[[var]] <- factor(
                orig_levels_var[imputed_values],
                levels = orig_levels_var,
                ordered = TRUE
              )
            }
          }

          # Rebuild unordered categorical variables
          if (length(unordered_cat_vars) > 0) {
            if (exists("used_fallback", where = orig_data_info) && orig_data_info$used_fallback) {
              for (var in unordered_cat_vars) {
                if (var %in% colnames(imputed_i_num)) {
                  orig_levels_var <- orig_data_info$levels[[var]]
                  max_level <- length(orig_levels_var)
                  imputed_values <- imputed_i_num[[var]]

                  # Round and limit the range
                  imputed_values <- round(imputed_values)
                  imputed_values[imputed_values < 1] <- 1
                  imputed_values[imputed_values > max_level] <- max_level

                  # convert to factor
                  imputed_i[[var]] <- factor(
                    orig_levels_var[imputed_values],
                    levels = orig_levels_var
                  )
                }
              }
            } else if (exists("dummy_var_mapping", where = orig_data_info)) {
              for (var in unordered_cat_vars) {
                var_dummy_cols <- orig_data_info$dummy_var_mapping[[var]]

                if (all(var_dummy_cols %in% colnames(imputed_i_num))) {
                  dummy_matrix <- matrix(nrow = nrow(imputed_i_num), ncol = length(var_dummy_cols))

                  for (j in 1:length(var_dummy_cols)) {
                    dummy_matrix[, j] <- imputed_i_num[[var_dummy_cols[j]]]
                  }

                  # Find the index of the maximum value of each row
                  max_index <- apply(dummy_matrix, 1, which.max)

                  # Get the original level
                  orig_levels_var <- orig_data_info$levels[[var]]

                  imputed_i[[var]] <- factor(
                    orig_levels_var[max_index],
                    levels = orig_levels_var
                  )
                } else {
                  cat("Warning: Not all dummy variables found for", var, "\n")
                  # If cannot find the dummy variables, try to retain the original values
                  imputed_i[[var]] <- data_for_imputation[[var]]
                }
              }
            }
          }

          missing_cols <- setdiff(colnames(data_for_imputation), colnames(imputed_i))
          if (length(missing_cols) > 0) {
            cat("Note: Adding", length(missing_cols), "missing columns from original data\n")
            for (col in missing_cols) {
              imputed_i[[col]] <- data_for_imputation[[col]]
            }
          }

          all_imputations[[i]] <- imputed_i

          if (i == 1) {
            final_imputed_data <- imputed_i
          }
        }, error = function(e) {
          cat("Error processing imputation", i, ":", e$message, "\n")
        })
      }
    }
  }

  # save final imputed data
  if (!is.null(final_imputed_data)) {
    if (!is.null(info_data)) {
      final_output <- cbind(info_data, final_imputed_data)
    } else {
      final_output <- final_imputed_data
    }

    if (best_model$Method == "RandomForest") {
      file_name <- "final_imputed_data_rf.csv"
      write.csv(final_output, file.path(dir_imputed, file_name), row.names = FALSE)
    } else if (best_model$Method == "KNN") {
      file_name <- sprintf("final_imputed_data_knn_n=%d.csv", best_k)
      write.csv(final_output, file.path(dir_imputed, file_name), row.names = FALSE)
    } else if (best_model$Method == "MICE") {
      for (i in 1:mice_m) {
        file_name <- sprintf("final_imputed_data_mice_%d.csv", i)
        imputed_i <- complete(mice_model_full, i)
        if (!is.null(info_data)) {
          imputed_i <- cbind(info_data, imputed_i)
        }
        write.csv(imputed_i, file.path(dir_imputed, file_name), row.names = FALSE)
      }
    } else if (best_model$Method == "miceRanger") {
      if (exists("micer_model_full") && !is.null(micer_model_full)) {
        for (i in 1:mice_m) {
          if (i <= length(all_imputations) && !is.null(all_imputations[[i]])) {
            file_name <- sprintf("final_imputed_data_micerf_%d.csv", i)
            if (!is.null(info_vars)) {
              imputed_i_with_info <- cbind(info_vars, all_imputations[[i]])
              write.csv(imputed_i_with_info, file.path(dir_imputed, file_name), row.names = FALSE)
            } else {
              write.csv(all_imputations[[i]], file.path(dir_imputed, file_name), row.names = FALSE)
            }
          }
        }
        file_name <- sprintf("final_imputed_data_micerf_%d.csv", 1)
      } else {
        # if miceRanger failed
        file_name <- "final_imputed_data_micerf_fallback.csv"
        write.csv(final_output, file.path(dir_imputed, file_name), row.names = FALSE)
      }
    }
  }
  # ============ Before and After Imputation Comparison ============

  after_imputed_data <- final_imputed_data

  missing_summary <- data.frame(
    Variable = names(data_selected),
    Missing_Before = sapply(data_selected, function(x) sum(is.na(x))),
    Missing_After = sapply(after_imputed_data, function(x) sum(is.na(x)))
  )
  write.csv(missing_summary, file.path(dir_output, "missing_summary.csv"), row.names = FALSE)

  # Statistic Data Summary
  num_vars <- names(which(sapply(data_selected, is.numeric)))

  summary_before <- data_selected %>%
    select(num_vars) %>%
    summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

  summary_after <- after_imputed_data %>%
    select(num_vars) %>%
    summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

  summary_before <- t(summary_before)
  summary_after <- t(summary_after)
  colnames(summary_before) <- "Before Imputation"
  colnames(summary_after) <- "After Imputation"

  summary_stats <- data.frame(Variable_Statistic = rownames(summary_before),
                              Before = summary_before[,1],
                              After = summary_after[,1],
                              row.names = NULL,
                              stringsAsFactors = FALSE)

  write.csv(summary_stats, file.path(dir_output, "summary_stats.csv"), row.names = FALSE)


  boxplots <- list()
  for (var in num_vars) {
    p <- ggplot() +
      geom_boxplot(data = data_selected, aes(y = .data[[var]], color = "Before"), alpha = 0.5) +
      geom_boxplot(data = after_imputed_data, aes(y = .data[[var]], color = "After"), alpha = 0.5) +
      scale_color_manual(values = c("Before" = "indianred", "After" = "steelblue")) +
      labs(x = var, y = "Value") +
      theme_minimal()
    boxplots[[var]] <- p
  }
  final_boxplot <- wrap_plots(boxplots) + plot_annotation(title = "Boxplots: Before (Red) vs. After (Blue)")
  ggsave(file.path(dir_output, "boxplots_before_after.png"), final_boxplot, width = 10, height = 8, dpi = 300)

  density_plots <- list()
  for (var in num_vars) {
    p <- ggplot() +
      geom_density(data = data_selected, aes(x = .data[[var]], color = "Before"), alpha = 0.5) +
      geom_density(data = after_imputed_data, aes(x = .data[[var]], color = "After"), alpha = 0.5) +
      scale_color_manual(values = c("Before" = "indianred", "After" = "steelblue")) +
      labs(x = var, y = "Density") +
      theme_minimal()
    density_plots[[var]] <- p
  }
  final_density_plot <- wrap_plots(density_plots) + plot_annotation(title = "Density Plots: Before (Red) vs. After (Blue)")
  ggsave(file.path(dir_output, "density_plots_before_after.png"), final_density_plot, width = 10, height = 8, dpi = 300)

  # Categorical Variables Visualization (Before vs. After)
  cat_vars <- names(which(sapply(data_selected, is.factor)))

  barplots <- list()

  for (var in cat_vars) {
    df_before <- data.frame(Var = data_selected[[var]], Group = "Before")
    df_after <- data.frame(Var = after_imputed_data[[var]], Group = "After")
    df_combined <- rbind(df_before, df_after)

    p <- ggplot(df_combined, aes(x = Var, fill = Group)) +
      geom_bar(position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Before" = "indianred", "After" = "steelblue")) +
      labs(x = var, y = "Count") +
      theme_minimal()

    barplots[[var]] <- p
  }

  final_barplot <- wrap_plots(barplots) + plot_annotation(title = "Categorical Variables: Before vs. After")
  ggsave(file.path(dir_output, "barplots_cat_before_after.png"), final_barplot, width = 10, height = 8, dpi = 300)

  cat(sprintf("\nBest model selected: %s", best_model$Method))
  if (best_model$Method == "KNN" && !is.na(best_model$Best_K_Value)) {
    cat(sprintf(" (K = %d)", best_model$Best_K_Value))
  }
  cat("\n")

  return(list(
    imputed_data = final_imputed_data,
    all_imputations = if(exists("all_imputations")) all_imputations else list(final_imputed_data),
    performance = agg_results_all,
    combined_plot = combined_lineplot,
    best_model = best_model,
    best_model_text = ifelse(best_model$Method == "KNN",
                             paste0(best_model$Method, " (K = ", best_model$Best_K_Value, ")"),
                             best_model$Method),
    missing_summary = missing_summary,
    summary_stats = summary_stats,
    final_boxplot = final_boxplot,
    final_density_plot = final_density_plot,
    final_barplot = final_barplot,
    output_file_name = file_name,
    methods_used = methods
  ))
}
