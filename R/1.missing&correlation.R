#' Check Missing Values in Data
#'
#' Analyzes missing values in a dataset and creates visualizations of missing patterns.
#'
#' @param data A data frame to check for missing values
#' @param dir_output Directory where output visualizations will be saved
#' @param filename_prefix Prefix used for output files (default: "data_selected")
#'
#' @return The overall missing rate as a proportion between 0 and 1
#'
#' @details
#' This function performs the following:
#' \itemize{
#'   \item Calculates the overall missing rate in the dataset
#'   \item Creates a missing pattern visualization using `VIM::aggr`
#'   \item Generates a bar plot of missing values by variable
#'   \item Saves all visualizations to the specified output directory
#' }
#'
#' @examples
#' \dontrun{
#' # Create an output directory
#' dir.create("output_dir")
#'
#' # Analyze missing values in your dataset
#' missing_rate <- check_missing_values(
#'   data = your_data,
#'   dir_output = "output_dir/",
#'   filename_prefix = "mydata"
#' )
#' }
#'
#' @importFrom VIM aggr
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme element_text ggsave
#' @importFrom ggplot2 scale_fill_gradient

#' @export


### Check missing values
check_missing_values <- function(data, dir_output, filename_prefix = "data_selected") {
  # Overall missing rate
  raw_missing_rate <- sum(is.na(data)) / prod(dim(data))
  cat(sprintf("\nOverall Missing Rate: %.2f%%\n", raw_missing_rate * 100))

  # miss pattern
  output_file <- file.path(dir_output,"missing_pattern_plot.png")
  png(output_file, width = 1500, height = 600, res = 150)
  aggr(data, prop = FALSE, number = TRUE, cex.axis = 0.6)
  dev.off()
  message("Missing pattern plot saved to: ", dir_output)

  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_counts_sorted <- sort(missing_counts, decreasing = TRUE)
  missing_df <- data.frame(variables = names(missing_counts_sorted), counts = missing_counts_sorted)

  missing_plot <- ggplot(missing_df, aes(x = reorder(variables, -counts), y = counts)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Missing Values of `", filename_prefix, "`"), x = "Variables", y = "Number of Missing Values") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(filename = file.path(dir_output, "missing_values_plot.png"),
         plot = missing_plot, width = 10, height = 6, dpi = 300)
  message("Missing values barplot saved to: ", dir_output)

  return(raw_missing_rate)
}


#' Check Correlation Between Variables
#'
#' Calculates and visualizes correlations between variables in the dataset,
#' using different correlation methods depending on variable types.
#'
#' @param data A data frame containing the variables to analyze
#' @param continuous_vars Vector of column names for continuous variables
#' @param dir_output Directory where correlation heatmaps will be saved
#'
#' @return A vector of variable names that have strong correlations (abs(cor) > 0.5)
#'
#' @details
#' This function performs the following:
#' \itemize{
#'   \item Calculates pairwise correlations for continuous variables
#'   \item Calculates correlations between all variables using appropriate methods:
#'     \itemize{
#'       \item Pearson for numeric-numeric pairs
#'       \item Polychoric for categorical-categorical pairs
#'       \item Polyserial for numeric-categorical pairs
#'     }
#'   \item Creates heatmaps for visualizing correlations
#'   \item Tracks sample sizes used for each correlation pair
#'   \item Identifies variables with high correlation (|r| > 0.5)
#' }
#'
#' @examples
#' \dontrun{
#' # Create an output directory
#' dir.create("output_dir")
#'
#' # Get correlated variables
#' cor_vars <- check_correlation(
#'   data = your_data,
#'   continuous_vars = c("age", "weight", "height"),
#'   dir_output = "output_dir/"
#' )
#' }
#'
#' @importFrom polycor polychor polyserial
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_text theme_minimal theme element_text ggsave
#' @export

### Correlation
### Correlation with maximized sample sizes
check_correlation <- function(data, continuous_vars, dir_output) {
  # Continuous-only correlation
  # Calculate each correlation pair separately to maximize sample size
  cor_matrix_cont <- matrix(NA, nrow = length(continuous_vars), ncol = length(continuous_vars))
  rownames(cor_matrix_cont) <- continuous_vars
  colnames(cor_matrix_cont) <- continuous_vars

  # Fill the correlation matrix using pairwise complete observations
  for(i in 1:length(continuous_vars)) {
    for(j in 1:length(continuous_vars)) {
      if(i != j) {
        var1 <- continuous_vars[i]
        var2 <- continuous_vars[j]

        # Calculate correlation using only data available for these two variables
        pair_data <- data[, c(var1, var2)]
        pair_data <- pair_data[complete.cases(pair_data), ]

        if(nrow(pair_data) > 0) {
          cor_value <- cor(pair_data[[var1]], pair_data[[var2]])
          cor_matrix_cont[i, j] <- cor_value
        }
      }
    }
  }
  diag(cor_matrix_cont) <- 1

  melted_cor <- reshape2::melt(cor_matrix_cont)
  melted_cor$value[melted_cor$Var1 == melted_cor$Var2] <- NA  # Remove diagonal for plotting

  cor_plot_cont <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") +
    geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))), color = "black", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(filename = file.path(dir_output, "correlation_heatmap_continuous.png"),
         plot = cor_plot_cont, width = 8, height = 6, dpi = 300)
  message("Correlation heatmap (continuous) saved to: ", dir_output)

  # All variable correlation with maximized sample sizes
  all_vars <- colnames(data)
  cor_matrix_all <- matrix(NA, nrow = length(all_vars), ncol = length(all_vars))
  rownames(cor_matrix_all) <- all_vars
  colnames(cor_matrix_all) <- all_vars

  # Sample size tracking for each pair
  sample_sizes <- matrix(NA, nrow = length(all_vars), ncol = length(all_vars))
  rownames(sample_sizes) <- all_vars
  colnames(sample_sizes) <- all_vars

  # Fill the correlation matrix for all variables
  for(i in 1:length(all_vars)) {
    for(j in 1:length(all_vars)) {
      if(i != j) {
        var1 <- all_vars[i]
        var2 <- all_vars[j]

        # Calculate correlation using only data available for these two variables
        pair_data <- data[, c(var1, var2)]
        pair_data <- pair_data[complete.cases(pair_data), ]
        sample_sizes[i, j] <- nrow(pair_data)

        if(nrow(pair_data) > 0) {
          # Use appropriate correlation method based on variable types
          if(is.numeric(data[[var1]]) && is.numeric(data[[var2]])) {
            # Pearson correlation for numeric-numeric
            cor_value <- cor(pair_data[[var1]], pair_data[[var2]])
          } else if(is.factor(data[[var1]]) && is.factor(data[[var2]])) {
            # Polychoric correlation for categorical-categorical
            cor_value <- tryCatch({
              polycor::polychor(pair_data[[var1]], pair_data[[var2]])
            }, error = function(e) {
              NA
            })
          } else {
            # Polyserial correlation for numeric-categorical
            numeric_var <- if(is.numeric(data[[var1]])) var1 else var2
            factor_var <- if(is.factor(data[[var1]])) var1 else var2

            cor_value <- tryCatch({
              polycor::polyserial(pair_data[[numeric_var]], pair_data[[factor_var]])
            }, error = function(e) {
              NA
            })
          }
          cor_matrix_all[i, j] <- cor_value
        }
      }
    }
  }
  diag(cor_matrix_all) <- 1

  # Create a plot showing correlation values
  melted_cor_all <- reshape2::melt(cor_matrix_all)
  melted_cor_all$value[melted_cor_all$Var1 == melted_cor_all$Var2] <- NA  # Remove diagonal

  cor_plot_all <- ggplot(melted_cor_all, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-1, 1), name = "Correlation") +
    geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))), color = "black", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(filename = file.path(dir_output, "correlation_heatmap_all.png"),
         plot = cor_plot_all, width = 8, height = 6, dpi = 300)
  message("Correlation heatmap (all) saved to: ", dir_output)

  # Create a plot showing sample sizes used for each correlation
  melted_sample_sizes <- reshape2::melt(sample_sizes)
  melted_sample_sizes$value[melted_sample_sizes$Var1 == melted_sample_sizes$Var2] <- NA  # Remove diagonal

  sample_plot <- ggplot(melted_sample_sizes, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "darkgreen", name = "Sample Size") +
    geom_text(aes(label = ifelse(is.na(value), "", value)), color = "black", size = 3) +
    labs(title = "Sample Size for Each Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(filename = file.path(dir_output, "correlation_sample_sizes.png"),
         plot = sample_plot, width = 8, height = 6, dpi = 300)
  message("Correlation sample sizes heatmap saved to: ", dir_output)

  # Get high-correlation variables
  cor_vars <- c()
  for(i in 1:nrow(cor_matrix_all)) {
    if(any(abs(cor_matrix_all[i, ]) > 0.5, na.rm = TRUE)) {
      cor_vars <- c(cor_vars, rownames(cor_matrix_all)[i])
    }
  }
  cor_vars <- unique(cor_vars)

  return(cor_vars)
}
