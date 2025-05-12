# ImputationR

## Overview

ImputationR is a comprehensive R package used to evaluate and implement various methods for imputing in missing data. This package compares four popular imputation methods (Random Forest, K-nearest neighbor, MICE, and MICE with random forest), and provides detailed visualizations and reports to help users select the most appropriate method for a specific dataset.

## Function

- **Multi-method comparison** : Compare the performance of four imputation methods under different missing rates, Random Forest is implemented using `missforest`, KNN is implemented using `DMwR2`, Multivariate Imputation by Chained Equation (MICE) is implemented using `mice`, Multiple Imputation by Chained Equations with Random Forest is implemented using `miceRanger`.
- **Automatic selection** : Automatically select the best method based on MSE and classification accuracy
- **Final imputation** : Apply the selected best model to impute missing values in the real dataset
- **Visualization** : Generate detailed visual comparisons before and after imputation
- **Report Generation** : Create a comprehensive analysis report

## Installation

You can install the development version of ImputationR like so:

``` r
# Install devtools (if not installed yet)
if (! require("devtools")) install.packages("devtools")

# Install ImputationR from GitHub
devtools::install_github("jianvhuang/missing_data_imputation", build_vignettes = TRUE)

# Install tinytex for generating PDF report
if (!requireNamespace("tinytex", quietly = TRUE)) {
  install.packages("tinytex")
  library(tinytex)
  tinytex::install_tinytex()
} else if (!tinytex::is_tinytex()) {
  # Package is installed, but TinyTeX distribution is not
  tinytex::install_tinytex()
}
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
rm(list = ls())
library(ImputationR)

# Load all dependencies with one command
ImputationR::load_imputation_dependencies()
```
This example uses the blood_storage dataset from the medicaldata package. We introduce additional missing value for demonstration purpose.
``` r
# Create output directory
temp_dir <- tempdir()
dir_output <- file.path(temp_dir, "imputation-blood")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
cat("Output will be saved to:", dir_output, "\n")

# Users can use the test CSV inside our package under inst/extdata
data_path <- system.file("extdata", "blood_storage_add_missing.csv", package = "ImputationR")
data <- read.csv(data_path, check.names = FALSE)

# Define variable types
# Store information column (demographic/identifier variables)
info_vars <- NULL
info_data <- data %>% select(all_of(info_vars))

continuous_vars <- c("Median.RBC.Age", "Age", "PVol", "PreopPSA","Units", "TimeToRecurrence")
ordinal_vars <- c("RBC.Age.Group", "TVol", "T.Stage", "bGS", "sGS")
nominal_vars <- c("AA", "FamHx", "BN+", "OrganConfined", "PreopTherapy", "AnyAdjTherapy", "AdjRadTherapy", "Recurrence", "Censor")
categorical_vars <- c(ordinal_vars, nominal_vars)

# Select relevant variables
selected_vars <- c(continuous_vars, categorical_vars)

data_selected <- data %>% select(all_of(selected_vars))


# convert data types
data_selected <- data_selected %>%
  mutate(across(all_of(ordinal_vars), ~ factor(., ordered = TRUE)), 
         across(all_of(nominal_vars), as.factor)) 

lapply(data_selected[ordinal_vars], levels)


data_selected[categorical_vars] <- lapply(data_selected[categorical_vars], as.factor)

data_selected[continuous_vars] <- lapply(data_selected[continuous_vars], as.numeric)

# Check data types
str(data_selected)
```

## Basic Example Code
### Using Individual Functions (Modular Workflow)

You can use the individual functions for specific tasks:
```r
# Step 1: Analyze missing values
missing_rate <- check_missing_values(data_selected, dir_output)
cat("Overall missing rate:", missing_rate[[1]] * 100, "%\n")

# Step 2: Check correlations among variables
cor_vars <- check_correlation(data_selected, continuous_vars, dir_output)
cat("Correlated variables:", paste(cor_vars, collapse = ", "), "\n")

# Step 3: Run the multi-model imputation simulation
mi_result <- MI_func(
  data_selected = data_selected,
  dir_output = dir_output, 
  info_vars = NULL,
  list_noNA = c(0.1, 0.2),
  seed = 123,
  niter = 2,
  ntree = 50,
  maxiter = 5,
  k_values = c(3, 5, 7),
  mice_m = 3,
  mice_maxit = 5,
  micer_num_trees = 50,
  selected_features = NULL,
  methods = c("rf", "knn", "mice", "mice_rf") # default methods
  )
```
This modular approach gives you more flexibility and control when testing or debugging individual components.

### Using run_imputation_analysis() (Integrated Workflow)
Alternatively, you can use the `run_imputation_analysis()` function to execute the entire imputation pipeline in one step. This function internally calls the above steps — including missing value analysis, correlation checking, model comparison, and final imputation report generation — with a single command.
```r                  
# Run imputation analysis
result <- run_imputation_analysis(
  data = data_selected,
  continuous_vars = continuous_vars,
  ordinal_vars = ordinal_vars,
  nominal_vars = nominal_vars,
  info_vars = NULL,
  dir_output = dir_output,
  data_file_name = "blood_storage_example",
  output_file_name = "blood_imputation_report.pdf",
  
  # Simplified for demonstration, all these paramters can be adjusted
  list_noNA = c(0.1, 0.2),
  seed = 123,
  niter = 2,
  ntree = 50,
  maxiter = 5,
  k_values = c(3, 5, 7),
  mice_m = 3,
  mice_maxit = 5,
  micer_num_trees = 50,
  user_defined_features = NULL,
  methods = c("rf", "knn", "mice", "mice_rf") # default methods
) 

list.files(dir_output)

# List the generated imputed dataset
list.files(file.path(dir_output, "imputed dataset"))
```
The final imputed datasets are saved in the `imputed dataset/` folder within the specified output directory. The number and naming of output files depend on the selected imputation method:

| Method                        | Output Files                                                                 |
|------------------------------|------------------------------------------------------------------------------|
| Random Forest (`"rf"`)       | `final_imputed_data_rf.csv`<br>→ One single imputed dataset                 |
| KNN (`"knn"`)                | `final_imputed_data_knn_n=5.csv`<br>→ One file; `n` reflects selected `K`   |
| MICE (`"mice"`)              | `final_imputed_data_mice_1.csv` to `final_imputed_data_mice_m.csv`<br>→ Multiple imputed datasets (controlled by `mice_m`) |
| miceRanger (`"mice_rf"`)     | `final_imputed_data_micerf_1.csv` to `final_imputed_data_micerf_m.csv`<br>→ Multiple imputed datasets (controlled by `mice_m`)<br>Fallback: `final_imputed_data_micerf_fallback.csv` if model fails |


## Interpreting Results

The primary output of `run_imputation_analysis()` is a list containing:

- `imputed_data`: The final imputed dataset
- `all_imputations`: List of all imputed datasets (if using multiple imputation)
- `performance`: Performance metrics for each imputation method
- `best_model`: Information about the best-performing model
- `output_file_name`: Name of the output file with imputed data

Additionally, the function generates a PDF report in the specified output directory, containing visualizations and detailed analysis of the imputation process.

**Note:** PDF report generation requires a working LaTeX installation (e.g., via the `tinytex` package, see the Installation section above).


```r
# open genterated report
report_path <- file.path(dir_output, "blood_imputation_report.pdf")
if (file.exists(report_path)) {
  cat("\nTest completed successfully.\n")
  cat("You can view the report at:", report_path, "\n")

# If success, automatically open PDF files
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

error = function(e) {
cat("\n--- ERROR ENCOUNTERED ---\n")
cat("Error message:", e$message, "\n\n")
cat("Call stack:\n")
print(sys.calls())
cat("\n--- Debug Information ---\n")
cat("Working directory:", getwd(), "\n")
cat("Output directory exists:", dir.exists(dir_output), "\n")
}
```

## Documentation

For a comprehensive guide on using ImputationR, please refer to the package vignette:

```r
vignette("imputation-guide", package = "ImputationR")
```
