# =============================================================================
# AUSFTA Gravity Model Project - Master Script
# =============================================================================

# ----------------------
# 1. Set Up Environment
# ----------------------
# Load renv-managed environment if applicable
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::restore(prompt = FALSE)

# Clean R environment
rm(list = ls())
cat("\014")  # clears console
options(stringsAsFactors = FALSE)

# Set working directory to the project root (assumes you're running from .Rproj)
library(here)
here::here()
getwd()

# ----------------------
# 2. Load Packages
# ----------------------
# (All dependencies are already handled through renv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(broom)
library(modelsummary)

# ----------------------
# 3. Run Analysis Scripts
# ----------------------
cat("Running Descriptive Analysis...\n")
source(here("01_code_for_descriptive_analysis.R"))

cat("Running Main Gravity Model Analysis...\n")
source(here("02_main_analysis_code.R"))

cat("Running Sectoral PPML Analysis...\n")
source(here("03_sectoral_analysis_code.R"))

cat("All scripts completed successfully!\n")

# ----------------------
# 4. Save Session Info
# ----------------------
writeLines(capture.output(sessionInfo()), here("output", "session_info.txt"))
