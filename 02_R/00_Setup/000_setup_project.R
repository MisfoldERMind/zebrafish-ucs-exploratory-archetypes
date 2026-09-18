# ============================================================
# LAPCOM UCS Resilience
# Master setup
# ============================================================

rm(list = ls())

base_dir <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

setwd(base_dir)

cat("\nProject loaded successfully.\n")
cat("Working directory:\n")
cat(getwd(), "\n")