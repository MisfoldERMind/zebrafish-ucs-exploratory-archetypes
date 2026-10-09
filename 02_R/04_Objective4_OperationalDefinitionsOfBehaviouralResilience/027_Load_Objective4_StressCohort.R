# ============================================================
# 027_Load_Objective4_StressCohort_CORREGIDO.R
#
# Objective 4
# Load the definitive Stress analytical cohort inherited from
# the corrected Objectives 1–3.
#
# IMPORTANT:
# - This script does NOT reload the raw Excel database.
# - This script does NOT renormalise any behavioural variable.
# - This script does NOT recalculate PCA, clustering, or classifications.
# - The analytical population is the same 89 UCS-exposed zebrafish
#   used in corrected Objective 3.
# - Traceability is Author + ID.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory and package
# ------------------------------------------------------------

base_dir <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Install package 'dplyr' before running this script.")
}

suppressPackageStartupMessages({
  library(dplyr)
})

# ------------------------------------------------------------
# 2. Definitive input inherited from corrected Objective 3
# ------------------------------------------------------------

stress_file <- file.path(
  "01_Data",
  "Processed",
  "objective3_stress_robust_z_control_reference.rds"
)

if (!file.exists(stress_file)) {
  stop(
    "Definitive corrected Stress dataset not found:\n",
    stress_file
  )
}

stress_objective4 <- readRDS(stress_file)

if (!is.data.frame(stress_objective4)) {
  stop(
    "The definitive Objective 3 Stress object is not a data frame:\n",
    stress_file
  )
}

# ------------------------------------------------------------
# 3. Fixed analytical requirements
# ------------------------------------------------------------

expected_n <- 89L

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

required_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  robust_variables
)

missing_columns <- setdiff(
  required_columns,
  names(stress_objective4)
)

if (length(missing_columns) > 0L) {
  stop(
    "The definitive Stress dataset is missing required columns:\n",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Integrity audit
# ------------------------------------------------------------

if (nrow(stress_objective4) != expected_n) {
  stop(
    "Objective 4 requires exactly 89 Stress fish; found ",
    nrow(stress_objective4),
    "."
  )
}

if (
  anyNA(stress_objective4$Author) ||
  anyNA(stress_objective4$ID) ||
  anyNA(stress_objective4$Traceability_ID)
) {
  stop("Missing values detected in Author, ID, or Traceability_ID.")
}

if (
  any(trimws(as.character(stress_objective4$Author)) == "") ||
  any(trimws(as.character(stress_objective4$ID)) == "") ||
  any(trimws(as.character(stress_objective4$Traceability_ID)) == "")
) {
  stop("Empty values detected in Author, ID, or Traceability_ID.")
}

if (anyDuplicated(stress_objective4$Traceability_ID) > 0L) {
  stop("Traceability_ID is not unique.")
}

author_id_audit <- stress_objective4 %>%
  dplyr::count(Author, ID, name = "n") %>%
  dplyr::filter(n != 1L)

if (nrow(author_id_audit) > 0L) {
  stop("Author + ID does not uniquely identify all Stress fish.")
}

if (
  anyNA(stress_objective4[, robust_variables]) ||
  any(!is.finite(as.matrix(stress_objective4[, robust_variables])))
) {
  stop("Missing or non-finite values detected in the four robust variables.")
}

author_levels <- sort(unique(as.character(stress_objective4$Author)))

if (!identical(author_levels, c("1", "2", "3", "4"))) {
  stop(
    "Expected Authors 1, 2, 3, and 4; found: ",
    paste(author_levels, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Console audit only
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - DEFINITIVE STRESS ANALYTICAL COHORT\n")
cat("============================================================\n")

cat("\nSource file:\n")
cat(stress_file, "\n")

cat("\nDimensions:\n")
print(dim(stress_objective4))

cat("\nStress fish:\n")
print(nrow(stress_objective4))

cat("\nFish per Author:\n")
print(table(stress_objective4$Author, useNA = "ifany"))

cat("\nDuplicate ID values if Author is ignored:\n")
print(sum(duplicated(stress_objective4$ID)))

cat("\nDuplicate Author + ID combinations:\n")
print(nrow(author_id_audit))

cat("\nDuplicate Traceability_ID values:\n")
print(sum(duplicated(stress_objective4$Traceability_ID)))

cat("\nMissing values in analytical fields:\n")
print(
  colSums(
    is.na(
      stress_objective4[, required_columns]
    )
  )
)

cat("\nFour inherited robust behavioural variables:\n")
print(robust_variables)

cat("\nDescriptive audit of inherited robust variables:\n")
print(
  stress_objective4 %>%
    dplyr::select(dplyr::all_of(robust_variables)) %>%
    summary()
)

cat("\nNo behavioural variable was recalculated or renormalised.\n")
cat("Traceability retained as Author + ID.\n")
cat("Analytical population retained as the definitive 89 Stress fish.\n")

cat("\n============================================================\n")
