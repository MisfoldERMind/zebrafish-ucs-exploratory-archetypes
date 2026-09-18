# ============================================================
# 032_Export_TwoStep_SPSS_CORREGIDO.R
#
# Objective 4
# Export blind dataset for SPSS Two-Step Cluster
#
# CORRECTED ANALYTICAL FOUNDATION
# - Analytical population: definitive Stress population, n = 89.
# - Traceability: Author + ID (Traceability_ID retained).
# - The ONLY behavioural predictors exported for Two-Step are:
#
#     distance_rz
#     crossing_rz
#     top_entry_rz
#     top_time_rz
#
# - These are the definitive robust coordinates inherited from
#   Objectives 1–3:
#
#   rz = (x - median of Control within the same Author) /
#        (1.4826 * raw MAD of Control within the same Author)
#
# - No mean/SD z-scores are used.
# - No new normalisation is performed here.
# - Author, ID and Traceability_ID are identifiers only and MUST
#   NOT be entered as clustering predictors in SPSS.
# - PC1-PC4 and Behavioural_Index are retained only in the
#   complete blind dataset for downstream traceability; they MUST
#   NOT be entered as Two-Step predictors.
# ============================================================

script_version <- paste(
  "032_Export_TwoStep_SPSS_CORREGIDO",
  "| 2026-08-10",
  "| robust within-Author Control-referenced predictors"
)

cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 1. Project directory and packages
# ------------------------------------------------------------

project_root <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

if (!dir.exists(project_root)) {
  stop("Project directory not found:\n", project_root)
}

setwd(project_root)

required_packages <- c(
  "dplyr",
  "tibble",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)
})

# ------------------------------------------------------------
# 2. Fixed analytical configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_authors <- c("1", "2", "3", "4")
numeric_tolerance <- 1e-10

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

pc_variables <- paste0(
  "PC",
  1:4
)

# ------------------------------------------------------------
# 3. Define project paths
# ------------------------------------------------------------

spss_base <- file.path(
  "02_R",
  "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
  "SPSS_TwoStep"
)

input_dir <- file.path(spss_base, "Input")
output_dir <- file.path(spss_base, "Output")
figures_dir <- file.path(spss_base, "Figures")
syntax_dir <- file.path(spss_base, "Syntax")
exported_dir <- file.path(
  spss_base,
  "Exported_Classifications"
)

directories <- c(
  input_dir,
  output_dir,
  figures_dir,
  syntax_dir,
  exported_dir
)

for (directory_i in directories) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

objects_dir <- file.path(
  "03_Results",
  "Objects"
)

processed_dir <- file.path(
  "01_Data",
  "Processed"
)

objective4_file <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

stress_objective3_file <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

required_input_files <- c(
  objective4_file,
  stress_objective3_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following corrected input files were not found:\n",
    paste(missing_input_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 4. Load corrected Objective 4 and definitive Objective 3 data
# ------------------------------------------------------------

objective4_data <- readRDS(
  objective4_file
)

stress_objective3 <- readRDS(
  stress_objective3_file
)

if (!is.data.frame(objective4_data)) {
  stop(
    basename(objective4_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(stress_objective3)) {
  stop(
    basename(stress_objective3_file),
    " does not contain a data frame."
  )
}

required_objective4_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  robust_variables,
  pc_variables,
  "Behavioural_Index"
)

missing_objective4_columns <- setdiff(
  required_objective4_columns,
  names(objective4_data)
)

if (length(missing_objective4_columns) > 0L) {
  stop(
    "The corrected Objective 4 object is missing:\n",
    paste(missing_objective4_columns, collapse = ", ")
  )
}

required_objective3_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  robust_variables
)

missing_objective3_columns <- setdiff(
  required_objective3_columns,
  names(stress_objective3)
)

if (length(missing_objective3_columns) > 0L) {
  stop(
    "The definitive Objective 3 Stress object is missing:\n",
    paste(missing_objective3_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Recover the definitive Stress population
# ------------------------------------------------------------

two_step_input <- objective4_data %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Treatment,
    dplyr::all_of(robust_variables),
    dplyr::all_of(pc_variables),
    Behavioural_Index
  ) %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

stress_objective3 <- stress_objective3 %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID))
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

if (
  nrow(two_step_input) != expected_stress_n ||
  nrow(stress_objective3) != expected_stress_n
) {
  stop(
    "Two-Step export requires exactly the definitive 89 Stress fish."
  )
}

if (!all(two_step_input$Treatment == "Stress")) {
  stop(
    "Non-Stress individuals were detected in the Two-Step export population."
  )
}

if (
  anyNA(two_step_input$Author) ||
  anyNA(two_step_input$ID) ||
  anyNA(two_step_input$Traceability_ID) ||
  any(two_step_input$Author == "") ||
  any(two_step_input$ID == "") ||
  any(two_step_input$Traceability_ID == "")
) {
  stop(
    "Missing or empty Author + ID traceability values were detected."
  )
}

if (
  anyDuplicated(
    two_step_input[c("Author", "ID")]
  ) > 0L ||
  anyDuplicated(
    two_step_input$Traceability_ID
  ) > 0L
) {
  stop(
    "Duplicated Author + ID traceability keys were detected."
  )
}

if (
  !identical(
    two_step_input$Traceability_ID,
    stress_objective3$Traceability_ID
  )
) {
  stop(
    "The Two-Step export does not contain exactly the same 89 fish ",
    "as the definitive Objective 3 Stress object."
  )
}

if (
  !identical(
    sort(unique(two_step_input$Author)),
    expected_authors
  )
) {
  stop(
    "Expected Authors 1, 2, 3, and 4 in the Stress population."
  )
}

# ------------------------------------------------------------
# 6. Verify that the exported predictors are EXACTLY the
#    definitive robust within-Author Control-referenced variables
# ------------------------------------------------------------

stress_provenance_difference <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        two_step_input[[variable_i]] -
          stress_objective3[[variable_i]]
      )
    )
  },
  numeric(1)
)

if (
  any(!is.finite(stress_provenance_difference)) ||
  any(stress_provenance_difference > numeric_tolerance)
) {
  stop(
    "The Two-Step predictors do not exactly reproduce the definitive ",
    "Objective 3 robust coordinates."
  )
}

predictor_matrix <- as.matrix(
  two_step_input[
    robust_variables
  ]
)

storage.mode(predictor_matrix) <- "double"

if (any(!is.finite(predictor_matrix))) {
  stop(
    "Missing or non-finite values were detected in the four Two-Step predictors."
  )
}

# Explicitly reject the legacy mean/SD-standardised predictor names.
legacy_predictors <- c(
  "distance_author_z",
  "crossing_author_z",
  "top_entry_author_z",
  "top_time_author_z",
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z"
)

legacy_predictors_present <- intersect(
  legacy_predictors,
  names(two_step_input)
)

if (length(legacy_predictors_present) > 0L) {
  stop(
    "Legacy mean/SD-standardised predictors are present in the Two-Step export:\n",
    paste(legacy_predictors_present, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 7. Dataset to be imported into SPSS for Two-Step
# ------------------------------------------------------------

# IMPORTANT:
# In SPSS, ONLY the following four columns are continuous predictors:
#   distance_rz, crossing_rz, top_entry_rz, top_time_rz
#
# Author, ID and Traceability_ID are identifiers and MUST NOT be
# entered as clustering predictors.

two_step_predictors <- two_step_input %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    dplyr::all_of(robust_variables)
  )

if (
  !identical(
    names(two_step_predictors),
    c(
      "Author",
      "ID",
      "Traceability_ID",
      robust_variables
    )
  )
) {
  stop(
    "The SPSS predictor export contains unexpected columns."
  )
}

# ------------------------------------------------------------
# 8. Data dictionary
# ------------------------------------------------------------

data_dictionary <- data.frame(
  Variable = c(
    "Author",
    "ID",
    "Traceability_ID",
    "distance_rz",
    "crossing_rz",
    "top_entry_rz",
    "top_time_rz"
  ),

  Role = c(
    "Identifier only - DO NOT use as predictor",
    "Identifier only - DO NOT use as predictor",
    "Unique Author + ID traceability key - DO NOT use as predictor",
    "Two-Step continuous predictor",
    "Two-Step continuous predictor",
    "Two-Step continuous predictor",
    "Two-Step continuous predictor"
  ),

  Description = c(
    "Experimental cohort/Author identifier",
    "Fish identifier within the analytical dataset",
    "Unique fish key constructed from Author + ID",
    "Distance travelled robustly normalised within Author using the corresponding Control median and 1.4826 × raw MAD",
    "Line crossings robustly normalised within Author using the corresponding Control median and 1.4826 × raw MAD",
    "Upper-zone entries robustly normalised within Author using the corresponding Control median and 1.4826 × raw MAD",
    "Upper-zone time robustly normalised within Author using the corresponding Control median and 1.4826 × raw MAD"
  ),

  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 9. Export audit table
# ------------------------------------------------------------

export_audit <- tibble::tibble(
  Criterion = c(
    "Analytical population",
    "Stress n",
    "Traceability",
    "Predictor 1",
    "Predictor 2",
    "Predictor 3",
    "Predictor 4",
    "Normalisation",
    "Maximum provenance difference",
    "Legacy mean/SD predictors exported"
  ),
  Value = c(
    "Stress only",
    as.character(expected_stress_n),
    "Author + ID",
    "distance_rz",
    "crossing_rz",
    "top_entry_rz",
    "top_time_rz",
    "Within Author: (x - Control median) / (1.4826 x raw Control MAD)",
    format(
      max(stress_provenance_difference),
      scientific = TRUE
    ),
    "No"
  )
)

# ------------------------------------------------------------
# 10. Export Excel file
# ------------------------------------------------------------

excel_output <- file.path(
  input_dir,
  "Objective4_TwoStep_Input.xlsx"
)

openxlsx::write.xlsx(
  list(
    TwoStep_predictors =
      two_step_predictors,

    Complete_blind_dataset =
      two_step_input,

    Data_dictionary =
      data_dictionary,

    Export_audit =
      export_audit
  ),
  file =
    excel_output,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 11. Export CSV files
# ------------------------------------------------------------

predictor_csv_output <- file.path(
  input_dir,
  "Objective4_TwoStep_Predictors.csv"
)

complete_csv_output <- file.path(
  input_dir,
  "Objective4_TwoStep_CompleteBlindDataset.csv"
)

utils::write.csv(
  two_step_predictors,
  file =
    predictor_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  two_step_input,
  file =
    complete_csv_output,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 12. Save R object
# ------------------------------------------------------------

rds_output <- file.path(
  objects_dir,
  "objective4_twostep_input_dataset.rds"
)

saveRDS(
  two_step_input,
  rds_output
)

# ------------------------------------------------------------
# 13. Confirm exported files exist
# ------------------------------------------------------------

expected_output_files <- c(
  excel_output,
  predictor_csv_output,
  complete_csv_output,
  rds_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following Two-Step export files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 14. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - TWO-STEP SPSS EXPORT\n")
cat("============================================================\n")

cat("\nSource Objective 4 object:\n")
cat(objective4_file, "\n")

cat("\nDefinitive Objective 3 Stress source:\n")
cat(stress_objective3_file, "\n")

cat("\nStress fish exported:\n")
print(nrow(two_step_input))

cat("\nFish per Author:\n")
print(
  table(
    two_step_input$Author,
    useNA = "ifany"
  )
)

cat("\nDuplicate Author + ID combinations:\n")
print(
  sum(
    duplicated(
      two_step_input[c("Author", "ID")]
    )
  )
)

cat("\nDuplicate Traceability_ID values:\n")
print(
  sum(
    duplicated(
      two_step_input$Traceability_ID
    )
  )
)

cat(
  "\nMaximum absolute difference from definitive Objective 3 robust coordinates:\n"
)
print(
  stress_provenance_difference
)

cat("\nSPSS Two-Step continuous predictors:\n")
print(
  robust_variables
)

cat(
  "\nNormalisation of exported predictors:\n",
  "Within Author: (x - Control median) / (1.4826 x raw Control MAD)\n",
  sep = ""
)

cat("\nColumns in TwoStep_predictors:\n")
print(
  names(two_step_predictors)
)

cat("\nMissing values in TwoStep_predictors:\n")
print(
  colSums(
    is.na(two_step_predictors)
  )
)

cat(
  "\nIMPORTANT FOR SPSS:\n",
  "Use ONLY distance_rz, crossing_rz, top_entry_rz, and top_time_rz ",
  "as continuous clustering predictors.\n",
  "Author, ID, and Traceability_ID are identifiers only.\n",
  sep = ""
)

cat("\nExcel file:\n")
cat(excel_output, "\n")

cat("\nPredictor CSV file:\n")
cat(predictor_csv_output, "\n")

cat("\nTwo-Step SPSS files exported successfully.\n")
cat("\n============================================================\n")