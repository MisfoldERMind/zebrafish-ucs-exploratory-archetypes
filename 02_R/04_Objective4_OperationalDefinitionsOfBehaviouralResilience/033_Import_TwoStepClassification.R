# ============================================================
# 033_Import_TwoStepClassification_CORREGIDO.R
#
# Objective 4
# Import and harmonise SPSS Two-Step classification
#
# SPSS analysis already completed:
# - Stress only, n = 89
# - Predictors: distance_rz, crossing_rz, top_entry_rz, top_time_rz
# - No additional standardisation in SPSS
# - Automatic Two-Step solution: 2 clusters
# - Raw cluster sizes: 21 and 68
#
# R DOES NOT RECALCULATE THE CLUSTERING.
# R only imports SPSS membership, audits traceability, restores
# corrected behavioural information, orders the SPSS clusters by
# the inherited Behavioural_Index, and harmonises the labels.
# ============================================================

script_version <- paste(
  "033_Import_TwoStepClassification_CORREGIDO",
  "| 2026-08-10",
  "| SPSS import + corrected traceability"
)

cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 1. Project directory and packages
# ------------------------------------------------------------

project_root <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

if (!dir.exists(project_root)) {
  stop("Project directory not found:\n", project_root)
}

setwd(project_root)

required_packages <- c(
  "dplyr",
  "tibble",
  "readxl",
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
  library(readxl)
  library(openxlsx)
})

# ------------------------------------------------------------
# 2. Fixed analytical configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_number_of_clusters <- 2L
expected_raw_cluster_sizes <- c(21L, 68L)
expected_authors <- c("1", "2", "3", "4")
numeric_tolerance <- 1e-10

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

pc_variables <- paste0("PC", 1:4)

# ------------------------------------------------------------
# 3. Paths
# ------------------------------------------------------------

spss_base <- file.path(
  "02_R",
  "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
  "SPSS_TwoStep"
)

exported_dir <- file.path(
  spss_base,
  "Exported_Classifications"
)

twostep_file <- file.path(
  exported_dir,
  "Objective4_TwoStep_Classification_CORREGIDO.xlsx"
)

objects_dir <- file.path(
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  "03_Results",
  "Tables"
)

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

swaminathan_file <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

stress_objective3_file <- file.path(
  "01_Data",
  "Processed",
  "objective3_stress_robust_z_control_reference.rds"
)

# Use only the corrected method-size table already validated in
# the current workflow. Do not read any historical after-Louvain
# object from the previous, invalid analysis.
method_size_input <- file.path(
  objects_dir,
  "objective4_method_size_table_after_gmm.rds"
)

required_input_files <- c(
  twostep_file,
  swaminathan_file,
  stress_objective3_file,
  method_size_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required corrected files were not found:\n",
    paste(missing_input_files, collapse = "\n")
  )
}

classification_output <- file.path(
  objects_dir,
  "objective4_twostep_classification.rds"
)

cluster_summary_output <- file.path(
  objects_dir,
  "objective4_twostep_cluster_summary.rds"
)

class_summary_output <- file.path(
  objects_dir,
  "objective4_twostep_class_summary.rds"
)

cluster_order_output <- file.path(
  objects_dir,
  "objective4_twostep_cluster_order.rds"
)

method_size_output <- file.path(
  objects_dir,
  "objective4_method_size_table_after_twostep.rds"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective4_TwoStep_Classification.xlsx"
)

session_info_output <- file.path(
  objects_dir,
  "objective4_twostep_import_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Import classification exported directly from SPSS
# ------------------------------------------------------------

sheet_names <- readxl::excel_sheets(
  twostep_file
)

if (!"Classification" %in% sheet_names) {
  stop(
    "Expected sheet 'Classification' was not found.\n",
    "Available sheets: ",
    paste(sheet_names, collapse = ", ")
  )
}

twostep_raw <- readxl::read_excel(
  twostep_file,
  sheet = "Classification"
)

required_spss_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "TwoStep_Cluster"
)

missing_spss_columns <- setdiff(
  required_spss_columns,
  names(twostep_raw)
)

if (length(missing_spss_columns) > 0L) {
  stop(
    "The SPSS-exported classification is missing:\n",
    paste(missing_spss_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Standardise identifiers and audit SPSS membership
# ------------------------------------------------------------

twostep <- twostep_raw %>%
  dplyr::transmute(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    TwoStep_Cluster = factor(
      trimws(as.character(TwoStep_Cluster))
    )
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

if (nrow(twostep) != expected_stress_n) {
  stop(
    "Expected 89 Stress fish in the SPSS classification; found ",
    nrow(twostep),
    "."
  )
}

if (
  anyNA(twostep$Author) ||
  anyNA(twostep$ID) ||
  anyNA(twostep$Traceability_ID) ||
  anyNA(twostep$TwoStep_Cluster) ||
  any(twostep$Author == "") ||
  any(twostep$ID == "") ||
  any(twostep$Traceability_ID == "")
) {
  stop(
    "Missing or empty values detected in the imported SPSS classification."
  )
}

if (
  anyDuplicated(twostep[c("Author", "ID")]) > 0L ||
  anyDuplicated(twostep$Traceability_ID) > 0L
) {
  stop(
    "Duplicated Author + ID traceability keys detected in the SPSS file."
  )
}

if (
  !identical(
    sort(unique(twostep$Author)),
    expected_authors
  )
) {
  stop(
    "Expected Authors 1, 2, 3, and 4 in the SPSS classification."
  )
}

traceability_reconstructed <- paste(
  twostep$Author,
  twostep$ID,
  sep = " || "
)

if (
  !identical(
    twostep$Traceability_ID,
    traceability_reconstructed
  )
) {
  stop(
    "Traceability_ID exported from SPSS does not exactly match Author + ID."
  )
}

raw_cluster_sizes <- twostep %>%
  dplyr::count(
    TwoStep_Cluster,
    name = "n",
    .drop = FALSE
  )

if (
  nrow(raw_cluster_sizes) != expected_number_of_clusters ||
  !identical(
    sort(raw_cluster_sizes$n),
    expected_raw_cluster_sizes
  )
) {
  stop(
    "The imported SPSS solution does not reproduce the validated ",
    "cluster sizes of 21 and 68."
  )
}

# ------------------------------------------------------------
# 6. Load corrected behavioural information
# ------------------------------------------------------------

classification_swaminathan <- readRDS(
  swaminathan_file
)

stress_objective3 <- readRDS(
  stress_objective3_file
)

if (!is.data.frame(classification_swaminathan)) {
  stop(
    basename(swaminathan_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(stress_objective3)) {
  stop(
    basename(stress_objective3_file),
    " does not contain a data frame."
  )
}

required_index_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  robust_variables,
  pc_variables,
  "Behavioural_Index"
)

missing_index_columns <- setdiff(
  required_index_columns,
  names(classification_swaminathan)
)

if (length(missing_index_columns) > 0L) {
  stop(
    "The corrected Swaminathan object is missing:\n",
    paste(missing_index_columns, collapse = ", ")
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

index_data <- classification_swaminathan %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::transmute(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    dplyr::across(
      dplyr::all_of(robust_variables),
      as.numeric
    ),
    dplyr::across(
      dplyr::all_of(pc_variables),
      as.numeric
    ),
    Behavioural_Index = as.numeric(Behavioural_Index)
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
  nrow(index_data) != expected_stress_n ||
  nrow(stress_objective3) != expected_stress_n
) {
  stop(
    "Corrected upstream Stress objects do not contain exactly 89 fish."
  )
}

if (
  anyDuplicated(index_data[c("Author", "ID")]) > 0L ||
  anyDuplicated(index_data$Traceability_ID) > 0L
) {
  stop(
    "Duplicated Author + ID keys detected in corrected behavioural data."
  )
}

if (
  !identical(
    twostep$Traceability_ID,
    index_data$Traceability_ID
  ) ||
  !identical(
    twostep$Traceability_ID,
    stress_objective3$Traceability_ID
  )
) {
  stop(
    "The SPSS Two-Step file does not contain exactly the same 89 Stress fish ",
    "as the corrected Objective 3/4 datasets."
  )
}

stress_provenance_difference <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        index_data[[variable_i]] -
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
    "Corrected behavioural data do not reproduce the definitive ",
    "Objective 3 robust coordinates."
  )
}

# ------------------------------------------------------------
# 7. Join SPSS membership to corrected behavioural information
# ------------------------------------------------------------

twostep <- twostep %>%
  dplyr::left_join(
    index_data,
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  )

required_joined_numeric <- c(
  robust_variables,
  pc_variables,
  "Behavioural_Index"
)

if (
  any(
    !is.finite(
      as.matrix(
        twostep[
          required_joined_numeric
        ]
      )
    )
  )
) {
  stop(
    "Missing or non-finite behavioural values occurred after the join."
  )
}

# ------------------------------------------------------------
# 8. Characterise and order SPSS Two-Step clusters
# ------------------------------------------------------------

twostep_cluster_summary <- twostep %>%
  dplyr::group_by(
    TwoStep_Cluster
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Percentage = 100 * n / expected_stress_n,

    Mean_Index = mean(Behavioural_Index),
    SD_Index = stats::sd(Behavioural_Index),
    Median_Index = stats::median(Behavioural_Index),
    IQR_Index = stats::IQR(Behavioural_Index),

    Mean_PC1 = mean(PC1),
    Mean_PC2 = mean(PC2),
    Mean_PC3 = mean(PC3),
    Mean_PC4 = mean(PC4),

    Mean_distance_rz = mean(distance_rz),
    Mean_crossing_rz = mean(crossing_rz),
    Mean_top_entry_rz = mean(top_entry_rz),
    Mean_top_time_rz = mean(top_time_rz),

    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Mean_Index
  ) %>%
  dplyr::mutate(
    TwoStep_Order = dplyr::row_number()
  )

# ------------------------------------------------------------
# 9. Harmonise the two SPSS clusters
# ------------------------------------------------------------

twostep_order <- twostep_cluster_summary %>%
  dplyr::select(
    TwoStep_Cluster,
    TwoStep_Order,
    Mean_Index
  )

twostep <- twostep %>%
  dplyr::left_join(
    twostep_order,
    by = "TwoStep_Cluster"
  ) %>%
  dplyr::mutate(
    TwoStep_Class = dplyr::case_when(
      TwoStep_Order == 1L ~ "Susceptible",
      TwoStep_Order == 2L ~ "Resilient",
      TRUE ~ NA_character_
    ),
    TwoStep_Class = factor(
      TwoStep_Class,
      levels = c(
        "Susceptible",
        "Resilient"
      )
    )
  )

if (anyNA(twostep$TwoStep_Class)) {
  stop(
    "Missing harmonised Two-Step classifications were produced."
  )
}

# ------------------------------------------------------------
# 10. Harmonised-class summary
# ------------------------------------------------------------

twostep_class_summary <- twostep %>%
  dplyr::group_by(
    TwoStep_Class
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Percentage = 100 * n / expected_stress_n,

    Mean_Index = mean(Behavioural_Index),
    SD_Index = stats::sd(Behavioural_Index),
    Median_Index = stats::median(Behavioural_Index),
    IQR_Index = stats::IQR(Behavioural_Index),

    Mean_distance_rz = mean(distance_rz),
    Mean_crossing_rz = mean(crossing_rz),
    Mean_top_entry_rz = mean(top_entry_rz),
    Mean_top_time_rz = mean(top_time_rz),

    .groups = "drop"
  )

if (
  sum(twostep_class_summary$n) != expected_stress_n
) {
  stop(
    "Harmonised Two-Step class sizes do not sum to 89."
  )
}

# ------------------------------------------------------------
# 11. Individual classification lists
# ------------------------------------------------------------

susceptible_ids <- twostep %>%
  dplyr::filter(
    TwoStep_Class == "Susceptible"
  ) %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    TwoStep_Cluster,
    TwoStep_Class,
    Behavioural_Index
  )

resilient_ids <- twostep %>%
  dplyr::filter(
    TwoStep_Class == "Resilient"
  ) %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    TwoStep_Cluster,
    TwoStep_Class,
    Behavioural_Index
  )

# ------------------------------------------------------------
# 12. Update corrected method-size table
# ------------------------------------------------------------

method_size_table <- readRDS(
  method_size_input
)

if (!is.data.frame(method_size_table)) {
  stop(
    basename(method_size_input),
    " does not contain a data frame."
  )
}

twostep_size_row <- tibble::tibble(
  Method = "Two-Step",
  Susceptible = sum(
    twostep$TwoStep_Class == "Susceptible"
  ),
  Intermediate = NA_integer_,
  Resilient = sum(
    twostep$TwoStep_Class == "Resilient"
  )
)

method_size_table <- dplyr::bind_rows(
  method_size_table,
  twostep_size_row
)

# ------------------------------------------------------------
# 13. Export tables
# ------------------------------------------------------------

classification_export <- twostep %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    dplyr::all_of(robust_variables),
    dplyr::all_of(pc_variables),
    Behavioural_Index,
    TwoStep_Cluster,
    TwoStep_Order,
    TwoStep_Class
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

openxlsx::write.xlsx(
  list(
    Raw_SPSS_cluster_sizes = raw_cluster_sizes,
    Cluster_summary = twostep_cluster_summary,
    Cluster_order = twostep_order,
    Class_summary = twostep_class_summary,
    Individual_classification = classification_export,
    Susceptible_IDs = susceptible_ids,
    Resilient_IDs = resilient_ids,
    Method_sizes_after_TwoStep = method_size_table
  ),
  file = xlsx_output,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 14. Save corrected R objects
# ------------------------------------------------------------

saveRDS(
  twostep,
  classification_output
)

saveRDS(
  twostep_cluster_summary,
  cluster_summary_output
)

saveRDS(
  twostep_class_summary,
  class_summary_output
)

saveRDS(
  twostep_order,
  cluster_order_output
)

saveRDS(
  method_size_table,
  method_size_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 15. Confirm outputs
# ------------------------------------------------------------

expected_output_files <- c(
  classification_output,
  cluster_summary_output,
  class_summary_output,
  cluster_order_output,
  method_size_output,
  xlsx_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following corrected Two-Step outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 16. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - IMPORTED SPSS TWO-STEP CLASSIFICATION\n")
cat("============================================================\n")

cat("\nSPSS classification source:\n")
cat(twostep_file, "\n")

cat("\nStress fish imported:\n")
print(nrow(twostep))

cat("\nFish per Author:\n")
print(table(twostep$Author, useNA = "ifany"))

cat("\nDuplicate Author + ID combinations:\n")
print(
  sum(
    duplicated(
      twostep[c("Author", "ID")]
    )
  )
)

cat("\nDuplicate Traceability_ID values:\n")
print(
  sum(
    duplicated(
      twostep$Traceability_ID
    )
  )
)

cat(
  "\nMaximum absolute difference from definitive Objective 3 robust coordinates:\n"
)
print(stress_provenance_difference)

cat("\nRaw SPSS Two-Step cluster sizes:\n")
print(raw_cluster_sizes, n = Inf, width = Inf)

cat("\nTwo-Step clusters ordered by Behavioural_Index:\n")
print(twostep_cluster_summary, n = Inf, width = Inf)

cat("\nHarmonised Two-Step classification:\n")
print(table(twostep$TwoStep_Class, useNA = "ifany"))

cat("\nTwo-Step class summary:\n")
print(twostep_class_summary, n = Inf, width = Inf)

cat("\nCorrected method-size table after Two-Step:\n")
print(method_size_table, n = Inf, width = Inf)

cat(
  "\nNo Two-Step clustering was recalculated in R.\n",
  "R only imported SPSS membership, audited traceability, ",
  "and harmonised clusters using the inherited Behavioural_Index.\n",
  sep = ""
)

cat("\nOutputs saved successfully.\n")
cat("\n============================================================\n")
