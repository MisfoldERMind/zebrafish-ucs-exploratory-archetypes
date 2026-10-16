# ============================================================
# 030_GMM_Classification_CORREGIDO.R
#
# Objective 4
# Gaussian Mixture Model classification
# Multivariate model based on PC1-PC4
#
# Corrected analytical foundation:
# - Stress only, n = 89
# - Traceability = Author + ID
# - Same definitive Stress fish as Objective 3
# - Robust coordinates inherited unchanged
# - PC1-PC4 and Behavioural_Index inherited from the corrected
#   Swaminathan/PCA + quantiles workflow
# - No renormalisation
# - No PCA refit
# ============================================================

script_version <- paste(
  "030_GMM_Classification_CORREGIDO",
  "| 2026-08-10",
  "| corrected Objective 1-4 dependencies"
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
  "ggplot2",
  "mclust",
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
  library(ggplot2)
  library(mclust)
  library(openxlsx)
})

set.seed(1234)

# ------------------------------------------------------------
# 2. Fixed configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_authors <- c("1", "2", "3", "4")
gmm_component_range <- 1:8
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

processed_dir <- file.path(project_root, "01_Data", "Processed")
objects_dir <- file.path(project_root, "03_Results", "Objects")
tables_dir <- file.path(project_root, "03_Results", "Tables")
figures_dir <- file.path(project_root, "03_Results", "Figures")

for (directory_i in c(objects_dir, tables_dir, figures_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

pca_quantile_file <- file.path(
  objects_dir,
  "objective4_pca_quantile_classification.rds"
)

swaminathan_file <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

stress_objective3_file <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

method_size_input <- file.path(
  objects_dir,
  "objective4_preliminary_method_size_table.rds"
)

required_input_files <- c(
  pca_quantile_file,
  swaminathan_file,
  stress_objective3_file,
  method_size_input
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

gmm_model_output <- file.path(
  objects_dir,
  "objective4_gmm_model.rds"
)

gmm_classification_output <- file.path(
  objects_dir,
  "objective4_gmm_classification.rds"
)

gmm_component_summary_output <- file.path(
  objects_dir,
  "objective4_gmm_component_summary.rds"
)

gmm_class_summary_output <- file.path(
  objects_dir,
  "objective4_gmm_class_summary.rds"
)

method_size_output <- file.path(
  objects_dir,
  "objective4_method_size_table_after_gmm.rds"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective4_GMM_Classification.xlsx"
)

bic_pdf_output <- file.path(
  figures_dir,
  "Objective4_GMM_BIC.pdf"
)

bic_tiff_output <- file.path(
  figures_dir,
  "Objective4_GMM_BIC.tiff"
)

pc_pdf_output <- file.path(
  figures_dir,
  "Objective4_GMM_PC1_PC2.pdf"
)

pc_tiff_output <- file.path(
  figures_dir,
  "Objective4_GMM_PC1_PC2.tiff"
)

uncertainty_pdf_output <- file.path(
  figures_dir,
  "Objective4_GMM_Uncertainty.pdf"
)

uncertainty_tiff_output <- file.path(
  figures_dir,
  "Objective4_GMM_Uncertainty.tiff"
)

session_info_output <- file.path(
  objects_dir,
  "objective4_gmm_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Load corrected upstream objects
# ------------------------------------------------------------

stress_gmm <- readRDS(pca_quantile_file)
classification_swaminathan <- readRDS(swaminathan_file)
stress_objective3 <- readRDS(stress_objective3_file)
method_size_table <- readRDS(method_size_input)

if (!is.data.frame(stress_gmm)) {
  stop(basename(pca_quantile_file), " does not contain a data frame.")
}

if (!is.data.frame(classification_swaminathan)) {
  stop(basename(swaminathan_file), " does not contain a data frame.")
}

if (!is.data.frame(stress_objective3)) {
  stop(basename(stress_objective3_file), " does not contain a data frame.")
}

if (!is.data.frame(method_size_table)) {
  stop(basename(method_size_input), " does not contain a data frame.")
}

# ------------------------------------------------------------
# 5. Audit required columns and traceability
# ------------------------------------------------------------

required_gmm_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  robust_variables,
  pc_variables,
  "Behavioural_Index",
  "PCA_Quantile_Class"
)

missing_gmm_columns <- setdiff(
  required_gmm_columns,
  names(stress_gmm)
)

if (length(missing_gmm_columns) > 0L) {
  stop(
    "The corrected PCA + quantiles object is missing:\n",
    paste(missing_gmm_columns, collapse = ", ")
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

required_swaminathan_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  robust_variables,
  pc_variables,
  "Behavioural_Index"
)

missing_swaminathan_columns <- setdiff(
  required_swaminathan_columns,
  names(classification_swaminathan)
)

if (length(missing_swaminathan_columns) > 0L) {
  stop(
    "The corrected Swaminathan object is missing:\n",
    paste(missing_swaminathan_columns, collapse = ", ")
  )
}

stress_gmm <- stress_gmm %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
  ) %>%
  dplyr::arrange(Traceability_ID)

stress_objective3 <- stress_objective3 %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID))
  ) %>%
  dplyr::arrange(Traceability_ID)

swaminathan_stress <- classification_swaminathan %>%
  dplyr::filter(Treatment == "Stress") %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID))
  ) %>%
  dplyr::arrange(Traceability_ID)

if (
  nrow(stress_gmm) != expected_stress_n ||
  !all(stress_gmm$Treatment == "Stress")
) {
  stop("The GMM analytical population must contain exactly 89 Stress fish.")
}

if (
  nrow(stress_objective3) != expected_stress_n ||
  nrow(swaminathan_stress) != expected_stress_n
) {
  stop("Upstream corrected Stress objects do not contain exactly 89 fish.")
}

if (
  anyNA(stress_gmm$Author) ||
  anyNA(stress_gmm$ID) ||
  anyNA(stress_gmm$Traceability_ID) ||
  any(stress_gmm$Author == "") ||
  any(stress_gmm$ID == "") ||
  any(stress_gmm$Traceability_ID == "")
) {
  stop("Missing or empty Author + ID traceability values were detected.")
}

if (
  anyDuplicated(stress_gmm[c("Author", "ID")]) > 0L ||
  anyDuplicated(stress_gmm$Traceability_ID) > 0L
) {
  stop("Duplicated Author + ID traceability keys were detected.")
}

if (
  !identical(
    stress_gmm$Traceability_ID,
    stress_objective3$Traceability_ID
  ) ||
  !identical(
    stress_gmm$Traceability_ID,
    swaminathan_stress$Traceability_ID
  )
) {
  stop(
    "The corrected GMM input does not contain the same 89 Stress fish ",
    "as Objective 3 and corrected Swaminathan."
  )
}

if (!identical(sort(unique(stress_gmm$Author)), expected_authors)) {
  stop("The GMM population must contain Authors 1, 2, 3, and 4.")
}

# ------------------------------------------------------------
# 6. Verify exact continuity of robust coordinates, PCs, and index
# ------------------------------------------------------------

stress_provenance_difference <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        stress_gmm[[variable_i]] -
          stress_objective3[[variable_i]]
      )
    )
  },
  numeric(1)
)

swaminathan_pc_difference <- vapply(
  pc_variables,
  function(variable_i) {
    max(
      abs(
        stress_gmm[[variable_i]] -
          swaminathan_stress[[variable_i]]
      )
    )
  },
  numeric(1)
)

behavioural_index_difference <- max(
  abs(
    stress_gmm$Behavioural_Index -
      swaminathan_stress$Behavioural_Index
  )
)

if (
  any(!is.finite(stress_provenance_difference)) ||
  any(stress_provenance_difference > numeric_tolerance)
) {
  stop(
    "The GMM input does not reproduce the definitive Objective 3 robust coordinates."
  )
}

if (
  any(!is.finite(swaminathan_pc_difference)) ||
  any(swaminathan_pc_difference > numeric_tolerance) ||
  !is.finite(behavioural_index_difference) ||
  behavioural_index_difference > numeric_tolerance
) {
  stop(
    "PC1-PC4 or Behavioural_Index changed between corrected upstream objects."
  )
}

# ------------------------------------------------------------
# 7. Matrix used by GMM
# ------------------------------------------------------------

gmm_matrix <- as.matrix(
  stress_gmm[
    pc_variables
  ]
)

storage.mode(gmm_matrix) <- "double"

if (
  nrow(gmm_matrix) != expected_stress_n ||
  ncol(gmm_matrix) != 4L ||
  any(!is.finite(gmm_matrix))
) {
  stop("The GMM matrix must contain 89 complete rows of PC1-PC4.")
}

if (qr(gmm_matrix)$rank != 4L) {
  stop("The PC1-PC4 GMM matrix is rank deficient.")
}

# ------------------------------------------------------------
# 8. Fit Gaussian mixture models
# ------------------------------------------------------------

gmm_model <- mclust::Mclust(
  gmm_matrix,
  G = gmm_component_range,
  verbose = FALSE
)

if (is.null(gmm_model)) {
  stop("mclust did not return a valid GMM model.")
}

if (
  length(gmm_model$G) != 1L ||
  !is.finite(gmm_model$G) ||
  !gmm_model$G %in% gmm_component_range
) {
  stop("The selected number of GMM components is invalid.")
}

if (
  length(gmm_model$modelName) != 1L ||
  is.na(gmm_model$modelName) ||
  gmm_model$modelName == ""
) {
  stop("The selected mclust covariance model is invalid.")
}

# ------------------------------------------------------------
# 9. Membership and uncertainty
# ------------------------------------------------------------

stress_gmm$GMM_Component <- factor(
  gmm_model$classification
)

stress_gmm$GMM_Uncertainty <- as.numeric(
  gmm_model$uncertainty
)

stress_gmm$GMM_MaxProbability <- apply(
  gmm_model$z,
  1L,
  max
)

if (
  any(!is.finite(stress_gmm$GMM_Uncertainty)) ||
  any(!is.finite(stress_gmm$GMM_MaxProbability)) ||
  any(stress_gmm$GMM_Uncertainty < 0) ||
  any(stress_gmm$GMM_Uncertainty > 1) ||
  any(stress_gmm$GMM_MaxProbability < 0) ||
  any(stress_gmm$GMM_MaxProbability > 1)
) {
  stop("Invalid GMM uncertainty/probability values were produced.")
}

raw_component_sizes <- stress_gmm %>%
  dplyr::count(
    GMM_Component,
    name = "n",
    .drop = FALSE
  )

if (
  sum(raw_component_sizes$n) != expected_stress_n ||
  nrow(raw_component_sizes) != as.integer(gmm_model$G)
) {
  stop("Raw GMM component sizes do not match the selected model.")
}

# ------------------------------------------------------------
# 10. Characterise components
# ------------------------------------------------------------

gmm_component_summary <- stress_gmm %>%
  dplyr::group_by(GMM_Component) %>%
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

    Mean_uncertainty = mean(GMM_Uncertainty),
    Mean_max_probability = mean(GMM_MaxProbability),

    .groups = "drop"
  ) %>%
  dplyr::arrange(Mean_Index)

# ------------------------------------------------------------
# 11. Order components by Behavioural_Index
# ------------------------------------------------------------

component_order <- gmm_component_summary %>%
  dplyr::mutate(
    GMM_Order = dplyr::row_number()
  ) %>%
  dplyr::select(
    GMM_Component,
    GMM_Order,
    Mean_Index
  )

stress_gmm <- stress_gmm %>%
  dplyr::left_join(
    component_order,
    by = "GMM_Component"
  )

if (anyNA(stress_gmm$GMM_Order)) {
  stop("The ordered GMM component mapping failed.")
}

# ------------------------------------------------------------
# 12. Harmonised GMM class
# ------------------------------------------------------------

n_components <- nrow(gmm_component_summary)

if (n_components == 1L) {

  stress_gmm$GMM_Class <- factor(
    rep("Single component", expected_stress_n),
    levels = "Single component"
  )

} else if (n_components == 2L) {

  stress_gmm$GMM_Class <- dplyr::case_when(
    stress_gmm$GMM_Order == 1L ~ "Susceptible",
    stress_gmm$GMM_Order == 2L ~ "Resilient",
    TRUE ~ NA_character_
  )

  stress_gmm$GMM_Class <- factor(
    stress_gmm$GMM_Class,
    levels = c("Susceptible", "Resilient")
  )

} else {

  stress_gmm$GMM_Class <- dplyr::case_when(
    stress_gmm$GMM_Order == 1L ~ "Susceptible",
    stress_gmm$GMM_Order == n_components ~ "Resilient",
    TRUE ~ "Intermediate"
  )

  stress_gmm$GMM_Class <- factor(
    stress_gmm$GMM_Class,
    levels = c(
      "Susceptible",
      "Intermediate",
      "Resilient"
    )
  )
}

if (anyNA(stress_gmm$GMM_Class)) {
  stop("Missing harmonised GMM classifications were produced.")
}

# ------------------------------------------------------------
# 13. Class summary
# ------------------------------------------------------------

gmm_class_summary <- stress_gmm %>%
  dplyr::group_by(GMM_Class) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Percentage = 100 * n / expected_stress_n,
    Mean_Index = mean(Behavioural_Index),
    SD_Index = stats::sd(Behavioural_Index),
    Median_Index = stats::median(Behavioural_Index),
    IQR_Index = stats::IQR(Behavioural_Index),
    Mean_uncertainty = mean(GMM_Uncertainty),
    Mean_max_probability = mean(GMM_MaxProbability),
    .groups = "drop"
  )

if (sum(gmm_class_summary$n) != expected_stress_n) {
  stop("Harmonised GMM class sizes do not sum to 89.")
}

# ------------------------------------------------------------
# 14. Diagnostic figures
# ------------------------------------------------------------

grDevices::cairo_pdf(
  filename = bic_pdf_output,
  width = 8,
  height = 6
)

plot(
  gmm_model,
  what = "BIC"
)

grDevices::dev.off()

grDevices::tiff(
  filename = bic_tiff_output,
  width = 8,
  height = 6,
  units = "in",
  res = 600,
  compression = "lzw"
)

plot(
  gmm_model,
  what = "BIC"
)

grDevices::dev.off()

p_gmm <- ggplot2::ggplot(
  stress_gmm,
  ggplot2::aes(
    x = PC1,
    y = PC2,
    colour = GMM_Component
  )
) +
  ggplot2::geom_point(
    ggplot2::aes(
      alpha = GMM_MaxProbability
    ),
    size = 2.8
  ) +
  ggplot2::scale_alpha_continuous(
    range = c(0.35, 1)
  ) +
  ggplot2::labs(
    x = "PC1",
    y = "PC2",
    colour = "GMM component",
    alpha = "Maximum\nprobability"
  ) +
  ggplot2::theme_classic(
    base_size = 14,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    legend.position = "right"
  )

print(p_gmm)

ggplot2::ggsave(
  filename = pc_pdf_output,
  plot = p_gmm,
  device = grDevices::cairo_pdf,
  width = 7.5,
  height = 6,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = pc_tiff_output,
  plot = p_gmm,
  width = 7.5,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

p_uncertainty <- ggplot2::ggplot(
  stress_gmm,
  ggplot2::aes(
    x = GMM_Class,
    y = GMM_Uncertainty
  )
) +
  ggplot2::geom_boxplot(
    outlier.shape = NA
  ) +
  ggplot2::geom_jitter(
    width = 0.12,
    alpha = 0.65,
    size = 2
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Classification uncertainty"
  ) +
  ggplot2::theme_classic(
    base_size = 14,
    base_family = "Arial"
  )

print(p_uncertainty)

ggplot2::ggsave(
  filename = uncertainty_pdf_output,
  plot = p_uncertainty,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 5.5,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = uncertainty_tiff_output,
  plot = p_uncertainty,
  width = 6,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ------------------------------------------------------------
# 15. Update method-size table
# ------------------------------------------------------------

if (
  !all(
    c(
      "Method",
      "Susceptible",
      "Intermediate",
      "Resilient"
    ) %in%
      names(method_size_table)
  )
) {
  stop(
    "The preliminary method-size table does not have the expected columns."
  )
}

if (n_components == 1L) {

  gmm_size_row <- tibble::tibble(
    Method = "GMM",
    Susceptible = NA_integer_,
    Intermediate = NA_integer_,
    Resilient = NA_integer_
  )

} else {

  gmm_size_row <- tibble::tibble(
    Method = "GMM",
    Susceptible = sum(
      stress_gmm$GMM_Class == "Susceptible"
    ),
    Intermediate =
      if ("Intermediate" %in% levels(stress_gmm$GMM_Class)) {
        sum(
          stress_gmm$GMM_Class == "Intermediate"
        )
      } else {
        NA_integer_
      },
    Resilient = sum(
      stress_gmm$GMM_Class == "Resilient"
    )
  )
}

method_size_table <- dplyr::bind_rows(
  method_size_table,
  gmm_size_row
)

# ------------------------------------------------------------
# 16. Export tables
# ------------------------------------------------------------

classification_export <- stress_gmm %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Treatment,
    dplyr::all_of(robust_variables),
    dplyr::all_of(pc_variables),
    Behavioural_Index,
    GMM_Component,
    GMM_Order,
    GMM_Class,
    GMM_MaxProbability,
    GMM_Uncertainty
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

selected_model_export <- data.frame(
  Model = gmm_model$modelName,
  Components = gmm_model$G,
  BIC = gmm_model$bic,
  Log_likelihood = gmm_model$loglik,
  n = gmm_model$n,
  d = gmm_model$d
)

openxlsx::write.xlsx(
  list(
    Selected_model = selected_model_export,
    Component_summary = gmm_component_summary,
    Class_summary = gmm_class_summary,
    Classification = classification_export,
    Method_sizes = method_size_table
  ),
  file = xlsx_output,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 17. Save corrected objects
# ------------------------------------------------------------

saveRDS(
  gmm_model,
  gmm_model_output
)

saveRDS(
  stress_gmm,
  gmm_classification_output
)

saveRDS(
  gmm_component_summary,
  gmm_component_summary_output
)

saveRDS(
  gmm_class_summary,
  gmm_class_summary_output
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
# 18. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - GMM CLASSIFICATION\n")
cat("============================================================\n")

cat("\nStress fish:\n")
print(nrow(stress_gmm))

cat("\nFish per Author:\n")
print(table(stress_gmm$Author, useNA = "ifany"))

cat("\nDuplicate Author + ID combinations:\n")
print(
  sum(
    duplicated(
      stress_gmm[c("Author", "ID")]
    )
  )
)

cat(
  "\nMaximum absolute difference from definitive Objective 3 robust coordinates:\n"
)
print(stress_provenance_difference)

cat(
  "\nMaximum absolute difference from corrected Swaminathan PC1-PC4:\n"
)
print(swaminathan_pc_difference)

cat(
  "\nMaximum absolute difference from corrected Swaminathan Behavioural_Index:\n"
)
print(behavioural_index_difference)

cat("\nGMM input:\n")
cat("PC1-PC4; all four components retained; no PCA refitted.\n")

cat("\nSelected mclust covariance model:\n")
print(gmm_model$modelName)

cat("\nSelected number of components:\n")
print(gmm_model$G)

cat("\nSelected BIC:\n")
print(gmm_model$bic)

cat("\nRaw GMM component sizes:\n")
print(raw_component_sizes, n = Inf, width = Inf)

cat("\nGMM component summary ordered by Behavioural_Index:\n")
print(gmm_component_summary, n = Inf, width = Inf)

cat("\nHarmonised GMM classification:\n")
print(table(stress_gmm$GMM_Class, useNA = "ifany"))

cat("\nGMM class summary:\n")
print(gmm_class_summary, n = Inf, width = Inf)

cat("\nUpdated method-size table:\n")
print(method_size_table, n = Inf, width = Inf)

cat("\nNo behavioural variable was renormalised.\n")
cat("No PCA was refitted and no new Behavioural_Index was constructed.\n")
cat("GMM was fitted exclusively to the definitive 89 Stress fish.\n")

cat("\nOutputs saved successfully.\n")
cat("\n============================================================\n")
