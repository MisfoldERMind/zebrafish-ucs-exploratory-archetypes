# ============================================================
# 031_Louvain_Resilience_Classification_CORREGIDO_v2.R
#
# Objective 4
# Louvain-based operational classification
#
# IMPORTANT CONCEPTUAL POINT
# This Objective 4 classifier uses the PURE de novo Louvain
# partition obtained in corrected Objective 3.
#
# It does NOT use the later Objective 3 refinement based on
# compatibility with the Control-derived reference profiles.
# Therefore, the 23 higher-exploration/Lowe-reference fish are
# not reclassified here as "Unresolved".
#
# Rationale:
# Objective 4 compares five classification METHODS. For Louvain
# to remain one of those methods, its operational classification
# must be derived from the Louvain partition itself.
#
# DEFINITIVE OBJECTIVE 3 FOUNDATION
# - Stress only, n = 89
# - Traceability = Author + ID / Traceability_ID
# - Robust coordinates inherited unchanged:
#     distance_rz
#     crossing_rz
#     top_entry_rz
#     top_time_rz
# - Definitive de novo Louvain solution:
#     lower-exploration behavioural cluster  = 44 fish
#     higher-exploration behavioural cluster = 45 fish
# - Selected k = 48 within the stable k = 41–56 plateau
#
# OBJECTIVE 4 HARMONISATION
# - The two Louvain clusters are ordered by the inherited
#   Behavioural_Index.
# - Lower mean Behavioural_Index  -> Susceptible
# - Higher mean Behavioural_Index -> Resilient
#
# No Louvain clustering is recalculated in this script.
# ============================================================

script_version <- paste(
  "031_Louvain_Resilience_Classification_CORREGIDO_v2",
  "| 2026-08-10",
  "| definitive Objective 3 Louvain partition"
)

cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 1. Project directory and packages
# ------------------------------------------------------------

project_root <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(project_root)) {
  stop("Project directory not found:\n", project_root)
}

setwd(project_root)

required_packages <- c(
  "dplyr",
  "tibble",
  "ggplot2",
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
  library(openxlsx)
})

# ------------------------------------------------------------
# 2. Fixed analytical configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_cluster_sizes <- c(44L, 45L)
expected_selected_k <- 48L
expected_plateau_start <- 41L
expected_plateau_end <- 56L
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
# 3. Input and output paths
# ------------------------------------------------------------

objects_dir <- file.path(
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  "03_Results",
  "Tables"
)

figures_dir <- file.path(
  "03_Results",
  "Figures"
)

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

objective3_characterisation_file <- file.path(
  objects_dir,
  "objective3_stress_community_characterisation_results.rds"
)

objective3_selected_louvain_file <- file.path(
  objects_dir,
  "objective3_selected_louvain_configuration.rds"
)

swaminathan_file <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

stress_objective3_file <- file.path(
  "01_Data",
  "Processed",
  "objective3_stress_robust_z_control_reference.rds"
)

# At the current point of the corrected workflow, the validated
# method-size table already contains Swaminathan, PCA + quantiles,
# GMM, and Two-Step. Louvain is added here as the fifth method.
method_size_input <- file.path(
  objects_dir,
  "objective4_method_size_table_after_twostep.rds"
)

required_input_files <- c(
  objective3_characterisation_file,
  objective3_selected_louvain_file,
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

classification_output <- file.path(
  objects_dir,
  "objective4_louvain_classification.rds"
)

community_summary_output <- file.path(
  objects_dir,
  "objective4_louvain_community_summary.rds"
)

class_summary_output <- file.path(
  objects_dir,
  "objective4_louvain_class_summary.rds"
)

community_order_output <- file.path(
  objects_dir,
  "objective4_louvain_community_order.rds"
)

method_size_output <- file.path(
  objects_dir,
  "objective4_five_method_size_table.rds"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective4_Louvain_Classification.xlsx"
)

pc_pdf_output <- file.path(
  figures_dir,
  "Objective4_Louvain_PC1_PC2.pdf"
)

pc_tiff_output <- file.path(
  figures_dir,
  "Objective4_Louvain_PC1_PC2.tiff"
)

index_pdf_output <- file.path(
  figures_dir,
  "Objective4_Louvain_BehaviouralIndex.pdf"
)

index_tiff_output <- file.path(
  figures_dir,
  "Objective4_Louvain_BehaviouralIndex.tiff"
)

session_info_output <- file.path(
  objects_dir,
  "objective4_louvain_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Load definitive Objective 3 Louvain objects
# ------------------------------------------------------------

objective3_results <- readRDS(
  objective3_characterisation_file
)

selected_louvain <- readRDS(
  objective3_selected_louvain_file
)

if (!is.list(objective3_results)) {
  stop(
    basename(objective3_characterisation_file),
    " does not contain a list."
  )
}

if (!is.list(selected_louvain)) {
  stop(
    basename(objective3_selected_louvain_file),
    " does not contain a list."
  )
}

required_objective3_elements <- c(
  "Stress_n",
  "Individual_classification"
)

missing_objective3_elements <- setdiff(
  required_objective3_elements,
  names(objective3_results)
)

if (length(missing_objective3_elements) > 0L) {
  stop(
    "The Objective 3 characterisation object is missing:\n",
    paste(missing_objective3_elements, collapse = ", ")
  )
}

required_louvain_elements <- c(
  "Selected_k",
  "Selected_plateau_start_k",
  "Selected_plateau_end_k",
  "Selected_number_of_communities"
)

missing_louvain_elements <- setdiff(
  required_louvain_elements,
  names(selected_louvain)
)

if (length(missing_louvain_elements) > 0L) {
  stop(
    "The selected Louvain configuration is missing:\n",
    paste(missing_louvain_elements, collapse = ", ")
  )
}

if (
  as.integer(objective3_results$Stress_n) != expected_stress_n ||
  as.integer(selected_louvain$Selected_k) != expected_selected_k ||
  as.integer(selected_louvain$Selected_plateau_start_k) != expected_plateau_start ||
  as.integer(selected_louvain$Selected_plateau_end_k) != expected_plateau_end ||
  as.integer(selected_louvain$Selected_number_of_communities) != 2L
) {
  stop(
    "The loaded Objective 3 Louvain objects do not correspond to the ",
    "definitive corrected solution."
  )
}

stress_louvain <- objective3_results$Individual_classification

if (!is.data.frame(stress_louvain)) {
  stop(
    "objective3_results$Individual_classification is not a data frame."
  )
}

required_louvain_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Stress_Cluster",
  robust_variables,
  "PC1",
  "PC2"
)

missing_louvain_columns <- setdiff(
  required_louvain_columns,
  names(stress_louvain)
)

if (length(missing_louvain_columns) > 0L) {
  stop(
    "The definitive Objective 3 individual classification is missing:\n",
    paste(missing_louvain_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Load inherited Behavioural_Index and full PC1-PC4 coordinates
# ------------------------------------------------------------

index_data <- readRDS(
  swaminathan_file
)

stress_objective3 <- readRDS(
  stress_objective3_file
)

if (!is.data.frame(index_data)) {
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
  names(index_data)
)

if (length(missing_index_columns) > 0L) {
  stop(
    "The corrected Swaminathan object is missing:\n",
    paste(missing_index_columns, collapse = ", ")
  )
}

index_data <- index_data %>%
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

# ------------------------------------------------------------
# 6. Standardise and audit Objective 3 Louvain membership
# ------------------------------------------------------------

segment_to_integer <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  x <- trimws(as.character(x))
  x <- sub(
    pattern = "^Cluster[[:space:]]+",
    replacement = "",
    x = x,
    ignore.case = TRUE
  )

  result <- suppressWarnings(
    as.integer(x)
  )

  if (
    anyNA(result) ||
    any(!result %in% c(1L, 2L))
  ) {
    stop(
      "Invalid Stress_Cluster labels in the definitive Objective 3 object."
    )
  }

  result
}

stress_louvain <- stress_louvain %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Stress_Cluster_integer = segment_to_integer(
      Stress_Cluster
    )
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

if (
  nrow(stress_louvain) != expected_stress_n ||
  nrow(index_data) != expected_stress_n ||
  nrow(stress_objective3) != expected_stress_n
) {
  stop(
    "All corrected Stress objects must contain exactly 89 fish."
  )
}

for (data_i in list(
  stress_louvain,
  index_data,
  stress_objective3
)) {
  if (
    anyDuplicated(
      data_i[c("Author", "ID")]
    ) > 0L ||
    anyDuplicated(
      data_i$Traceability_ID
    ) > 0L
  ) {
    stop(
      "Duplicated Author + ID traceability keys were detected."
    )
  }
}

if (
  !identical(
    stress_louvain$Traceability_ID,
    index_data$Traceability_ID
  ) ||
  !identical(
    stress_louvain$Traceability_ID,
    stress_objective3$Traceability_ID
  )
) {
  stop(
    "The definitive Objective 3 Louvain membership and corrected Objective 4 ",
    "data do not contain exactly the same 89 Stress fish."
  )
}

if (
  !identical(
    sort(unique(stress_louvain$Author)),
    expected_authors
  )
) {
  stop(
    "Expected Authors 1, 2, 3, and 4."
  )
}

cluster_size_audit <- stress_louvain %>%
  dplyr::count(
    Stress_Cluster_integer,
    name = "n"
  )

if (
  nrow(cluster_size_audit) != 2L ||
  !identical(
    sort(cluster_size_audit$n),
    expected_cluster_sizes
  )
) {
  stop(
    "The Objective 3 Louvain membership does not reproduce the ",
    "definitive 44/45 partition."
  )
}

# ------------------------------------------------------------
# 7. Verify robust-coordinate continuity
# ------------------------------------------------------------

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

louvain_robust_difference <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        stress_louvain[[variable_i]] -
          stress_objective3[[variable_i]]
      )
    )
  },
  numeric(1)
)

if (
  any(!is.finite(stress_provenance_difference)) ||
  any(stress_provenance_difference > numeric_tolerance) ||
  any(!is.finite(louvain_robust_difference)) ||
  any(louvain_robust_difference > numeric_tolerance)
) {
  stop(
    "The corrected Louvain/Objective 4 objects do not reproduce the ",
    "definitive Objective 3 robust coordinates."
  )
}

# ------------------------------------------------------------
# 8. Join inherited PC1-PC4 and Behavioural_Index by full key
# ------------------------------------------------------------

# IMPORTANT:
# The PC1/PC2 columns stored in the Objective 3 characterisation
# object belong to the de novo Stress PCA used to describe the
# internal Stress architecture (Objective 3).
#
# The PC1-PC4 columns in index_data belong to the definitive
# Control-derived Objective 1 PCA used to construct the Objective 4
# Behavioural_Index.
#
# These are two different PCA coordinate systems and therefore must
# NOT be expected to match.
#
# Louvain itself was derived in the full four-dimensional robust
# behavioural space, not from either PCA. We therefore inherit only
# the definitive Louvain membership and robust coordinates from
# Objective 3, and join the Control-derived PC1-PC4 plus
# Behavioural_Index from the corrected Objective 4 object.

stress_louvain_o4 <- stress_louvain %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Stress_Cluster_integer,
    dplyr::all_of(robust_variables)
  ) %>%
  dplyr::left_join(
    index_data %>%
      dplyr::select(
        Author,
        ID,
        Traceability_ID,
        dplyr::all_of(pc_variables),
        Behavioural_Index
      ),
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  ) %>%
  dplyr::transmute(
    Author,
    ID,
    Traceability_ID,
    Stress_Cluster = factor(
      Stress_Cluster_integer
    ),
    dplyr::across(
      dplyr::all_of(robust_variables)
    ),
    dplyr::across(
      dplyr::all_of(pc_variables)
    ),
    Behavioural_Index
  )

if (
  any(
    !is.finite(
      as.matrix(
        stress_louvain_o4[
          c(
            robust_variables,
            pc_variables,
            "Behavioural_Index"
          )
        ]
      )
    )
  )
) {
  stop(
    "Missing or non-finite analytical values detected after Louvain join."
  )
}

# ------------------------------------------------------------
# 9. Characterise Louvain clusters
# ------------------------------------------------------------

louvain_community_summary <- stress_louvain_o4 %>%
  dplyr::group_by(
    Stress_Cluster
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),

    Percentage =
      100 *
      n /
      expected_stress_n,

    Mean_Index =
      mean(Behavioural_Index),

    SD_Index =
      stats::sd(Behavioural_Index),

    Median_Index =
      stats::median(Behavioural_Index),

    IQR_Index =
      stats::IQR(Behavioural_Index),

    Mean_PC1 =
      mean(PC1),

    Mean_PC2 =
      mean(PC2),

    Mean_PC3 =
      mean(PC3),

    Mean_PC4 =
      mean(PC4),

    Mean_distance_rz =
      mean(distance_rz),

    Mean_crossing_rz =
      mean(crossing_rz),

    Mean_top_entry_rz =
      mean(top_entry_rz),

    Mean_top_time_rz =
      mean(top_time_rz),

    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Mean_Index
  )

# ------------------------------------------------------------
# 10. Order clusters and harmonise Louvain classification
# ------------------------------------------------------------

louvain_order <- louvain_community_summary %>%
  dplyr::mutate(
    Louvain_Order =
      dplyr::row_number()
  ) %>%
  dplyr::select(
    Stress_Cluster,
    Louvain_Order,
    Mean_Index
  )

stress_louvain_o4 <- stress_louvain_o4 %>%
  dplyr::left_join(
    louvain_order,
    by = "Stress_Cluster"
  ) %>%
  dplyr::mutate(
    Louvain_Class =
      dplyr::case_when(
        Louvain_Order == 1L ~
          "Susceptible",
        Louvain_Order == 2L ~
          "Resilient",
        TRUE ~
          NA_character_
      ),
    Louvain_Class = factor(
      Louvain_Class,
      levels = c(
        "Susceptible",
        "Resilient"
      )
    )
  )

if (anyNA(stress_louvain_o4$Louvain_Class)) {
  stop(
    "Missing harmonised Louvain classifications were produced."
  )
}

# ------------------------------------------------------------
# 11. Harmonised-class summary
# ------------------------------------------------------------

louvain_class_summary <- stress_louvain_o4 %>%
  dplyr::group_by(
    Louvain_Class
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),

    Percentage =
      100 *
      n /
      expected_stress_n,

    Mean_Index =
      mean(Behavioural_Index),

    SD_Index =
      stats::sd(Behavioural_Index),

    Median_Index =
      stats::median(Behavioural_Index),

    IQR_Index =
      stats::IQR(Behavioural_Index),

    Mean_distance_rz =
      mean(distance_rz),

    Mean_crossing_rz =
      mean(crossing_rz),

    Mean_top_entry_rz =
      mean(top_entry_rz),

    Mean_top_time_rz =
      mean(top_time_rz),

    .groups = "drop"
  )

# ------------------------------------------------------------
# 12. Diagnostic PC1-PC2 figure
# ------------------------------------------------------------

p_louvain <- ggplot2::ggplot(
  stress_louvain_o4,
  ggplot2::aes(
    x = PC1,
    y = PC2,
    colour = Louvain_Class
  )
) +
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.8
  ) +
  ggplot2::labs(
    x = "PC1",
    y = "PC2",
    colour = "Louvain class"
  ) +
  ggplot2::theme_classic(
    base_size = 14,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    legend.position = "right"
  )

print(p_louvain)

ggplot2::ggsave(
  filename = pc_pdf_output,
  plot = p_louvain,
  device = grDevices::cairo_pdf,
  width = 7.5,
  height = 6,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = pc_tiff_output,
  plot = p_louvain,
  width = 7.5,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ------------------------------------------------------------
# 13. Behavioural-index diagnostic figure
# ------------------------------------------------------------

p_louvain_index <- ggplot2::ggplot(
  stress_louvain_o4,
  ggplot2::aes(
    x = Louvain_Class,
    y = Behavioural_Index,
    fill = Louvain_Class
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
    y = "Multivariate behavioural index",
    fill = NULL
  ) +
  ggplot2::theme_classic(
    base_size = 14,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    legend.position = "none"
  )

print(p_louvain_index)

ggplot2::ggsave(
  filename = index_pdf_output,
  plot = p_louvain_index,
  device = grDevices::cairo_pdf,
  width = 7.5,
  height = 5.5,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = index_tiff_output,
  plot = p_louvain_index,
  width = 7.5,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ------------------------------------------------------------
# 14. Add Louvain to the corrected four-method size table
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

louvain_size_row <- tibble::tibble(
  Method =
    "Louvain",

  Susceptible =
    sum(
      stress_louvain_o4$Louvain_Class ==
        "Susceptible"
    ),

  Intermediate =
    NA_integer_,

  Resilient =
    sum(
      stress_louvain_o4$Louvain_Class ==
        "Resilient"
    )
)

method_size_table <- dplyr::bind_rows(
  method_size_table,
  louvain_size_row
)

# ------------------------------------------------------------
# 15. Export tables
# ------------------------------------------------------------

classification_export <- stress_louvain_o4 %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    dplyr::all_of(robust_variables),
    dplyr::all_of(pc_variables),
    Behavioural_Index,
    Stress_Cluster,
    Louvain_Order,
    Louvain_Class
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

openxlsx::write.xlsx(
  list(
    Objective3_Louvain_audit =
      data.frame(
        Stress_n = expected_stress_n,
        Selected_k = expected_selected_k,
        Plateau_start_k = expected_plateau_start,
        Plateau_end_k = expected_plateau_end,
        Number_of_clusters = 2L
      ),

    Raw_cluster_sizes =
      cluster_size_audit,

    Community_summary =
      louvain_community_summary,

    Community_order =
      louvain_order,

    Class_summary =
      louvain_class_summary,

    Classification =
      classification_export,

    Five_method_sizes =
      method_size_table
  ),
  file =
    xlsx_output,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 16. Save corrected Objective 4 Louvain objects
# ------------------------------------------------------------

saveRDS(
  stress_louvain_o4,
  classification_output
)

saveRDS(
  louvain_community_summary,
  community_summary_output
)

saveRDS(
  louvain_class_summary,
  class_summary_output
)

saveRDS(
  louvain_order,
  community_order_output
)

saveRDS(
  method_size_table,
  method_size_output
)

capture.output(
  utils::sessionInfo(),
  file =
    session_info_output
)

# ------------------------------------------------------------
# 17. Confirm outputs
# ------------------------------------------------------------

expected_output_files <- c(
  classification_output,
  community_summary_output,
  class_summary_output,
  community_order_output,
  method_size_output,
  xlsx_output,
  pc_pdf_output,
  pc_tiff_output,
  index_pdf_output,
  index_tiff_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following corrected Louvain outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 18. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - LOUVAIN OPERATIONAL CLASSIFICATION\n")
cat("============================================================\n")

cat("\nDefinitive Objective 3 source:\n")
cat(objective3_characterisation_file, "\n")

cat("\nSelected Louvain configuration source:\n")
cat(objective3_selected_louvain_file, "\n")

cat("\nStress fish:\n")
print(nrow(stress_louvain_o4))

cat("\nFish per Author:\n")
print(
  table(
    stress_louvain_o4$Author,
    useNA = "ifany"
  )
)

cat("\nDuplicate Author + ID combinations:\n")
print(
  sum(
    duplicated(
      stress_louvain_o4[c("Author", "ID")]
    )
  )
)

cat("\nMaximum absolute difference from definitive Objective 3 robust coordinates:\n")
print(
  stress_provenance_difference
)

cat("\nMaximum absolute difference between Objective 3 Louvain data and definitive robust coordinates:\n")
print(
  louvain_robust_difference
)

cat(
  "\nPCA coordinate systems:\n",
  "Objective 3 de novo Stress PCA scores were not compared with the ",
  "Control-derived Objective 1 PCA scores because they are distinct PCA spaces.\n",
  "Objective 4 PC1-PC4 and Behavioural_Index were inherited from the ",
  "corrected Control-derived PCA workflow.\n",
  sep = ""
)

cat("\nDefinitive Objective 3 Louvain configuration:\n")
cat("Selected k: ", expected_selected_k, "\n", sep = "")
cat(
  "Stable two-cluster plateau: k = ",
  expected_plateau_start,
  "–",
  expected_plateau_end,
  "\n",
  sep = ""
)

cat("\nOriginal de novo Louvain cluster sizes:\n")
print(
  cluster_size_audit,
  n = Inf,
  width = Inf
)

cat("\nLouvain clusters ordered by Behavioural_Index:\n")
print(
  louvain_community_summary,
  n = Inf,
  width = Inf
)

cat("\nHarmonised Louvain classification:\n")
print(
  table(
    stress_louvain_o4$Louvain_Class,
    useNA = "ifany"
  )
)

cat("\nLouvain class summary:\n")
print(
  louvain_class_summary,
  n = Inf,
  width = Inf
)

cat("\nFive-method size table:\n")
print(
  method_size_table,
  n = Inf,
  width = Inf
)

cat(
  "\nNo Louvain clustering was recalculated in Objective 4.\n",
  "The definitive de novo Objective 3 partition was inherited unchanged.\n",
  "The Control-derived reference-profile refinement from Objective 3 was NOT ",
  "used to redefine this pure Louvain classifier.\n",
  sep = ""
)

cat("\nOutputs saved successfully.\n")
cat("\n============================================================\n")
