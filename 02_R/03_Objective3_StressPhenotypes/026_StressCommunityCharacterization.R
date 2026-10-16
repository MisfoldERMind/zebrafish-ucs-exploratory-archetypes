# ============================================================
# 026_StressCommunityCharacterization.R
#
# Objective 3
# Characterisation and operational interpretation of the
# stability-assessed post-stress Louvain communities
#
# Scientific question:
#   What internal behavioural structure emerges within
#   the UCS-exposed population?
#
# Analytical role:
#   Characterise the two-community candidate partition identified
#   de novo in corrected Script 024 and evaluated for algorithmic
#   and sampling stability in corrected Script 025.
#
# Important interpretive constraints:
#   - The same four behavioural variables were used to derive and
#     characterise the communities. Their comparisons are therefore
#     descriptive, not independent confirmatory tests.
#   - Susceptible-like and resilient-like are operational labels
#     applied only to UCS-exposed fish. The suffix "-like" is retained
#     because biological validation is not yet available.
#   - The de novo Stress communities are not assumed to be equivalent
#     to the Control-derived reference profiles from Objective 2.
#   - Legacy object/column names containing Normative_* or Archetype_*
#     are retained only for compatibility with corrected Script 016.
#
# Required upstream scripts:
#   016_Nearest_Normative_Archetype_CORREGIDO_v2.R
#   023_Figure2_StressPCA_CORREGIDO_v2.R
#   024_Stress_Louvain_CORREGIDO_v2.R
#   025_Stress_Louvain_Stability_CORREGIDO.R
# ============================================================

script_version <- "026_StressCommunityCharacterization_CORREGIDO | 2026-08-07 | Author+ID audit"
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
  "tidyr",
  "tibble",
  "ggplot2",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(openxlsx)
})

# ------------------------------------------------------------
# 2. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(project_root, "01_Data", "Processed")
objects_dir <- file.path(project_root, "03_Results", "Objects")
tables_dir <- file.path(project_root, "03_Results", "Tables")
figures_dir <- file.path(project_root, "03_Results", "Figures")

if (!dir.exists(processed_dir)) {
  stop(
    "Processed-data directory not found:\n",
    processed_dir
  )
}

for (directory_i in c(objects_dir, tables_dir, figures_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

stress_louvain_input <- file.path(
  processed_dir,
  "stress_louvain_clusters.rds"
)

selected_configuration_input <- file.path(
  objects_dir,
  "objective3_selected_louvain_configuration.rds"
)

selected_membership_input <- file.path(
  objects_dir,
  "objective3_selected_louvain_membership.rds"
)

stability_results_input <- file.path(
  objects_dir,
  "objective3_louvain_stability_results.rds"
)

pca_results_input <- file.path(
  objects_dir,
  "objective3_stress_pca_results.rds"
)

normative_projection_input <- file.path(
  processed_dir,
  "population_normative_archetype_projection_robust.rds"
)

required_input_files <- c(
  stress_louvain_input,
  selected_configuration_input,
  selected_membership_input,
  stability_results_input,
  pca_results_input,
  normative_projection_input
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required upstream files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun corrected Script 016 and corrected Objective 3 Scripts 023-025 first."
  )
}

phenotype_data_output <- file.path(
  processed_dir,
  "stress_poststress_phenotypes.rds"
)

pca_pdf_output <- file.path(
  figures_dir,
  "Objective3_Figure3_StressCommunityPCA.pdf"
)

pca_tiff_output <- file.path(
  figures_dir,
  "Objective3_Figure3_StressCommunityPCA.tiff"
)

profile_pdf_output <- file.path(
  figures_dir,
  "Objective3_Figure4_StressCommunityProfiles.pdf"
)

profile_tiff_output <- file.path(
  figures_dir,
  "Objective3_Figure4_StressCommunityProfiles.tiff"
)

workbook_output <- file.path(
  tables_dir,
  "Objective3_StressCommunityCharacterisation.xlsx"
)

community_sizes_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_Sizes.csv"
)

community_centroids_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_Centroids.csv"
)

descriptive_statistics_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_DescriptiveStatistics.csv"
)

community_comparisons_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_DescriptiveComparisons.csv"
)

normative_mapping_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_NormativeProfileMapping.csv"
)

individual_classification_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_IndividualClassification.csv"
)

label_definition_csv <- file.path(
  tables_dir,
  "Objective3_StressCommunity_OperationalLabels.csv"
)

results_object_output <- file.path(
  objects_dir,
  "objective3_stress_community_characterisation_results.rds"
)

community_sizes_output <- file.path(objects_dir, "stress_community_sizes.rds")
community_centroids_output <- file.path(objects_dir, "stress_community_centroids.rds")
descriptive_statistics_output <- file.path(
  objects_dir,
  "stress_community_descriptive_statistics.rds"
)
community_comparisons_output <- file.path(
  objects_dir,
  "stress_community_descriptive_comparisons.rds"
)
normative_mapping_output <- file.path(
  objects_dir,
  "stress_community_normative_profile_mapping.rds"
)
pca_plot_output <- file.path(objects_dir, "stress_community_pca_plot.rds")
profile_plot_output <- file.path(objects_dir, "stress_community_profile_plot.rds")
session_info_output <- file.path(
  objects_dir,
  "objective3_stress_community_characterisation_sessionInfo.txt"
)

# ------------------------------------------------------------
# 3. Load upstream objects
# ------------------------------------------------------------

stress_louvain <- readRDS(stress_louvain_input)
selected_configuration <- readRDS(selected_configuration_input)
selected_membership <- readRDS(selected_membership_input)
stability_results <- readRDS(stability_results_input)
pca_results <- readRDS(pca_results_input)
normative_projection <- readRDS(normative_projection_input)

if (!is.data.frame(stress_louvain)) {
  stop(basename(stress_louvain_input), " does not contain a data frame.")
}
if (!is.list(selected_configuration)) {
  stop(basename(selected_configuration_input), " does not contain the expected list.")
}
if (!is.data.frame(selected_membership)) {
  stop(basename(selected_membership_input), " does not contain a data frame.")
}
if (!is.list(stability_results)) {
  stop(basename(stability_results_input), " does not contain the expected list.")
}
if (!is.list(pca_results)) {
  stop(basename(pca_results_input), " does not contain the expected list.")
}
if (!is.data.frame(normative_projection)) {
  stop(basename(normative_projection_input), " does not contain a data frame.")
}

# ------------------------------------------------------------
# 4. Fixed definitions and helper functions
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_number_of_communities <- 2L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"
expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

numeric_tolerance <- 1e-10
ellipse_level <- 0.95

behavioural_variables <- c("distance", "crossing", "top_entry", "top_time")
robust_variables <- c("distance_rz", "crossing_rz", "top_entry_rz", "top_time_rz")

variable_labels <- c(
  distance_rz = "Distance travelled",
  crossing_rz = "Line crossings",
  top_entry_rz = "Upper-zone entries",
  top_time_rz = "Upper-zone time"
)

phenotype_levels <- c("Susceptible-like", "Resilient-like")
phenotype_colours <- c(
  "Susceptible-like" = "#F07A5A",
  "Resilient-like" = "#63B8A6"
)

cluster_to_integer <- function(x, object_name) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  result <- suppressWarnings(as.integer(x))

  if (length(result) == 0L || anyNA(result)) {
    stop("Invalid community labels were detected in ", object_name, ".")
  }

  result
}

scalar_integer <- function(x, object_name) {
  value <- suppressWarnings(as.integer(x))

  if (length(value) != 1L || is.na(value)) {
    stop(object_name, " must contain one valid integer.")
  }

  value
}

scalar_numeric <- function(x, object_name) {
  value <- suppressWarnings(as.numeric(x))

  if (length(value) != 1L || is.na(value) || !is.finite(value)) {
    stop(object_name, " must contain one finite numeric value.")
  }

  value
}

format_p_value <- function(p_value) {
  if (length(p_value) != 1L || is.na(p_value) || !is.finite(p_value)) {
    return(NA_character_)
  }

  if (p_value < 0.001) {
    return(format(p_value, scientific = TRUE, digits = 3))
  }

  sprintf("%.4f", p_value)
}

rank_biserial_magnitude <- function(effect_size) {
  absolute_effect <- abs(effect_size)

  if (absolute_effect < 0.10) return("Negligible")
  if (absolute_effect < 0.30) return("Small")
  if (absolute_effect < 0.50) return("Moderate")
  "Large"
}

adjusted_rand_index <- function(partition_1, partition_2) {
  if (length(partition_1) != length(partition_2)) {
    stop("Partitions supplied to adjusted_rand_index() have different lengths.")
  }

  contingency <- table(partition_1, partition_2)
  choose_2 <- function(x) x * (x - 1) / 2

  sum_cells <- sum(choose_2(contingency))
  row_pairs <- sum(choose_2(rowSums(contingency)))
  column_pairs <- sum(choose_2(colSums(contingency)))
  total_pairs <- choose_2(length(partition_1))
  expected_index <- row_pairs * column_pairs / total_pairs
  maximum_index <- 0.5 * (row_pairs + column_pairs)
  denominator <- maximum_index - expected_index

  if (abs(denominator) <= .Machine$double.eps) {
    return(if (identical(as.integer(factor(partition_1)), as.integer(factor(partition_2)))) 1 else 0)
  }

  ari <- (sum_cells - expected_index) / denominator
  max(-1, min(1, as.numeric(ari)))
}

make_normal_data_ellipse <- function(data, group_variable, x_variable, y_variable, level = 0.95) {
  group_values <- unique(as.character(data[[group_variable]]))

  ellipse_list <- lapply(group_values, function(group_i) {
    group_index <- as.character(data[[group_variable]]) == group_i
    group_matrix <- cbind(data[[x_variable]][group_index], data[[y_variable]][group_index])

    if (nrow(group_matrix) < 3L || any(!is.finite(group_matrix))) {
      stop("Insufficient or invalid PCA data for ellipse: ", group_i, ".")
    }

    covariance_matrix <- stats::cov(group_matrix)
    eigen_decomposition <- eigen(covariance_matrix, symmetric = TRUE)

    if (any(!is.finite(eigen_decomposition$values)) || any(eigen_decomposition$values <= 0)) {
      stop("The PCA covariance matrix is not positive definite for ", group_i, ".")
    }

    angle_values <- seq(0, 2 * pi, length.out = 240L)
    unit_circle <- rbind(cos(angle_values), sin(angle_values))
    ellipse_scale <- sqrt(stats::qchisq(level, df = 2))

    ellipse_coordinates <- eigen_decomposition$vectors %*%
      diag(sqrt(eigen_decomposition$values), nrow = 2) %*%
      unit_circle * ellipse_scale

    centroid <- colMeans(group_matrix)

    tibble::tibble(
      PostStress_Phenotype = group_i,
      PC1 = ellipse_coordinates[1, ] + centroid[1],
      PC2 = ellipse_coordinates[2, ] + centroid[2]
    )
  })

  dplyr::bind_rows(ellipse_list)
}

# ------------------------------------------------------------
# 5. Validate corrected Script 024 configuration and Stress data
# ------------------------------------------------------------

required_configuration_fields <- c(
  "Scientific_question",
  "Operational_question",
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Normalisation",
  "Robust_variables",
  "Selected_k",
  "Selected_number_of_communities",
  "Selected_plateau_start_k",
  "Selected_plateau_end_k",
  "Selected_plateau_length",
  "Selected_weighted_modularity"
)

missing_configuration_fields <- setdiff(
  required_configuration_fields,
  names(selected_configuration)
)

if (length(missing_configuration_fields) > 0L) {
  stop(
    "The corrected selected Louvain configuration is missing:\n",
    paste(missing_configuration_fields, collapse = ", ")
  )
}

selected_k <- scalar_integer(
  selected_configuration$Selected_k,
  "selected_configuration$Selected_k"
)

selected_number_of_communities <- scalar_integer(
  selected_configuration$Selected_number_of_communities,
  "selected_configuration$Selected_number_of_communities"
)

selected_plateau_start_k <- scalar_integer(
  selected_configuration$Selected_plateau_start_k,
  "selected_configuration$Selected_plateau_start_k"
)

selected_plateau_end_k <- scalar_integer(
  selected_configuration$Selected_plateau_end_k,
  "selected_configuration$Selected_plateau_end_k"
)

selected_plateau_length <- scalar_integer(
  selected_configuration$Selected_plateau_length,
  "selected_configuration$Selected_plateau_length"
)

selected_weighted_modularity <- scalar_numeric(
  selected_configuration$Selected_weighted_modularity,
  "selected_configuration$Selected_weighted_modularity"
)

if (
  !identical(
    as.character(selected_configuration$Analytical_population),
    "Stress only"
  )
) {
  stop(
    "The selected configuration does not define Stress as the sole ",
    "analytical population."
  )
}

if (
  as.integer(selected_configuration$Stress_n) != expected_stress_n
) {
  stop(
    "The selected configuration does not report Stress n = 89."
  )
}

if (
  !identical(
    as.character(selected_configuration$Traceability),
    expected_traceability
  )
) {
  stop(
    "The selected configuration does not preserve Author + ID traceability."
  )
}

if (
  !identical(
    as.character(selected_configuration$Normalisation),
    expected_normalisation
  )
) {
  stop(
    "The selected configuration does not preserve the definitive ",
    "Author-specific Control-reference normalisation."
  )
}

if (
  !identical(
    as.character(selected_configuration$Robust_variables),
    robust_variables
  )
) {
  stop(
    "The selected configuration does not use the definitive robust variables."
  )
}

configuration_authors <- sort(
  as.character(selected_configuration$Authors)
)

if (
  length(configuration_authors) != expected_number_of_authors
) {
  stop(
    "The selected configuration does not retain the expected four Authors."
  )
}

if (
  selected_number_of_communities != expected_number_of_communities
) {
  stop(
    "This script requires the definitive two-community candidate ",
    "partition; observed communities = ",
    selected_number_of_communities,
    "."
  )
}

if (
  selected_plateau_start_k > selected_k ||
  selected_plateau_end_k < selected_k ||
  selected_plateau_length !=
    selected_plateau_end_k -
    selected_plateau_start_k +
    1L
) {
  stop(
    "The selected k and plateau information are inconsistent."
  )
}

required_stress_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  behavioural_variables,
  robust_variables,
  "PC1",
  "PC2",
  "Stress_Cluster",
  "Stress_Community"
)

missing_stress_columns <- setdiff(
  required_stress_columns,
  names(stress_louvain)
)

if (length(missing_stress_columns) > 0L) {
  stop(
    "The corrected Stress Louvain dataset is missing required columns:\n",
    paste(missing_stress_columns, collapse = ", ")
  )
}

stress_louvain <- stress_louvain %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
  )

expected_stress_keys <- paste(
  stress_louvain$Author,
  stress_louvain$ID,
  sep = " || "
)

if (
  nrow(stress_louvain) != expected_stress_n ||
  anyNA(
    stress_louvain[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  any(stress_louvain$Author == "") ||
  any(stress_louvain$ID == "") ||
  any(stress_louvain$Traceability_ID == "") ||
  anyDuplicated(stress_louvain$Traceability_ID) > 0L ||
  !identical(
    stress_louvain$Traceability_ID,
    expected_stress_keys
  ) ||
  !all(stress_louvain$Treatment == "Stress")
) {
  stop(
    "The corrected Stress Louvain dataset fails the Author + ID / ",
    "Stress-only audit."
  )
}

population_authors <- sort(
  unique(stress_louvain$Author)
)

if (
  length(population_authors) != expected_number_of_authors ||
  !identical(
    population_authors,
    configuration_authors
  )
) {
  stop(
    "The Stress dataset and corrected Script 024 configuration do not ",
    "contain the same four Authors."
  )
}

legacy_variables <- c(
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z"
)

legacy_variables_present <- intersect(
  legacy_variables,
  names(stress_louvain)
)

if (length(legacy_variables_present) > 0L) {
  stop(
    "Legacy mean/SD-standardised variables were detected:\n",
    paste(legacy_variables_present, collapse = ", ")
  )
}

numeric_columns <- c(
  behavioural_variables,
  robust_variables,
  "PC1",
  "PC2"
)

non_numeric_columns <- numeric_columns[
  !vapply(
    stress_louvain[numeric_columns],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_columns) > 0L) {
  stop(
    "The following analytical variables are not numeric:\n",
    paste(non_numeric_columns, collapse = ", ")
  )
}

analytical_matrix <- as.matrix(
  stress_louvain[
    robust_variables
  ]
)

storage.mode(analytical_matrix) <- "double"
rownames(analytical_matrix) <- stress_louvain$Traceability_ID

if (
  any(!is.finite(analytical_matrix)) ||
  any(!is.finite(stress_louvain$PC1)) ||
  any(!is.finite(stress_louvain$PC2))
) {
  stop(
    "Missing or non-finite behavioural or PCA values were detected."
  )
}

centered_analytical_matrix <- sweep(
  analytical_matrix,
  2L,
  colMeans(analytical_matrix),
  FUN = "-"
)

if (
  qr(centered_analytical_matrix)$rank != length(robust_variables)
) {
  stop(
    "The centred Stress robust behavioural matrix does not have full rank."
  )
}

stress_louvain <- stress_louvain %>%
  dplyr::mutate(
    Stress_Cluster_integer = cluster_to_integer(
      Stress_Cluster,
      "stress_louvain$Stress_Cluster"
    )
  )

if (
  !identical(
    sort(unique(stress_louvain$Stress_Cluster_integer)),
    c(1L, 2L)
  )
) {
  stop(
    "The definitive Stress community labels must be exactly 1 and 2."
  )
}

# ------------------------------------------------------------
# 6. Verify corrected selected membership by Author + ID
# ------------------------------------------------------------

required_membership_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  "k",
  "Stress_Cluster",
  "Stress_Community",
  "PC1",
  "PC2"
)

missing_membership_columns <- setdiff(
  required_membership_columns,
  names(selected_membership)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The corrected selected-membership table is missing required columns:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

selected_membership <- selected_membership %>%
  dplyr::transmute(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment),
    k = as.integer(k),
    Stress_Cluster_integer = cluster_to_integer(
      Stress_Cluster,
      "selected_membership$Stress_Cluster"
    ),
    Stress_Community = as.character(Stress_Community),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2)
  )

expected_membership_keys <- paste(
  selected_membership$Author,
  selected_membership$ID,
  sep = " || "
)

if (
  nrow(selected_membership) != expected_stress_n ||
  anyNA(
    selected_membership[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  anyDuplicated(selected_membership$Traceability_ID) > 0L ||
  !identical(
    selected_membership$Traceability_ID,
    expected_membership_keys
  ) ||
  !setequal(
    selected_membership$Traceability_ID,
    stress_louvain$Traceability_ID
  ) ||
  !all(selected_membership$Treatment == "Stress")
) {
  stop(
    "The corrected selected-membership Author + ID keys do not match ",
    "the Stress dataset."
  )
}

selected_membership <- selected_membership[
  match(
    stress_louvain$Traceability_ID,
    selected_membership$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    selected_membership$Traceability_ID,
    stress_louvain$Traceability_ID
  ) ||
  !identical(
    selected_membership$Author,
    stress_louvain$Author
  ) ||
  !identical(
    selected_membership$ID,
    stress_louvain$ID
  )
) {
  stop(
    "The selected membership could not be aligned exactly by Author + ID."
  )
}

if (
  any(selected_membership$k != selected_k) ||
  !identical(
    selected_membership$Stress_Cluster_integer,
    stress_louvain$Stress_Cluster_integer
  )
) {
  stop(
    "The selected membership does not reproduce the definitive Stress ",
    "community assignments."
  )
}

maximum_membership_pca_difference <- max(
  abs(
    selected_membership$PC1 -
      stress_louvain$PC1
  ),
  abs(
    selected_membership$PC2 -
      stress_louvain$PC2
  )
)

if (
  !is.finite(maximum_membership_pca_difference) ||
  maximum_membership_pca_difference > numeric_tolerance
) {
  stop(
    "PCA scores differ between the selected membership and corrected ",
    "Stress Louvain dataset."
  )
}

# ------------------------------------------------------------
# 7. Validate stability evidence from corrected Script 025
# ------------------------------------------------------------

required_stability_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Normalisation",
  "Robust_variables",
  "Maximum_upstream_recalculation_difference",
  "Selected_k",
  "Reference_number_of_communities",
  "Reference_community_sizes",
  "Reference_membership",
  "Seed_stability_summary",
  "Bootstrap_summary",
  "Bootstrap_cluster_count_distribution"
)

missing_stability_elements <- setdiff(
  required_stability_elements,
  names(stability_results)
)

if (length(missing_stability_elements) > 0L) {
  stop(
    "The corrected stability results object is missing:\n",
    paste(missing_stability_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(stability_results$Analytical_population),
    "Stress only"
  ) ||
  as.integer(stability_results$Stress_n) != expected_stress_n ||
  !identical(
    sort(as.character(stability_results$Authors)),
    population_authors
  ) ||
  !identical(
    as.character(stability_results$Traceability),
    expected_traceability
  ) ||
  !identical(
    as.character(stability_results$Normalisation),
    expected_normalisation
  ) ||
  !identical(
    as.character(stability_results$Robust_variables),
    robust_variables
  ) ||
  as.integer(stability_results$Selected_k) != selected_k ||
  as.integer(
    stability_results$Reference_number_of_communities
  ) != expected_number_of_communities
) {
  stop(
    "The corrected Script 025 stability object is inconsistent with ",
    "corrected Script 024."
  )
}

maximum_upstream_recalculation_difference <- scalar_numeric(
  stability_results$Maximum_upstream_recalculation_difference,
  "stability_results$Maximum_upstream_recalculation_difference"
)

if (
  maximum_upstream_recalculation_difference > numeric_tolerance
) {
  stop(
    "Corrected Script 025 does not preserve exact upstream robust scores."
  )
}

reference_membership_stored <-
  stability_results$Reference_membership

reference_membership <-
  as.integer(reference_membership_stored)

reference_membership_names <-
  names(reference_membership_stored)

if (
  length(reference_membership) != expected_stress_n ||
  is.null(reference_membership_names) ||
  anyNA(reference_membership_names) ||
  anyDuplicated(reference_membership_names) > 0L ||
  !setequal(
    reference_membership_names,
    stress_louvain$Traceability_ID
  )
) {
  stop(
    "The corrected Script 025 reference membership cannot be aligned ",
    "by Author + ID."
  )
}

reference_membership <- reference_membership[
  match(
    stress_louvain$Traceability_ID,
    reference_membership_names
  )
]

if (
  !identical(
    reference_membership,
    stress_louvain$Stress_Cluster_integer
  )
) {
  stop(
    "The corrected Script 025 reference membership differs from the ",
    "definitive assignments."
  )
}

seed_stability_summary <-
  stability_results$Seed_stability_summary

bootstrap_summary <-
  stability_results$Bootstrap_summary

bootstrap_cluster_count_distribution <-
  stability_results$Bootstrap_cluster_count_distribution

if (
  !is.data.frame(seed_stability_summary) ||
  nrow(seed_stability_summary) != 1L ||
  !is.data.frame(bootstrap_summary) ||
  nrow(bootstrap_summary) != 1L ||
  !is.data.frame(bootstrap_cluster_count_distribution)
) {
  stop(
    "The stability summary objects do not have the expected structure."
  )
}

required_seed_columns <- c(
  "Seed_repetitions",
  "Exact_reference_rate",
  "Reference_community_count_rate",
  "Mean_ARI_to_reference",
  "Mean_pairwise_ARI"
)

required_bootstrap_columns <- c(
  "Bootstrap_iterations_successful",
  "Mean_ARI_Unique_Fish",
  "Median_ARI_Unique_Fish",
  "ARI_Unique_Fish_percentile_2_5",
  "Reference_community_count_recovery_rate",
  "Mean_repeated_copy_consistency"
)

if (
  length(
    setdiff(
      required_seed_columns,
      names(seed_stability_summary)
    )
  ) > 0L ||
  length(
    setdiff(
      required_bootstrap_columns,
      names(bootstrap_summary)
    )
  ) > 0L
) {
  stop(
    "The corrected stability summaries are missing required fields."
  )
}

stability_numeric_values <- c(
  as.numeric(seed_stability_summary$Seed_repetitions),
  as.numeric(seed_stability_summary$Exact_reference_rate),
  as.numeric(seed_stability_summary$Reference_community_count_rate),
  as.numeric(seed_stability_summary$Mean_ARI_to_reference),
  as.numeric(seed_stability_summary$Mean_pairwise_ARI),
  as.numeric(bootstrap_summary$Bootstrap_iterations_successful),
  as.numeric(bootstrap_summary$Mean_ARI_Unique_Fish),
  as.numeric(bootstrap_summary$Median_ARI_Unique_Fish),
  as.numeric(bootstrap_summary$ARI_Unique_Fish_percentile_2_5),
  as.numeric(
    bootstrap_summary$Reference_community_count_recovery_rate
  ),
  as.numeric(bootstrap_summary$Mean_repeated_copy_consistency)
)

if (
  anyNA(stability_numeric_values) ||
  any(!is.finite(stability_numeric_values))
) {
  stop(
    "At least one required corrected stability result is non-finite."
  )
}

# ------------------------------------------------------------
# 8. Validate and align the corrected Stress PCA
# ------------------------------------------------------------

required_pca_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Normalisation",
  "Robust_variables",
  "PCA_variance",
  "PCA_loadings",
  "PCA_scores",
  "PCA_center",
  "PCA_scale",
  "Stress_matrix_rank"
)

missing_pca_elements <- setdiff(
  required_pca_elements,
  names(pca_results)
)

if (length(missing_pca_elements) > 0L) {
  stop(
    "The corrected Stress PCA object is missing:\n",
    paste(missing_pca_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(pca_results$Analytical_population),
    "Stress only"
  ) ||
  as.integer(pca_results$Stress_n) != expected_stress_n ||
  !identical(
    sort(as.character(pca_results$Authors)),
    population_authors
  ) ||
  !identical(
    as.character(pca_results$Traceability),
    expected_traceability
  ) ||
  !identical(
    as.character(pca_results$Normalisation),
    expected_normalisation
  ) ||
  !identical(
    as.character(pca_results$Robust_variables),
    robust_variables
  ) ||
  !isTRUE(pca_results$PCA_center) ||
  !identical(pca_results$PCA_scale, FALSE) ||
  as.integer(pca_results$Stress_matrix_rank) !=
    length(robust_variables)
) {
  stop(
    "The corrected PCA configuration is inconsistent with Objective 3."
  )
}

pca_scores <- pca_results$PCA_scores
pca_variance <- pca_results$PCA_variance
pca_loadings <- pca_results$PCA_loadings

if (
  !is.data.frame(pca_scores) ||
  !is.data.frame(pca_variance) ||
  !is.data.frame(pca_loadings)
) {
  stop(
    "The corrected PCA tables do not have the expected structure."
  )
}

required_pca_score_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  "PC1",
  "PC2"
)

if (
  length(
    setdiff(
      required_pca_score_columns,
      names(pca_scores)
    )
  ) > 0L
) {
  stop(
    "The corrected PCA-score table is missing Author + ID provenance ",
    "or PC1/PC2."
  )
}

pca_scores <- pca_scores %>%
  dplyr::transmute(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2)
  )

expected_pca_keys <- paste(
  pca_scores$Author,
  pca_scores$ID,
  sep = " || "
)

if (
  nrow(pca_scores) != expected_stress_n ||
  anyNA(
    pca_scores[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  anyDuplicated(pca_scores$Traceability_ID) > 0L ||
  !identical(
    pca_scores$Traceability_ID,
    expected_pca_keys
  ) ||
  !setequal(
    pca_scores$Traceability_ID,
    stress_louvain$Traceability_ID
  ) ||
  !all(pca_scores$Treatment == "Stress")
) {
  stop(
    "The corrected PCA-score Author + ID keys do not match the Stress dataset."
  )
}

pca_scores <- pca_scores[
  match(
    stress_louvain$Traceability_ID,
    pca_scores$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    pca_scores$Traceability_ID,
    stress_louvain$Traceability_ID
  ) ||
  !identical(
    pca_scores$Author,
    stress_louvain$Author
  ) ||
  !identical(
    pca_scores$ID,
    stress_louvain$ID
  )
) {
  stop(
    "The corrected PCA scores could not be aligned exactly by Author + ID."
  )
}

maximum_pca_score_difference <- max(
  abs(
    pca_scores$PC1 -
      stress_louvain$PC1
  ),
  abs(
    pca_scores$PC2 -
      stress_louvain$PC2
  )
)

if (
  !is.finite(maximum_pca_score_difference) ||
  maximum_pca_score_difference > numeric_tolerance
) {
  stop(
    "PCA scores differ between corrected Script 023 and the Stress ",
    "Louvain dataset."
  )
}

if (
  length(
    setdiff(
      c("Variable", "PC1"),
      names(pca_loadings)
    )
  ) > 0L
) {
  stop(
    "The PCA-loading table is missing Variable or PC1."
  )
}

pc1_loadings <- pca_loadings$PC1[
  match(
    robust_variables,
    pca_loadings$Variable
  )
]

if (
  length(pc1_loadings) != length(robust_variables) ||
  anyNA(pc1_loadings) ||
  any(!is.finite(pc1_loadings)) ||
  !all(pc1_loadings > 0)
) {
  stop(
    "PC1 is not oriented from lower to higher post-stress exploration."
  )
}

# ------------------------------------------------------------
# 9. Community sizes and descriptive statistics
# ------------------------------------------------------------

community_sizes <- stress_louvain %>%
  dplyr::count(
    Stress_Cluster_integer,
    name = "n"
  ) %>%
  dplyr::arrange(Stress_Cluster_integer) %>%
  dplyr::mutate(
    Percentage = 100 * n / expected_stress_n,
    Mean_PC1 = vapply(
      Stress_Cluster_integer,
      function(cluster_i) {
        mean(stress_louvain$PC1[stress_louvain$Stress_Cluster_integer == cluster_i])
      },
      numeric(1)
    ),
    Median_PC1 = vapply(
      Stress_Cluster_integer,
      function(cluster_i) {
        stats::median(stress_louvain$PC1[stress_louvain$Stress_Cluster_integer == cluster_i])
      },
      numeric(1)
    )
  )

if (
  nrow(community_sizes) != expected_number_of_communities ||
  sum(community_sizes$n) != expected_stress_n ||
  any(community_sizes$n <= 0L) ||
  !identical(community_sizes$Stress_Cluster_integer, c(1L, 2L))
) {
  stop("The definitive two-community size table is inconsistent.")
}

if (!(community_sizes$Mean_PC1[1] < community_sizes$Mean_PC1[2])) {
  stop("Community numbering is not ordered from lower to higher PC1.")
}

descriptive_statistics_list <- lapply(
  robust_variables,
  function(variable_i) {
    dplyr::bind_rows(
      lapply(
        c(1L, 2L),
        function(cluster_i) {
          cluster_index <- stress_louvain$Stress_Cluster_integer == cluster_i
          values_i <- stress_louvain[[variable_i]][cluster_index]

          if (length(values_i) != sum(cluster_index) || any(!is.finite(values_i))) {
            stop(
              "Invalid values were detected for ",
              variable_i,
              " in Community ",
              cluster_i,
              "."
            )
          }

          tibble::tibble(
            Variable_code = variable_i,
            Variable = unname(variable_labels[[variable_i]]),
            Stress_Cluster_integer = cluster_i,
            n = length(values_i),
            Mean = mean(values_i),
            SD = stats::sd(values_i),
            Median = stats::median(values_i),
            Q1 = stats::quantile(values_i, 0.25, names = FALSE, type = 7),
            Q3 = stats::quantile(values_i, 0.75, names = FALSE, type = 7),
            IQR = stats::IQR(values_i, type = 7),
            MAD_scaled = stats::mad(values_i, constant = 1.4826),
            Minimum = min(values_i),
            Maximum = max(values_i)
          )
        }
      )
    )
  }
)

descriptive_statistics <- dplyr::bind_rows(descriptive_statistics_list) %>%
  dplyr::arrange(
    match(Variable_code, robust_variables),
    Stress_Cluster_integer
  )

if (nrow(descriptive_statistics) != length(robust_variables) * 2L) {
  stop("The community descriptive table must contain eight rows.")
}

# ------------------------------------------------------------
# 10. Community centroids and profile contrasts
# ------------------------------------------------------------

community_centroids <- stress_louvain %>%
  dplyr::group_by(Stress_Cluster_integer) %>%
  dplyr::summarise(
    n = dplyr::n(),
    dplyr::across(dplyr::all_of(robust_variables), mean),
    Mean_PC1 = mean(PC1),
    Median_PC1 = stats::median(PC1),
    Mean_PC2 = mean(PC2),
    Median_PC2 = stats::median(PC2),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Stress_Cluster_integer)

community_medians <- stress_louvain %>%
  dplyr::group_by(Stress_Cluster_integer) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(robust_variables), stats::median),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Stress_Cluster_integer)

if (nrow(community_centroids) != 2L || nrow(community_medians) != 2L) {
  stop("Community centroids or medians do not contain two rows.")
}

mean_centroid_matrix <- as.matrix(community_centroids[robust_variables])
median_profile_matrix <- as.matrix(community_medians[robust_variables])
storage.mode(mean_centroid_matrix) <- "double"
storage.mode(median_profile_matrix) <- "double"

if (any(!is.finite(mean_centroid_matrix)) || any(!is.finite(median_profile_matrix))) {
  stop("At least one community profile statistic is non-finite.")
}

mean_difference_2_minus_1 <- mean_centroid_matrix[2, ] - mean_centroid_matrix[1, ]
median_difference_2_minus_1 <- median_profile_matrix[2, ] - median_profile_matrix[1, ]

profile_contrasts <- tibble::tibble(
  Variable_code = robust_variables,
  Variable = unname(variable_labels[robust_variables]),
  Mean_Community_1 = as.numeric(mean_centroid_matrix[1, ]),
  Mean_Community_2 = as.numeric(mean_centroid_matrix[2, ]),
  Mean_difference_2_minus_1 = as.numeric(mean_difference_2_minus_1),
  Median_Community_1 = as.numeric(median_profile_matrix[1, ]),
  Median_Community_2 = as.numeric(median_profile_matrix[2, ]),
  Median_difference_2_minus_1 = as.numeric(median_difference_2_minus_1)
)

mean_profile_scores <- rowMeans(mean_centroid_matrix)
centroid_distances_to_control_origin <- sqrt(rowSums(mean_centroid_matrix^2))

community_centroids$Mean_profile_score <- mean_profile_scores
community_centroids$Euclidean_distance_to_Control_origin <- centroid_distances_to_control_origin

centroid_distance <- sqrt(sum((mean_centroid_matrix[2, ] - mean_centroid_matrix[1, ])^2))

if (!is.finite(centroid_distance) || centroid_distance <= 0) {
  stop("The Euclidean distance between community centroids is invalid.")
}

# ------------------------------------------------------------
# 11. Descriptive Wilcoxon and rank-biserial comparisons
# ------------------------------------------------------------

comparison_results_list <- lapply(
  robust_variables,
  function(variable_i) {
    community_1_values <- stress_louvain[[variable_i]][
      stress_louvain$Stress_Cluster_integer == 1L
    ]
    community_2_values <- stress_louvain[[variable_i]][
      stress_louvain$Stress_Cluster_integer == 2L
    ]

    n_community_1 <- length(community_1_values)
    n_community_2 <- length(community_2_values)

    wilcoxon_result <- stats::wilcox.test(
      x = community_1_values,
      y = community_2_values,
      alternative = "two.sided",
      exact = FALSE,
      correct = FALSE
    )

    pooled_values <- c(community_1_values, community_2_values)
    pooled_ranks <- rank(pooled_values, ties.method = "average")
    rank_sum_community_1 <- sum(pooled_ranks[seq_len(n_community_1)])
    u_community_1 <- rank_sum_community_1 - n_community_1 * (n_community_1 + 1) / 2
    rank_biserial <- 2 * u_community_1 / (n_community_1 * n_community_2) - 1

    if (
      !is.finite(rank_biserial) ||
      rank_biserial < -1 - numeric_tolerance ||
      rank_biserial > 1 + numeric_tolerance
    ) {
      stop("An invalid rank-biserial effect size was generated for ", variable_i, ".")
    }

    rank_biserial <- max(-1, min(1, rank_biserial))

    tibble::tibble(
      Variable_code = variable_i,
      Variable = unname(variable_labels[[variable_i]]),
      Group_1 = "Community 1",
      Group_2 = "Community 2",
      n_Community_1 = n_community_1,
      n_Community_2 = n_community_2,
      Median_Community_1 = stats::median(community_1_values),
      Median_Community_2 = stats::median(community_2_values),
      Median_difference_2_minus_1 =
        stats::median(community_2_values) - stats::median(community_1_values),
      Wilcoxon_W = as.numeric(wilcoxon_result$statistic),
      P_raw = as.numeric(wilcoxon_result$p.value),
      Rank_biserial_Community1_vs_Community2 = as.numeric(rank_biserial),
      Effect_direction = if (
        rank_biserial < 0
      ) {
        "Community 1 < Community 2"
      } else if (
        rank_biserial > 0
      ) {
        "Community 1 > Community 2"
      } else {
        "No directional difference"
      },
      Effect_magnitude = rank_biserial_magnitude(rank_biserial)
    )
  }
)

community_comparisons <- dplyr::bind_rows(comparison_results_list) %>%
  dplyr::mutate(
    P_Holm = stats::p.adjust(P_raw, method = "holm"),
    P_raw_formatted = vapply(P_raw, format_p_value, character(1)),
    P_Holm_formatted = vapply(P_Holm, format_p_value, character(1)),
    Interpretation = paste(
      "Descriptive characterisation only;",
      "communities were derived using this variable"
    )
  ) %>%
  dplyr::arrange(match(Variable_code, robust_variables))

comparison_numeric_columns <- c(
  "Wilcoxon_W",
  "P_raw",
  "P_Holm",
  "Rank_biserial_Community1_vs_Community2"
)

if (
  nrow(community_comparisons) != length(robust_variables) ||
  any(!is.finite(as.matrix(community_comparisons[comparison_numeric_columns])))
) {
  stop("The descriptive community-comparison table is invalid.")
}

# ------------------------------------------------------------
# 12. Assign operational post-stress labels after characterisation
# ------------------------------------------------------------

number_higher_mean_variables <- sum(mean_difference_2_minus_1 > 0)
number_higher_median_variables <- sum(median_difference_2_minus_1 > 0)
mean_profile_difference <- mean_profile_scores[2] - mean_profile_scores[1]
pc1_mean_difference <- community_sizes$Mean_PC1[2] - community_sizes$Mean_PC1[1]

if (
  number_higher_mean_variables < 3L ||
  mean_profile_difference <= 0 ||
  pc1_mean_difference <= 0
) {
  stop(
    "The community ordering does not support an operational lower- versus higher-exploration interpretation."
  )
}

community_label_map <- tibble::tibble(
  Stress_Cluster_integer = c(1L, 2L),
  Stress_Community = c("Community_1", "Community_2"),
  PostStress_Phenotype = c("Susceptible-like", "Resilient-like"),
  Operational_basis = c(
    "Lower mean Stress PC1 and lower overall post-stress exploratory profile",
    "Higher mean Stress PC1 and higher overall post-stress exploratory profile"
  ),
  Biological_validation_status = c(
    "Not independently biologically validated",
    "Not independently biologically validated"
  )
)

stress_phenotypes <- stress_louvain %>%
  dplyr::select(-Stress_Community) %>%
  dplyr::left_join(
    community_label_map,
    by = "Stress_Cluster_integer"
  ) %>%
  dplyr::mutate(
    Stress_Cluster = factor(Stress_Cluster_integer, levels = c(1L, 2L)),
    Stress_Community = factor(
      Stress_Community,
      levels = c("Community_1", "Community_2")
    ),
    PostStress_Phenotype = factor(
      PostStress_Phenotype,
      levels = phenotype_levels
    )
  )

if (
  nrow(stress_phenotypes) != expected_stress_n ||
  anyNA(stress_phenotypes$PostStress_Phenotype) ||
  !identical(as.integer(stress_phenotypes$Stress_Cluster), stress_phenotypes$Stress_Cluster_integer)
) {
  stop("Operational phenotype labels could not be assigned consistently.")
}

community_sizes <- community_sizes %>%
  dplyr::left_join(
    community_label_map[
      c("Stress_Cluster_integer", "Stress_Community", "PostStress_Phenotype")
    ],
    by = "Stress_Cluster_integer"
  ) %>%
  dplyr::mutate(
    PostStress_Phenotype = factor(PostStress_Phenotype, levels = phenotype_levels)
  )

community_centroids <- community_centroids %>%
  dplyr::left_join(
    community_label_map[c("Stress_Cluster_integer", "PostStress_Phenotype")],
    by = "Stress_Cluster_integer"
  ) %>%
  dplyr::mutate(
    PostStress_Phenotype = factor(PostStress_Phenotype, levels = phenotype_levels)
  )

descriptive_statistics <- descriptive_statistics %>%
  dplyr::left_join(
    community_label_map[c("Stress_Cluster_integer", "PostStress_Phenotype")],
    by = "Stress_Cluster_integer"
  ) %>%
  dplyr::mutate(
    PostStress_Phenotype = factor(PostStress_Phenotype, levels = phenotype_levels)
  )

# ------------------------------------------------------------
# 13. Descriptively map de novo Stress phenotypes to the
#     Control-derived reference profiles from Objective 2
#
# NOTE:
#   Normative_* and Archetype_* field names are retained because they
#   are the definitive corrected Script 016 schema. In interpretation,
#   these are referred to as Control-derived reference profiles.
# ------------------------------------------------------------

required_projection_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Analysis_ID",
  "Treatment",
  robust_variables,
  "Normative_Profile",
  "Minimum_Archetype_Distance",
  "Assignment_Margin",
  "Distance_Reference_Method"
)

missing_projection_columns <- setdiff(
  required_projection_columns,
  names(normative_projection)
)

if (length(missing_projection_columns) > 0L) {
  stop(
    "The corrected Objective 2 reference-profile projection is missing:\n",
    paste(missing_projection_columns, collapse = ", ")
  )
}

stress_normative_projection <- normative_projection %>%
  dplyr::filter(
    as.character(Treatment) == "Stress"
  ) %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Analysis_ID = trimws(as.character(Analysis_ID)),
    Treatment = as.character(Treatment),
    Normative_Profile = factor(
      as.character(Normative_Profile),
      levels = c(
        "Higher-exploration",
        "Lower-exploration"
      )
    )
  )

expected_projection_keys <- paste(
  stress_normative_projection$Author,
  stress_normative_projection$ID,
  sep = " || "
)

expected_analysis_ids <- paste(
  stress_normative_projection$Treatment,
  stress_normative_projection$Author,
  stress_normative_projection$ID,
  sep = "_"
)

if (
  nrow(stress_normative_projection) != expected_stress_n ||
  anyNA(
    stress_normative_projection[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Analysis_ID",
        "Treatment",
        "Normative_Profile"
      )
    ]
  ) ||
  anyDuplicated(stress_normative_projection$Traceability_ID) > 0L ||
  !identical(
    stress_normative_projection$Traceability_ID,
    expected_projection_keys
  ) ||
  !identical(
    stress_normative_projection$Analysis_ID,
    expected_analysis_ids
  ) ||
  !setequal(
    stress_normative_projection$Traceability_ID,
    stress_phenotypes$Traceability_ID
  )
) {
  stop(
    "The corrected Objective 2 Stress projection cannot be aligned ",
    "with Objective 3 by Author + ID."
  )
}

if (
  any(
    !grepl(
      "corrected Objective 1",
      as.character(
        stress_normative_projection$Distance_Reference_Method
      ),
      fixed = TRUE
    )
  )
) {
  stop(
    "The Objective 2 Stress projection does not document the corrected ",
    "Objective 1 centroid reference."
  )
}

stress_normative_projection <- stress_normative_projection[
  match(
    stress_phenotypes$Traceability_ID,
    stress_normative_projection$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    stress_normative_projection$Traceability_ID,
    stress_phenotypes$Traceability_ID
  ) ||
  !identical(
    stress_normative_projection$Author,
    stress_phenotypes$Author
  ) ||
  !identical(
    stress_normative_projection$ID,
    stress_phenotypes$ID
  )
) {
  stop(
    "The Objective 2 projection could not be aligned exactly by Author + ID."
  )
}

projection_score_differences <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        stress_normative_projection[[variable_i]] -
          stress_phenotypes[[variable_i]]
      )
    )
  },
  numeric(1)
)

maximum_projection_score_difference <- max(
  projection_score_differences
)

if (
  !is.finite(maximum_projection_score_difference) ||
  maximum_projection_score_difference > numeric_tolerance
) {
  stop(
    "Robust behavioural scores differ between the corrected Objective 2 ",
    "projection and Objective 3 dataset."
  )
}

stress_phenotypes <- stress_phenotypes %>%
  dplyr::mutate(
    Normative_Profile =
      stress_normative_projection$Normative_Profile,
    Minimum_Archetype_Distance =
      as.numeric(
        stress_normative_projection$Minimum_Archetype_Distance
      ),
    Normative_Assignment_Margin =
      as.numeric(
        stress_normative_projection$Assignment_Margin
      )
  )

if (
  any(!is.finite(stress_phenotypes$Minimum_Archetype_Distance)) ||
  any(!is.finite(stress_phenotypes$Normative_Assignment_Margin))
) {
  stop(
    "The inherited Objective 2 reference-profile distances or margins ",
    "contain non-finite values."
  )
}

normative_mapping_counts <- stress_phenotypes %>%
  dplyr::count(
    PostStress_Phenotype,
    Normative_Profile,
    name = "n",
    .drop = FALSE
  ) %>%
  dplyr::group_by(
    PostStress_Phenotype
  ) %>%
  dplyr::mutate(
    Percentage_within_poststress_phenotype =
      100 *
      n /
      sum(n)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    Normative_Profile
  ) %>%
  dplyr::mutate(
    Percentage_within_normative_profile =
      100 *
      n /
      sum(n)
  ) %>%
  dplyr::ungroup()

normative_profile_binary <- ifelse(
  stress_phenotypes$Normative_Profile ==
    "Lower-exploration",
  1L,
  2L
)

community_normative_ari <- adjusted_rand_index(
  stress_phenotypes$Stress_Cluster_integer,
  normative_profile_binary
)

conceptual_match <- (
  stress_phenotypes$PostStress_Phenotype ==
    "Susceptible-like" &
    stress_phenotypes$Normative_Profile ==
    "Lower-exploration"
) | (
  stress_phenotypes$PostStress_Phenotype ==
    "Resilient-like" &
    stress_phenotypes$Normative_Profile ==
    "Higher-exploration"
)

conceptual_match_rate <- mean(
  conceptual_match
)

normative_mapping_summary <- tibble::tibble(
  Stress_n = expected_stress_n,
  Community_reference_profile_ARI =
    community_normative_ari,
  Conceptual_direction_match_n =
    sum(conceptual_match),
  Conceptual_direction_match_rate =
    conceptual_match_rate,
  Interpretation = paste(
    "Descriptive correspondence only;",
    "the de novo post-stress phenotypes and Control-derived reference",
    "profiles are not equivalent classifications"
  )
)

# ------------------------------------------------------------
# 14. PCA figure coloured by post-stress phenotype
# ------------------------------------------------------------

pca_plot_data <- stress_phenotypes %>%
  dplyr::transmute(
    Author,
    ID,
    Traceability_ID,
    PC1,
    PC2,
    PostStress_Phenotype
  )

pca_ellipse_data <- make_normal_data_ellipse(
  data = pca_plot_data,
  group_variable = "PostStress_Phenotype",
  x_variable = "PC1",
  y_variable = "PC2",
  level = ellipse_level
) %>%
  dplyr::mutate(
    PostStress_Phenotype = factor(
      PostStress_Phenotype,
      levels = phenotype_levels
    )
  )

pca_centroids <- pca_plot_data %>%
  dplyr::group_by(PostStress_Phenotype) %>%
  dplyr::summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  )

if (
  nrow(pca_centroids) != 2L ||
  any(!is.finite(pca_centroids$PC1)) ||
  any(!is.finite(pca_centroids$PC2))
) {
  stop("The PCA phenotype-centroid table is invalid.")
}

required_variance_columns <- c("Component", "Percentage_of_variance")

if (length(setdiff(required_variance_columns, names(pca_variance))) > 0L) {
  stop("The PCA variance table is missing required columns.")
}

pc1_percent <- pca_variance$Percentage_of_variance[
  pca_variance$Component == "PC1"
]
pc2_percent <- pca_variance$Percentage_of_variance[
  pca_variance$Component == "PC2"
]

if (
  length(pc1_percent) != 1L ||
  length(pc2_percent) != 1L ||
  !is.finite(pc1_percent) ||
  !is.finite(pc2_percent)
) {
  stop("PC1 or PC2 explained variance could not be recovered.")
}

pca_plot <- ggplot2::ggplot(
  pca_plot_data,
  ggplot2::aes(x = PC1, y = PC2)
) +
  ggplot2::geom_polygon(
    data = pca_ellipse_data,
    ggplot2::aes(
      fill = PostStress_Phenotype,
      group = PostStress_Phenotype
    ),
    alpha = 0.16,
    colour = NA
  ) +
  ggplot2::geom_path(
    data = pca_ellipse_data,
    ggplot2::aes(
      colour = PostStress_Phenotype,
      group = PostStress_Phenotype
    ),
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = PostStress_Phenotype),
    shape = 21,
    size = 2.5,
    stroke = 0.35,
    colour = "black",
    alpha = 0.82
  ) +
  ggplot2::geom_point(
    data = pca_centroids,
    ggplot2::aes(fill = PostStress_Phenotype),
    shape = 21,
    size = 5.0,
    stroke = 0.9,
    colour = "black"
  ) +
  ggplot2::scale_fill_manual(values = phenotype_colours, drop = FALSE) +
  ggplot2::scale_colour_manual(values = phenotype_colours, drop = FALSE) +
  ggplot2::labs(
    x = paste0("PC1 (", sprintf("%.1f", pc1_percent), "%)"),
    y = paste0("PC2 (", sprintf("%.1f", pc2_percent), "%)"),
    fill = NULL,
    colour = NULL
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    legend.position = "right",
    legend.key = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    colour = "none",
    fill = ggplot2::guide_legend(
      override.aes = list(
        shape = 21,
        size = 4,
        colour = "black",
        alpha = 1
      )
    )
  )

# ------------------------------------------------------------
# 15. Mean behavioural-profile figure
# ------------------------------------------------------------

profile_data <- community_centroids %>%
  dplyr::select(
    Stress_Cluster_integer,
    PostStress_Phenotype,
    dplyr::all_of(robust_variables)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(robust_variables),
    names_to = "Variable_code",
    values_to = "Mean_robust_score"
  ) %>%
  dplyr::mutate(
    Variable = factor(
      Variable_code,
      levels = robust_variables,
      labels = unname(variable_labels[robust_variables])
    ),
    PostStress_Phenotype = factor(
      PostStress_Phenotype,
      levels = phenotype_levels
    )
  )

if (
  nrow(profile_data) != expected_number_of_communities * length(robust_variables) ||
  any(!is.finite(profile_data$Mean_robust_score))
) {
  stop("The behavioural-profile figure data are incomplete.")
}

profile_plot <- ggplot2::ggplot(
  profile_data,
  ggplot2::aes(
    x = Variable,
    y = Mean_robust_score,
    group = PostStress_Phenotype,
    colour = PostStress_Phenotype,
    fill = PostStress_Phenotype
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    colour = "grey45"
  ) +
  ggplot2::geom_line(linewidth = 1.15) +
  ggplot2::geom_point(
    shape = 21,
    size = 4.0,
    stroke = 0.7,
    colour = "black"
  ) +
  ggplot2::scale_colour_manual(values = phenotype_colours, drop = FALSE) +
  ggplot2::scale_fill_manual(values = phenotype_colours, drop = FALSE) +
  ggplot2::labs(
    x = NULL,
    y = "Mean Control-referenced robust score",
    colour = NULL,
    fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      colour = "black",
      angle = 20,
      hjust = 1
    ),
    axis.text.y = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    legend.position = "right",
    legend.key = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    colour = "none",
    fill = ggplot2::guide_legend(
      override.aes = list(
        shape = 21,
        size = 4,
        colour = "black"
      )
    )
  )

# ------------------------------------------------------------
# 16. Export figures to the definitive Figures directory
# ------------------------------------------------------------

print(pca_plot)
print(profile_plot)

ggplot2::ggsave(
  filename = pca_pdf_output,
  plot = pca_plot,
  width = 7.2,
  height = 6.0,
  units = "in"
)

ggplot2::ggsave(
  filename = pca_tiff_output,
  plot = pca_plot,
  width = 7.2,
  height = 6.0,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggplot2::ggsave(
  filename = profile_pdf_output,
  plot = profile_plot,
  width = 8.2,
  height = 5.5,
  units = "in"
)

ggplot2::ggsave(
  filename = profile_tiff_output,
  plot = profile_plot,
  width = 8.2,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------------------
# 17. Analysis configuration and interpretation notes
# ------------------------------------------------------------

analysis_configuration <- tibble::tibble(
  Parameter = c(
    "Analytical population",
    "Stress fish",
    "Traceability",
    "Authors/cohorts",
    "Robust variables",
    "Normalisation",
    "Selected k",
    "Selected k plateau",
    "Reference communities",
    "Reference community sizes",
    "Weighted modularity",
    "Seed exact-recovery rate",
    "Bootstrap mean ARI",
    "Bootstrap median ARI",
    "Bootstrap two-community recovery rate",
    "Operational labels",
    "Figure palette"
  ),
  Value = c(
    "Stress only",
    as.character(expected_stress_n),
    expected_traceability,
    paste(population_authors, collapse = ", "),
    paste(robust_variables, collapse = "; "),
    expected_normalisation,
    as.character(selected_k),
    paste0(
      selected_plateau_start_k,
      "-",
      selected_plateau_end_k,
      " (",
      selected_plateau_length,
      " values)"
    ),
    as.character(selected_number_of_communities),
    paste(community_sizes$n, collapse = " and "),
    format(selected_weighted_modularity, digits = 6),
    format(seed_stability_summary$Exact_reference_rate, digits = 6),
    format(bootstrap_summary$Mean_ARI_Unique_Fish, digits = 6),
    format(bootstrap_summary$Median_ARI_Unique_Fish, digits = 6),
    format(
      bootstrap_summary$Reference_community_count_recovery_rate,
      digits = 6
    ),
    paste(phenotype_levels, collapse = "; "),
    paste0(
      "Susceptible-like ",
      phenotype_colours["Susceptible-like"],
      "; Resilient-like ",
      phenotype_colours["Resilient-like"]
    )
  )
)

analysis_notes <- tibble::tibble(
  Note = c(
    paste0(
      "The Stress communities were identified de novo using the same four ",
      "behavioural variables characterised in this script. Wilcoxon p-values ",
      "and rank-biserial correlations describe the resulting partition and ",
      "are not independent confirmatory tests."
    ),
    paste0(
      "Community 1 and Community 2 were numbered in Script 024 from lower ",
      "to higher mean Stress PC1. PC1 had positive loadings for all four ",
      "variables and therefore represented a general post-stress exploratory gradient."
    ),
    paste0(
      "Susceptible-like and resilient-like are operational labels applied ",
      "only to UCS-exposed fish. The suffix '-like' is retained because the ",
      "classification has not yet been independently validated biologically."
    ),
    paste0(
      "The Control-derived reference-profile mapping from Objective 2 is used only to ",
      "describe correspondence between the de novo Stress communities and ",
      "the fixed Control-derived reference architecture. It did not contribute to the clustering."
    ),
    paste0(
      "The bootstrap results support a dominant bipartite backbone but also ",
      "show that a three-community subdivision can emerge under sample ",
      "perturbation. The two reference communities should therefore be ",
      "treated as organised regions within a behavioural continuum, not rigid types."
    ),
    paste0(
      "The 95% ellipses in the PCA figure are normal-theory data ellipses. ",
      "They are descriptive regions and are not confidence intervals."
    )
  )
)

# ------------------------------------------------------------
# 18. Final individual classification and results object
# ------------------------------------------------------------

individual_classification <- stress_phenotypes %>%
  dplyr::transmute(
    Author,
    ID,
    Traceability_ID,
    Treatment,
    Stress_Cluster,
    Stress_Community,
    PostStress_Phenotype,
    Normative_Profile,
    PC1,
    PC2,
    dplyr::across(dplyr::all_of(robust_variables)),
    Minimum_Archetype_Distance,
    Normative_Assignment_Margin
  )

if (
  nrow(individual_classification) != expected_stress_n ||
  anyNA(individual_classification$PostStress_Phenotype)
) {
  stop("The final individual-classification table is incomplete.")
}

results_object <- list(
  Objective = "Objective 3",
  Scientific_question =
    selected_configuration$Scientific_question,
  Operational_question =
    selected_configuration$Operational_question,
  Analytical_population = "Stress only",
  Stress_n = expected_stress_n,
  Authors = population_authors,
  Traceability = expected_traceability,
  Normalisation = expected_normalisation,
  Robust_variables = robust_variables,
  Maximum_upstream_recalculation_difference =
    maximum_upstream_recalculation_difference,
  Selected_k = selected_k,
  Selected_plateau = c(selected_plateau_start_k, selected_plateau_end_k),
  Selected_plateau_length = selected_plateau_length,
  Selected_weighted_modularity = selected_weighted_modularity,
  Stability_seed_summary = seed_stability_summary,
  Stability_bootstrap_summary = bootstrap_summary,
  Stability_bootstrap_cluster_counts = bootstrap_cluster_count_distribution,
  Community_sizes = community_sizes,
  Community_centroids = community_centroids,
  Community_medians = community_medians,
  Profile_contrasts = profile_contrasts,
  Community_descriptive_statistics = descriptive_statistics,
  Community_descriptive_comparisons = community_comparisons,
  Community_centroid_distance = centroid_distance,
  Operational_label_map = community_label_map,
  Individual_classification = individual_classification,
  Normative_mapping_counts = normative_mapping_counts,
  Normative_mapping_summary = normative_mapping_summary,
  PCA_plot_data = pca_plot_data,
  PCA_ellipse_data = pca_ellipse_data,
  Profile_plot_data = profile_data,
  Source_Stress_Louvain = stress_louvain_input,
  Source_selected_configuration = selected_configuration_input,
  Source_selected_membership = selected_membership_input,
  Source_stability_results = stability_results_input,
  Source_PCA_results = pca_results_input,
  Source_normative_projection = normative_projection_input,
  Interpretive_constraint = paste(
    "The two communities represent the stability-assessed dominant",
    "bipartite organisation within the post-stress exploratory continuum.",
    "Algorithmic stability is very high, whereas sampling stability is",
    "more moderate and permits finer subdivision. Susceptible-like and",
    "resilient-like are operational behavioural labels and are not",
    "independent biological diagnoses."
  )
)

# ------------------------------------------------------------
# 19. Export tables and R objects
# ------------------------------------------------------------

openxlsx::write.xlsx(
  x = list(
    Community_sizes = community_sizes,
    Community_centroids = community_centroids,
    Community_medians = community_medians,
    Profile_contrasts = profile_contrasts,
    Descriptive_statistics = descriptive_statistics,
    Descriptive_comparisons = community_comparisons,
    Operational_labels = community_label_map,
    Normative_mapping = normative_mapping_counts,
    Normative_summary = normative_mapping_summary,
    Individual_classification = individual_classification,
    Stability_seed = seed_stability_summary,
    Stability_bootstrap = bootstrap_summary,
    Bootstrap_cluster_counts = bootstrap_cluster_count_distribution,
    Analysis_configuration = analysis_configuration,
    Interpretation_notes = analysis_notes
  ),
  file = workbook_output,
  overwrite = TRUE
)

utils::write.csv(community_sizes, community_sizes_csv, row.names = FALSE, na = "")
utils::write.csv(community_centroids, community_centroids_csv, row.names = FALSE, na = "")
utils::write.csv(
  descriptive_statistics,
  descriptive_statistics_csv,
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  community_comparisons,
  community_comparisons_csv,
  row.names = FALSE,
  na = ""
)
utils::write.csv(normative_mapping_counts, normative_mapping_csv, row.names = FALSE, na = "")
utils::write.csv(
  individual_classification,
  individual_classification_csv,
  row.names = FALSE,
  na = ""
)
utils::write.csv(community_label_map, label_definition_csv, row.names = FALSE, na = "")

saveRDS(stress_phenotypes, phenotype_data_output)
saveRDS(community_sizes, community_sizes_output)
saveRDS(community_centroids, community_centroids_output)
saveRDS(descriptive_statistics, descriptive_statistics_output)
saveRDS(community_comparisons, community_comparisons_output)
saveRDS(
  list(Counts = normative_mapping_counts, Summary = normative_mapping_summary),
  normative_mapping_output
)
saveRDS(pca_plot, pca_plot_output)
saveRDS(profile_plot, profile_plot_output)
saveRDS(results_object, results_object_output)

capture.output(utils::sessionInfo(), file = session_info_output)

# ------------------------------------------------------------
# 20. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  phenotype_data_output,
  pca_pdf_output,
  pca_tiff_output,
  profile_pdf_output,
  profile_tiff_output,
  workbook_output,
  community_sizes_csv,
  community_centroids_csv,
  descriptive_statistics_csv,
  community_comparisons_csv,
  normative_mapping_csv,
  individual_classification_csv,
  label_definition_csv,
  results_object_output,
  community_sizes_output,
  community_centroids_output,
  descriptive_statistics_output,
  community_comparisons_output,
  normative_mapping_output,
  pca_plot_output,
  profile_plot_output,
  session_info_output
)

missing_output_files <- expected_output_files[!file.exists(expected_output_files)]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

output_file_information <- file.info(expected_output_files)

if (
  any(is.na(output_file_information$size)) ||
  any(output_file_information$size <= 0)
) {
  stop("At least one expected output file is empty or inaccessible.")
}

# ------------------------------------------------------------
# 21. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 3 - STRESS COMMUNITY CHARACTERISATION\n")
cat("============================================================\n")

cat(
  "\nScientific question:\n",
  results_object$Scientific_question,
  "\n",
  sep = ""
)

cat(
  "\nAnalytical population: Stress only (n = ",
  expected_stress_n,
  ")\n",
  sep = ""
)

cat(
  "Traceability: ",
  expected_traceability,
  "\n",
  sep = ""
)

cat(
  "Authors/cohorts retained: ",
  paste(
    population_authors,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Normalisation: ",
  expected_normalisation,
  "\n",
  sep = ""
)

cat(
  "Maximum upstream robust-score recalculation difference: ",
  format(
    maximum_upstream_recalculation_difference,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Analytical variables: ",
  paste(robust_variables, collapse = "; "),
  "\n",
  sep = ""
)

cat(
  "Selected k inherited from corrected Script 024: ",
  selected_k,
  "\n",
  sep = ""
)

cat(
  "Selected k plateau: ",
  selected_plateau_start_k,
  "-",
  selected_plateau_end_k,
  " (",
  selected_plateau_length,
  " values).\n",
  sep = ""
)

cat(
  "Weighted modularity: ",
  sprintf("%.4f", selected_weighted_modularity),
  "\n",
  sep = ""
)

cat("\nStability evidence inherited from corrected Script 025:\n")

cat(
  "Exact reference recovery across seeds: ",
  sprintf("%.1f", 100 * seed_stability_summary$Exact_reference_rate),
  "%\n",
  sep = ""
)

cat(
  "Mean bootstrap ARI on unique fish: ",
  sprintf("%.3f", bootstrap_summary$Mean_ARI_Unique_Fish),
  "\n",
  sep = ""
)

cat(
  "Median bootstrap ARI on unique fish: ",
  sprintf("%.3f", bootstrap_summary$Median_ARI_Unique_Fish),
  "\n",
  sep = ""
)

cat(
  "Two-community bootstrap recovery: ",
  sprintf(
    "%.1f",
    100 * bootstrap_summary$Reference_community_count_recovery_rate
  ),
  "%\n",
  sep = ""
)

cat("\nOperational post-stress phenotypes:\n")
print(community_sizes, n = Inf, width = Inf)

cat("\nMean Control-referenced community centroids:\n")
print(community_centroids, n = Inf, width = Inf)

cat(
  "\nEuclidean distance between community centroids: ",
  sprintf("%.4f", centroid_distance),
  "\n",
  sep = ""
)

cat("\nVariable-wise profile contrasts:\n")
print(profile_contrasts, n = Inf, width = Inf)

cat(
  "\nVariables with higher mean values in the resilient-like community: ",
  number_higher_mean_variables,
  " of ",
  length(robust_variables),
  ".\n",
  sep = ""
)

cat(
  "Variables with higher median values in the resilient-like community: ",
  number_higher_median_variables,
  " of ",
  length(robust_variables),
  ".\n",
  sep = ""
)

cat("\nDescriptive Wilcoxon/rank-biserial characterisation:\n")
print(community_comparisons, n = Inf, width = Inf)

cat("\nObjective 2 Control-derived reference-profile mapping by Objective 3 phenotype:\n")
print(normative_mapping_counts, n = Inf, width = Inf)

cat(
  "\nARI between the de novo Stress partition and normative-profile assignment: ",
  sprintf("%.3f", community_normative_ari),
  "\n",
  sep = ""
)

cat(
  "Directional conceptual match rate: ",
  sprintf("%.1f", 100 * conceptual_match_rate),
  "%.\n",
  sep = ""
)

cat(
  "\nInterpretive constraint: susceptible-like and resilient-like are ",
  "operational post-stress behavioural labels. The communities were ",
  "derived from the four variables characterised here, so their ",
  "comparisons are descriptive rather than independent validation. ",
  "The bootstrap supports a dominant bipartite backbone while also ",
  "indicating possible finer subdivision.\n",
  sep = ""
)

cat("\nFigures saved as:\n")
cat(
  paste0(
    "- ",
    normalizePath(
      c(
        pca_pdf_output,
        pca_tiff_output,
        profile_pdf_output,
        profile_tiff_output
      ),
      winslash = "/",
      mustWork = FALSE
    ),
    collapse = "\n"
  ),
  "\n",
  sep = ""
)

cat("\nDefinitive phenotype dataset saved as:\n")
cat(
  normalizePath(
    phenotype_data_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)

cat("============================================================\n")
