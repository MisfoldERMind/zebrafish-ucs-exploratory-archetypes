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