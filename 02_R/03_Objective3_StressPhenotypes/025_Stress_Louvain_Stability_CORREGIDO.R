# ============================================================
# 025_Stress_Louvain_Stability.R
#
# Objective 3
# Stability and reproducibility of the candidate de novo Louvain
# architecture identified within the UCS-exposed population
#
# Scientific question:
#   What internal behavioural structure emerges within
#   the UCS-exposed population?
#
# Analytical role:
#   Evaluate whether the candidate Stress partition selected in
#   024_Stress_Louvain_CORREGIDO_v2.R is reproducible.
#
# Stability design:
#   1. Algorithmic stability:
#      Repeat weighted Louvain on the fixed selected graph across
#      500 deterministic seeds.
#
#   2. Sampling stability:
#      Perform 1,000 case-bootstrap iterations with replacement.
#      Each iteration contains 89 rows, reconstructs the same
#      weighted undirected union-kNN graph at the selected k, and
#      reruns weighted Louvain.
#
# Primary bootstrap metric:
#   Adjusted Rand Index (ARI) calculated once per unique original
#   fish represented in each bootstrap sample. When a fish is
#   sampled more than once, its bootstrap community is determined
#   by deterministic majority consensus across its copies.
#
# Secondary bootstrap metric:
#   ARI across all 89 bootstrap rows, including repeated copies.
#
# Important:
#   - The selected k is inherited from Script 024 and is not
#     re-optimised here.
#   - The four-dimensional Control-referenced robust coordinates
#     remain fixed.
#   - This script evaluates reproducibility; it does not assign
#     resilient-like or susceptible-like labels.
#
# Required upstream script:
#   024_Stress_Louvain_CORREGIDO_v2.R
# ============================================================

script_version <- "025_Stress_Louvain_Stability_CORREGIDO | 2026-08-07 | Author+ID audit"
cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 1. Project directory
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

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "igraph",
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
  library(igraph)
  library(openxlsx)
})

# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(
  project_root,
  "01_Data",
  "Processed"
)

objects_dir <- file.path(
  project_root,
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  project_root,
  "03_Results",
  "Tables"
)

if (!dir.exists(processed_dir)) {
  stop(
    "Processed-data directory not found:\n",
    processed_dir
  )
}

for (directory_i in c(objects_dir, tables_dir)) {
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

selected_graph_input <- file.path(
  objects_dir,
  "stress_knn_graph.rds"
)

selected_partition_input <- file.path(
  objects_dir,
  "stress_louvain_partition.rds"
)

selected_configuration_input <- file.path(
  objects_dir,
  "objective3_selected_louvain_configuration.rds"
)

selected_membership_input <- file.path(
  objects_dir,
  "objective3_selected_louvain_membership.rds"
)

louvain_results_input <- file.path(
  objects_dir,
  "objective3_stress_louvain_results.rds"
)

required_input_files <- c(
  stress_louvain_input,
  selected_graph_input,
  selected_partition_input,
  selected_configuration_input,
  selected_membership_input,
  louvain_results_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive Script 024 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 024_Stress_Louvain_CORREGIDO_v2.R first."
  )
}

seed_iterations_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_SeedStability_Iterations.csv"
)

seed_summary_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_SeedStability_Summary.csv"
)

seed_cluster_counts_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_SeedStability_ClusterCounts.csv"
)

bootstrap_iterations_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_BootstrapIterations.csv"
)

bootstrap_summary_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_BootstrapSummary.csv"
)

bootstrap_cluster_counts_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_BootstrapClusterCounts.csv"
)

reference_validation_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_Stability_ReferenceValidation.csv"
)

configuration_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_Stability_Configuration.csv"
)

analysis_notes_csv <- file.path(
  tables_dir,
  "Objective3_Louvain_Stability_AnalysisNotes.csv"
)

workbook_output <- file.path(
  tables_dir,
  "Objective3_Louvain_Stability.xlsx"
)

seed_memberships_output <- file.path(
  objects_dir,
  "objective3_louvain_seed_stability_memberships.rds"
)

seed_results_output <- file.path(
  objects_dir,
  "objective3_louvain_seed_stability_results.rds"
)

bootstrap_iterations_output <- file.path(
  objects_dir,
  "objective3_louvain_bootstrap_iterations.rds"
)

bootstrap_summary_output <- file.path(
  objects_dir,
  "objective3_louvain_bootstrap_summary.rds"
)

stability_results_output <- file.path(
  objects_dir,
  "objective3_louvain_stability_results.rds"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_louvain_stability_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Load definitive Script 024 objects
# ------------------------------------------------------------

stress_louvain <- readRDS(
  stress_louvain_input
)

selected_graph <- readRDS(
  selected_graph_input
)

selected_partition <- readRDS(
  selected_partition_input
)

selected_configuration <- readRDS(
  selected_configuration_input
)

selected_membership_table <- readRDS(
  selected_membership_input
)

louvain_results <- readRDS(
  louvain_results_input
)

if (!is.data.frame(stress_louvain)) {
  stop(
    basename(stress_louvain_input),
    " does not contain a data frame."
  )
}

if (!igraph::is_igraph(selected_graph)) {
  stop(
    basename(selected_graph_input),
    " does not contain an igraph object."
  )
}

if (!inherits(selected_partition, "communities")) {
  stop(
    basename(selected_partition_input),
    " does not contain an igraph communities object."
  )
}

if (!is.list(selected_configuration)) {
  stop(
    basename(selected_configuration_input),
    " does not contain the expected configuration list."
  )
}

if (!is.data.frame(selected_membership_table)) {
  stop(
    basename(selected_membership_input),
    " does not contain a data frame."
  )
}

if (!is.list(louvain_results)) {
  stop(
    basename(louvain_results_input),
    " does not contain the expected results list."
  )
}

# ------------------------------------------------------------
# 5. Scalar-validation functions
# ------------------------------------------------------------

scalar_integer <- function(
  x,
  object_name
) {
  if (
    length(x) != 1L ||
    is.na(x) ||
    !is.finite(x) ||
    x != as.integer(x)
  ) {
    stop(
      object_name,
      " must contain one finite integer."
    )
  }

  as.integer(x)
}

scalar_numeric <- function(
  x,
  object_name
) {
  if (
    length(x) != 1L ||
    is.na(x) ||
    !is.finite(x)
  ) {
    stop(
      object_name,
      " must contain one finite numeric value."
    )
  }

  as.numeric(x)
}

scalar_character <- function(
  x,
  object_name
) {
  if (
    length(x) != 1L ||
    is.na(x) ||
    !nzchar(as.character(x))
  ) {
    stop(
      object_name,
      " must contain one non-empty character value."
    )
  }

  as.character(x)
}

# ------------------------------------------------------------
# 6. Fixed analytical definitions inherited from Script 024
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"
expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

expected_number_of_variables <- length(
  robust_variables
)

expected_distance <- "Euclidean"
expected_similarity <- "1 / (1 + distance)"
expected_graph_type <- "Weighted, undirected union kNN"

required_configuration_fields <- c(
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Clustering_scope",
  "Normalisation",
  "Robust_variables",
  "Distance",
  "Similarity",
  "Graph",
  "Louvain_seed",
  "Selected_k",
  "Selected_number_of_communities",
  "Selected_weighted_modularity",
  "Selected_plateau_start_k",
  "Selected_plateau_end_k",
  "Selected_plateau_length",
  "Selected_rebuild_ARI"
)

missing_configuration_fields <- setdiff(
  required_configuration_fields,
  names(selected_configuration)
)

if (length(missing_configuration_fields) > 0L) {
  stop(
    "The selected configuration is missing required fields:\n",
    paste(missing_configuration_fields, collapse = ", ")
  )
}

selected_k <- scalar_integer(
  selected_configuration$Selected_k,
  "selected_configuration$Selected_k"
)

reference_number_of_communities <- scalar_integer(
  selected_configuration$Selected_number_of_communities,
  paste0(
    "selected_configuration$",
    "Selected_number_of_communities"
  )
)

reference_louvain_seed <- scalar_integer(
  selected_configuration$Louvain_seed,
  "selected_configuration$Louvain_seed"
)

reference_weighted_modularity <- scalar_numeric(
  selected_configuration$Selected_weighted_modularity,
  paste0(
    "selected_configuration$",
    "Selected_weighted_modularity"
  )
)

selected_plateau_start_k <- scalar_integer(
  selected_configuration$Selected_plateau_start_k,
  paste0(
    "selected_configuration$",
    "Selected_plateau_start_k"
  )
)

selected_plateau_end_k <- scalar_integer(
  selected_configuration$Selected_plateau_end_k,
  paste0(
    "selected_configuration$",
    "Selected_plateau_end_k"
  )
)

selected_plateau_length <- scalar_integer(
  selected_configuration$Selected_plateau_length,
  paste0(
    "selected_configuration$",
    "Selected_plateau_length"
  )
)

selected_rebuild_ari <- scalar_numeric(
  selected_configuration$Selected_rebuild_ARI,
  paste0(
    "selected_configuration$",
    "Selected_rebuild_ARI"
  )
)

configuration_population <- scalar_character(
  selected_configuration$Analytical_population,
  "selected_configuration$Analytical_population"
)

configuration_scope <- scalar_character(
  selected_configuration$Clustering_scope,
  "selected_configuration$Clustering_scope"
)

configuration_traceability <- scalar_character(
  selected_configuration$Traceability,
  "selected_configuration$Traceability"
)

configuration_normalisation <- scalar_character(
  selected_configuration$Normalisation,
  "selected_configuration$Normalisation"
)

configuration_distance <- scalar_character(
  selected_configuration$Distance,
  "selected_configuration$Distance"
)

configuration_similarity <- scalar_character(
  selected_configuration$Similarity,
  "selected_configuration$Similarity"
)

configuration_graph <- scalar_character(
  selected_configuration$Graph,
  "selected_configuration$Graph"
)

if (!identical(configuration_population, "Stress only")) {
  stop(
    "The selected configuration does not define Stress as the ",
    "sole analytical population."
  )
}

if (
  !grepl(
    "De novo",
    configuration_scope,
    fixed = TRUE
  )
) {
  stop(
    "The selected configuration does not describe de novo ",
    "community detection within Stress."
  )
}

if (
  !identical(
    configuration_traceability,
    expected_traceability
  )
) {
  stop(
    "The selected configuration does not preserve Author + ID ",
    "traceability."
  )
}

if (
  !identical(
    configuration_normalisation,
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
    as.integer(selected_configuration$Stress_n),
    expected_stress_n
  )
) {
  stop(
    "The selected configuration does not contain Stress n = 89."
  )
}

if (
  !identical(
    as.character(selected_configuration$Robust_variables),
    robust_variables
  )
) {
  stop(
    "The selected configuration does not use the definitive ",
    "four robust variables."
  )
}

if (!identical(configuration_distance, expected_distance)) {
  stop("The selected configuration does not use Euclidean distance.")
}

if (!identical(configuration_similarity, expected_similarity)) {
  stop(
    "The selected configuration does not use similarity ",
    "1 / (1 + distance)."
  )
}

if (!identical(configuration_graph, expected_graph_type)) {
  stop(
    "The selected configuration does not use the definitive ",
    "weighted, undirected union-kNN graph."
  )
}

if (
  selected_k < 1L ||
  selected_k >= expected_stress_n
) {
  stop("The selected k is invalid for 89 Stress fish.")
}

if (reference_number_of_communities < 2L) {
  stop(
    "The selected reference partition contains fewer than ",
    "two communities."
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
    "The selected k and plateau information are internally ",
    "inconsistent."
  )
}

if (
  abs(selected_rebuild_ari - 1) >
  1e-12
) {
  stop(
    "Script 024 did not report exact reconstruction of the ",
    "selected partition."
  )
}

# ------------------------------------------------------------
# 7. Audit Stress data and selected membership
# ------------------------------------------------------------

required_stress_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  robust_variables,
  "Stress_Cluster",
  "Stress_Community",
  "PC1",
  "PC2"
)

missing_stress_columns <- setdiff(
  required_stress_columns,
  names(stress_louvain)
)

if (length(missing_stress_columns) > 0L) {
  stop(
    "The corrected Script 024 Stress dataset is missing required columns:\n",
    paste(missing_stress_columns, collapse = ", ")
  )
}

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
  names(selected_membership_table)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The selected membership table is missing required columns:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

stress_louvain <- stress_louvain %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
  )

selected_membership_table <- selected_membership_table %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
  )

if (
  nrow(stress_louvain) != expected_stress_n ||
  nrow(selected_membership_table) != expected_stress_n
) {
  stop(
    "Both corrected Script 024 data objects must contain exactly 89 rows."
  )
}

expected_stress_traceability <- paste(
  stress_louvain$Author,
  stress_louvain$ID,
  sep = " || "
)

expected_membership_traceability <- paste(
  selected_membership_table$Author,
  selected_membership_table$ID,
  sep = " || "
)

if (
  anyNA(
    stress_louvain[
      c("Author", "ID", "Traceability_ID")
    ]
  ) ||
  any(stress_louvain$Author == "") ||
  any(stress_louvain$ID == "") ||
  any(stress_louvain$Traceability_ID == "") ||
  anyDuplicated(stress_louvain$Traceability_ID) > 0L ||
  !identical(
    stress_louvain$Traceability_ID,
    expected_stress_traceability
  )
) {
  stop(
    "The corrected stress_louvain object fails the Author + ID ",
    "traceability audit."
  )
}

if (
  anyNA(
    selected_membership_table[
      c("Author", "ID", "Traceability_ID")
    ]
  ) ||
  any(selected_membership_table$Author == "") ||
  any(selected_membership_table$ID == "") ||
  any(selected_membership_table$Traceability_ID == "") ||
  anyDuplicated(selected_membership_table$Traceability_ID) > 0L ||
  !identical(
    selected_membership_table$Traceability_ID,
    expected_membership_traceability
  )
) {
  stop(
    "The selected membership table fails the Author + ID ",
    "traceability audit."
  )
}

population_authors <- sort(
  unique(stress_louvain$Author)
)

if (
  length(population_authors) != expected_number_of_authors ||
  !setequal(
    as.character(selected_configuration$Authors),
    population_authors
  )
) {
  stop(
    "The four Authors/cohorts are not preserved consistently from ",
    "corrected Script 024."
  )
}

if (
  anyNA(stress_louvain$Treatment) ||
  !all(stress_louvain$Treatment == "Stress")
) {
  stop(
    "stress_louvain_clusters.rds must contain only Stress fish."
  )
}

if (
  anyNA(selected_membership_table$Treatment) ||
  !all(selected_membership_table$Treatment == "Stress")
) {
  stop(
    "The selected membership table must contain only Stress fish."
  )
}

if (
  !setequal(
    stress_louvain$Traceability_ID,
    selected_membership_table$Traceability_ID
  )
) {
  stop(
    "The Author + ID keys in the corrected Script 024 dataset and ",
    "selected membership table do not correspond exactly."
  )
}

selected_membership_table <- selected_membership_table[
  match(
    stress_louvain$Traceability_ID,
    selected_membership_table$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    stress_louvain$Traceability_ID,
    selected_membership_table$Traceability_ID
  ) ||
  !identical(
    stress_louvain$Author,
    selected_membership_table$Author
  ) ||
  !identical(
    stress_louvain$ID,
    selected_membership_table$ID
  )
) {
  stop(
    "The selected membership table could not be aligned exactly ",
    "by Author + ID."
  )
}

if (
  anyNA(selected_membership_table$k) ||
  any(
    as.integer(selected_membership_table$k) != selected_k
  )
) {
  stop(
    "The selected membership table does not contain the selected k ",
    "for all fish."
  )
}

non_numeric_robust_variables <- robust_variables[
  !vapply(
    stress_louvain[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_robust_variables) > 0L) {
  stop(
    "The following robust variables are not numeric:\n",
    paste(non_numeric_robust_variables, collapse = ", ")
  )
}

stress_matrix <- as.matrix(
  stress_louvain[
    ,
    robust_variables,
    drop = FALSE
  ]
)

storage.mode(stress_matrix) <- "double"
rownames(stress_matrix) <- stress_louvain$Traceability_ID
colnames(stress_matrix) <- robust_variables

if (
  !identical(
    dim(stress_matrix),
    c(
      expected_stress_n,
      expected_number_of_variables
    )
  )
) {
  stop(
    "The Stress behavioural matrix must have dimensions 89 x 4."
  )
}

if (any(!is.finite(stress_matrix))) {
  stop(
    "Missing or non-finite values were detected in the Stress ",
    "behavioural matrix."
  )
}

centered_stress_matrix <- sweep(
  stress_matrix,
  2L,
  colMeans(stress_matrix),
  FUN = "-"
)

stress_matrix_rank <- qr(
  centered_stress_matrix
)$rank

if (
  stress_matrix_rank != expected_number_of_variables
) {
  stop(
    "The centred Stress behavioural matrix does not have full rank."
  )
}

if (
  sum(
    duplicated(
      as.data.frame(stress_matrix)
    )
  ) != 0L
) {
  stop(
    "Exact duplicated profiles were detected in the original ",
    "Stress matrix."
  )
}

reference_membership <- as.integer(
  factor(
    as.character(
      selected_membership_table$Stress_Cluster
    ),
    levels = sort(
      unique(
        as.character(
          selected_membership_table$Stress_Cluster
        )
      )
    )
  )
)

names(reference_membership) <-
  stress_louvain$Traceability_ID

stress_object_membership <- as.integer(
  factor(
    as.character(
      stress_louvain$Stress_Cluster
    ),
    levels = sort(
      unique(
        as.character(
          stress_louvain$Stress_Cluster
        )
      )
    )
  )
)