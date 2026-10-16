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

project_root <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

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

names(stress_object_membership) <-
  stress_louvain$Traceability_ID

if (
  anyNA(reference_membership) ||
  anyNA(stress_object_membership)
) {
  stop(
    "At least one Stress fish lacks a valid candidate community."
  )
}

if (
  length(unique(reference_membership)) !=
    reference_number_of_communities
) {
  stop(
    "The selected membership does not contain the number of ",
    "communities recorded by Script 024."
  )
}

reference_cluster_sizes <- sort(
  as.integer(
    table(reference_membership)
  )
)

if (
  length(reference_cluster_sizes) !=
    reference_number_of_communities ||
  sum(reference_cluster_sizes) !=
    expected_stress_n ||
  any(reference_cluster_sizes < 1L)
) {
  stop(
    "The reference community-size vector is invalid."
  )
}

# ------------------------------------------------------------
# 8. Partition-comparison functions
# ------------------------------------------------------------

adjusted_rand_index <- function(
  partition_1,
  partition_2
) {
  partition_1 <- as.integer(partition_1)
  partition_2 <- as.integer(partition_2)

  if (length(partition_1) != length(partition_2)) {
    stop("ARI partitions have different lengths.")
  }

  if (length(partition_1) < 2L) {
    stop(
      "At least two observations are required to calculate ARI."
    )
  }

  if (
    anyNA(partition_1) ||
    anyNA(partition_2)
  ) {
    stop("Missing values were detected in an ARI partition.")
  }

  contingency_table <- table(
    partition_1,
    partition_2
  )

  combination_2 <- function(x) {
    x * (x - 1) / 2
  }

  sum_cells <- sum(
    combination_2(
      contingency_table
    )
  )

  sum_rows <- sum(
    combination_2(
      rowSums(
        contingency_table
      )
    )
  )

  sum_columns <- sum(
    combination_2(
      colSums(
        contingency_table
      )
    )
  )

  total_pairs <- combination_2(
    sum(contingency_table)
  )

  expected_index <-
    (sum_rows * sum_columns) /
    total_pairs

  maximum_index <-
    (sum_rows + sum_columns) /
    2

  denominator <-
    maximum_index -
    expected_index

  if (
    abs(denominator) <=
    sqrt(.Machine$double.eps)
  ) {
    same_relationships <- identical(
      outer(
        partition_1,
        partition_1,
        `==`
      ),
      outer(
        partition_2,
        partition_2,
        `==`
      )
    )

    return(
      if (same_relationships) 1 else 0
    )
  }

  ari_value <-
    (sum_cells - expected_index) /
    denominator

  if (
    length(ari_value) != 1L ||
    !is.finite(ari_value)
  ) {
    stop("The adjusted Rand index is not finite.")
  }

  as.numeric(ari_value)
}

partitions_are_equivalent <- function(
  partition_1,
  partition_2
) {
  if (length(partition_1) != length(partition_2)) {
    stop(
      "Partition-equivalence vectors have different lengths."
    )
  }

  if (
    anyNA(partition_1) ||
    anyNA(partition_2)
  ) {
    stop(
      "Missing values were detected in a partition-equivalence check."
    )
  }

  contingency_table <- table(
    partition_1,
    partition_2
  )

  equivalent <- (
    all(
      rowSums(
        contingency_table > 0
      ) == 1L
    ) &&
    all(
      colSums(
        contingency_table > 0
      ) == 1L
    )
  )

  isTRUE(equivalent)
}

reference_object_ari <- adjusted_rand_index(
  reference_membership,
  stress_object_membership
)

reference_object_equivalent <- partitions_are_equivalent(
  reference_membership,
  stress_object_membership
)

if (
  abs(reference_object_ari - 1) > 1e-12 ||
  !reference_object_equivalent
) {
  stop(
    "The community assignments in stress_louvain_clusters.rds ",
    "do not match the selected membership table."
  )
}

# ------------------------------------------------------------
# 9. Exact weighted union-kNN graph constructor
# ------------------------------------------------------------

construct_weighted_union_knn_graph <- function(
  data_matrix,
  k_value,
  vertex_names
) {
  data_matrix <- as.matrix(data_matrix)
  storage.mode(data_matrix) <- "double"

  number_of_rows <- nrow(data_matrix)
  number_of_variables <- ncol(data_matrix)

  if (
    is.null(number_of_rows) ||
    number_of_rows < 2L
  ) {
    stop(
      "The input matrix must contain at least two rows."
    )
  }

  if (
    is.null(number_of_variables) ||
    number_of_variables < 1L
  ) {
    stop(
      "The input matrix contains no behavioural variables."
    )
  }

  if (
    length(k_value) != 1L ||
    is.na(k_value) ||
    !is.finite(k_value) ||
    k_value != as.integer(k_value)
  ) {
    stop("k must be one finite integer.")
  }

  k_value <- as.integer(k_value)

  if (
    k_value < 1L ||
    k_value >= number_of_rows
  ) {
    stop(
      "k must be at least one and smaller than the number of rows."
    )
  }

  vertex_names <- as.character(
    vertex_names
  )

  if (length(vertex_names) != number_of_rows) {
    stop(
      "There must be one vertex name per matrix row."
    )
  }

  if (
    anyNA(vertex_names) ||
    any(!nzchar(vertex_names)) ||
    anyDuplicated(vertex_names) > 0L
  ) {
    stop(
      "Vertex names must be non-missing, non-empty, and unique."
    )
  }

  if (any(!is.finite(data_matrix))) {
    stop(
      "The input matrix contains missing or non-finite values."
    )
  }

  distance_matrix <- as.matrix(
    stats::dist(
      data_matrix,
      method = "euclidean",
      diag = TRUE,
      upper = TRUE
    )
  )

  if (
    !identical(
      dim(distance_matrix),
      c(number_of_rows, number_of_rows)
    )
  ) {
    stop(
      "The Euclidean distance matrix has incorrect dimensions."
    )
  }

  if (
    any(!is.finite(distance_matrix)) ||
    any(distance_matrix < 0)
  ) {
    stop(
      "The Euclidean distance matrix contains invalid values."
    )
  }

  diag(distance_matrix) <- Inf

  nearest_neighbour_index <- matrix(
    NA_integer_,
    nrow = number_of_rows,
    ncol = k_value
  )

  tie_breaker <- seq_len(number_of_rows)

  for (row_i in seq_len(number_of_rows)) {
    neighbour_order <- order(
      distance_matrix[row_i, ],
      tie_breaker,
      na.last = NA
    )

    if (length(neighbour_order) < k_value) {
      stop(
        "Fewer than k valid neighbours were available for row ",
        row_i,
        "."
      )
    }

    nearest_neighbour_index[
      row_i,
      seq_len(k_value)
    ] <- neighbour_order[
      seq_len(k_value)
    ]
  }

  if (anyNA(nearest_neighbour_index)) {
    stop(
      "Missing nearest-neighbour indices were generated."
    )
  }

  from_index <- rep(
    seq_len(number_of_rows),
    each = k_value
  )

  to_index <- as.vector(
    t(nearest_neighbour_index)
  )

  if (any(from_index == to_index)) {
    stop(
      "Self-neighbour relationships were detected."
    )
  }

  lower_index <- pmin(
    from_index,
    to_index
  )

  upper_index <- pmax(
    from_index,
    to_index
  )

  edge_key <- paste(
    lower_index,
    upper_index,
    sep = "__"
  )

  keep_edge <- !duplicated(
    edge_key
  )

  lower_index <- lower_index[
    keep_edge
  ]

  upper_index <- upper_index[
    keep_edge
  ]

  edge_distance <- distance_matrix[
    cbind(
      lower_index,
      upper_index
    )
  ]

  edge_weight <- 1 / (
    1 + edge_distance
  )

  edge_table <- tibble::tibble(
    from = vertex_names[
      lower_index
    ],
    to = vertex_names[
      upper_index
    ],
    distance = as.numeric(
      edge_distance
    ),
    weight = as.numeric(
      edge_weight
    ),
    Lower_index = lower_index,
    Upper_index = upper_index
  )

  if (
    nrow(edge_table) < 1L ||
    anyNA(edge_table) ||
    any(!is.finite(edge_table$distance)) ||
    any(!is.finite(edge_table$weight)) ||
    any(edge_table$distance < 0) ||
    any(edge_table$weight <= 0)
  ) {
    stop(
      "Invalid weighted union-kNN edges were generated."
    )
  }

  if (any(edge_table$from == edge_table$to)) {
    stop(
      "Self-loops were detected in the undirected edge table."
    )
  }

  vertices <- tibble::tibble(
    name = vertex_names,
    Original_row = seq_len(number_of_rows)
  )

  graph <- igraph::graph_from_data_frame(
    d = edge_table %>%
      dplyr::select(
        from,
        to,
        distance,
        weight
      ),
    directed = FALSE,
    vertices = vertices
  )

  if (
    igraph::is_directed(graph) ||
    !igraph::is_simple(graph)
  ) {
    stop(
      "The reconstructed graph is directed or is not simple."
    )
  }

  if (
    igraph::vcount(graph) != number_of_rows ||
    igraph::ecount(graph) != nrow(edge_table)
  ) {
    stop(
      "The reconstructed graph dimensions are inconsistent."
    )
  }

  if (
    !identical(
      as.character(
        igraph::V(graph)$name
      ),
      vertex_names
    )
  ) {
    stop(
      "Graph vertices are not in the expected row order."
    )
  }

  edge_weights <- igraph::E(graph)$weight

  if (
    length(edge_weights) != igraph::ecount(graph) ||
    any(!is.finite(edge_weights)) ||
    any(edge_weights <= 0)
  ) {
    stop(
      "The reconstructed graph contains invalid edge weights."
    )
  }

  if (
    any(
      igraph::degree(graph) < 1L
    )
  ) {
    stop(
      "At least one reconstructed graph vertex is isolated."
    )
  }

  list(
    Graph = graph,
    Edge_table = edge_table,
    Directed_relations =
      number_of_rows * k_value
  )
}

canonical_edge_table <- function(
  graph,
  reference_ids
) {
  if (!igraph::is_igraph(graph)) {
    stop(
      "canonical_edge_table() requires an igraph object."
    )
  }

  reference_ids <- as.character(
    reference_ids
  )

  if (
    !setequal(
      as.character(
        igraph::V(graph)$name
      ),
      reference_ids
    )
  ) {
    stop(
      "Graph vertices do not match the supplied reference IDs."
    )
  }

  edge_data <- igraph::as_data_frame(
    graph,
    what = "edges"
  )

  required_edge_columns <- c(
    "from",
    "to",
    "weight"
  )

  missing_edge_columns <- setdiff(
    required_edge_columns,
    names(edge_data)
  )

  if (length(missing_edge_columns) > 0L) {
    stop(
      "The graph edge table is missing required columns:\n",
      paste(missing_edge_columns, collapse = ", ")
    )
  }

  from_index <- match(
    as.character(edge_data$from),
    reference_ids
  )

  to_index <- match(
    as.character(edge_data$to),
    reference_ids
  )

  if (
    anyNA(from_index) ||
    anyNA(to_index)
  ) {
    stop(
      "At least one graph edge could not be aligned with the ",
      "reference IDs."
    )
  }

  lower_index <- pmin(
    from_index,
    to_index
  )

  upper_index <- pmax(
    from_index,
    to_index
  )

  canonical_table <- tibble::tibble(
    Edge_key = paste(
      lower_index,
      upper_index,
      sep = "__"
    ),
    Lower_index = lower_index,
    Upper_index = upper_index,
    Weight = as.numeric(
      edge_data$weight
    )
  ) %>%
    dplyr::arrange(
      Lower_index,
      Upper_index
    )

  if (
    anyNA(canonical_table) ||
    any(!is.finite(canonical_table$Weight)) ||
    any(canonical_table$Weight <= 0) ||
    anyDuplicated(canonical_table$Edge_key) > 0L
  ) {
    stop(
      "The canonical graph edge table is invalid."
    )
  }

  canonical_table
}

# ------------------------------------------------------------
# 10. Reconstruct and validate the selected reference graph
# ------------------------------------------------------------

reference_graph_result <-
  construct_weighted_union_knn_graph(
    data_matrix = stress_matrix,
    k_value = selected_k,
    vertex_names = stress_louvain$Traceability_ID
  )

reconstructed_graph <-
  reference_graph_result$Graph

if (
  !setequal(
    as.character(
      igraph::V(selected_graph)$name
    ),
    stress_louvain$Traceability_ID
  )
) {
  stop(
    "The stored selected graph vertices do not correspond to ",
    "the 89 Stress IDs."
  )
}

stored_edge_table <- canonical_edge_table(
  graph = selected_graph,
  reference_ids = stress_louvain$Traceability_ID
)

reconstructed_edge_table <- canonical_edge_table(
  graph = reconstructed_graph,
  reference_ids = stress_louvain$Traceability_ID
)

if (
  nrow(stored_edge_table) !=
  nrow(reconstructed_edge_table) ||
  !identical(
    stored_edge_table$Edge_key,
    reconstructed_edge_table$Edge_key
  )
) {
  stop(
    "The graph constructor in Script 025 does not reproduce ",
    "the selected edge set from Script 024."
  )
}

maximum_edge_weight_difference <- max(
  abs(
    stored_edge_table$Weight -
    reconstructed_edge_table$Weight
  )
)

if (
  !is.finite(maximum_edge_weight_difference) ||
  maximum_edge_weight_difference > 1e-12
) {
  stop(
    "The reconstructed edge weights do not reproduce the ",
    "selected graph from Script 024."
  )
}

if (
  !igraph::is_connected(reconstructed_graph)
) {
  stop(
    "The reconstructed selected graph is not connected."
  )
}

set.seed(
  reference_louvain_seed
)

reconstructed_partition <-
  igraph::cluster_louvain(
    graph = reconstructed_graph,
    weights = igraph::E(
      reconstructed_graph
    )$weight
  )

reconstructed_membership <- as.integer(
  igraph::membership(
    reconstructed_partition
  )
)

names(reconstructed_membership) <-
  as.character(
    igraph::V(
      reconstructed_graph
    )$name
  )

reconstructed_membership <-
  reconstructed_membership[
    stress_louvain$Traceability_ID
  ]

if (
  length(reconstructed_membership) !=
  expected_stress_n ||
  anyNA(reconstructed_membership)
) {
  stop(
    "The reconstructed reference membership could not be ",
    "aligned with the Stress IDs."
  )
}

reference_reconstruction_ari <- adjusted_rand_index(
  reference_membership,
  reconstructed_membership
)

reference_reconstruction_equivalent <-
  partitions_are_equivalent(
    reference_membership,
    reconstructed_membership
  )

if (
  abs(reference_reconstruction_ari - 1) >
  1e-12 ||
  !reference_reconstruction_equivalent
) {
  stop(
    "The reconstructed selected graph and Louvain seed do not ",
    "reproduce the candidate partition from Script 024."
  )
}

stored_partition_membership <- as.integer(
  igraph::membership(
    selected_partition
  )
)

stored_partition_names <- names(
  stored_partition_membership
)

if (is.null(stored_partition_names)) {
  if (
    length(stored_partition_membership) !=
    igraph::vcount(selected_graph)
  ) {
    stop(
      "The stored partition membership lacks names and has an ",
      "unexpected length."
    )
  }

  names(stored_partition_membership) <-
    as.character(
      igraph::V(selected_graph)$name
    )
}

stored_partition_membership <-
  stored_partition_membership[
    stress_louvain$Traceability_ID
  ]

if (anyNA(stored_partition_membership)) {
  stop(
    "The stored Script 024 partition could not be aligned with ",
    "the Stress IDs."
  )
}

stored_partition_ari <- adjusted_rand_index(
  reference_membership,
  stored_partition_membership
)

stored_partition_equivalent <-
  partitions_are_equivalent(
    reference_membership,
    stored_partition_membership
  )

if (
  abs(stored_partition_ari - 1) >
  1e-12 ||
  !stored_partition_equivalent
) {
  stop(
    "The stored Script 024 partition does not match the ",
    "selected membership table."
  )
}

reconstructed_weighted_modularity <- igraph::modularity(
  x = reconstructed_graph,
  membership = reference_membership[
    as.character(
      igraph::V(
        reconstructed_graph
      )$name
    )
  ],
  weights = igraph::E(
    reconstructed_graph
  )$weight,
  directed = FALSE
)

if (!is.finite(reconstructed_weighted_modularity)) {
  stop(
    "The reconstructed reference modularity is not finite."
  )
}

modularity_difference <- abs(
  as.numeric(
    reconstructed_weighted_modularity
  ) -
  reference_weighted_modularity
)

if (modularity_difference > 1e-10) {
  stop(
    "The reconstructed weighted modularity does not match ",
    "the selected configuration from Script 024."
  )
}

# ------------------------------------------------------------
# 11. Validate the integral corrected Script 024 results object
# ------------------------------------------------------------

required_results_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Normalisation",
  "Robust_variables",
  "Author_specific_normalisation_recalculation_audit",
  "Selected_configuration",
  "Selected_membership",
  "Community_sizes",
  "Selected_graph",
  "Selected_partition"
)

missing_results_elements <- setdiff(
  required_results_elements,
  names(louvain_results)
)

if (length(missing_results_elements) > 0L) {
  stop(
    "The integral corrected Script 024 results object is missing elements:\n",
    paste(missing_results_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(louvain_results$Analytical_population),
    "Stress only"
  ) ||
  as.integer(louvain_results$Stress_n) != expected_stress_n ||
  !identical(
    sort(as.character(louvain_results$Authors)),
    population_authors
  ) ||
  !identical(
    as.character(louvain_results$Traceability),
    expected_traceability
  ) ||
  !identical(
    as.character(louvain_results$Normalisation),
    expected_normalisation
  ) ||
  !identical(
    as.character(louvain_results$Robust_variables),
    robust_variables
  )
) {
  stop(
    "The integral corrected Script 024 results object is inconsistent ",
    "with the definitive Stress analysis."
  )
}

upstream_recalculation_audit <-
  louvain_results$Author_specific_normalisation_recalculation_audit

if (!is.data.frame(upstream_recalculation_audit)) {
  stop(
    "The corrected Script 024 normalisation recalculation audit is ",
    "not a data frame."
  )
}

required_recalculation_columns <- c(
  "Author",
  "Variable",
  "Robust_variable",
  "n",
  "Maximum_absolute_recalculation_difference"
)

missing_recalculation_columns <- setdiff(
  required_recalculation_columns,
  names(upstream_recalculation_audit)
)

if (length(missing_recalculation_columns) > 0L) {
  stop(
    "The corrected Script 024 recalculation audit is missing columns:\n",
    paste(missing_recalculation_columns, collapse = ", ")
  )
}

if (
  nrow(upstream_recalculation_audit) !=
    expected_number_of_authors *
    expected_number_of_variables ||
  any(
    !is.finite(
      upstream_recalculation_audit[[
        "Maximum_absolute_recalculation_difference"
      ]]
    )
  ) ||
  any(
    upstream_recalculation_audit[[
      "Maximum_absolute_recalculation_difference"
    ]] > 1e-10
  )
) {
  stop(
    "The corrected Script 024 object does not preserve an exact ",
    "Author-specific robust-coordinate audit."
  )
}

maximum_upstream_recalculation_difference <- max(
  upstream_recalculation_audit[[
    "Maximum_absolute_recalculation_difference"
  ]]
)

integral_membership <- louvain_results$Selected_membership

if (!is.data.frame(integral_membership)) {
  stop(
    "louvain_results$Selected_membership is not a data frame."
  )
}

required_integral_membership_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Stress_Cluster"
)

missing_integral_membership_columns <- setdiff(
  required_integral_membership_columns,
  names(integral_membership)
)

if (length(missing_integral_membership_columns) > 0L) {
  stop(
    "The integral selected-membership table is missing:\n",
    paste(missing_integral_membership_columns, collapse = ", ")
  )
}

integral_membership <- integral_membership %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID))
  )

if (
  anyDuplicated(integral_membership$Traceability_ID) > 0L ||
  !setequal(
    integral_membership$Traceability_ID,
    stress_louvain$Traceability_ID
  )
) {
  stop(
    "The integral selected-membership Author + ID keys do not match ",
    "the Stress population."
  )
}

integral_membership <- integral_membership[
  match(
    stress_louvain$Traceability_ID,
    integral_membership$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    integral_membership$Traceability_ID,
    stress_louvain$Traceability_ID
  ) ||
  !identical(
    integral_membership$Author,
    stress_louvain$Author
  ) ||
  !identical(
    integral_membership$ID,
    stress_louvain$ID
  )
) {
  stop(
    "The integral selected membership could not be aligned exactly ",
    "by Author + ID."
  )
}

integral_membership_vector <- as.integer(
  factor(
    as.character(
      integral_membership$Stress_Cluster
    )
  )
)

integral_membership_ari <- adjusted_rand_index(
  reference_membership,
  integral_membership_vector
)

if (
  abs(integral_membership_ari - 1) >
    1e-12
) {
  stop(
    "The integral corrected Script 024 membership does not reproduce ",
    "the selected reference partition."
  )
}

# ------------------------------------------------------------
# 12. Algorithmic stability parameters
# ------------------------------------------------------------

seed_repetitions <- 500L
seed_base <- 20260801L

if (
  seed_repetitions < 100L ||
  seed_repetitions !=
    as.integer(seed_repetitions)
) {
  stop(
    "Algorithmic stability requires at least 100 integer ",
    "seed repetitions."
  )
}

if (
  seed_base + seed_repetitions >
  .Machine$integer.max
) {
  stop(
    "Algorithmic-stability seeds exceed the integer range."
  )
}

# ------------------------------------------------------------
# 13. Repeat Louvain across seeds on the fixed graph
# ------------------------------------------------------------

seed_membership_matrix <- matrix(
  NA_integer_,
  nrow = expected_stress_n,
  ncol = seed_repetitions,
  dimnames = list(
    stress_louvain$Traceability_ID,
    sprintf(
      "SeedRun_%03d",
      seq_len(seed_repetitions)
    )
  )
)

seed_stability_results <- tibble::tibble(
  Iteration = seq_len(seed_repetitions),
  Seed = as.integer(
    seed_base +
    seq_len(seed_repetitions) -
    1L
  ),
  Number_of_communities = NA_integer_,
  Weighted_modularity = NA_real_,
  ARI_to_reference = NA_real_,
  Exact_reference_partition = NA
)

cat(
  "\nStarting ",
  seed_repetitions,
  " fixed-graph Louvain seed repetitions...\n",
  sep = ""
)

for (iteration_i in seq_len(seed_repetitions)) {
  seed_i <- seed_stability_results$Seed[
    iteration_i
  ]

  set.seed(seed_i)

  partition_i <- igraph::cluster_louvain(
    graph = reconstructed_graph,
    weights = igraph::E(
      reconstructed_graph
    )$weight
  )

  membership_i <- as.integer(
    igraph::membership(
      partition_i
    )
  )

  names(membership_i) <- as.character(
    igraph::V(
      reconstructed_graph
    )$name
  )

  membership_i <- membership_i[
    stress_louvain$Traceability_ID
  ]

  if (
    length(membership_i) != expected_stress_n ||
    anyNA(membership_i)
  ) {
    stop(
      "Seed repetition ",
      iteration_i,
      " produced an invalid membership vector."
    )
  }

  modularity_i <- igraph::modularity(
    x = reconstructed_graph,
    membership = membership_i[
      as.character(
        igraph::V(
          reconstructed_graph
        )$name
      )
    ],
    weights = igraph::E(
      reconstructed_graph
    )$weight,
    directed = FALSE
  )

  if (!is.finite(modularity_i)) {
    stop(
      "Seed repetition ",
      iteration_i,
      " produced non-finite modularity."
    )
  }

  seed_membership_matrix[
    ,
    iteration_i
  ] <- membership_i

  seed_stability_results$
    Number_of_communities[
      iteration_i
    ] <- length(
      unique(membership_i)
    )

  seed_stability_results$
    Weighted_modularity[
      iteration_i
    ] <- as.numeric(
      modularity_i
    )

  seed_stability_results$
    ARI_to_reference[
      iteration_i
    ] <- adjusted_rand_index(
      reference_membership,
      membership_i
    )

  seed_stability_results$
    Exact_reference_partition[
      iteration_i
    ] <- partitions_are_equivalent(
      reference_membership,
      membership_i
    )
}

if (
  anyNA(seed_membership_matrix) ||
  anyNA(seed_stability_results)
) {
  stop(
    "The fixed-graph seed stability results contain missing values."
  )
}

if (
  any(
    !is.finite(
      seed_stability_results$Weighted_modularity
    )
  ) ||
  any(
    !is.finite(
      seed_stability_results$ARI_to_reference
    )
  )
) {
  stop(
    "The fixed-graph seed stability results contain non-finite ",
    "values."
  )
}

# ------------------------------------------------------------
# 14. Pairwise agreement across seed repetitions
# ------------------------------------------------------------

number_seed_pairs <- choose(
  seed_repetitions,
  2
)

pairwise_seed_ari <- numeric(
  number_seed_pairs
)

pair_index <- 0L

for (i in seq_len(seed_repetitions - 1L)) {
  for (j in (i + 1L):seed_repetitions) {
    pair_index <- pair_index + 1L

    pairwise_seed_ari[
      pair_index
    ] <- adjusted_rand_index(
      seed_membership_matrix[, i],
      seed_membership_matrix[, j]
    )
  }
}

if (
  pair_index != number_seed_pairs ||
  any(!is.finite(pairwise_seed_ari))
) {
  stop(
    "The pairwise seed-partition ARI vector is incomplete or ",
    "contains non-finite values."
  )
}

seed_ari_quantiles <- stats::quantile(
  seed_stability_results$ARI_to_reference,
  probs = c(
    0.025,
    0.25,
    0.50,
    0.75,
    0.975
  ),
  names = FALSE,
  type = 7
)

pairwise_seed_quantiles <- stats::quantile(
  pairwise_seed_ari,
  probs = c(
    0.025,
    0.25,
    0.50,
    0.75,
    0.975
  ),
  names = FALSE,
  type = 7
)

seed_stability_summary <- tibble::tibble(
  Number_of_stress_fish = expected_stress_n,
  Selected_k = selected_k,
  Reference_communities =
    reference_number_of_communities,
  Seed_repetitions = seed_repetitions,
  Exact_reference_iterations = sum(
    seed_stability_results$
      Exact_reference_partition
  ),
  Exact_reference_rate = mean(
    seed_stability_results$
      Exact_reference_partition
  ),
  Reference_community_count_iterations = sum(
    seed_stability_results$
      Number_of_communities ==
      reference_number_of_communities
  ),
  Reference_community_count_rate = mean(
    seed_stability_results$
      Number_of_communities ==
      reference_number_of_communities
  ),
  Mean_ARI_to_reference = mean(
    seed_stability_results$
      ARI_to_reference
  ),
  SD_ARI_to_reference = stats::sd(
    seed_stability_results$
      ARI_to_reference
  ),
  Median_ARI_to_reference =
    seed_ari_quantiles[3],
  Q1_ARI_to_reference =
    seed_ari_quantiles[2],
  Q3_ARI_to_reference =
    seed_ari_quantiles[4],
  Minimum_ARI_to_reference = min(
    seed_stability_results$
      ARI_to_reference
  ),
  Maximum_ARI_to_reference = max(
    seed_stability_results$
      ARI_to_reference
  ),
  ARI_to_reference_percentile_2_5 =
    seed_ari_quantiles[1],
  ARI_to_reference_percentile_97_5 =
    seed_ari_quantiles[5],
  Mean_pairwise_ARI = mean(
    pairwise_seed_ari
  ),
  SD_pairwise_ARI = stats::sd(
    pairwise_seed_ari
  ),
  Median_pairwise_ARI =
    pairwise_seed_quantiles[3],
  Minimum_pairwise_ARI = min(
    pairwise_seed_ari
  ),
  Pairwise_ARI_percentile_2_5 =
    pairwise_seed_quantiles[1],
  Pairwise_ARI_percentile_97_5 =
    pairwise_seed_quantiles[5],
  Mean_weighted_modularity = mean(
    seed_stability_results$
      Weighted_modularity
  ),
  SD_weighted_modularity = stats::sd(
    seed_stability_results$
      Weighted_modularity
  ),
  Reference_weighted_modularity =
    reference_weighted_modularity,
  Seed_base = seed_base
)

seed_cluster_count_distribution <-
  seed_stability_results %>%
  dplyr::count(
    Number_of_communities,
    name = "Iterations"
  ) %>%
  dplyr::mutate(
    Percentage =
      100 *
      Iterations /
      sum(Iterations)
  ) %>%
  dplyr::arrange(
    Number_of_communities
  )

# ------------------------------------------------------------
# 15. Bootstrap parameters
# ------------------------------------------------------------

bootstrap_iterations <- 1000L
bootstrap_seed_base <- 20260801L
bootstrap_louvain_seed_base <- 620260801L
progress_interval <- 50L

if (
  bootstrap_iterations < 100L ||
  bootstrap_iterations !=
    as.integer(bootstrap_iterations)
) {
  stop(
    "Sampling stability requires at least 100 integer bootstrap ",
    "iterations."
  )
}

if (
  bootstrap_seed_base +
  bootstrap_iterations >
  .Machine$integer.max
) {
  stop(
    "Bootstrap sampling seeds exceed the integer range."
  )
}

if (
  bootstrap_louvain_seed_base +
  bootstrap_iterations >
  .Machine$integer.max
) {
  stop(
    "Bootstrap Louvain seeds exceed the integer range."
  )
}

# ------------------------------------------------------------
# 16. Function for one case-bootstrap iteration
# ------------------------------------------------------------

run_bootstrap_iteration <- function(
  iteration,
  data_matrix,
  reference_partition,
  k_value,
  bootstrap_seed_value,
  louvain_seed_value,
  reference_community_count
) {
  tryCatch({

    data_matrix <- as.matrix(
      data_matrix
    )

    storage.mode(data_matrix) <- "double"

    number_of_rows <- nrow(
      data_matrix
    )

    if (
      length(reference_partition) !=
      number_of_rows
    ) {
      stop(
        "The reference partition does not contain one membership ",
        "per original fish."
      )
    }

    iteration_bootstrap_seed <- as.integer(
      bootstrap_seed_value +
      iteration -
      1L
    )

    iteration_louvain_seed <- as.integer(
      louvain_seed_value +
      iteration -
      1L
    )

    set.seed(
      iteration_bootstrap_seed
    )

    bootstrap_indices <- sample.int(
      n = number_of_rows,
      size = number_of_rows,
      replace = TRUE
    )

    if (
      length(bootstrap_indices) != number_of_rows
    ) {
      stop(
        "The bootstrap sample has an incorrect number of rows."
      )
    }

    bootstrap_matrix <- data_matrix[
      bootstrap_indices,
      ,
      drop = FALSE
    ]

    bootstrap_reference_rows <-
      reference_partition[
        bootstrap_indices
      ]

    bootstrap_vertex_names <- sprintf(
      "B%04d_R%03d",
      iteration,
      seq_len(number_of_rows)
    )

    rownames(bootstrap_matrix) <-
      bootstrap_vertex_names

    graph_result <-
      construct_weighted_union_knn_graph(
        data_matrix = bootstrap_matrix,
        k_value = k_value,
        vertex_names =
          bootstrap_vertex_names
      )

    bootstrap_graph <- graph_result$Graph

    set.seed(
      iteration_louvain_seed
    )

    bootstrap_partition <-
      igraph::cluster_louvain(
        graph = bootstrap_graph,
        weights = igraph::E(
          bootstrap_graph
        )$weight
      )

    bootstrap_membership_rows <- as.integer(
      igraph::membership(
        bootstrap_partition
      )
    )

    names(bootstrap_membership_rows) <-
      as.character(
        igraph::V(
          bootstrap_graph
        )$name
      )

    bootstrap_membership_rows <-
      bootstrap_membership_rows[
        bootstrap_vertex_names
      ]

    if (
      length(bootstrap_membership_rows) !=
      number_of_rows ||
      anyNA(bootstrap_membership_rows)
    ) {
      stop(
        "The bootstrap membership could not be aligned with ",
        "the bootstrap rows."
      )
    }

    bootstrap_community_count <- length(
      unique(
        bootstrap_membership_rows
      )
    )

    if (bootstrap_community_count < 1L) {
      stop(
        "The bootstrap iteration produced no communities."
      )
    }

    ari_bootstrap_rows <- adjusted_rand_index(
      bootstrap_reference_rows,
      bootstrap_membership_rows
    )

    membership_split <- split(
      bootstrap_membership_rows,
      bootstrap_indices
    )

    unique_original_indices <- as.integer(
      names(
        membership_split
      )
    )

    numeric_order <- order(
      unique_original_indices
    )

    unique_original_indices <-
      unique_original_indices[
        numeric_order
      ]

    membership_split <-
      membership_split[
        numeric_order
      ]

    copy_counts <- vapply(
      membership_split,
      length,
      integer(1)
    )

    consensus_membership <- vapply(
      membership_split,
      function(cluster_values) {
        cluster_counts <- table(
          cluster_values
        )

        modal_labels <- as.integer(
          names(
            cluster_counts[
              cluster_counts ==
              max(cluster_counts)
            ]
          )
        )

        min(modal_labels)
      },
      integer(1)
    )

    tied_consensus <- vapply(
      membership_split,
      function(cluster_values) {
        cluster_counts <- table(
          cluster_values
        )

        sum(
          cluster_counts ==
          max(cluster_counts)
        ) > 1L
      },
      logical(1)
    )

    copy_consistency <- vapply(
      membership_split,
      function(cluster_values) {
        cluster_counts <- table(
          cluster_values
        )

        max(cluster_counts) /
          length(cluster_values)
      },
      numeric(1)
    )

    reference_unique <-
      reference_partition[
        unique_original_indices
      ]

    if (
      length(reference_unique) !=
      length(consensus_membership)
    ) {
      stop(
        "The unique-fish reference and consensus partitions ",
        "have different lengths."
      )
    }

    ari_unique_fish <- adjusted_rand_index(
      reference_unique,
      consensus_membership
    )

    repeated_fish <- copy_counts > 1L
    number_repeated_fish <- sum(
      repeated_fish
    )

    if (number_repeated_fish > 0L) {
      mean_repeated_copy_consistency <- mean(
        copy_consistency[
          repeated_fish
        ]
      )

      minimum_repeated_copy_consistency <- min(
        copy_consistency[
          repeated_fish
        ]
      )
    } else {
      mean_repeated_copy_consistency <- 1
      minimum_repeated_copy_consistency <- 1
    }

    graph_components <- igraph::components(
      bootstrap_graph
    )

    weighted_modularity <- igraph::modularity(
      x = bootstrap_graph,
      membership =
        bootstrap_membership_rows[
          as.character(
            igraph::V(
              bootstrap_graph
            )$name
          )
        ],
      weights = igraph::E(
        bootstrap_graph
      )$weight,
      directed = FALSE
    )

    if (!is.finite(weighted_modularity)) {
      stop(
        "Bootstrap weighted modularity is not finite."
      )
    }

    bootstrap_community_sizes <- as.integer(
      table(
        bootstrap_membership_rows
      )
    )

    tibble::tibble(
      Iteration = as.integer(
        iteration
      ),
      Success = TRUE,
      ARI_Unique_Fish =
        ari_unique_fish,
      ARI_Bootstrap_Rows =
        ari_bootstrap_rows,
      Unique_original_fish =
        length(
          unique_original_indices
        ),
      Repeated_original_fish =
        number_repeated_fish,
      Tied_consensus_assignments =
        sum(
          tied_consensus
        ),
      Mean_repeated_copy_consistency =
        mean_repeated_copy_consistency,
      Minimum_repeated_copy_consistency =
        minimum_repeated_copy_consistency,
      Reference_communities_present =
        length(
          unique(
            reference_unique
          )
        ),
      Bootstrap_communities =
        bootstrap_community_count,
      Reference_community_count_solution =
        bootstrap_community_count ==
        reference_community_count,
      Vertices =
        igraph::vcount(
          bootstrap_graph
        ),
      Directed_kNN_relations =
        graph_result$Directed_relations,
      Edges =
        igraph::ecount(
          bootstrap_graph
        ),
      Graph_density =
        igraph::edge_density(
          bootstrap_graph,
          loops = FALSE
        ),
      Mean_degree =
        mean(
          igraph::degree(
            bootstrap_graph
          )
        ),
      Connected_components =
        graph_components$no,
      Connected =
        igraph::is_connected(
          bootstrap_graph
        ),
      Weighted_modularity =
        as.numeric(
          weighted_modularity
        ),
      Minimum_community_size =
        min(
          bootstrap_community_sizes
        ),
      Maximum_community_size =
        max(
          bootstrap_community_sizes
        ),
      Bootstrap_seed =
        iteration_bootstrap_seed,
      Louvain_seed =
        iteration_louvain_seed,
      Error_Message =
        NA_character_
    )

  }, error = function(error_condition) {

    tibble::tibble(
      Iteration = as.integer(
        iteration
      ),
      Success = FALSE,
      ARI_Unique_Fish =
        NA_real_,
      ARI_Bootstrap_Rows =
        NA_real_,
      Unique_original_fish =
        NA_integer_,
      Repeated_original_fish =
        NA_integer_,
      Tied_consensus_assignments =
        NA_integer_,
      Mean_repeated_copy_consistency =
        NA_real_,
      Minimum_repeated_copy_consistency =
        NA_real_,
      Reference_communities_present =
        NA_integer_,
      Bootstrap_communities =
        NA_integer_,
      Reference_community_count_solution =
        NA,
      Vertices =
        NA_integer_,
      Directed_kNN_relations =
        NA_integer_,
      Edges =
        NA_integer_,
      Graph_density =
        NA_real_,
      Mean_degree =
        NA_real_,
      Connected_components =
        NA_integer_,
      Connected =
        NA,
      Weighted_modularity =
        NA_real_,
      Minimum_community_size =
        NA_integer_,
      Maximum_community_size =
        NA_integer_,
      Bootstrap_seed = as.integer(
        bootstrap_seed_value +
        iteration -
        1L
      ),
      Louvain_seed = as.integer(
        louvain_seed_value +
        iteration -
        1L
      ),
      Error_Message = conditionMessage(
        error_condition
      )
    )
  })
}

# ------------------------------------------------------------
# 17. Run case-bootstrap stability
# ------------------------------------------------------------

bootstrap_results_list <- vector(
  mode = "list",
  length = bootstrap_iterations
)

cat(
  "\nStarting ",
  bootstrap_iterations,
  " case-bootstrap iterations at selected k = ",
  selected_k,
  "...\n",
  sep = ""
)

for (
  bootstrap_iteration_i in
  seq_len(bootstrap_iterations)
) {
  bootstrap_results_list[[bootstrap_iteration_i]] <-
    run_bootstrap_iteration(
    iteration =
      bootstrap_iteration_i,
    data_matrix =
      stress_matrix,
    reference_partition =
      reference_membership,
    k_value =
      selected_k,
    bootstrap_seed_value =
      bootstrap_seed_base,
    louvain_seed_value =
      bootstrap_louvain_seed_base,
    reference_community_count =
      reference_number_of_communities
  )

  if (
    bootstrap_iteration_i == 1L ||
    bootstrap_iteration_i %%
      progress_interval == 0L ||
    bootstrap_iteration_i ==
      bootstrap_iterations
  ) {
    cat(
      "Completed bootstrap iteration ",
      bootstrap_iteration_i,
      " of ",
      bootstrap_iterations,
      ".\n",
      sep = ""
    )
  }
}

bootstrap_results <- dplyr::bind_rows(
  bootstrap_results_list
)

if (
  nrow(bootstrap_results) !=
  bootstrap_iterations ||
  !identical(
    bootstrap_results$Iteration,
    seq_len(
      bootstrap_iterations
    )
  )
) {
  stop(
    "The bootstrap result table is incomplete or incorrectly ",
    "ordered."
  )
}

failed_bootstrap_results <-
  bootstrap_results %>%
  dplyr::filter(
    !Success
  )

if (nrow(failed_bootstrap_results) > 0L) {
  failed_bootstrap_csv <- file.path(
    tables_dir,
    "Objective3_Louvain_BootstrapFailedIterations.csv"
  )

  utils::write.csv(
    failed_bootstrap_results,
    failed_bootstrap_csv,
    row.names = FALSE
  )

  stop(
    nrow(failed_bootstrap_results),
    " of ",
    bootstrap_iterations,
    " bootstrap iterations failed. Diagnostics were saved at:\n",
    normalizePath(
      failed_bootstrap_csv,
      winslash = "/",
      mustWork = FALSE
    ),
    "\nNo final stability summary was accepted."
  )
}

successful_bootstrap_results <-
  bootstrap_results

# ------------------------------------------------------------
# 18. Validate successful bootstrap results
# ------------------------------------------------------------

finite_bootstrap_columns <- c(
  "ARI_Unique_Fish",
  "ARI_Bootstrap_Rows",
  "Mean_repeated_copy_consistency",
  "Minimum_repeated_copy_consistency",
  "Graph_density",
  "Mean_degree",
  "Weighted_modularity"
)

for (column_i in finite_bootstrap_columns) {
  if (
    any(
      !is.finite(
        successful_bootstrap_results[[column_i]]
      )
    )
  ) {
    stop(
      "At least one value in ",
      column_i,
      " is not finite."
    )
  }
}

ari_tolerance <- 1e-10

for (
  ari_column_i in c(
    "ARI_Unique_Fish",
    "ARI_Bootstrap_Rows"
  )
) {
  if (
    any(
      successful_bootstrap_results[[ari_column_i]] < -1 - ari_tolerance |
      successful_bootstrap_results[[ari_column_i]] > 1 + ari_tolerance
    )
  ) {
    stop(
      "At least one value in ",
      ari_column_i,
      " is outside the valid ARI range."
    )
  }
}

if (
  any(
    successful_bootstrap_results$Vertices !=
    expected_stress_n
  )
) {
  stop(
    "At least one bootstrap graph does not contain 89 vertices."
  )
}

if (
  any(
    successful_bootstrap_results$
      Directed_kNN_relations !=
    expected_stress_n * selected_k
  )
) {
  stop(
    "At least one bootstrap graph does not contain exactly ",
    expected_stress_n * selected_k,
    " directed kNN relations."
  )
}

if (
  any(
    successful_bootstrap_results$
      Bootstrap_communities < 1L
  )
) {
  stop(
    "At least one bootstrap iteration produced no communities."
  )
}

if (
  any(
    successful_bootstrap_results$
      Reference_communities_present < 1L
  )
) {
  stop(
    "At least one bootstrap sample contains no reference ",
    "community."
  )
}

if (
  any(
    successful_bootstrap_results$
      Unique_original_fish < 2L |
    successful_bootstrap_results$
      Unique_original_fish >
      expected_stress_n
  )
) {
  stop(
    "At least one bootstrap sample contains an invalid number ",
    "of unique original fish."
  )
}

if (
  any(
    successful_bootstrap_results$
      Repeated_original_fish < 0L |
    successful_bootstrap_results$
      Repeated_original_fish >
      successful_bootstrap_results$
        Unique_original_fish
  )
) {
  stop(
    "At least one bootstrap sample contains an invalid count ",
    "of repeated original fish."
  )
}

if (
  any(
    successful_bootstrap_results$
      Tied_consensus_assignments < 0L |
    successful_bootstrap_results$
      Tied_consensus_assignments >
      successful_bootstrap_results$
        Unique_original_fish
  )
) {
  stop(
    "At least one bootstrap sample contains an invalid number ",
    "of tied consensus assignments."
  )
}

if (
  any(
    successful_bootstrap_results$
      Mean_repeated_copy_consistency < 0 |
    successful_bootstrap_results$
      Mean_repeated_copy_consistency > 1 |
    successful_bootstrap_results$
      Minimum_repeated_copy_consistency < 0 |
    successful_bootstrap_results$
      Minimum_repeated_copy_consistency > 1
  )
) {
  stop(
    "At least one repeated-copy consistency value lies outside ",
    "the interval [0, 1]."
  )
}

# ------------------------------------------------------------
# 19. Summarise bootstrap stability
# ------------------------------------------------------------

bootstrap_unique_quantiles <- stats::quantile(
  successful_bootstrap_results$
    ARI_Unique_Fish,
  probs = c(
    0.025,
    0.25,
    0.50,
    0.75,
    0.975
  ),
  names = FALSE,
  type = 7
)

bootstrap_rows_quantiles <- stats::quantile(
  successful_bootstrap_results$
    ARI_Bootstrap_Rows,
  probs = c(
    0.025,
    0.25,
    0.50,
    0.75,
    0.975
  ),
  names = FALSE,
  type = 7
)

bootstrap_summary <- tibble::tibble(
  Number_of_stress_fish = expected_stress_n,
  Reference_communities =
    reference_number_of_communities,
  Reference_community_sizes = paste(
    reference_cluster_sizes,
    collapse = "; "
  ),
  Selected_k = selected_k,
  Bootstrap_iterations_requested =
    bootstrap_iterations,
  Bootstrap_iterations_successful =
    nrow(
      successful_bootstrap_results
    ),
  Mean_ARI_Unique_Fish = mean(
    successful_bootstrap_results$
      ARI_Unique_Fish
  ),
  SD_ARI_Unique_Fish = stats::sd(
    successful_bootstrap_results$
      ARI_Unique_Fish
  ),
  Median_ARI_Unique_Fish =
    bootstrap_unique_quantiles[3],
  Q1_ARI_Unique_Fish =
    bootstrap_unique_quantiles[2],
  Q3_ARI_Unique_Fish =
    bootstrap_unique_quantiles[4],
  Minimum_ARI_Unique_Fish = min(
    successful_bootstrap_results$
      ARI_Unique_Fish
  ),
  Maximum_ARI_Unique_Fish = max(
    successful_bootstrap_results$
      ARI_Unique_Fish
  ),
  ARI_Unique_Fish_percentile_2_5 =
    bootstrap_unique_quantiles[1],
  ARI_Unique_Fish_percentile_97_5 =
    bootstrap_unique_quantiles[5],
  Mean_ARI_Bootstrap_Rows = mean(
    successful_bootstrap_results$
      ARI_Bootstrap_Rows
  ),
  SD_ARI_Bootstrap_Rows = stats::sd(
    successful_bootstrap_results$
      ARI_Bootstrap_Rows
  ),
  Median_ARI_Bootstrap_Rows =
    bootstrap_rows_quantiles[3],
  ARI_Bootstrap_Rows_percentile_2_5 =
    bootstrap_rows_quantiles[1],
  ARI_Bootstrap_Rows_percentile_97_5 =
    bootstrap_rows_quantiles[5],
  Reference_community_count_iterations =
    sum(
      successful_bootstrap_results$
        Reference_community_count_solution
    ),
  Reference_community_count_recovery_rate =
    mean(
      successful_bootstrap_results$
        Reference_community_count_solution
    ),
  ARI_Unique_Fish_ge_0_80 = mean(
    successful_bootstrap_results$
      ARI_Unique_Fish >= 0.80
  ),
  ARI_Unique_Fish_ge_0_90 = mean(
    successful_bootstrap_results$
      ARI_Unique_Fish >= 0.90
  ),
  Mean_bootstrap_communities = mean(
    successful_bootstrap_results$
      Bootstrap_communities
  ),
  SD_bootstrap_communities = stats::sd(
    successful_bootstrap_results$
      Bootstrap_communities
  ),
  Mean_unique_original_fish = mean(
    successful_bootstrap_results$
      Unique_original_fish
  ),
  SD_unique_original_fish = stats::sd(
    successful_bootstrap_results$
      Unique_original_fish
  ),
  Mean_repeated_copy_consistency = mean(
    successful_bootstrap_results$
      Mean_repeated_copy_consistency
  ),
  Minimum_repeated_copy_consistency_observed =
    min(
      successful_bootstrap_results$
        Minimum_repeated_copy_consistency
    ),
  Iterations_with_consensus_ties = sum(
    successful_bootstrap_results$
      Tied_consensus_assignments > 0L
  ),
  Connected_graph_rate = mean(
    successful_bootstrap_results$
      Connected
  ),
  Mean_weighted_modularity = mean(
    successful_bootstrap_results$
      Weighted_modularity
  ),
  SD_weighted_modularity = stats::sd(
    successful_bootstrap_results$
      Weighted_modularity
  ),
  Reference_weighted_modularity =
    reference_weighted_modularity,
  Bootstrap_seed_base =
    bootstrap_seed_base,
  Bootstrap_Louvain_seed_base =
    bootstrap_louvain_seed_base
)

bootstrap_cluster_count_distribution <-
  successful_bootstrap_results %>%
  dplyr::count(
    Bootstrap_communities,
    name = "Iterations"
  ) %>%
  dplyr::mutate(
    Percentage =
      100 *
      Iterations /
      sum(Iterations)
  ) %>%
  dplyr::arrange(
    Bootstrap_communities
  )

# ------------------------------------------------------------
# 20. Create reference-validation and configuration tables
# ------------------------------------------------------------

reference_validation <- tibble::tibble(
  Check = c(
    "Stress fish",
    "Traceability",
    "Authors/cohorts",
    "Normalisation",
    "Maximum upstream robust-score recalculation difference",
    "Robust behavioural variables",
    "Selected k",
    "Selected plateau",
    "Reference communities",
    "Reference community sizes",
    "Stored membership agreement",
    "Stored partition agreement",
    "Reference graph edge-set agreement",
    "Maximum edge-weight difference",
    "Reference reconstruction ARI",
    "Reference partition equivalence",
    "Reconstructed weighted modularity",
    "Stored weighted modularity",
    "Absolute modularity difference"
  ),
  Result = c(
    as.character(
      expected_stress_n
    ),
    expected_traceability,
    paste(
      population_authors,
      collapse = "; "
    ),
    expected_normalisation,
    format(
      maximum_upstream_recalculation_difference,
      scientific = TRUE,
      digits = 8
    ),
    paste(
      robust_variables,
      collapse = "; "
    ),
    as.character(
      selected_k
    ),
    paste0(
      selected_plateau_start_k,
      "-",
      selected_plateau_end_k,
      " (",
      selected_plateau_length,
      " k values)"
    ),
    as.character(
      reference_number_of_communities
    ),
    paste(
      reference_cluster_sizes,
      collapse = "; "
    ),
    if (
      reference_object_ari == 1 &&
      reference_object_equivalent
    ) {
      "Exact"
    } else {
      "Not exact"
    },
    if (
      stored_partition_ari == 1 &&
      stored_partition_equivalent
    ) {
      "Exact"
    } else {
      "Not exact"
    },
    "Exact",
    format(
      maximum_edge_weight_difference,
      scientific = TRUE,
      digits = 8
    ),
    format(
      reference_reconstruction_ari,
      digits = 16
    ),
    as.character(
      reference_reconstruction_equivalent
    ),
    format(
      as.numeric(
        reconstructed_weighted_modularity
      ),
      digits = 16
    ),
    format(
      reference_weighted_modularity,
      digits = 16
    ),
    format(
      modularity_difference,
      scientific = TRUE,
      digits = 8
    )
  )
)

stability_configuration <- tibble::tibble(
  Parameter = c(
    "Objective",
    "Reference population",
    "Traceability",
    "Authors/cohorts",
    "Reference architecture",
    "Selected k",
    "Selected k plateau",
    "Input scale",
    "Distance",
    "Similarity",
    "Graph",
    "Community detection",
    "Reference Louvain seed",
    "Algorithmic seed repetitions",
    "Sampling method",
    "Bootstrap rows per iteration",
    "Bootstrap iterations",
    "Graph k during bootstrap",
    "Primary bootstrap metric",
    "Repeated-fish consensus",
    "Secondary bootstrap metric",
    "Interpretive rule"
  ),
  Value = c(
    paste0(
      "Objective 3: de novo internal organisation of the ",
      "UCS-exposed population"
    ),
    "89 Stress fish",
    expected_traceability,
    paste(
      population_authors,
      collapse = ", "
    ),
    paste0(
      reference_number_of_communities,
      " candidate Louvain communities"
    ),
    as.character(
      selected_k
    ),
    paste0(
      selected_plateau_start_k,
      "-",
      selected_plateau_end_k
    ),
    paste0(
      "Four Author-specific Control-referenced robust variables; ",
      "no Stress-specific rescaling"
    ),
    expected_distance,
    expected_similarity,
    expected_graph_type,
    "Weighted Louvain",
    as.character(
      reference_louvain_seed
    ),
    as.character(
      seed_repetitions
    ),
    "Case bootstrap with replacement",
    as.character(
      expected_stress_n
    ),
    as.character(
      bootstrap_iterations
    ),
    as.character(
      selected_k
    ),
    paste0(
      "Adjusted Rand Index on unique original fish represented ",
      "in each bootstrap sample"
    ),
    paste0(
      "Modal Louvain membership across resampled copies; ",
      "numeric ties resolved deterministically"
    ),
    paste0(
      "Adjusted Rand Index across all 89 bootstrap rows, ",
      "including repeated copies"
    ),
    paste0(
      "Algorithmic and sampling stability are interpreted jointly. ",
      "No universal ARI threshold is imposed, and k is not reselected."
    )
  )
)

analysis_notes <- tibble::tibble(
  Note = c(
    paste0(
      "The four Author-specific Control-referenced robust coordinates ",
      "were inherited unchanged from corrected Scripts 020-024."
    ),
    paste0(
      "Algorithmic stability repeats weighted Louvain on the fixed ",
      "selected graph and therefore isolates sensitivity to the ",
      "stochastic optimisation order."
    ),
    paste0(
      "The bootstrap samples 89 rows with replacement and rebuilds ",
      "the same weighted, undirected union-kNN graph at the selected k."
    ),
    paste0(
      "The primary bootstrap ARI gives each unique original fish one ",
      "consensus assignment, preventing repeatedly sampled fish from ",
      "receiving greater weight."
    ),
    paste0(
      "The row-level bootstrap ARI is retained as a secondary ",
      "diagnostic across all resampled occurrences."
    ),
    paste0(
      "The k plateau was established in Script 024. Script 025 ",
      "evaluates the fixed candidate solution and does not optimise k."
    ),
    paste0(
      "A reproducible statistical partition can support subsequent ",
      "operational resilient-like and susceptible-like interpretation, ",
      "but it does not constitute independent biological validation."
    )
  )
)

# ------------------------------------------------------------
# 21. Assemble the definitive stability results object
# ------------------------------------------------------------

stability_results_object <- list(
  Objective = "Objective 3",
  Analytical_population = "Stress only",
  Stress_n = expected_stress_n,
  Authors = population_authors,
  Traceability = expected_traceability,
  Normalisation = expected_normalisation,
  Robust_variables = robust_variables,
  Maximum_upstream_recalculation_difference =
    maximum_upstream_recalculation_difference,
  Selected_k = selected_k,
  Reference_number_of_communities =
    reference_number_of_communities,
  Reference_community_sizes =
    reference_cluster_sizes,
  Reference_membership =
    reference_membership,
  Reference_weighted_modularity =
    reference_weighted_modularity,
  Reference_validation =
    reference_validation,
  Stability_configuration =
    stability_configuration,
  Seed_stability_iterations =
    seed_stability_results,
  Seed_stability_summary =
    seed_stability_summary,
  Seed_cluster_count_distribution =
    seed_cluster_count_distribution,
  Seed_pairwise_ARI =
    pairwise_seed_ari,
  Seed_membership_matrix =
    seed_membership_matrix,
  Bootstrap_iterations =
    bootstrap_results,
  Bootstrap_summary =
    bootstrap_summary,
  Bootstrap_cluster_count_distribution =
    bootstrap_cluster_count_distribution,
  Analysis_notes =
    analysis_notes,
  Source_stress_louvain_file =
    stress_louvain_input,
  Source_selected_graph_file =
    selected_graph_input,
  Source_selected_partition_file =
    selected_partition_input,
  Source_selected_configuration_file =
    selected_configuration_input,
  Source_selected_membership_file =
    selected_membership_input,
  Interpretive_constraint = paste(
    "This analysis evaluates reproducibility of the candidate de novo",
    "Stress partition. It does not independently validate resilient-like",
    "or susceptible-like biology."
  )
)

# ------------------------------------------------------------
# 22. Export results
# ------------------------------------------------------------

utils::write.csv(
  seed_stability_results,
  seed_iterations_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  seed_stability_summary,
  seed_summary_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  seed_cluster_count_distribution,
  seed_cluster_counts_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  bootstrap_results,
  bootstrap_iterations_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  bootstrap_summary,
  bootstrap_summary_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  bootstrap_cluster_count_distribution,
  bootstrap_cluster_counts_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  reference_validation,
  reference_validation_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  stability_configuration,
  configuration_csv,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  analysis_notes,
  analysis_notes_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  x = list(
    Seed_summary =
      seed_stability_summary,
    Seed_cluster_counts =
      seed_cluster_count_distribution,
    Seed_iterations =
      seed_stability_results,
    Bootstrap_summary =
      bootstrap_summary,
    Bootstrap_cluster_counts =
      bootstrap_cluster_count_distribution,
    Bootstrap_iterations =
      bootstrap_results,
    Reference_validation =
      reference_validation,
    Configuration =
      stability_configuration,
    Analysis_notes =
      analysis_notes
  ),
  file = workbook_output,
  overwrite = TRUE
)

saveRDS(
  seed_membership_matrix,
  seed_memberships_output
)

saveRDS(
  list(
    Iterations =
      seed_stability_results,
    Summary =
      seed_stability_summary,
    Cluster_count_distribution =
      seed_cluster_count_distribution,
    Pairwise_ARI =
      pairwise_seed_ari
  ),
  seed_results_output
)

saveRDS(
  bootstrap_results,
  bootstrap_iterations_output
)

saveRDS(
  bootstrap_summary,
  bootstrap_summary_output
)

saveRDS(
  stability_results_object,
  stability_results_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 23. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  seed_iterations_csv,
  seed_summary_csv,
  seed_cluster_counts_csv,
  bootstrap_iterations_csv,
  bootstrap_summary_csv,
  bootstrap_cluster_counts_csv,
  reference_validation_csv,
  configuration_csv,
  analysis_notes_csv,
  workbook_output,
  seed_memberships_output,
  seed_results_output,
  bootstrap_iterations_output,
  bootstrap_summary_output,
  stability_results_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected stability outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

output_file_information <- file.info(
  expected_output_files
)

if (
  any(is.na(output_file_information$size)) ||
  any(output_file_information$size <= 0)
) {
  stop(
    "At least one expected stability output file is empty or ",
    "inaccessible."
  )
}

# ------------------------------------------------------------
# 24. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 3 - STRESS LOUVAIN STABILITY\n")
cat("============================================================\n")

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
  "Analytical space: four Author-specific Control-referenced robust ",
  "variables; no Stress-specific rescaling.\n",
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
  "Selected k inherited from corrected Script 024: ",
  selected_k,
  "\n",
  sep = ""
)

cat(
  "Reference communities: ",
  reference_number_of_communities,
  "\n",
  sep = ""
)

cat(
  "Reference community sizes: ",
  paste(
    reference_cluster_sizes,
    collapse = " and "
  ),
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

cat("\nReference reconstruction validation:\n")

print(
  reference_validation,
  n = Inf,
  width = Inf
)

cat("\nAlgorithmic seed stability summary:\n")

print(
  seed_stability_summary,
  n = Inf,
  width = Inf
)

cat("\nSeed community-count distribution:\n")

print(
  seed_cluster_count_distribution,
  n = Inf,
  width = Inf
)

cat("\nBootstrap sampling-stability summary:\n")

print(
  bootstrap_summary,
  n = Inf,
  width = Inf
)

cat("\nBootstrap community-count distribution:\n")

print(
  bootstrap_cluster_count_distribution,
  n = Inf,
  width = Inf
)

cat(
  "\nMean seed ARI to reference: ",
  sprintf(
    "%.3f",
    seed_stability_summary$
      Mean_ARI_to_reference
  ),
  "\n",
  sep = ""
)

cat(
  "Mean pairwise seed ARI: ",
  sprintf(
    "%.3f",
    seed_stability_summary$
      Mean_pairwise_ARI
  ),
  "\n",
  sep = ""
)

cat(
  "Exact reference recovery across seeds: ",
  sprintf(
    "%.1f",
    100 *
    seed_stability_summary$
      Exact_reference_rate
  ),
  "%\n",
  sep = ""
)

cat(
  "Mean bootstrap ARI on unique fish: ",
  sprintf(
    "%.3f",
    bootstrap_summary$
      Mean_ARI_Unique_Fish
  ),
  "\n",
  sep = ""
)

cat(
  "Median bootstrap ARI on unique fish: ",
  sprintf(
    "%.3f",
    bootstrap_summary$
      Median_ARI_Unique_Fish
  ),
  "\n",
  sep = ""
)

cat(
  "Bootstrap ARI 2.5th percentile: ",
  sprintf(
    "%.3f",
    bootstrap_summary$
      ARI_Unique_Fish_percentile_2_5
  ),
  "\n",
  sep = ""
)

cat(
  "Reference community-count recovery rate: ",
  sprintf(
    "%.1f",
    100 *
    bootstrap_summary$
      Reference_community_count_recovery_rate
  ),
  "%\n",
  sep = ""
)

cat(
  "Mean repeated-copy consistency: ",
  sprintf(
    "%.3f",
    bootstrap_summary$
      Mean_repeated_copy_consistency
  ),
  "\n",
  sep = ""
)

cat(
  "\nInterpretive constraint: stability must be interpreted jointly ",
  "from fixed-graph seed agreement, the bootstrap ARI distribution, ",
  "community-count recovery, and duplicate-copy consistency. ",
  "Statistical reproducibility does not independently establish ",
  "biological resilient-like or susceptible-like validity.\n",
  sep = ""
)

cat("\nOutputs saved as:\n")

cat(
  paste0(
    "- ",
    normalizePath(
      expected_output_files,
      winslash = "/",
      mustWork = FALSE
    ),
    collapse = "\n"
  ),
  "\n",
  sep = ""
)

cat("============================================================\n")
