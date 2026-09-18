# ============================================================
# 010_LouvainExploratorySubsamplingStability.R
#
# Objective 1
# Conditional subsampling stability of the exploratory Louvain
# segmentation returned by scripts 006.0, 006.1 and 006.
#
# Scientific scope:
# - This is a supplementary algorithmic-stability analysis.
# - It does not test whether discrete behavioural communities exist.
# - It does not repeat or replace the selection-adjusted null test
#   performed by script 006.1.
# - It does not validate biological phenotypes, archetypes or
#   classification thresholds.
#
# Resampling design:
# - 80% stratified subsampling without replacement within Author.
# - No original fish can appear more than once in a subsample.
# - Therefore, identical bootstrap copies are never converted into
#   artificial zero-distance graph vertices.
# - Robust coordinates remain fixed at the values produced by script 005.
#
# Graph-scale analyses in every common subsample:
# 1. Primary conditional analysis: fixed k = 45.
# 2. Sensitivity analysis: k scaled to preserve the approximate
#    proportion of available neighbours (k = 36 for n = 71).
#
# Louvain stochasticity:
# - 25 deterministic seed runs are performed for each graph.
# - The representative partition is the run with the highest mean
#   ARI to all other runs; modularity and seed order break exact ties.
#
# Main descriptive stability metric:
# - Adjusted Rand Index (ARI) between the subsample partition and
#   the original exploratory segmentation restricted to the same fish.
#
# Required upstream order:
# 005 -> 006.0 -> 006.1 -> corrected 006 -> this script
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c("dplyr", "tibble", "igraph", "openxlsx")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running the script:\n",
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
# 3. Input and output directories
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")

if (!dir.exists(objects_dir)) {
  stop("Objects directory not found:\n", objects_dir)
}

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# 4. Required corrected-006 inputs
# ------------------------------------------------------------

segmented_data_file <- file.path(
  objects_dir,
  "objective1_control_louvain_exploratory_segmentation.rds"
)

membership_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_membership.rds"
)

graph_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph_summary.rds"
)

inference_status_file <- file.path(
  objects_dir,
  "objective1_louvain_inference_status.rds"
)

dependency_audit_006_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_DependencyAudit.csv"
)

required_input_files <- c(
  segmented_data_file,
  membership_file,
  graph_file,
  graph_summary_file,
  inference_status_file,
  dependency_audit_006_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following corrected-006 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun the corrected 006_FigureS_Louvain_ExploratorySegmentation.R ",
    "before this script."
  )
}

# ------------------------------------------------------------
# 5. Load and type-check corrected-006 objects
# ------------------------------------------------------------

control_segmented <- readRDS(segmented_data_file)
segment_membership <- readRDS(membership_file)
stored_reference_graph <- readRDS(graph_file)
graph_summary <- readRDS(graph_summary_file)
inference_status <- readRDS(inference_status_file)

dependency_audit_006 <- utils::read.csv(
  dependency_audit_006_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!is.data.frame(control_segmented)) {
  stop("The corrected-006 segmented-control object is not a data frame.")
}

if (!is.data.frame(segment_membership)) {
  stop("The corrected-006 membership object is not a data frame.")
}

if (!inherits(stored_reference_graph, "igraph")) {
  stop("The corrected-006 graph object is not an igraph graph.")
}

if (!is.data.frame(graph_summary) || nrow(graph_summary) != 1L) {
  stop("The corrected-006 graph summary must be a one-row data frame.")
}

if (!is.list(inference_status)) {
  stop("The corrected-006 inference-status object is not a list.")
}

if (!is.data.frame(dependency_audit_006)) {
  stop("The corrected-006 dependency audit is not a data frame.")
}

# ------------------------------------------------------------
# 6. Verify the corrected-006 dependency audit by MD5
# ------------------------------------------------------------

required_dependency_columns <- c("Dependency", "File", "MD5", "Modified")
missing_dependency_columns <- setdiff(
  required_dependency_columns,
  names(dependency_audit_006)
)

if (length(missing_dependency_columns) > 0L) {
  stop(
    "The corrected-006 dependency audit lacks:\n",
    paste(missing_dependency_columns, collapse = ", ")
  )
}

dependency_audit_006 <- dependency_audit_006 %>%
  dplyr::transmute(
    Dependency = trimws(as.character(Dependency)),
    File = trimws(as.character(File)),
    MD5 = tolower(trimws(as.character(MD5))),
    Modified = as.character(Modified)
  )

if (
  nrow(dependency_audit_006) < 1L ||
    anyNA(dependency_audit_006[c("Dependency", "File", "MD5")]) ||
    any(dependency_audit_006$Dependency == "") ||
    any(dependency_audit_006$File == "") ||
    any(!grepl("^[[:xdigit:]]{32}$", dependency_audit_006$MD5))
) {
  stop("The corrected-006 dependency audit contains invalid entries.")
}

if (anyDuplicated(dependency_audit_006$Dependency) > 0L) {
  stop("The corrected-006 dependency audit contains duplicated labels.")
}

missing_audited_dependencies <- dependency_audit_006$File[
  !file.exists(dependency_audit_006$File)
]

if (length(missing_audited_dependencies) > 0L) {
  stop(
    "At least one dependency audited by corrected 006 is now missing:\n",
    paste(missing_audited_dependencies, collapse = "\n")
  )
}

current_dependency_md5 <- tolower(as.character(unname(
  tools::md5sum(dependency_audit_006$File)
)))

if (!identical(dependency_audit_006$MD5, current_dependency_md5)) {
  mismatched_dependencies <- dependency_audit_006$Dependency[
    dependency_audit_006$MD5 != current_dependency_md5
  ]

  stop(
    "At least one upstream dependency changed after corrected 006:\n",
    paste(mismatched_dependencies, collapse = "\n"),
    "\nRerun the affected upstream scripts before 010."
  )
}

# ------------------------------------------------------------
# 7. Fixed analytical contract and traceability
# ------------------------------------------------------------

expected_number_of_fish <- 89L
expected_number_of_authors <- 4L
expected_selected_k <- 45L
expected_number_of_segments <- 2L
expected_segment_sizes <- c(40L, 49L)
primary_alpha <- 0.05

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

required_control_columns <- c(
  "Author",
  "ID",
  "Algorithmic_Segment",
  robust_variables
)

missing_control_columns <- setdiff(
  required_control_columns,
  names(control_segmented)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The corrected-006 segmented dataset lacks:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_membership_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Algorithmic_Segment"
)

missing_membership_columns <- setdiff(
  required_membership_columns,
  names(segment_membership)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The corrected-006 membership object lacks:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

make_traceability_key <- function(author, id) {
  paste(as.character(author), as.character(id), sep = " || ")
}

control_segmented <- control_segmented %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = make_traceability_key(Author, ID),
    Algorithmic_Segment = droplevels(factor(Algorithmic_Segment))
  )

segment_membership <- segment_membership %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Algorithmic_Segment = droplevels(factor(Algorithmic_Segment))
  )

if (nrow(control_segmented) != expected_number_of_fish) {
  stop(
    "The corrected-006 segmented dataset contains ",
    nrow(control_segmented),
    " fish; ",
    expected_number_of_fish,
    " were expected."
  )
}

if (nrow(segment_membership) != expected_number_of_fish) {
  stop(
    "The corrected-006 membership contains ",
    nrow(segment_membership),
    " rows; ",
    expected_number_of_fish,
    " were expected."
  )
}

if (
  anyNA(control_segmented[c("Author", "ID", "Traceability_ID")]) ||
    any(control_segmented$Author == "") ||
    any(control_segmented$ID == "") ||
    anyDuplicated(control_segmented[c("Author", "ID")]) > 0L ||
    anyDuplicated(control_segmented$Traceability_ID) > 0L
) {
  stop("Invalid or duplicated Author + ID keys were detected in corrected 006.")
}

if (
  anyNA(segment_membership[c("Author", "ID", "Traceability_ID")]) ||
    anyDuplicated(segment_membership[c("Author", "ID")]) > 0L ||
    anyDuplicated(segment_membership$Traceability_ID) > 0L
) {
  stop("Invalid or duplicated Author + ID keys were detected in the membership object.")
}

expected_membership_keys <- make_traceability_key(
  segment_membership$Author,
  segment_membership$ID
)

if (!identical(segment_membership$Traceability_ID, expected_membership_keys)) {
  stop("The membership Traceability_ID does not equal Author + ID.")
}

if (!setequal(
  control_segmented$Traceability_ID,
  segment_membership$Traceability_ID
)) {
  stop("The corrected-006 data and membership contain different fish.")
}

segment_membership <- segment_membership[
  match(control_segmented$Traceability_ID, segment_membership$Traceability_ID),
  ,
  drop = FALSE
]

if (!identical(
  control_segmented$Traceability_ID,
  segment_membership$Traceability_ID
)) {
  stop("The corrected-006 membership could not be aligned by Author + ID.")
}

if (dplyr::n_distinct(control_segmented$Author) != expected_number_of_authors) {
  stop("The expected four Author cohorts were not found.")
}

non_numeric_robust_variables <- robust_variables[
  !vapply(control_segmented[robust_variables], is.numeric, logical(1))
]

if (length(non_numeric_robust_variables) > 0L) {
  stop(
    "The following robust variables are not numeric:\n",
    paste(non_numeric_robust_variables, collapse = ", ")
  )
}

X <- as.matrix(control_segmented[robust_variables])
storage.mode(X) <- "double"
rownames(X) <- control_segmented$Traceability_ID

if (!identical(dim(X), c(expected_number_of_fish, length(robust_variables)))) {
  stop("The robust behavioural matrix does not have dimensions 89 x 4.")
}

if (any(!is.finite(X))) {
  stop("The robust behavioural matrix contains non-finite values.")
}

# ------------------------------------------------------------
# 8. Audit robust normalisation within each Author
# ------------------------------------------------------------

author_levels <- sort(unique(control_segmented$Author))

robust_audit <- dplyr::bind_rows(lapply(
  author_levels,
  function(author_i) {
    author_data_i <- control_segmented[
      control_segmented$Author == author_i,
      robust_variables,
      drop = FALSE
    ]

    dplyr::bind_rows(lapply(
      robust_variables,
      function(variable_i) {
        values_i <- author_data_i[[variable_i]]

        tibble::tibble(
          Author = author_i,
          Variable = variable_i,
          n = length(values_i),
          Median = stats::median(values_i),
          Scaled_MAD = stats::mad(
            values_i,
            center = stats::median(values_i),
            constant = 1.4826
          )
        )
      }
    ))
  }
)) %>%
  dplyr::arrange(Author, match(Variable, robust_variables))

normalisation_tolerance <- 1e-8

if (
  nrow(robust_audit) !=
    expected_number_of_authors * length(robust_variables)
) {
  stop("The within-Author robust-normalisation audit has an unexpected size.")
}

if (any(!is.finite(as.matrix(robust_audit[c("Median", "Scaled_MAD")])))) {
  stop("The within-Author robust-normalisation audit contains non-finite values.")
}

if (any(abs(robust_audit$Median) > normalisation_tolerance)) {
  stop("At least one within-Author robust median differs from zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > normalisation_tolerance)) {
  stop("At least one within-Author scaled MAD differs from one.")
}

# ------------------------------------------------------------
# 9. Partition-comparison functions
# ------------------------------------------------------------

adjusted_rand_index <- function(partition_1, partition_2) {
  partition_1 <- as.integer(factor(partition_1))
  partition_2 <- as.integer(factor(partition_2))

  if (length(partition_1) != length(partition_2)) {
    stop("ARI partitions have different lengths.")
  }

  if (length(partition_1) < 2L) {
    stop("At least two observations are required to calculate ARI.")
  }

  if (anyNA(partition_1) || anyNA(partition_2)) {
    stop("Missing values were detected in an ARI partition.")
  }

  contingency_table <- table(partition_1, partition_2)
  combination_2 <- function(x) x * (x - 1) / 2

  sum_cells <- sum(combination_2(contingency_table))
  sum_rows <- sum(combination_2(rowSums(contingency_table)))
  sum_columns <- sum(combination_2(colSums(contingency_table)))
  total_pairs <- combination_2(sum(contingency_table))

  expected_index <- (sum_rows * sum_columns) / total_pairs
  maximum_index <- (sum_rows + sum_columns) / 2
  denominator <- maximum_index - expected_index

  if (abs(denominator) <= sqrt(.Machine$double.eps)) {
    same_relationships <- identical(
      outer(partition_1, partition_1, `==`),
      outer(partition_2, partition_2, `==`)
    )

    return(if (same_relationships) 1 else 0)
  }

  ari_value <- (sum_cells - expected_index) / denominator

  if (length(ari_value) != 1L || !is.finite(ari_value)) {
    stop("The adjusted Rand index is not finite.")
  }

  as.numeric(ari_value)
}

partitions_are_equivalent <- function(partition_1, partition_2) {
  if (length(partition_1) != length(partition_2)) {
    stop("Partition-equivalence vectors have different lengths.")
  }

  contingency_table <- table(partition_1, partition_2)

  isTRUE(
    all(rowSums(contingency_table > 0) == 1L) &&
      all(colSums(contingency_table > 0) == 1L)
  )
}

reference_membership <- as.integer(factor(
  as.character(segment_membership$Algorithmic_Segment),
  levels = sort(unique(as.character(segment_membership$Algorithmic_Segment)))
))
names(reference_membership) <- segment_membership$Traceability_ID

control_membership <- as.integer(factor(
  as.character(control_segmented$Algorithmic_Segment),
  levels = sort(unique(as.character(control_segmented$Algorithmic_Segment)))
))
names(control_membership) <- control_segmented$Traceability_ID

membership_ari <- adjusted_rand_index(
  reference_membership,
  control_membership
)

membership_equivalent <- partitions_are_equivalent(
  reference_membership,
  control_membership
)

if (
  abs(membership_ari - 1) > 1e-12 ||
    !membership_equivalent
) {
  stop("The two corrected-006 membership representations differ.")
}

if (length(unique(reference_membership)) != expected_number_of_segments) {
  stop("The corrected-006 membership does not contain two segments.")
}

reference_segment_sizes <- sort(as.integer(table(reference_membership)))

if (!identical(reference_segment_sizes, expected_segment_sizes)) {
  stop("The corrected-006 segmentation does not contain 40 and 49 fish.")
}

# ------------------------------------------------------------
# 10. Validate the inferential status inherited from 006.1
# ------------------------------------------------------------

required_inference_fields <- c(
  "Selected_k",
  "Number_of_algorithmic_segments",
  "Segment_sizes",
  "Observed_weighted_modularity",
  "Primary_empirical_p",
  "Alpha",
  "Evidence_of_discrete_communities",
  "Interpretation",
  "Permitted_use",
  "Prohibited_use"
)

missing_inference_fields <- setdiff(
  required_inference_fields,
  names(inference_status)
)

if (length(missing_inference_fields) > 0L) {
  stop(
    "The corrected-006 inference status lacks:\n",
    paste(missing_inference_fields, collapse = ", ")
  )
}

selected_k <- as.integer(inference_status$Selected_k)
number_of_segments <- as.integer(
  inference_status$Number_of_algorithmic_segments
)
stored_segment_sizes <- sort(as.integer(inference_status$Segment_sizes))
primary_empirical_p <- as.numeric(inference_status$Primary_empirical_p)
stored_alpha <- as.numeric(inference_status$Alpha)
evidence_of_discrete_communities <- isTRUE(
  inference_status$Evidence_of_discrete_communities
)

if (
  length(selected_k) != 1L ||
    selected_k != expected_selected_k ||
    length(number_of_segments) != 1L ||
    number_of_segments != expected_number_of_segments ||
    !identical(stored_segment_sizes, expected_segment_sizes)
) {
  stop("The corrected-006 selected segmentation differs from k = 45 and 40/49.")
}

if (
  length(primary_empirical_p) != 1L ||
    !is.finite(primary_empirical_p) ||
    primary_empirical_p < 0 ||
    primary_empirical_p > 1
) {
  stop("The selection-adjusted empirical p value is invalid.")
}

if (
  length(stored_alpha) != 1L ||
    !is.finite(stored_alpha) ||
    abs(stored_alpha - primary_alpha) > 1e-12
) {
  stop("The corrected-006 alpha differs from 0.05.")
}

if (
  evidence_of_discrete_communities ||
    primary_empirical_p < primary_alpha
) {
  stop(
    "The upstream inference no longer supports the continuous-space ",
    "interpretation required by this supplementary stability script."
  )
}

# ------------------------------------------------------------
# 11. Exact weighted union-kNN graph constructor
# ------------------------------------------------------------

construct_weighted_union_knn_graph <- function(
    data_matrix,
    k_value,
    vertex_names
) {
  data_matrix <- as.matrix(data_matrix)
  storage.mode(data_matrix) <- "double"

  number_of_rows <- nrow(data_matrix)

  if (number_of_rows < 2L || ncol(data_matrix) < 1L) {
    stop("The graph matrix has invalid dimensions.")
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

  if (k_value < 1L || k_value >= number_of_rows) {
    stop("k must be at least one and smaller than the number of rows.")
  }

  vertex_names <- as.character(vertex_names)

  if (
    length(vertex_names) != number_of_rows ||
      anyNA(vertex_names) ||
      any(vertex_names == "") ||
      anyDuplicated(vertex_names) > 0L
  ) {
    stop("Graph vertex names must be complete and unique.")
  }

  if (any(!is.finite(data_matrix))) {
    stop("The graph matrix contains non-finite values.")
  }

  distance_matrix <- as.matrix(stats::dist(
    data_matrix,
    method = "euclidean",
    diag = TRUE,
    upper = TRUE
  ))

  if (
    !identical(dim(distance_matrix), c(number_of_rows, number_of_rows)) ||
      any(!is.finite(distance_matrix)) ||
      any(distance_matrix < 0)
  ) {
    stop("The Euclidean distance matrix is invalid.")
  }

  diag(distance_matrix) <- Inf
  row_tie_breaker <- seq_len(number_of_rows)

  nearest_neighbour_index <- matrix(
    NA_integer_,
    nrow = number_of_rows,
    ncol = k_value
  )

  for (row_i in seq_len(number_of_rows)) {
    neighbour_order <- order(
      distance_matrix[row_i, ],
      row_tie_breaker,
      na.last = NA
    )

    nearest_neighbour_index[row_i, ] <- neighbour_order[seq_len(k_value)]
  }

  from_index <- rep(seq_len(number_of_rows), each = k_value)
  to_index <- as.vector(t(nearest_neighbour_index))
  neighbour_distance <- distance_matrix[cbind(from_index, to_index)]

  if (
    anyNA(nearest_neighbour_index) ||
      any(from_index == to_index) ||
      any(!is.finite(neighbour_distance)) ||
      any(neighbour_distance < 0)
  ) {
    stop("Invalid nearest-neighbour relations were generated.")
  }

  edge_list_directed <- tibble::tibble(
    From_Index = from_index,
    To_Index = to_index,
    From_ID = vertex_names[from_index],
    To_ID = vertex_names[to_index],
    Neighbour_Rank = rep(seq_len(k_value), times = number_of_rows),
    Distance = neighbour_distance,
    Similarity = 1 / (1 + neighbour_distance)
  )

  edge_list_undirected <- edge_list_directed %>%
    dplyr::mutate(
      Undirected_From = pmin(From_Index, To_Index),
      Undirected_To = pmax(From_Index, To_Index)
    ) %>%
    dplyr::group_by(Undirected_From, Undirected_To) %>%
    dplyr::summarise(
      Distance = mean(Distance),
      weight = mean(Similarity),
      Directed_Relations = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      from = vertex_names[Undirected_From],
      to = vertex_names[Undirected_To],
      .before = 1
    )

  if (
    nrow(edge_list_undirected) < 1L ||
      any(edge_list_undirected$from == edge_list_undirected$to) ||
      any(!is.finite(edge_list_undirected$weight)) ||
      any(edge_list_undirected$weight <= 0)
  ) {
    stop("The undirected weighted edge list is invalid.")
  }

  graph_knn <- igraph::graph_from_data_frame(
    d = edge_list_undirected %>%
      dplyr::select(from, to, weight, Distance, Directed_Relations),
    directed = FALSE,
    vertices = tibble::tibble(name = vertex_names)
  )

  if (
    igraph::is_directed(graph_knn) ||
      igraph::vcount(graph_knn) != number_of_rows ||
      any(igraph::degree(graph_knn) < 1L)
  ) {
    stop("The reconstructed kNN graph failed validation.")
  }

  list(
    graph = graph_knn,
    directed_relations = edge_list_directed,
    undirected_edges = edge_list_undirected
  )
}

canonical_graph_edges <- function(graph_object) {
  edge_table <- igraph::as_data_frame(graph_object, what = "edges")

  if (!all(c("from", "to", "weight") %in% names(edge_table))) {
    stop("A graph lacks from, to or weight edge information.")
  }

  edge_table %>%
    dplyr::mutate(
      original_from = as.character(from),
      original_to = as.character(to)
    ) %>%
    dplyr::transmute(
      from = pmin(original_from, original_to),
      to = pmax(original_from, original_to),
      weight = as.numeric(weight)
    ) %>%
    dplyr::arrange(from, to)
}

# ------------------------------------------------------------
# 12. Reconstruct and verify the full reference graph
# ------------------------------------------------------------

reference_graph_result <- construct_weighted_union_knn_graph(
  data_matrix = X,
  k_value = selected_k,
  vertex_names = control_segmented$Traceability_ID
)

reconstructed_reference_graph <- reference_graph_result$graph

if (!setequal(
  igraph::V(reconstructed_reference_graph)$name,
  igraph::V(stored_reference_graph)$name
)) {
  stop("The stored and reconstructed reference graphs contain different vertices.")
}

stored_edges <- canonical_graph_edges(stored_reference_graph)
reconstructed_edges <- canonical_graph_edges(reconstructed_reference_graph)

if (
  nrow(stored_edges) != nrow(reconstructed_edges) ||
    !identical(stored_edges$from, reconstructed_edges$from) ||
    !identical(stored_edges$to, reconstructed_edges$to)
) {
  stop("The stored and reconstructed reference graphs contain different edges.")
}

maximum_reference_edge_weight_difference <- max(abs(
  stored_edges$weight - reconstructed_edges$weight
))

if (
  !is.finite(maximum_reference_edge_weight_difference) ||
    maximum_reference_edge_weight_difference > 1e-12
) {
  stop("The stored and reconstructed reference graph weights differ.")
}

reference_membership_graph_order <- reference_membership[
  igraph::V(reconstructed_reference_graph)$name
]

if (anyNA(reference_membership_graph_order)) {
  stop("The reference membership could not be aligned with the graph.")
}

reference_modularity <- as.numeric(igraph::modularity(
  reconstructed_reference_graph,
  membership = reference_membership_graph_order,
  weights = igraph::E(reconstructed_reference_graph)$weight,
  directed = FALSE
))

required_graph_summary_columns <- c(
  "Number_of_fish",
  "Number_of_behavioural_variables",
  "Traceability_key",
  "Normalisation",
  "Distance",
  "Similarity",
  "Graph_type",
  "Selected_k",
  "Vertices",
  "Directed_kNN_relations",
  "Number_of_algorithmic_segments",
  "Weighted_modularity",
  "Primary_empirical_p",
  "Evidence_of_discrete_communities"
)

missing_graph_summary_columns <- setdiff(
  required_graph_summary_columns,
  names(graph_summary)
)

if (length(missing_graph_summary_columns) > 0L) {
  stop(
    "The corrected-006 graph summary lacks:\n",
    paste(missing_graph_summary_columns, collapse = ", ")
  )
}

stored_graph_modularity <- as.numeric(graph_summary$Weighted_modularity[[1]])
stored_inference_modularity <- as.numeric(
  inference_status$Observed_weighted_modularity
)

if (
  any(!is.finite(c(
    reference_modularity,
    stored_graph_modularity,
    stored_inference_modularity
  ))) ||
    abs(reference_modularity - stored_graph_modularity) > 1e-10 ||
    abs(reference_modularity - stored_inference_modularity) > 1e-10
) {
  stop("The reference modularity differs across corrected-006 objects.")
}

if (
  as.integer(graph_summary$Number_of_fish[[1]]) != expected_number_of_fish ||
    as.integer(graph_summary$Number_of_behavioural_variables[[1]]) !=
      length(robust_variables) ||
    as.character(graph_summary$Traceability_key[[1]]) != "Author + ID" ||
    as.character(graph_summary$Normalisation[[1]]) !=
      "Within Author: (x - median) / (1.4826 x raw MAD)" ||
    as.character(graph_summary$Distance[[1]]) != "Euclidean" ||
    as.character(graph_summary$Similarity[[1]]) != "1 / (1 + D)" ||
    as.character(graph_summary$Graph_type[[1]]) !=
      "Weighted, undirected union kNN" ||
    as.integer(graph_summary$Selected_k[[1]]) != selected_k ||
    as.integer(graph_summary$Vertices[[1]]) != expected_number_of_fish ||
    as.integer(graph_summary$Directed_kNN_relations[[1]]) !=
      expected_number_of_fish * selected_k ||
    as.integer(graph_summary$Number_of_algorithmic_segments[[1]]) !=
      expected_number_of_segments ||
    abs(as.numeric(graph_summary$Primary_empirical_p[[1]]) -
      primary_empirical_p) > 1e-12 ||
    isTRUE(graph_summary$Evidence_of_discrete_communities[[1]])
) {
  stop("The corrected-006 graph-summary contract failed.")
}

# ------------------------------------------------------------
# 13. Stratified subsampling and graph-scale parameters
# ------------------------------------------------------------

subsample_fraction <- 0.80
number_of_iterations <- 1000L
number_of_louvain_seed_runs <- 25L
subsample_seed_base <- 20260803L
louvain_seed_base <- 720260803L
progress_interval <- 25L

if (
  subsample_fraction <= 0 ||
    subsample_fraction >= 1 ||
    number_of_iterations < 100L ||
    number_of_louvain_seed_runs < 2L
) {
  stop("The subsampling or Louvain-run parameters are invalid.")
}

target_subsample_n <- as.integer(round(
  expected_number_of_fish * subsample_fraction
))

cohort_allocation <- control_segmented %>%
  dplyr::count(Author, name = "Cohort_n") %>%
  dplyr::arrange(Author) %>%
  dplyr::mutate(
    Ideal_subsample_n = Cohort_n * target_subsample_n /
      expected_number_of_fish,
    Subsample_n = floor(Ideal_subsample_n),
    Remainder = Ideal_subsample_n - Subsample_n
  )

remaining_positions <- target_subsample_n - sum(cohort_allocation$Subsample_n)

if (remaining_positions > 0L) {
  allocation_order <- order(
    -cohort_allocation$Remainder,
    cohort_allocation$Author
  )
