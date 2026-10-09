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

base_dir <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

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

  increment_positions <- allocation_order[seq_len(remaining_positions)]
  cohort_allocation$Subsample_n[increment_positions] <-
    cohort_allocation$Subsample_n[increment_positions] + 1L
}

cohort_allocation <- cohort_allocation %>%
  dplyr::mutate(
    Retained_percentage = 100 * Subsample_n / Cohort_n
  )

if (
  sum(cohort_allocation$Subsample_n) != target_subsample_n ||
    any(cohort_allocation$Subsample_n < 2L) ||
    any(cohort_allocation$Subsample_n >= cohort_allocation$Cohort_n)
) {
  stop("The stratified subsample allocation is invalid.")
}

proportional_k <- as.integer(round(
  selected_k * (target_subsample_n - 1) /
    (expected_number_of_fish - 1)
))

method_configuration <- tibble::tibble(
  Method = c("Fixed k", "Proportional k"),
  Role = c(
    "Primary conditional stability",
    "Graph-scale sensitivity analysis"
  ),
  k = c(selected_k, proportional_k),
  Rationale = c(
    paste0(
      "Retains the selected k = ",
      selected_k,
      " hyperparameter in every subsample"
    ),
    paste0(
      "Preserves approximately k/(n-1); k = ",
      proportional_k,
      " for n = ",
      target_subsample_n
    )
  )
)

if (
  any(method_configuration$k < 1L) ||
    any(method_configuration$k >= target_subsample_n) ||
    anyDuplicated(method_configuration$Method) > 0L
) {
  stop("The graph-scale method configuration is invalid.")
}

if (
  subsample_seed_base + number_of_iterations > .Machine$integer.max ||
    louvain_seed_base +
      number_of_iterations * nrow(method_configuration) *
      number_of_louvain_seed_runs > .Machine$integer.max
) {
  stop("The deterministic seeds exceed the R integer range.")
}

RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

# ------------------------------------------------------------
# 14. Representative Louvain solution across deterministic seeds
# ------------------------------------------------------------

fit_representative_louvain <- function(
    data_matrix,
    vertex_names,
    k_value,
    seed_values
) {
  graph_result <- construct_weighted_union_knn_graph(
    data_matrix = data_matrix,
    k_value = k_value,
    vertex_names = vertex_names
  )

  graph_object <- graph_result$graph
  edge_weights <- igraph::E(graph_object)$weight
  number_of_runs <- length(seed_values)

  seed_memberships <- matrix(
    NA_integer_,
    nrow = nrow(data_matrix),
    ncol = number_of_runs,
    dimnames = list(vertex_names, as.character(seed_values))
  )

  seed_modularity <- numeric(number_of_runs)
  seed_segment_counts <- integer(number_of_runs)

  for (seed_position in seq_along(seed_values)) {
    set.seed(seed_values[seed_position])

    louvain_model <- igraph::cluster_louvain(
      graph_object,
      weights = edge_weights
    )

    membership_seed <- as.integer(igraph::membership(louvain_model))
    names(membership_seed) <- igraph::V(graph_object)$name
    membership_seed <- membership_seed[vertex_names]

    if (anyNA(membership_seed)) {
      stop("A Louvain membership could not be aligned with its vertices.")
    }

    seed_memberships[, seed_position] <- membership_seed
    seed_segment_counts[seed_position] <- length(unique(membership_seed))
    seed_modularity[seed_position] <- as.numeric(igraph::modularity(
      graph_object,
      membership = membership_seed[
        igraph::V(graph_object)$name
      ],
      weights = edge_weights,
      directed = FALSE
    ))
  }

  seed_pairwise_ari <- matrix(
    1,
    nrow = number_of_runs,
    ncol = number_of_runs
  )

  for (seed_i in seq_len(number_of_runs - 1L)) {
    for (seed_j in seq.int(seed_i + 1L, number_of_runs)) {
      ari_ij <- adjusted_rand_index(
        seed_memberships[, seed_i],
        seed_memberships[, seed_j]
      )

      seed_pairwise_ari[seed_i, seed_j] <- ari_ij
      seed_pairwise_ari[seed_j, seed_i] <- ari_ij
    }
  }

  mean_ari_to_other_runs <- vapply(
    seq_len(number_of_runs),
    function(seed_position) {
      mean(seed_pairwise_ari[seed_position, -seed_position])
    },
    numeric(1)
  )

  exact_tolerance <- 1e-12
  representative_candidates <- which(
    abs(mean_ari_to_other_runs - max(mean_ari_to_other_runs)) <=
      exact_tolerance
  )

  if (length(representative_candidates) > 1L) {
    candidate_modularity <- seed_modularity[representative_candidates]
    representative_candidates <- representative_candidates[
      abs(candidate_modularity - max(candidate_modularity)) <=
        exact_tolerance
    ]
  }

  representative_position <- min(representative_candidates)
  representative_membership <- seed_memberships[, representative_position]

  ari_to_representative <- vapply(
    seq_len(number_of_runs),
    function(seed_position) {
      adjusted_rand_index(
        seed_memberships[, seed_position],
        representative_membership
      )
    },
    numeric(1)
  )

  pairwise_ari_values <- seed_pairwise_ari[upper.tri(seed_pairwise_ari)]
  segment_count_table <- sort(table(seed_segment_counts), decreasing = TRUE)
  modal_segment_count <- as.integer(names(segment_count_table)[1])

  graph_components <- igraph::components(graph_object)

  list(
    membership = representative_membership,
    representative_seed = as.integer(seed_values[representative_position]),
    representative_segments = length(unique(representative_membership)),
    representative_modularity = seed_modularity[representative_position],
    representative_mean_ari_to_other_runs =
      mean_ari_to_other_runs[representative_position],
    mean_ari_to_representative = mean(ari_to_representative),
    median_pairwise_seed_ari = stats::median(pairwise_ari_values),
    minimum_pairwise_seed_ari = min(pairwise_ari_values),
    percentile_2_5_pairwise_seed_ari = unname(stats::quantile(
      pairwise_ari_values,
      0.025,
      type = 7
    )),
    exact_representative_recovery_rate = mean(
      ari_to_representative >= 1 - exact_tolerance
    ),
    modal_seed_segments = modal_segment_count,
    modal_seed_segment_rate = as.numeric(segment_count_table[1]) /
      number_of_runs,
    directed_relations = nrow(graph_result$directed_relations),
    edges = igraph::ecount(graph_object),
    graph_density = igraph::edge_density(graph_object, loops = FALSE),
    mean_degree = mean(igraph::degree(graph_object)),
    connected_components = graph_components$no,
    connected = igraph::is_connected(graph_object)
  )
}

# ------------------------------------------------------------
# 15. Run one stratified subsampling iteration
# ------------------------------------------------------------

failure_result_row <- function(
    iteration,
    method_name,
    k_value,
    subsample_seed,
    first_louvain_seed,
    last_louvain_seed,
    error_message
) {
  tibble::tibble(
    Iteration = as.integer(iteration),
    Method = as.character(method_name),
    k = as.integer(k_value),
    Success = FALSE,
    ARI_to_reference_subset = NA_real_,
    Subsample_n = as.integer(target_subsample_n),
    Duplicated_vertices = 0L,
    Reference_segments_present = NA_integer_,
    Recovered_segments = NA_integer_,
    Two_segment_solution = NA,
    Weighted_modularity = NA_real_,
    Representative_mean_ARI_to_other_runs = NA_real_,
    Mean_ARI_to_representative = NA_real_,
    Median_pairwise_seed_ARI = NA_real_,
    Minimum_pairwise_seed_ARI = NA_real_,
    Pairwise_seed_ARI_percentile_2_5 = NA_real_,
    Exact_representative_recovery_rate = NA_real_,
    Modal_seed_segments = NA_integer_,
    Modal_seed_segment_rate = NA_real_,
    Directed_kNN_relations = NA_integer_,
    Edges = NA_integer_,
    Graph_density = NA_real_,
    Mean_degree = NA_real_,
    Connected_components = NA_integer_,
    Connected = NA,
    Subsample_seed = as.integer(subsample_seed),
    Representative_Louvain_seed = NA_integer_,
    First_Louvain_seed = as.integer(first_louvain_seed),
    Last_Louvain_seed = as.integer(last_louvain_seed),
    Error_Message = as.character(error_message)
  )
}

run_subsampling_iteration <- function(iteration) {
  iteration_subsample_seed <- as.integer(
    subsample_seed_base + iteration - 1L
  )

  set.seed(iteration_subsample_seed)

  selected_indices <- unlist(lapply(
    seq_len(nrow(cohort_allocation)),
    function(cohort_position) {
      author_i <- cohort_allocation$Author[cohort_position]
      author_indices <- which(control_segmented$Author == author_i)

      sample(
        author_indices,
        size = cohort_allocation$Subsample_n[cohort_position],
        replace = FALSE
      )
    }
  ), use.names = FALSE)

  selected_indices <- sort(as.integer(selected_indices))

  if (
    length(selected_indices) != target_subsample_n ||
      anyDuplicated(selected_indices) > 0L
  ) {
    stop("A stratified subsample contains an invalid set of fish.")
  }

  subsample_matrix <- X[selected_indices, , drop = FALSE]
  subsample_ids <- control_segmented$Traceability_ID[selected_indices]
  reference_subset <- reference_membership[subsample_ids]

  if (
    anyNA(reference_subset) ||
      length(unique(reference_subset)) != expected_number_of_segments
  ) {
    stop("A subsample does not retain both reference segments.")
  }

  result_rows <- vector("list", nrow(method_configuration))
  membership_results <- vector("list", nrow(method_configuration))
  names(membership_results) <- method_configuration$Method

  for (method_position in seq_len(nrow(method_configuration))) {
    method_name <- method_configuration$Method[method_position]
    method_k <- method_configuration$k[method_position]

    seed_offset <- (
      (iteration - 1L) * nrow(method_configuration) +
        (method_position - 1L)
    ) * number_of_louvain_seed_runs

    seed_values <- as.integer(
      louvain_seed_base +
        seed_offset +
        seq_len(number_of_louvain_seed_runs) -
        1L
    )

    method_output <- tryCatch({
      representative_fit <- fit_representative_louvain(
        data_matrix = subsample_matrix,
        vertex_names = subsample_ids,
        k_value = method_k,
        seed_values = seed_values
      )

      ari_to_reference <- adjusted_rand_index(
        reference_subset,
        representative_fit$membership
      )

      list(
        result = tibble::tibble(
          Iteration = as.integer(iteration),
          Method = method_name,
          k = as.integer(method_k),
          Success = TRUE,
          ARI_to_reference_subset = ari_to_reference,
          Subsample_n = as.integer(target_subsample_n),
          Duplicated_vertices = 0L,
          Reference_segments_present = length(unique(reference_subset)),
          Recovered_segments = representative_fit$representative_segments,
          Two_segment_solution =
            representative_fit$representative_segments ==
            expected_number_of_segments,
          Weighted_modularity =
            representative_fit$representative_modularity,
          Representative_mean_ARI_to_other_runs =
            representative_fit$representative_mean_ari_to_other_runs,
          Mean_ARI_to_representative =
            representative_fit$mean_ari_to_representative,
          Median_pairwise_seed_ARI =
            representative_fit$median_pairwise_seed_ari,
          Minimum_pairwise_seed_ARI =
            representative_fit$minimum_pairwise_seed_ari,
          Pairwise_seed_ARI_percentile_2_5 =
            representative_fit$percentile_2_5_pairwise_seed_ari,
          Exact_representative_recovery_rate =
            representative_fit$exact_representative_recovery_rate,
          Modal_seed_segments =
            representative_fit$modal_seed_segments,
          Modal_seed_segment_rate =
            representative_fit$modal_seed_segment_rate,
          Directed_kNN_relations =
            representative_fit$directed_relations,
          Edges = representative_fit$edges,
          Graph_density = representative_fit$graph_density,
          Mean_degree = representative_fit$mean_degree,
          Connected_components = representative_fit$connected_components,
          Connected = representative_fit$connected,
          Subsample_seed = iteration_subsample_seed,
          Representative_Louvain_seed =
            representative_fit$representative_seed,
          First_Louvain_seed = min(seed_values),
          Last_Louvain_seed = max(seed_values),
          Error_Message = NA_character_
        ),
        membership = tibble::tibble(
          Traceability_ID = subsample_ids,
          Reference_Segment = as.integer(reference_subset),
          Resampled_Segment = as.integer(representative_fit$membership)
        )
      )
    }, error = function(error_condition) {
      list(
        result = failure_result_row(
          iteration = iteration,
          method_name = method_name,
          k_value = method_k,
          subsample_seed = iteration_subsample_seed,
          first_louvain_seed = min(seed_values),
          last_louvain_seed = max(seed_values),
          error_message = conditionMessage(error_condition)
        ),
        membership = NULL
      )
    })

    result_rows[[method_position]] <- method_output$result
    membership_results[[method_position]] <- method_output$membership
  }

  list(
    results = dplyr::bind_rows(result_rows),
    memberships = membership_results
  )
}

# ------------------------------------------------------------
# 16. Run all subsampling iterations
# ------------------------------------------------------------

iteration_results_list <- vector("list", number_of_iterations)
iteration_memberships <- vector("list", number_of_iterations)

cat(
  "\nStarting ",
  number_of_iterations,
  " stratified subsampling iterations (",
  nrow(method_configuration),
  " graph-scale analyses per iteration)...\n",
  sep = ""
)

for (iteration_i in seq_len(number_of_iterations)) {
  iteration_output <- tryCatch(
    run_subsampling_iteration(iteration_i),
    error = function(error_condition) {
      failure_rows <- dplyr::bind_rows(lapply(
        seq_len(nrow(method_configuration)),
        function(method_position) {
          seed_offset <- (
            (iteration_i - 1L) * nrow(method_configuration) +
              (method_position - 1L)
          ) * number_of_louvain_seed_runs

          seed_values <- as.integer(
            louvain_seed_base +
              seed_offset +
              seq_len(number_of_louvain_seed_runs) -
              1L
          )

          failure_result_row(
            iteration = iteration_i,
            method_name = method_configuration$Method[method_position],
            k_value = method_configuration$k[method_position],
            subsample_seed = subsample_seed_base + iteration_i - 1L,
            first_louvain_seed = min(seed_values),
            last_louvain_seed = max(seed_values),
            error_message = conditionMessage(error_condition)
          )
        }
      ))

      list(
        results = failure_rows,
        memberships = stats::setNames(
          vector("list", nrow(method_configuration)),
          method_configuration$Method
        )
      )
    }
  )

  iteration_results_list[[iteration_i]] <- iteration_output$results
  iteration_memberships[[iteration_i]] <- iteration_output$memberships

  if (
    iteration_i == 1L ||
      iteration_i %% progress_interval == 0L ||
      iteration_i == number_of_iterations
  ) {
    cat(
      "Completed subsampling iteration ",
      iteration_i,
      " of ",
      number_of_iterations,
      ".\n",
      sep = ""
    )
  }
}

subsampling_results <- dplyr::bind_rows(iteration_results_list) %>%
  dplyr::arrange(Iteration, match(Method, method_configuration$Method))

# ------------------------------------------------------------
# 17. Validate iteration-level results and failures
# ------------------------------------------------------------

expected_result_rows <- number_of_iterations * nrow(method_configuration)

if (nrow(subsampling_results) != expected_result_rows) {
  stop("The iteration-level result table has an unexpected number of rows.")
}

expected_iteration_method_grid <- tibble::tibble(
  Iteration = rep(
    seq_len(number_of_iterations),
    each = nrow(method_configuration)
  ),
  Method = rep(
    method_configuration$Method,
    times = number_of_iterations
  )
)

if (
  !identical(
    as.integer(subsampling_results$Iteration),
    as.integer(expected_iteration_method_grid$Iteration)
  ) ||
    !identical(
      as.character(subsampling_results$Method),
      as.character(expected_iteration_method_grid$Method)
    )
) {
  stop("The iteration-by-method grid is incomplete or incorrectly ordered.")
}

iterations_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingIterations.csv"
)

failed_iterations_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingFailures.csv"
)

iterations_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_iterations.rds"
)

memberships_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_memberships.rds"
)

utils::write.csv(subsampling_results, iterations_csv, row.names = FALSE)
saveRDS(subsampling_results, iterations_rds)
saveRDS(iteration_memberships, memberships_rds)

failed_iterations <- subsampling_results %>%
  dplyr::filter(!Success)

if (nrow(failed_iterations) > 0L) {
  utils::write.csv(failed_iterations, failed_iterations_csv, row.names = FALSE)

  stop(
    nrow(failed_iterations),
    " method-specific subsampling fits failed. Diagnostics were saved at:\n",
    normalizePath(failed_iterations_csv, winslash = "/", mustWork = FALSE),
    "\nNo final stability summary was accepted."
  )
}

successful_results <- subsampling_results

finite_result_columns <- c(
  "ARI_to_reference_subset",
  "Weighted_modularity",
  "Representative_mean_ARI_to_other_runs",
  "Mean_ARI_to_representative",
  "Median_pairwise_seed_ARI",
  "Minimum_pairwise_seed_ARI",
  "Pairwise_seed_ARI_percentile_2_5",
  "Exact_representative_recovery_rate",
  "Modal_seed_segment_rate",
  "Graph_density",
  "Mean_degree"
)

for (column_i in finite_result_columns) {
  if (any(!is.finite(successful_results[[column_i]]))) {
    stop("Non-finite values were detected in ", column_i, ".")
  }
}

ari_columns <- c(
  "ARI_to_reference_subset",
  "Representative_mean_ARI_to_other_runs",
  "Mean_ARI_to_representative",
  "Median_pairwise_seed_ARI",
  "Minimum_pairwise_seed_ARI",
  "Pairwise_seed_ARI_percentile_2_5"
)

for (column_i in ari_columns) {
  if (
    any(successful_results[[column_i]] < -1 - 1e-10) ||
      any(successful_results[[column_i]] > 1 + 1e-10)
  ) {
    stop("Values outside the valid ARI range were detected in ", column_i, ".")
  }
}

if (
  any(successful_results$Subsample_n != target_subsample_n) ||
    any(successful_results$Duplicated_vertices != 0L) ||
    any(successful_results$Reference_segments_present !=
      expected_number_of_segments) ||
    any(successful_results$Directed_kNN_relations !=
      successful_results$Subsample_n * successful_results$k) ||
    any(successful_results$Recovered_segments < 1L)
) {
  stop("At least one successful subsampling fit violates the design contract.")
}

# ------------------------------------------------------------
# 18. Pairwise co-segmentation consensus
# ------------------------------------------------------------

number_of_methods <- nrow(method_configuration)
all_ids <- control_segmented$Traceability_ID

co_sampled_counts <- stats::setNames(
  lapply(seq_len(number_of_methods), function(x) {
    matrix(
      0L,
      nrow = expected_number_of_fish,
      ncol = expected_number_of_fish,
      dimnames = list(all_ids, all_ids)
    )
  }),
  method_configuration$Method
)

co_segmented_counts <- stats::setNames(
  lapply(seq_len(number_of_methods), function(x) {
    matrix(
      0L,
      nrow = expected_number_of_fish,
      ncol = expected_number_of_fish,
      dimnames = list(all_ids, all_ids)
    )
  }),
  method_configuration$Method
)

for (iteration_i in seq_len(number_of_iterations)) {
  for (method_name in method_configuration$Method) {
    membership_i <- iteration_memberships[[iteration_i]][[method_name]]

    if (is.null(membership_i)) {
      stop("A successful iteration lacks stored representative memberships.")
    }

    matrix_indices <- match(membership_i$Traceability_ID, all_ids)

    if (anyNA(matrix_indices) || anyDuplicated(matrix_indices) > 0L) {
      stop("A stored subsample membership contains invalid fish keys.")
    }

    co_sampled_counts[[method_name]][matrix_indices, matrix_indices] <-
      co_sampled_counts[[method_name]][matrix_indices, matrix_indices] + 1L

    same_segment_i <- outer(
      membership_i$Resampled_Segment,
      membership_i$Resampled_Segment,
      `==`
    )

    co_segmented_counts[[method_name]][matrix_indices, matrix_indices] <-
      co_segmented_counts[[method_name]][matrix_indices, matrix_indices] +
      same_segment_i
  }
}

upper_pair_indices <- which(
  upper.tri(matrix(
    FALSE,
    nrow = expected_number_of_fish,
    ncol = expected_number_of_fish
  )),
  arr.ind = TRUE
)

pairwise_consensus <- dplyr::bind_rows(lapply(
  method_configuration$Method,
  function(method_name) {
    co_sampled_i <- co_sampled_counts[[method_name]][upper_pair_indices]
    co_segmented_i <- co_segmented_counts[[method_name]][upper_pair_indices]

    if (any(co_sampled_i < 1L)) {
      stop("At least one fish pair was never observed together.")
    }

    tibble::tibble(
      Method = method_name,
      Fish_1_Author = control_segmented$Author[upper_pair_indices[, 1]],
      Fish_1_ID = control_segmented$ID[upper_pair_indices[, 1]],
      Fish_1_Traceability_ID = all_ids[upper_pair_indices[, 1]],
      Fish_2_Author = control_segmented$Author[upper_pair_indices[, 2]],
      Fish_2_ID = control_segmented$ID[upper_pair_indices[, 2]],
      Fish_2_Traceability_ID = all_ids[upper_pair_indices[, 2]],
      Reference_same_segment =
        reference_membership[upper_pair_indices[, 1]] ==
        reference_membership[upper_pair_indices[, 2]],
      Co_sampled_iterations = as.integer(co_sampled_i),
      Co_segmented_together_iterations = as.integer(co_segmented_i),
      Co_segmentation_probability = co_segmented_i / co_sampled_i
    )
  }
))

if (
  any(!is.finite(pairwise_consensus$Co_segmentation_probability)) ||
    any(pairwise_consensus$Co_segmentation_probability < 0) ||
    any(pairwise_consensus$Co_segmentation_probability > 1)
) {
  stop("Invalid pairwise co-segmentation probabilities were generated.")
}

# ------------------------------------------------------------
# 19. Descriptive stability summaries
# ------------------------------------------------------------

quantiles_ari <- function(x) {
  stats::quantile(
    x,
    probs = c(0.025, 0.25, 0.50, 0.75, 0.975),
    names = FALSE,
    type = 7
  )
}

stability_summary <- successful_results %>%
  dplyr::group_by(Method, k) %>%
  dplyr::summarise(
    Iterations = dplyr::n(),
    Mean_ARI = mean(ARI_to_reference_subset),
    SD_ARI = stats::sd(ARI_to_reference_subset),
    Minimum_ARI = min(ARI_to_reference_subset),
    ARI_percentile_2_5 = quantiles_ari(ARI_to_reference_subset)[1],
    Q1_ARI = quantiles_ari(ARI_to_reference_subset)[2],
    Median_ARI = quantiles_ari(ARI_to_reference_subset)[3],
    Q3_ARI = quantiles_ari(ARI_to_reference_subset)[4],
    ARI_percentile_97_5 = quantiles_ari(ARI_to_reference_subset)[5],
    Maximum_ARI = max(ARI_to_reference_subset),
    Two_segment_recovery_rate = mean(Two_segment_solution),
    Mean_recovered_segments = mean(Recovered_segments),
    Mean_median_pairwise_seed_ARI = mean(Median_pairwise_seed_ARI),
    Mean_minimum_pairwise_seed_ARI = mean(Minimum_pairwise_seed_ARI),
    Mean_exact_representative_recovery_rate = mean(
      Exact_representative_recovery_rate
    ),
    Connected_graph_rate = mean(Connected),
    Mean_weighted_modularity = mean(Weighted_modularity),
    SD_weighted_modularity = stats::sd(Weighted_modularity),
    .groups = "drop"
  ) %>%
  dplyr::arrange(match(Method, method_configuration$Method))

segment_count_distribution <- successful_results %>%
  dplyr::count(Method, k, Recovered_segments, name = "Iterations") %>%
  dplyr::group_by(Method, k) %>%
  dplyr::mutate(Percentage = 100 * Iterations / sum(Iterations)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    match(Method, method_configuration$Method),
    Recovered_segments
  )

pairwise_consensus_summary <- pairwise_consensus %>%
  dplyr::mutate(
    Reference_relationship = ifelse(
      Reference_same_segment,
      "Same reference segment",
      "Different reference segments"
    )
  ) %>%
  dplyr::group_by(Method, Reference_relationship) %>%
  dplyr::summarise(
    Fish_pairs = dplyr::n(),
    Mean_co_sampled_iterations = mean(Co_sampled_iterations),
    Minimum_co_sampled_iterations = min(Co_sampled_iterations),
    Mean_co_segmentation_probability = mean(Co_segmentation_probability),
    SD_co_segmentation_probability = stats::sd(Co_segmentation_probability),
    Q1_co_segmentation_probability = unname(stats::quantile(
      Co_segmentation_probability,
      0.25,
      type = 7
    )),
    Median_co_segmentation_probability = stats::median(
      Co_segmentation_probability
    ),
    Q3_co_segmentation_probability = unname(stats::quantile(
      Co_segmentation_probability,
      0.75,
      type = 7
    )),
    Minimum_co_segmentation_probability = min(Co_segmentation_probability),
    Maximum_co_segmentation_probability = max(Co_segmentation_probability),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    match(Method, method_configuration$Method),
    dplyr::desc(Reference_relationship)
  )

reference_validation <- tibble::tibble(
  Check = c(
    "Control fish",
    "Author cohorts",
    "Traceability key",
    "Selected k",
    "Reference algorithmic segments",
    "Reference segment sizes",
    "Membership agreement across corrected-006 objects",
    "Maximum stored-vs-reconstructed edge-weight difference",
    "Reference weighted modularity",
    "Maximum modularity difference across corrected-006 objects",
    "Primary selection-adjusted empirical p",
    "Evidence of discrete communities",
    "Duplicated vertices introduced by subsampling"
  ),
  Result = c(
    as.character(expected_number_of_fish),
    as.character(expected_number_of_authors),
    "Author + ID",
    as.character(selected_k),
    as.character(expected_number_of_segments),
    paste(expected_segment_sizes, collapse = "/"),
    "Exact",
    format(
      maximum_reference_edge_weight_difference,
      scientific = TRUE,
      digits = 6
    ),
    format(reference_modularity, digits = 16),
    format(
      max(abs(c(
        reference_modularity - stored_graph_modularity,
        reference_modularity - stored_inference_modularity
      ))),
      scientific = TRUE,
      digits = 6
    ),
    sprintf("%.9f", primary_empirical_p),
    "FALSE",
    "NONE"
  )
)

analysis_configuration <- tibble::tibble(
  Parameter = c(
    "Analytical status",
    "Scientific question",
    "Inferential question excluded",
    "Reference sample",
    "Traceability",
    "Reference segmentation",
    "Primary upstream empirical p",
    "Resampling method",
    "Stratification",
    "Subsample fraction",
    "Subsample fish",
    "Iterations",
    "Duplicated graph vertices",
    "Input coordinates",
    "Distance",
    "Similarity",
    "Graph",
    "Primary graph-scale analysis",
    "Secondary graph-scale analysis",
    "Louvain runs per graph",
    "Representative-partition rule",
    "Primary descriptive metric",
    "Permitted interpretation",
    "Prohibited interpretation"
  ),
  Value = c(
    "Supplementary conditional algorithmic-stability analysis",
    paste0(
      "How reproducibly does the exploratory segmentation reappear ",
      "after stratified removal of individual fish?"
    ),
    "Whether discrete behavioural communities exist; tested only by 006.1",
    "89 LAPCOM control fish",
    "Author + ID",
    "k = 45; two algorithmic segments of 40 and 49 fish",
    sprintf("%.9f", primary_empirical_p),
    "Stratified subsampling without replacement",
    "Within Author",
    sprintf("%.2f", subsample_fraction),
    as.character(target_subsample_n),
    as.character(number_of_iterations),
    "None",
    paste0(
      "Fixed within-Author robust coordinates from 005; ",
      "no re-normalisation inside subsamples"
    ),
    "Euclidean",
    "1 / (1 + distance)",
    "Weighted, undirected union kNN",
    paste0("Fixed selected k = ", selected_k),
    paste0("Proportional-neighbour k = ", proportional_k),
    as.character(number_of_louvain_seed_runs),
    paste0(
      "Maximum mean ARI to other seed runs; modularity and seed order ",
      "break exact ties"
    ),
    "ARI to the original exploratory segmentation restricted to the same fish",
    paste0(
      "Conditional reproducibility of an algorithmic division of a ",
      "continuous behavioural space"
    ),
    paste0(
      "Evidence of discrete communities, biological validation, ",
      "phenotype labels, archetypes or classification thresholds"
    )
  )
)

analysis_notes <- tibble::tibble(
  Note = c(
    paste0(
      "The primary 006.1 selection-adjusted empirical p is ",
      sprintf("%.6f", primary_empirical_p),
      "; therefore, this stability analysis cannot establish discreteness."
    ),
    paste0(
      "Subsampling without replacement was used because ordinary case ",
      "bootstrap copies would create repeated, zero-distance graph vertices."
    ),
    paste0(
      "The fixed-k analysis conditions on the selected k = 45 ",
      "hyperparameter and is the primary stability description."
    ),
    paste0(
      "The proportional-k analysis is a sensitivity analysis for the ",
      "change in graph density induced by reducing n from 89 to ",
      target_subsample_n,
      "."
    ),
    paste0(
      "The same stratified subsample is used for both graph-scale methods ",
      "within each iteration, permitting direct descriptive comparison."
    ),
    paste0(
      "No universal ARI cut-off, p value, confidence interval or ",
      "biological classification is imposed."
    ),
    paste0(
      "High stability would mean that Louvain reproducibly divides this ",
      "continuous space; it would not overturn the negative 006.1 result."
    )
  )
)

# ------------------------------------------------------------
# 20. Historical superseded outputs: detect but do not modify
# ------------------------------------------------------------

historical_candidates <- c(
  file.path(tables_dir, "ClusterStability_BootstrapIterations.csv"),
  file.path(tables_dir, "ClusterStability_BootstrapSummary.csv"),
  file.path(tables_dir, "ClusterStability_BootstrapConfiguration.csv"),
  file.path(tables_dir, "ClusterStability_BootstrapClusterCounts.csv"),
  file.path(tables_dir, "ClusterStability_ReferenceValidation.csv"),
  file.path(tables_dir, "ClusterStability_AnalysisNotes.csv"),
  file.path(tables_dir, "ClusterStability_Bootstrap.xlsx"),
  file.path(objects_dir, "objective1_cluster_stability_bootstrap_iterations.rds"),
  file.path(objects_dir, "objective1_cluster_stability_bootstrap_summary.rds"),
  file.path(objects_dir, "objective1_cluster_stability_bootstrap_configuration.rds"),
  file.path(objects_dir, "objective1_cluster_stability_cluster_counts.rds"),
  file.path(objects_dir, "objective1_cluster_stability_reference_validation.rds")
)

existing_historical_outputs <- historical_candidates[
  file.exists(historical_candidates)
]

historical_md5_before <- if (length(existing_historical_outputs) > 0L) {
  tools::md5sum(existing_historical_outputs)
} else {
  character(0)
}

# ------------------------------------------------------------
# 21. Export corrected supplementary stability outputs
# ------------------------------------------------------------

summary_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingSummary.csv"
)

segment_counts_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingSegmentCounts.csv"
)

pairwise_summary_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_PairwiseConsensusSummary.csv"
)

pairwise_consensus_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_PairwiseConsensus.csv"
)

configuration_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingConfiguration.csv"
)

reference_validation_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingReferenceValidation.csv"
)

dependency_audit_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingDependencyAudit.csv"
)

excel_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_SubsamplingStability.xlsx"
)

summary_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_summary.rds"
)

configuration_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_configuration.rds"
)

pairwise_consensus_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_pairwise_consensus.rds"
)

complete_results_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_complete_results.rds"
)

session_info_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_subsampling_sessionInfo.txt"
)

dependency_audit <- tibble::tibble(
  Dependency = c(
    "Corrected 006 segmented controls",
    "Corrected 006 exploratory membership",
    "Corrected 006 exploratory graph",
    "Corrected 006 graph summary",
    "Corrected 006 inference status",
    "Corrected 006 dependency audit"
  ),
  File = required_input_files,
  MD5 = unname(tools::md5sum(required_input_files)),
  Modified = as.character(file.info(required_input_files)$mtime)
)

complete_results <- list(
  Status = "Supplementary conditional algorithmic-stability analysis",
  Primary_selection_adjusted_empirical_p = primary_empirical_p,
  Evidence_of_discrete_communities = FALSE,
  Reference_validation = reference_validation,
  Method_configuration = method_configuration,
  Cohort_allocation = cohort_allocation,
  Stability_summary = stability_summary,
  Segment_count_distribution = segment_count_distribution,
  Pairwise_consensus_summary = pairwise_consensus_summary,
  Pairwise_consensus = pairwise_consensus,
  Iteration_results = subsampling_results,
  Iteration_memberships = iteration_memberships,
  Analysis_configuration = analysis_configuration,
  Analysis_notes = analysis_notes,
  Dependency_audit = dependency_audit
)

utils::write.csv(stability_summary, summary_csv, row.names = FALSE)
utils::write.csv(segment_count_distribution, segment_counts_csv, row.names = FALSE)
utils::write.csv(pairwise_consensus_summary, pairwise_summary_csv, row.names = FALSE)
utils::write.csv(pairwise_consensus, pairwise_consensus_csv, row.names = FALSE)
utils::write.csv(analysis_configuration, configuration_csv, row.names = FALSE)
utils::write.csv(reference_validation, reference_validation_csv, row.names = FALSE)
utils::write.csv(dependency_audit, dependency_audit_csv, row.names = FALSE)

openxlsx::write.xlsx(
  x = list(
    Stability_summary = stability_summary,
    Segment_counts = segment_count_distribution,
    Pairwise_summary = pairwise_consensus_summary,
    Reference_validation = reference_validation,
    Methods = method_configuration,
    Cohort_allocation = cohort_allocation,
    Configuration = analysis_configuration,
    Notes = analysis_notes,
    Dependency_audit = dependency_audit,
    Iteration_results = subsampling_results
  ),
  file = excel_output,
  overwrite = TRUE
)

saveRDS(stability_summary, summary_rds)
saveRDS(analysis_configuration, configuration_rds)
saveRDS(pairwise_consensus, pairwise_consensus_rds)
saveRDS(complete_results, complete_results_rds)
capture.output(utils::sessionInfo(), file = session_info_file)

expected_output_files <- c(
  iterations_csv,
  iterations_rds,
  memberships_rds,
  summary_csv,
  segment_counts_csv,
  pairwise_summary_csv,
  pairwise_consensus_csv,
  configuration_csv,
  reference_validation_csv,
  dependency_audit_csv,
  excel_output,
  summary_rds,
  configuration_rds,
  pairwise_consensus_rds,
  complete_results_rds,
  session_info_file
)

missing_output_files <- expected_output_files[!file.exists(expected_output_files)]

if (length(missing_output_files) > 0L) {
  stop(
    "The following corrected-010 outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

if (length(existing_historical_outputs) > 0L) {
  historical_md5_after <- tools::md5sum(existing_historical_outputs)

  if (!identical(
    unname(historical_md5_before),
    unname(historical_md5_after)
  )) {
    stop("At least one superseded historical output was modified.")
  }
}

# ------------------------------------------------------------
# 22. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SCRIPT 010 - CONDITIONAL SUBSAMPLING STABILITY\n")
cat("============================================================\n")
cat("\nStatus: supplementary algorithmic-stability analysis only.\n")
cat("Control fish: ", expected_number_of_fish, "\n", sep = "")
cat("Author cohorts: ", expected_number_of_authors, "\n", sep = "")
cat("Traceability: Author + ID\n")
cat("Reference k: ", selected_k, "\n", sep = "")
cat("Reference segment sizes: 40/49\n")
cat(
  "Primary selection-adjusted empirical p: ",
  sprintf("%.6f", primary_empirical_p),
  "\n",
  sep = ""
)
cat("Evidence of discrete communities: FALSE\n")
cat("Subsampling: 80% within Author, without replacement\n")
cat("Fish per subsample: ", target_subsample_n, "\n", sep = "")
cat("Duplicated graph vertices: NONE\n")
cat("Iterations: ", number_of_iterations, "\n", sep = "")
cat(
  "Louvain seed runs per graph: ",
  number_of_louvain_seed_runs,
  "\n",
  sep = ""
)
cat("Inferential p values generated by 010: NONE\n")
cat("Biological labels or thresholds generated: NONE\n")

cat("\nWithin-Author robust-normalisation audit:\n")
print(robust_audit, n = Inf, width = Inf)

cat("\nReference validation:\n")
print(reference_validation, n = Inf, width = Inf)

cat("\nStratified cohort allocation:\n")
print(cohort_allocation, n = Inf, width = Inf)

cat("\nGraph-scale methods:\n")
print(method_configuration, n = Inf, width = Inf)

cat("\nConditional stability summary:\n")
print(stability_summary, n = Inf, width = Inf)

cat("\nRecovered segment-count distribution:\n")
print(segment_count_distribution, n = Inf, width = Inf)

cat("\nPairwise co-segmentation summary:\n")
print(pairwise_consensus_summary, n = Inf, width = Inf)

cat(
  "\nCorrected supplementary workbook saved as:\n",
  normalizePath(excel_output, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

cat(
  "Complete corrected-010 results saved as:\n",
  normalizePath(complete_results_rds, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

if (length(existing_historical_outputs) > 0L) {
  cat(
    "\nWARNING: superseded ClusterStability_Bootstrap outputs remain on disk.\n",
    "They were not modified or deleted and must not be treated as current results:\n",
    paste(existing_historical_outputs, collapse = "\n"),
    "\n",
    sep = ""
  )
}

cat("\nAll dependency and integrity checks passed.\n")
cat("============================================================\n")
