# ============================================================
# 006.1_ModularityNullTest_Louvain.R
#
# Objective 1
# Formal null-model validation of weighted kNN-Louvain
# modularity in the corrected LAPCOM control population.
#
# Primary inference:
# - Gaussian-copula data-space null;
# - exact preservation of each pooled robust-score marginal;
# - approximate preservation of rank dependence;
# - complete k = 5:80 scan repeated in every null replicate;
# - k selected by the same internal-index and stability rule;
# - empirical, selection-adjusted modularity test.
#
# Complementary inference:
# - fixed-k data-space test at k = 45;
# - family-wise maximum standardised modularity test across k;
# - degree-preserving graph-rewiring test at k = 45.
#
# This script does not label communities biologically and does
# not replace the later resampling stability analysis.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory and required packages
# ------------------------------------------------------------

base_dir <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)

required_packages <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "igraph"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running the script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tibble)
library(ggplot2)
library(igraph)

# ------------------------------------------------------------
# 2. Input and output paths
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")
figures_dir <- file.path("03_Results", "Figures")

for (directory_i in c(objects_dir, tables_dir, figures_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

robust_data_file <- file.path(
  objects_dir,
  "objective1_control_robust_z.rds"
)

optimisation_results_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_results_robust.rds"
)

optimisation_memberships_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_memberships_robust.rds"
)

required_input_files <- c(
  robust_data_file,
  optimisation_results_file,
  optimisation_memberships_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "Required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 005_Figure3_PCA.R and 006.0_Optimisation_k_Louvain.R first."
  )
}

# ------------------------------------------------------------
# 3. Parameters fixed before the null simulations
# ------------------------------------------------------------

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

expected_number_of_fish <- 89L
expected_number_of_cohorts <- 4L
expected_selected_k <- 45L
expected_selected_communities <- 2L
expected_selected_sizes <- c(40L, 49L)

k_values <- 5:80
adjacent_ari_threshold <- 0.90
near_optimum_fraction <- 0.05
exact_ari_tolerance <- 1e-12
normalisation_tolerance <- 1e-8

# At least 1,000 replicates are retained for final inference.
number_of_data_null_replicates <- 1000L
number_of_graph_null_replicates <- 1000L

# The 100-run seed assessment from 006.0 is repeated exactly.
# This requires 7.6 million Louvain fits for the data-space null.
number_of_louvain_runs_per_k <- 100L
number_of_graph_louvain_runs <- 100L

data_null_seed_base <- 610000000L
graph_null_seed_base <- 710000000L

checkpoint_batch_size <- 10L
maximum_workers <- 4L
detected_cores <- parallel::detectCores(logical = FALSE)

if (!is.finite(detected_cores) || detected_cores < 1L) {
  detected_cores <- 1L
}

number_of_workers <- max(
  1L,
  min(maximum_workers, as.integer(detected_cores) - 1L)
)

graph_rewire_multiplier <- 20L

if (
  number_of_data_null_replicates < 1000L ||
  number_of_graph_null_replicates < 1000L
) {
  stop("Final inference requires at least 1,000 null replicates per null model.")
}

if (
  number_of_louvain_runs_per_k < 2L ||
  number_of_graph_louvain_runs < 2L
) {
  stop("At least two Louvain runs are required for each graph.")
}

if (
  anyDuplicated(k_values) > 0L ||
  any(diff(k_values) != 1L) ||
  min(k_values) < 1L ||
  max(k_values) >= expected_number_of_fish
) {
  stop("k_values must be consecutive integers between 1 and n - 1.")
}

RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

# ------------------------------------------------------------
# 4. Load and audit the corrected robust control matrix
# ------------------------------------------------------------

control_robust <- readRDS(robust_data_file)
optimisation_results <- readRDS(optimisation_results_file)
optimisation_memberships <- readRDS(optimisation_memberships_file)

if (!is.data.frame(control_robust)) {
  stop("objective1_control_robust_z.rds is not a data frame.")
}

required_columns <- c(
  "Author",
  "ID",
  "Treatment",
  robust_variables
)

missing_columns <- setdiff(required_columns, names(control_robust))

if (length(missing_columns) > 0L) {
  stop(
    "The robust control object is missing columns:\n",
    paste(missing_columns, collapse = ", ")
  )
}

control_robust <- control_robust %>%
  dplyr::mutate(
    Author = as.character(Author),
    ID = as.character(ID),
    Treatment = as.character(Treatment),
    Node_ID = paste(Author, ID, sep = "::")
  )

if (nrow(control_robust) != expected_number_of_fish) {
  stop(
    "Expected ", expected_number_of_fish,
    " controls; found ", nrow(control_robust), "."
  )
}

if (
  anyNA(control_robust[c("Author", "ID", "Treatment")]) ||
  any(trimws(control_robust$Author) == "") ||
  any(trimws(control_robust$ID) == "") ||
  any(trimws(control_robust$Treatment) == "") ||
  any(control_robust$Treatment != "Control")
) {
  stop("The individual-traceability or Treatment audit failed.")
}

if (
  anyDuplicated(control_robust[c("Author", "ID")]) > 0L ||
  anyDuplicated(control_robust$Node_ID) > 0L
) {
  stop("Duplicated Author + ID keys were detected.")
}

if (
  dplyr::n_distinct(control_robust$Author) !=
    expected_number_of_cohorts
) {
  stop("The expected four Author cohorts were not found.")
}

non_numeric_variables <- robust_variables[
  !vapply(control_robust[robust_variables], is.numeric, logical(1))
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "Non-numeric robust variables detected:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

cohort_ids <- sort(unique(control_robust$Author))

robust_audit <- dplyr::bind_rows(
  lapply(
    cohort_ids,
    function(cohort_id) {
      cohort_rows <- control_robust$Author == cohort_id

      dplyr::bind_rows(
        lapply(
          robust_variables,
          function(variable_name) {
            values <- control_robust[[variable_name]][cohort_rows]

            tibble::tibble(
              Author = cohort_id,
              Variable = variable_name,
              n = length(values),
              Available_n = sum(is.finite(values)),
              Median_rz = stats::median(values),
              MAD_scaled_rz = stats::mad(
                values,
                center = stats::median(values),
                constant = 1.4826
              ),
              Unique_values = length(unique(values))
            )
          }
        )
      )
    }
  )
)

if (
  nrow(robust_audit) !=
    expected_number_of_cohorts * length(robust_variables) ||
  any(robust_audit$Available_n != robust_audit$n) ||
  any(!is.finite(robust_audit$Median_rz)) ||
  any(!is.finite(robust_audit$MAD_scaled_rz)) ||
  any(robust_audit$Unique_values < 2L) ||
  any(abs(robust_audit$Median_rz) > normalisation_tolerance) ||
  any(abs(robust_audit$MAD_scaled_rz - 1) > normalisation_tolerance)
) {
  print(as.data.frame(robust_audit), row.names = FALSE)
  stop("The 16 Author-by-variable robust-normalisation checks failed.")
}

X <- control_robust %>%
  dplyr::select(dplyr::all_of(robust_variables)) %>%
  as.matrix()

storage.mode(X) <- "double"
rownames(X) <- control_robust$Node_ID

if (
  nrow(X) != expected_number_of_fish ||
  ncol(X) != length(robust_variables) ||
  any(!is.finite(X)) ||
  any(vapply(as.data.frame(X), function(x) {
    length(unique(x)) < 2L
  }, logical(1)))
) {
  stop("The joint 89 x 4 robust behavioural matrix failed its audit.")
}

input_md5 <- unname(tools::md5sum(robust_data_file))

# ------------------------------------------------------------
# 5. Reusable graph, clustering and validation functions
# ------------------------------------------------------------

combination_2 <- function(x) {
  x * (x - 1) / 2
}

adjusted_rand_index <- function(partition_1, partition_2) {
  if (length(partition_1) != length(partition_2)) {
    stop("ARI partitions have different lengths.")
  }

  contingency_table <- table(partition_1, partition_2)
  sum_cells <- sum(combination_2(contingency_table))
  sum_rows <- sum(combination_2(rowSums(contingency_table)))
  sum_columns <- sum(combination_2(colSums(contingency_table)))
  total_pairs <- combination_2(sum(contingency_table))

  if (total_pairs == 0) {
    return(NA_real_)
  }

  expected_index <- (sum_rows * sum_columns) / total_pairs
  maximum_index <- (sum_rows + sum_columns) / 2
  denominator <- maximum_index - expected_index

  if (abs(denominator) <= sqrt(.Machine$double.eps)) {
    relationship_1 <- outer(partition_1, partition_1, `==`)
    relationship_2 <- outer(partition_2, partition_2, `==`)
    return(if (identical(relationship_1, relationship_2)) 1 else 0)
  }

  as.numeric((sum_cells - expected_index) / denominator)
}

canonicalise_membership <- function(membership_vector, data_matrix) {
  membership_vector <- as.integer(membership_vector)
  community_labels <- sort(unique(membership_vector))
  individual_composite <- rowMeans(data_matrix)

  community_centroids <- vapply(
    community_labels,
    function(cluster_i) {
      mean(individual_composite[membership_vector == cluster_i])
    },
    numeric(1)
  )

  community_order <- order(-community_centroids, community_labels)
  label_map <- stats::setNames(
    seq_along(community_order),
    community_labels[community_order]
  )

  as.integer(unname(label_map[as.character(membership_vector)]))
}

build_union_knn_graph <- function(
  data_matrix,
  distance_matrix,
  k_value,
  node_names = rownames(data_matrix)
) {
  number_of_fish <- nrow(data_matrix)

  if (is.null(node_names)) {
    node_names <- paste0("Node_", seq_len(number_of_fish))
  }

  neighbour_index <- matrix(
    NA_integer_,
    nrow = number_of_fish,
    ncol = k_value
  )

  for (fish_i in seq_len(number_of_fish)) {
    ordered_candidates <- order(
      distance_matrix[fish_i, ],
      seq_len(number_of_fish)
    )
    ordered_candidates <- ordered_candidates[ordered_candidates != fish_i]
    neighbour_index[fish_i, ] <- ordered_candidates[seq_len(k_value)]
  }

  directed_from <- rep(seq_len(number_of_fish), each = k_value)
  directed_to <- as.vector(t(neighbour_index))
  undirected_from <- pmin(directed_from, directed_to)
  undirected_to <- pmax(directed_from, directed_to)
  edge_key <- paste(undirected_from, undirected_to, sep = "::")
  keep_edge <- !duplicated(edge_key)
  undirected_from <- undirected_from[keep_edge]
  undirected_to <- undirected_to[keep_edge]

  edge_distance <- distance_matrix[
    cbind(undirected_from, undirected_to)
  ]
  edge_similarity <- 1 / (1 + edge_distance)

  edge_table <- data.frame(
    from = node_names[undirected_from],
    to = node_names[undirected_to],
    weight = edge_similarity,
    stringsAsFactors = FALSE
  )

  graph_knn <- igraph::graph_from_data_frame(
    d = edge_table,
    directed = FALSE,
    vertices = data.frame(
      name = node_names,
      stringsAsFactors = FALSE
    )
  )

  if (
    igraph::is_directed(graph_knn) ||
    igraph::vcount(graph_knn) != number_of_fish ||
    !identical(as.character(igraph::V(graph_knn)$name), node_names) ||
    any(igraph::which_loop(graph_knn)) ||
    !igraph::is_simple(graph_knn) ||
    any(!is.finite(igraph::E(graph_knn)$weight)) ||
    any(igraph::E(graph_knn)$weight <= 0) ||
    any(igraph::E(graph_knn)$weight > 1)
  ) {
    stop("The union kNN graph failed at k = ", k_value, ".")
  }

  graph_knn
}

calinski_harabasz_index <- function(data_matrix, membership_vector) {
  cluster_labels <- sort(unique(membership_vector))
  number_of_clusters <- length(cluster_labels)
  number_of_fish <- nrow(data_matrix)

  if (number_of_clusters < 2L || number_of_clusters >= number_of_fish) {
    return(NA_real_)
  }

  global_centroid <- colMeans(data_matrix)
  between_sum_squares <- 0
  within_sum_squares <- 0

  for (cluster_i in cluster_labels) {
    cluster_data <- data_matrix[
      membership_vector == cluster_i,
      ,
      drop = FALSE
    ]
    cluster_centroid <- colMeans(cluster_data)
    between_sum_squares <- between_sum_squares +
      nrow(cluster_data) * sum((cluster_centroid - global_centroid)^2)
    within_sum_squares <- within_sum_squares +
      sum(rowSums((sweep(cluster_data, 2, cluster_centroid))^2))
  }

  if (within_sum_squares <= 0) {
    return(NA_real_)
  }

  as.numeric(
    (between_sum_squares / (number_of_clusters - 1)) /
      (within_sum_squares / (number_of_fish - number_of_clusters))
  )
}

mean_silhouette_index <- function(distance_matrix, membership_vector) {
  cluster_labels <- sort(unique(membership_vector))

  if (length(cluster_labels) < 2L) {
    return(NA_real_)
  }

  silhouette_values <- numeric(length(membership_vector))

  for (fish_i in seq_along(membership_vector)) {
    own_cluster <- membership_vector[fish_i]
    own_members <- which(membership_vector == own_cluster)
    own_members <- own_members[own_members != fish_i]

    if (length(own_members) == 0L) {
      silhouette_values[fish_i] <- 0
      next
    }

    within_distance <- mean(distance_matrix[fish_i, own_members])
    other_cluster_distances <- vapply(
      cluster_labels[cluster_labels != own_cluster],
      function(cluster_i) {
        mean(distance_matrix[fish_i, membership_vector == cluster_i])
      },
      numeric(1)
    )
    nearest_other_distance <- min(other_cluster_distances)
    denominator <- max(within_distance, nearest_other_distance)
    silhouette_values[fish_i] <- if (denominator > 0) {
      (nearest_other_distance - within_distance) / denominator
    } else {
      0
    }
  }

  mean(silhouette_values)
}

davies_bouldin_index <- function(data_matrix, membership_vector) {
  cluster_labels <- sort(unique(membership_vector))
  number_of_clusters <- length(cluster_labels)

  if (number_of_clusters < 2L) {
    return(NA_real_)
  }

  centroids <- t(vapply(
    cluster_labels,
    function(cluster_i) {
      colMeans(data_matrix[
        membership_vector == cluster_i,
        ,
        drop = FALSE
      ])
    },
    numeric(ncol(data_matrix))
  ))

  within_scatter <- vapply(
    seq_along(cluster_labels),
    function(cluster_position) {
      cluster_data <- data_matrix[
        membership_vector == cluster_labels[cluster_position],
        ,
        drop = FALSE
      ]
      cluster_centroid <- centroids[cluster_position, ]
      mean(sqrt(rowSums((sweep(
        cluster_data,
        2,
        cluster_centroid
      ))^2)))
    },
    numeric(1)
  )

  centroid_distances <- as.matrix(
    stats::dist(centroids, method = "euclidean")
  )
  comparison_matrix <- matrix(
    NA_real_,
    nrow = number_of_clusters,
    ncol = number_of_clusters
  )

  for (i in seq_len(number_of_clusters)) {
    for (j in seq_len(number_of_clusters)) {
      if (i != j) {
        comparison_matrix[i, j] <- if (
          centroid_distances[i, j] <= 0
        ) {
          Inf
        } else {
          (within_scatter[i] + within_scatter[j]) /
            centroid_distances[i, j]
        }
      }
    }
  }

  as.numeric(mean(apply(comparison_matrix, 1, max, na.rm = TRUE)))
}

stable_louvain_partition <- function(
  graph_object,
  data_matrix,
  seed_values
) {
  number_of_runs <- length(seed_values)
  number_of_fish <- nrow(data_matrix)
  seed_memberships <- matrix(
    NA_integer_,
    nrow = number_of_fish,
    ncol = number_of_runs
  )
  seed_modularity <- numeric(number_of_runs)

  for (seed_position in seq_along(seed_values)) {
    set.seed(seed_values[seed_position])
    louvain_model <- igraph::cluster_louvain(
      graph_object,
      weights = igraph::E(graph_object)$weight
    )
    membership_seed <- as.integer(igraph::membership(louvain_model))
    membership_seed <- canonicalise_membership(
      membership_seed,
      data_matrix
    )
    seed_memberships[, seed_position] <- membership_seed
    seed_modularity[seed_position] <- igraph::modularity(
      graph_object,
      membership_seed,
      weights = igraph::E(graph_object)$weight
    )
  }

  membership_keys <- apply(
    seed_memberships,
    2,
    paste,
    collapse = ","
  )
  unique_keys <- unique(membership_keys)
  unique_index <- match(membership_keys, unique_keys)
  unique_positions <- match(unique_keys, membership_keys)
  unique_memberships <- seed_memberships[
    ,
    unique_positions,
    drop = FALSE
  ]
  unique_counts <- tabulate(
    unique_index,
    nbins = length(unique_keys)
  )
  number_of_unique_partitions <- length(unique_keys)

  unique_pairwise_ari <- matrix(
    1,
    nrow = number_of_unique_partitions,
    ncol = number_of_unique_partitions
  )

  if (number_of_unique_partitions > 1L) {
    for (i in seq_len(number_of_unique_partitions - 1L)) {
      for (j in seq.int(i + 1L, number_of_unique_partitions)) {
        ari_ij <- adjusted_rand_index(
          unique_memberships[, i],
          unique_memberships[, j]
        )
        unique_pairwise_ari[i, j] <- ari_ij
        unique_pairwise_ari[j, i] <- ari_ij
      }
    }
  }

  weighted_ari_sum <- as.numeric(
    unique_pairwise_ari %*% unique_counts
  )
  mean_ari_to_other_seeds <- (
    weighted_ari_sum - 1
  ) / (number_of_runs - 1)

  reference_candidates <- which(
    abs(
      mean_ari_to_other_seeds - max(mean_ari_to_other_seeds)
    ) <= exact_ari_tolerance
  )

  if (length(reference_candidates) > 1L) {
    candidate_modularity <- vapply(
      reference_candidates,
      function(unique_position) {
        max(seed_modularity[unique_index == unique_position])
      },
      numeric(1)
    )
    reference_candidates <- reference_candidates[
      abs(
        candidate_modularity - max(candidate_modularity)
      ) <= exact_ari_tolerance
    ]
  }

  reference_unique_position <- min(reference_candidates)
  reference_run_candidates <- which(
    unique_index == reference_unique_position
  )
  reference_position <- reference_run_candidates[which.max(
    seed_modularity[reference_run_candidates]
  )]
  reference_membership <- seed_memberships[, reference_position]
  ari_to_reference <- unique_pairwise_ari[
    unique_index,
    reference_unique_position
  ]

  list(
    membership = reference_membership,
    modularity = seed_modularity[reference_position],
    maximum_modularity = max(seed_modularity),
    median_ari_to_reference = stats::median(ari_to_reference),
    minimum_ari_to_reference = min(ari_to_reference),
    exact_recovery_rate = mean(
      ari_to_reference >= 1 - exact_ari_tolerance
    ),
    unique_seed_partitions = number_of_unique_partitions,
    communities = length(unique(reference_membership)),
    community_sizes = sort(as.integer(table(reference_membership)))
  )
}

rank_with_missing_last <- function(values, decreasing = FALSE) {
  ranked_values <- if (decreasing) -values else values
  ranks <- rank(ranked_values, ties.method = "min", na.last = "keep")
  ranks[!is.finite(ranks)] <- Inf
  as.numeric(ranks)
}

select_k_from_scan <- function(scan_table, membership_matrix) {
  eligible <-
    scan_table$Communities >= 2L &
    is.finite(scan_table$Calinski_Harabasz) &
    is.finite(scan_table$Silhouette) &
    is.finite(scan_table$Davies_Bouldin) &
    is.finite(scan_table$Weighted_modularity)

  if (!any(eligible)) {
    stop("No eligible multi-community solution was produced.")
  }

  scan_table$Rank_Calinski_Harabasz <- rank_with_missing_last(
    scan_table$Calinski_Harabasz,
    decreasing = TRUE
  )
  scan_table$Rank_Silhouette <- rank_with_missing_last(
    scan_table$Silhouette,
    decreasing = TRUE
  )
  scan_table$Rank_Davies_Bouldin <- rank_with_missing_last(
    scan_table$Davies_Bouldin,
    decreasing = FALSE
  )
  scan_table$Internal_rank_sum <-
    scan_table$Rank_Calinski_Harabasz +
    scan_table$Rank_Silhouette +
    scan_table$Rank_Davies_Bouldin

  scan_table$Near_optimum_Calinski_Harabasz <- FALSE
  scan_table$Near_optimum_Silhouette <- FALSE
  scan_table$Near_optimum_Davies_Bouldin <- FALSE

  scan_table$Near_optimum_Calinski_Harabasz[eligible] <-
    scan_table$Calinski_Harabasz[eligible] >=
    (1 - near_optimum_fraction) *
    max(scan_table$Calinski_Harabasz[eligible])

  scan_table$Near_optimum_Silhouette[eligible] <-
    scan_table$Silhouette[eligible] >=
    (1 - near_optimum_fraction) *
    max(scan_table$Silhouette[eligible])

  scan_table$Near_optimum_Davies_Bouldin[eligible] <-
    scan_table$Davies_Bouldin[eligible] <=
    (1 + near_optimum_fraction) *
    min(scan_table$Davies_Bouldin[eligible])

  scan_table$Near_optimal_and_locally_stable <-
    eligible &
    scan_table$Near_optimum_Calinski_Harabasz &
    scan_table$Near_optimum_Silhouette &
    scan_table$Near_optimum_Davies_Bouldin &
    scan_table$Minimum_local_adjacent_ARI >= adjacent_ari_threshold &
    scan_table$Median_ARI_to_reference >= adjacent_ari_threshold

  candidate_positions <- which(
    scan_table$Near_optimal_and_locally_stable
  )

  if (length(candidate_positions) > 0L) {
    candidate_group <- cumsum(c(
      TRUE,
      diff(scan_table$k[candidate_positions]) != 1L
    ))

    group_summary <- do.call(
      rbind,
      lapply(
        unique(candidate_group),
        function(group_id) {
          positions <- candidate_positions[candidate_group == group_id]
          data.frame(
            Group = group_id,
            Minimum_rank_sum = min(
              scan_table$Internal_rank_sum[positions]
            ),
            Length = length(positions),
            Median_seed_ARI = stats::median(
              scan_table$Median_ARI_to_reference[positions]
            ),
            Minimum_k = min(scan_table$k[positions]),
            stringsAsFactors = FALSE
          )
        }
      )
    )

    group_summary <- group_summary[order(
      group_summary$Minimum_rank_sum,
      -group_summary$Length,
      -group_summary$Median_seed_ARI,
      group_summary$Minimum_k
    ), , drop = FALSE]

    selected_group <- group_summary$Group[1]
    selected_group_positions <- candidate_positions[
      candidate_group == selected_group
    ]
    group_midpoint <- stats::median(
      scan_table$k[selected_group_positions]
    )

    selection_order <- order(
      scan_table$Internal_rank_sum[selected_group_positions],
      abs(scan_table$k[selected_group_positions] - group_midpoint),
      -scan_table$Median_ARI_to_reference[selected_group_positions],
      -scan_table$Exact_recovery_rate[selected_group_positions],
      scan_table$k[selected_group_positions]
    )

    selected_position <- selected_group_positions[selection_order[1]]
    selection_rule <- paste0(
      "Near-optimal and locally stable range; minimum internal rank, ",
      "then central k and seed stability"
    )
  } else {
    eligible_positions <- which(eligible)
    selection_order <- order(
      scan_table$Internal_rank_sum[eligible_positions],
      -scan_table$Median_ARI_to_reference[eligible_positions],
      -scan_table$Exact_recovery_rate[eligible_positions],
      scan_table$k[eligible_positions]
    )
    selected_position <- eligible_positions[selection_order[1]]
    selection_rule <- paste0(
      "Fallback: minimum three-index rank sum, then seed stability"
    )
  }

  list(
    table = scan_table,
    selected_position = selected_position,
    selected_membership = membership_matrix[, selected_position],
    selection_rule = selection_rule
  )
}

scan_louvain_k_grid <- function(
  data_matrix,
  scan_seed_base,
  louvain_runs_per_k
) {
  distance_matrix <- as.matrix(
    stats::dist(data_matrix, method = "euclidean")
  )
  number_of_k <- length(k_values)
  scan_rows <- vector("list", number_of_k)
  membership_matrix <- matrix(
    NA_integer_,
    nrow = nrow(data_matrix),
    ncol = number_of_k,
    dimnames = list(rownames(data_matrix), as.character(k_values))
  )

  for (k_position in seq_along(k_values)) {
    k_value <- k_values[k_position]
    graph_object <- build_union_knn_graph(
      data_matrix,
      distance_matrix,
      k_value
    )
    seed_values <- scan_seed_base +
      k_position * 1000L +
      seq_len(louvain_runs_per_k)
    louvain_result <- stable_louvain_partition(
      graph_object,
      data_matrix,
      seed_values
    )
    membership_matrix[, k_position] <- louvain_result$membership

    scan_rows[[k_position]] <- data.frame(
      k = k_value,
      Communities = louvain_result$communities,
      Community_sizes = paste(
        louvain_result$community_sizes,
        collapse = "; "
      ),
      Calinski_Harabasz = calinski_harabasz_index(
        data_matrix,
        louvain_result$membership
      ),
      Silhouette = mean_silhouette_index(
        distance_matrix,
        louvain_result$membership
      ),
      Davies_Bouldin = davies_bouldin_index(
        data_matrix,
        louvain_result$membership
      ),
      Weighted_modularity = louvain_result$modularity,
      Maximum_seed_modularity = louvain_result$maximum_modularity,
      Median_ARI_to_reference =
        louvain_result$median_ari_to_reference,
      Minimum_ARI_to_reference =
        louvain_result$minimum_ari_to_reference,
      Exact_recovery_rate = louvain_result$exact_recovery_rate,
      Unique_seed_partitions = louvain_result$unique_seed_partitions,
      Graph_density = igraph::edge_density(
        graph_object,
        loops = FALSE
      ),
      Connected = igraph::is_connected(graph_object),
      stringsAsFactors = FALSE
    )
  }

  scan_table <- do.call(rbind, scan_rows)
  adjacent_ari <- vapply(
    seq.int(2L, number_of_k),
    function(k_position) {
      adjusted_rand_index(
        membership_matrix[, k_position - 1L],
        membership_matrix[, k_position]
      )
    },
    numeric(1)
  )

  scan_table$ARI_to_previous_k <- c(NA_real_, adjacent_ari)
  scan_table$ARI_to_next_k <- c(adjacent_ari, NA_real_)
  scan_table$Minimum_local_adjacent_ARI <- vapply(
    seq_len(number_of_k),
    function(k_position) {
      local_values <- c(
        scan_table$ARI_to_previous_k[k_position],
        scan_table$ARI_to_next_k[k_position]
      )
      local_values <- local_values[is.finite(local_values)]
      if (length(local_values) == 0L) NA_real_ else min(local_values)
    },
    numeric(1)
  )

  selection <- select_k_from_scan(scan_table, membership_matrix)
  selected_position <- selection$selected_position
  selected_table <- selection$table[selected_position, , drop = FALSE]
  selected_table$Selection_rule <- selection$selection_rule

  list(
    by_k = selection$table,
    selected = selected_table,
    selected_membership = selection$selected_membership,
    membership_matrix = membership_matrix
  )
}

# ------------------------------------------------------------
# 6. Audit the saved k = 45 solution and the encoded rule
# ------------------------------------------------------------

required_optimisation_columns <- c(
  "k",
  "Clusters",
  "Cluster_sizes",
  "Calinski_Harabasz",
  "Silhouette",
  "Davies_Bouldin",
  "Weighted_modularity"
)

if (
  !is.data.frame(optimisation_results) ||
  !all(required_optimisation_columns %in% names(optimisation_results)) ||
  !identical(as.integer(optimisation_results$k), as.integer(k_values))
) {
  stop("The saved 006.0 optimisation-results object is incompatible.")
}

if (
  !is.list(optimisation_memberships) ||
  length(optimisation_memberships) != length(k_values)
) {
  stop("The saved 006.0 membership object is incompatible.")
}

extract_saved_membership <- function(k_value) {
  k_position <- match(k_value, k_values)
  membership_table <- optimisation_memberships[[k_position]]

  if (
    !is.data.frame(membership_table) ||
    !all(c("Author", "ID", "Node_ID", "Cluster") %in%
      names(membership_table)) ||
    anyDuplicated(membership_table$Node_ID) > 0L ||
    !setequal(membership_table$Node_ID, control_robust$Node_ID)
  ) {
    stop("Invalid saved membership at k = ", k_value, ".")
  }

  membership_table <- membership_table[
    match(control_robust$Node_ID, membership_table$Node_ID),
    ,
    drop = FALSE
  ]

  as.integer(membership_table$Cluster)
}

saved_membership_45 <- extract_saved_membership(expected_selected_k)
saved_sizes_45 <- sort(as.integer(table(saved_membership_45)))
saved_result_45 <- optimisation_results[
  optimisation_results$k == expected_selected_k,
  ,
  drop = FALSE
]

if (
  nrow(saved_result_45) != 1L ||
  saved_result_45$Clusters != expected_selected_communities ||
  !identical(saved_sizes_45, expected_selected_sizes)
) {
  stop("The saved corrected k = 45 solution is not the expected 40/49 partition.")
}

saved_plateau_memberships <- lapply(
  44:46,
  extract_saved_membership
)

saved_plateau_ari <- c(
  adjusted_rand_index(
    saved_plateau_memberships[[1]],
    saved_plateau_memberships[[2]]
  ),
  adjusted_rand_index(
    saved_plateau_memberships[[2]],
    saved_plateau_memberships[[3]]
  ),
  adjusted_rand_index(
    saved_plateau_memberships[[1]],
    saved_plateau_memberships[[3]]
  )
)

if (any(saved_plateau_ari < 1 - exact_ari_tolerance)) {
  stop("The saved k = 44-46 exact-equivalence plateau was not recovered.")
}

cat("\nAuditing the deterministic null-pipeline selection rule on observed data...\n")

observed_scan <- scan_louvain_k_grid(
  data_matrix = X,
  scan_seed_base = data_null_seed_base - 1000000L,
  louvain_runs_per_k = number_of_louvain_runs_per_k
)

observed_selected_k <- as.integer(observed_scan$selected$k)
observed_selected_sizes <- sort(
  as.integer(table(observed_scan$selected_membership))
)
observed_saved_ari <- adjusted_rand_index(
  observed_scan$selected_membership,
  saved_membership_45
)

if (
  observed_selected_k != expected_selected_k ||
  !identical(observed_selected_sizes, expected_selected_sizes) ||
  observed_saved_ari < 1 - exact_ari_tolerance
) {
  stop(
    "The deterministic null-pipeline rule did not reproduce the ",
    "audited k = 45, 40/49 solution. Do not run the simulations."
  )
}

observed_by_k <- observed_scan$by_k
observed_selected <- observed_scan$selected
observed_q_selected <- as.numeric(observed_selected$Weighted_modularity)
observed_q_fixed_45 <- observed_by_k$Weighted_modularity[
  observed_by_k$k == expected_selected_k
]

# ------------------------------------------------------------
# 7. Fit and audit the Gaussian-copula data-space null
# ------------------------------------------------------------

make_positive_definite_correlation <- function(correlation_matrix) {
  correlation_matrix <- (
    correlation_matrix + t(correlation_matrix)
  ) / 2
  eigen_object <- eigen(correlation_matrix, symmetric = TRUE)
  eigenvalues <- pmax(eigen_object$values, 1e-8)
  adjusted <- eigen_object$vectors %*%
    diag(eigenvalues, nrow = length(eigenvalues)) %*%
    t(eigen_object$vectors)
  scale_values <- sqrt(diag(adjusted))
  adjusted <- adjusted / outer(scale_values, scale_values)
  diag(adjusted) <- 1
  adjusted
}

normal_scores <- apply(
  X,
  2,
  function(values) {
    stats::qnorm((rank(values, ties.method = "average") - 0.5) / length(values))
  }
)

copula_correlation <- make_positive_definite_correlation(
  stats::cor(normal_scores, method = "pearson")
)
copula_cholesky <- chol(copula_correlation)
observed_sorted_marginals <- apply(X, 2, sort)
observed_spearman <- stats::cor(X, method = "spearman")
observed_pearson <- stats::cor(X, method = "pearson")

if (
  any(!is.finite(copula_correlation)) ||
  max(abs(copula_correlation - t(copula_correlation))) > 1e-10 ||
  max(abs(diag(copula_correlation) - 1)) > 1e-10
) {
  stop("The fitted Gaussian-copula correlation matrix failed its audit.")
}

simulate_gaussian_copula_null <- function(replicate_seed) {
  set.seed(replicate_seed)
  latent_independent <- matrix(
    stats::rnorm(nrow(X) * ncol(X)),
    nrow = nrow(X),
    ncol = ncol(X)
  )
  latent_correlated <- latent_independent %*% copula_cholesky
  null_matrix <- matrix(
    NA_real_,
    nrow = nrow(X),
    ncol = ncol(X),
    dimnames = dimnames(X)
  )

  for (variable_position in seq_len(ncol(X))) {
    latent_rank <- rank(
      latent_correlated[, variable_position],
      ties.method = "first"
    )
    null_matrix[, variable_position] <-
      observed_sorted_marginals[latent_rank, variable_position]
  }

  if (
    any(!is.finite(null_matrix)) ||
    any(vapply(
      seq_len(ncol(X)),
      function(variable_position) {
        !isTRUE(all.equal(
          unname(sort(null_matrix[, variable_position])),
          unname(observed_sorted_marginals[, variable_position]),
          tolerance = 0
        ))
      },
      logical(1)
    ))
  ) {
    stop("A Gaussian-copula null replicate failed marginal preservation.")
  }

  null_matrix
}

simulate_one_data_null <- function(replicate_id) {
  replicate_seed <- data_null_seed_base + replicate_id * 100000L
  null_matrix <- simulate_gaussian_copula_null(replicate_seed)
  null_scan <- scan_louvain_k_grid(
    data_matrix = null_matrix,
    scan_seed_base = replicate_seed + 10000L,
    louvain_runs_per_k = number_of_louvain_runs_per_k
  )

  null_by_k <- null_scan$by_k[, c(
    "k",
    "Communities",
    "Community_sizes",
    "Calinski_Harabasz",
    "Silhouette",
    "Davies_Bouldin",
    "Weighted_modularity",
    "Maximum_seed_modularity",
    "Median_ARI_to_reference",
    "Exact_recovery_rate",
    "Unique_seed_partitions",
    "Minimum_local_adjacent_ARI",
    "Near_optimal_and_locally_stable",
    "Graph_density"
  )]
  null_by_k$Replicate <- replicate_id
  null_by_k <- null_by_k[, c("Replicate", setdiff(
    names(null_by_k),
    "Replicate"
  ))]

  null_selected <- null_scan$selected[, c(
    "k",
    "Communities",
    "Community_sizes",
    "Calinski_Harabasz",
    "Silhouette",
    "Davies_Bouldin",
    "Weighted_modularity",
    "Median_ARI_to_reference",
    "Exact_recovery_rate",
    "Selection_rule"
  )]
  null_selected$Replicate <- replicate_id
  null_selected <- null_selected[, c(
    "Replicate",
    setdiff(names(null_selected), "Replicate")
  )]

  null_spearman <- stats::cor(null_matrix, method = "spearman")
  null_pearson <- stats::cor(null_matrix, method = "pearson")
  upper_positions <- upper.tri(observed_spearman)

  correlation_audit <- data.frame(
    Replicate = replicate_id,
    Maximum_absolute_Spearman_difference = max(abs(
      null_spearman[upper_positions] -
        observed_spearman[upper_positions]
    )),
    RMSE_Spearman = sqrt(mean((
      null_spearman[upper_positions] -
        observed_spearman[upper_positions]
    )^2)),
    Maximum_absolute_Pearson_difference = max(abs(
      null_pearson[upper_positions] -
        observed_pearson[upper_positions]
    )),
    RMSE_Pearson = sqrt(mean((
      null_pearson[upper_positions] -
        observed_pearson[upper_positions]
    )^2)),
    stringsAsFactors = FALSE
  )

  list(
    by_k = null_by_k,
    selected = null_selected,
    correlation_audit = correlation_audit
  )
}

# ------------------------------------------------------------
# 8. Resumable parallel execution of the data-space null
# ------------------------------------------------------------

data_null_checkpoint <- file.path(
  objects_dir,
  "objective1_modularity_data_null_checkpoint.rds"
)

data_null_configuration <- list(
  Script = "006.1_ModularityNullTest_Louvain.R",
  Method_version = "2026-08-03_v1",
  Input_MD5 = input_md5,
  Null_model = paste0(
    "Gaussian copula with exact pooled empirical marginals and ",
    "rank-dependence correlation"
  ),
  k_values = as.integer(k_values),
  Replicates = number_of_data_null_replicates,
  Louvain_runs_per_k = number_of_louvain_runs_per_k,
  Seed_base = data_null_seed_base,
  Adjacent_ARI_threshold = adjacent_ari_threshold,
  Near_optimum_fraction = near_optimum_fraction
)

if (file.exists(data_null_checkpoint)) {
  data_checkpoint_object <- readRDS(data_null_checkpoint)

  if (
    !is.list(data_checkpoint_object) ||
    !identical(
      data_checkpoint_object$Configuration,
      data_null_configuration
    )
  ) {
    stop(
      "An incompatible data-null checkpoint exists. Rename it before ",
      "running this configuration; it will not be overwritten."
    )
  }

  data_null_results_list <- data_checkpoint_object$Results
} else {
  data_null_results_list <- list()
}

completed_data_replicates <- as.integer(names(data_null_results_list))
completed_data_replicates <- completed_data_replicates[
  is.finite(completed_data_replicates)
]
pending_data_replicates <- setdiff(
  seq_len(number_of_data_null_replicates),
  completed_data_replicates
)

worker_function_names <- c(
  "combination_2",
  "adjusted_rand_index",
  "canonicalise_membership",
  "build_union_knn_graph",
  "calinski_harabasz_index",
  "mean_silhouette_index",
  "davies_bouldin_index",
  "stable_louvain_partition",
  "rank_with_missing_last",
  "select_k_from_scan",
  "scan_louvain_k_grid",
  "simulate_gaussian_copula_null",
  "simulate_one_data_null"
)

worker_data_names <- c(
  "X",
  "k_values",
  "adjacent_ari_threshold",
  "near_optimum_fraction",
  "exact_ari_tolerance",
  "number_of_louvain_runs_per_k",
  "data_null_seed_base",
  "copula_cholesky",
  "observed_sorted_marginals",
  "observed_spearman",
  "observed_pearson"
)

data_cluster <- NULL

if (length(pending_data_replicates) > 0L && number_of_workers > 1L) {
  data_cluster <- parallel::makeCluster(number_of_workers)
  parallel::clusterEvalQ(data_cluster, {
    library(igraph)
    NULL
  })
  parallel::clusterExport(
    data_cluster,
    varlist = c(worker_function_names, worker_data_names),
    envir = environment()
  )
}

tryCatch(
  {
    while (length(pending_data_replicates) > 0L) {
      batch_ids <- head(pending_data_replicates, checkpoint_batch_size)

      batch_results <- if (is.null(data_cluster)) {
        lapply(batch_ids, simulate_one_data_null)
      } else {
        parallel::parLapplyLB(
          data_cluster,
          batch_ids,
          simulate_one_data_null
        )
      }

      names(batch_results) <- as.character(batch_ids)
      data_null_results_list[names(batch_results)] <- batch_results
      data_null_results_list <- data_null_results_list[
        order(as.integer(names(data_null_results_list)))
      ]

      saveRDS(
        list(
          Configuration = data_null_configuration,
          Results = data_null_results_list
        ),
        data_null_checkpoint
      )

      pending_data_replicates <- setdiff(
        seq_len(number_of_data_null_replicates),
        as.integer(names(data_null_results_list))
      )

      cat(
        "Data-space null: ",
        length(data_null_results_list),
        "/",
        number_of_data_null_replicates,
        " replicates completed.\n",
        sep = ""
      )
    }
  },
  finally = {
    if (!is.null(data_cluster)) {
      parallel::stopCluster(data_cluster)
      data_cluster <- NULL
    }
  }
)

data_null_by_k <- do.call(
  rbind,
  lapply(data_null_results_list, `[[`, "by_k")
)
data_null_selected <- do.call(
  rbind,
  lapply(data_null_results_list, `[[`, "selected")
)
data_null_correlation_audit <- do.call(
  rbind,
  lapply(data_null_results_list, `[[`, "correlation_audit")
)

row.names(data_null_by_k) <- NULL
row.names(data_null_selected) <- NULL
row.names(data_null_correlation_audit) <- NULL

if (
  nrow(data_null_by_k) !=
    number_of_data_null_replicates * length(k_values) ||
  nrow(data_null_selected) != number_of_data_null_replicates ||
  nrow(data_null_correlation_audit) != number_of_data_null_replicates ||
  any(!is.finite(data_null_by_k$Weighted_modularity)) ||
  any(!is.finite(data_null_selected$Weighted_modularity))
) {
  stop("The aggregated data-space null results failed their audit.")
}

# ------------------------------------------------------------
# 9. Data-space null inference
# ------------------------------------------------------------

empirical_upper_p <- function(observed_value, null_values) {
  null_values <- null_values[is.finite(null_values)]
  (1 + sum(null_values >= observed_value)) / (length(null_values) + 1)
}

standardised_excess <- function(observed_value, null_values) {
  null_values <- null_values[is.finite(null_values)]
  null_sd <- stats::sd(null_values)
  if (!is.finite(null_sd) || null_sd <= 0) {
    return(NA_real_)
  }
  (observed_value - mean(null_values)) / null_sd
}

null_selected_q <- data_null_selected$Weighted_modularity
null_fixed_45_q <- data_null_by_k$Weighted_modularity[
  data_null_by_k$k == expected_selected_k
]

selection_adjusted_p <- empirical_upper_p(
  observed_q_selected,
  null_selected_q
)
fixed_k_p <- empirical_upper_p(
  observed_q_fixed_45,
  null_fixed_45_q
)

per_k_null_summary <- dplyr::bind_rows(
  lapply(
    k_values,
    function(k_value) {
      observed_row <- observed_by_k[
        observed_by_k$k == k_value,
        ,
        drop = FALSE
      ]
      null_values <- data_null_by_k$Weighted_modularity[
        data_null_by_k$k == k_value
      ]

      tibble::tibble(
        k = k_value,
        Observed_modularity = observed_row$Weighted_modularity,
        Null_mean = mean(null_values),
        Null_SD = stats::sd(null_values),
        Null_percentile_2_5 = unname(stats::quantile(null_values, 0.025)),
        Null_median = stats::median(null_values),
        Null_percentile_97_5 = unname(stats::quantile(null_values, 0.975)),
        Null_percentile_99 = unname(stats::quantile(null_values, 0.99)),
        Standardised_excess = standardised_excess(
          observed_row$Weighted_modularity,
          null_values
        ),
        Empirical_p = empirical_upper_p(
          observed_row$Weighted_modularity,
          null_values
        )
      )
    }
  )
) %>%
  dplyr::mutate(
    Empirical_p_BH = stats::p.adjust(Empirical_p, method = "BH")
  )

null_q_matrix <- matrix(
  NA_real_,
  nrow = number_of_data_null_replicates,
  ncol = length(k_values),
  dimnames = list(
    as.character(seq_len(number_of_data_null_replicates)),
    as.character(k_values)
  )
)

for (k_position in seq_along(k_values)) {
  k_value <- k_values[k_position]
  k_rows <- data_null_by_k[data_null_by_k$k == k_value, ]
  k_rows <- k_rows[order(k_rows$Replicate), ]
  null_q_matrix[, k_position] <- k_rows$Weighted_modularity
}

null_mean_by_k <- colMeans(null_q_matrix)
null_sd_by_k <- apply(null_q_matrix, 2, stats::sd)

if (any(!is.finite(null_sd_by_k)) || any(null_sd_by_k <= 0)) {
  stop("At least one k has zero or invalid null modularity variance.")
}

null_z_matrix <- sweep(
  sweep(null_q_matrix, 2, null_mean_by_k, "-"),
  2,
  null_sd_by_k,
  "/"
)
observed_z_by_k <- (
  observed_by_k$Weighted_modularity - null_mean_by_k
) / null_sd_by_k

null_maximum_z <- apply(null_z_matrix, 1, max)
observed_maximum_z <- max(observed_z_by_k)
observed_maximum_z_k <- k_values[which.max(observed_z_by_k)]
global_scan_p <- empirical_upper_p(
  observed_maximum_z,
  null_maximum_z
)

selected_k_distribution <- data_null_selected %>%
  dplyr::count(k, Communities, name = "Replicates") %>%
  dplyr::mutate(
    Percentage = 100 * Replicates / number_of_data_null_replicates
  ) %>%
  dplyr::arrange(dplyr::desc(Replicates), k, Communities)

# ------------------------------------------------------------
# 10. Degree-preserving graph-null sensitivity analysis
# ------------------------------------------------------------

observed_distance <- as.matrix(stats::dist(X, method = "euclidean"))
observed_graph_45 <- build_union_knn_graph(
  X,
  observed_distance,
  expected_selected_k
)
observed_degree_sequence <- igraph::degree(observed_graph_45)
observed_edge_weights <- igraph::E(observed_graph_45)$weight
observed_edge_list <- igraph::as_edgelist(
  observed_graph_45,
  names = FALSE
)
observed_edge_keys <- paste(
  pmin(observed_edge_list[, 1], observed_edge_list[, 2]),
  pmax(observed_edge_list[, 1], observed_edge_list[, 2]),
  sep = "::"
)
graph_rewire_iterations <- as.integer(
  graph_rewire_multiplier * igraph::ecount(observed_graph_45)
)

maximum_louvain_modularity <- function(graph_object, seed_values) {
  modularity_values <- numeric(length(seed_values))

  for (seed_position in seq_along(seed_values)) {
    set.seed(seed_values[seed_position])
    model <- igraph::cluster_louvain(
      graph_object,
      weights = igraph::E(graph_object)$weight
    )
    modularity_values[seed_position] <- igraph::modularity(
      graph_object,
      igraph::membership(model),
      weights = igraph::E(graph_object)$weight
    )
  }

  max(modularity_values)
}

observed_graph_q_max <- maximum_louvain_modularity(
  observed_graph_45,
  graph_null_seed_base - 100000L +
    seq_len(number_of_graph_louvain_runs)
)

simulate_one_graph_null <- function(replicate_id) {
  replicate_seed <- graph_null_seed_base + replicate_id * 10000L
  set.seed(replicate_seed)

  rewired_graph <- igraph::rewire(
    observed_graph_45,
    with = igraph::keeping_degseq(
      niter = graph_rewire_iterations,
      loops = FALSE
    )
  )
  igraph::E(rewired_graph)$weight <- sample(
    observed_edge_weights,
    length(observed_edge_weights),
    replace = FALSE
  )

  rewired_edge_list <- igraph::as_edgelist(
    rewired_graph,
    names = FALSE
  )
  rewired_edge_keys <- paste(
    pmin(rewired_edge_list[, 1], rewired_edge_list[, 2]),
    pmax(rewired_edge_list[, 1], rewired_edge_list[, 2]),
    sep = "::"
  )

  if (
    !igraph::is_simple(rewired_graph) ||
    any(igraph::which_loop(rewired_graph)) ||
    !identical(
      as.integer(igraph::degree(rewired_graph)),
      as.integer(observed_degree_sequence)
    ) ||
    !isTRUE(all.equal(
      sort(igraph::E(rewired_graph)$weight),
      sort(observed_edge_weights),
      tolerance = 0
    ))
  ) {
    stop("A degree-preserving graph-null replicate failed its audit.")
  }

  q_max <- maximum_louvain_modularity(
    rewired_graph,
    replicate_seed + seq_len(number_of_graph_louvain_runs)
  )

  data.frame(
    Replicate = replicate_id,
    Maximum_weighted_modularity = q_max,
    Connected = igraph::is_connected(rewired_graph),
    Edge_overlap_proportion = mean(
      rewired_edge_keys %in% observed_edge_keys
    ),
    Strength_correlation_with_observed = suppressWarnings(stats::cor(
      igraph::strength(rewired_graph, weights = igraph::E(rewired_graph)$weight),
      igraph::strength(
        observed_graph_45,
        weights = observed_edge_weights
      )
    )),
    stringsAsFactors = FALSE
  )
}

graph_null_checkpoint <- file.path(
  objects_dir,
  "objective1_modularity_graph_null_checkpoint.rds"
)

graph_null_configuration <- list(
  Script = "006.1_ModularityNullTest_Louvain.R",
  Method_version = "2026-08-03_v1",
  Input_MD5 = input_md5,
  k = expected_selected_k,
  Null_model = paste0(
    "Degree-preserving simple-graph rewiring with global edge-weight permutation"
  ),
  Replicates = number_of_graph_null_replicates,
  Louvain_runs_per_graph = number_of_graph_louvain_runs,
  Rewire_iterations = graph_rewire_iterations,
  Seed_base = graph_null_seed_base
)

if (file.exists(graph_null_checkpoint)) {
  graph_checkpoint_object <- readRDS(graph_null_checkpoint)

  if (
    !is.list(graph_checkpoint_object) ||
    !identical(
      graph_checkpoint_object$Configuration,
      graph_null_configuration
    )
  ) {
    stop(
      "An incompatible graph-null checkpoint exists. Rename it before ",
      "running this configuration; it will not be overwritten."
    )
  }

  graph_null_results_list <- graph_checkpoint_object$Results
} else {
  graph_null_results_list <- list()
}

completed_graph_replicates <- as.integer(names(graph_null_results_list))
completed_graph_replicates <- completed_graph_replicates[
  is.finite(completed_graph_replicates)
]
pending_graph_replicates <- setdiff(
  seq_len(number_of_graph_null_replicates),
  completed_graph_replicates
)

graph_worker_names <- c(
  "maximum_louvain_modularity",
  "simulate_one_graph_null"
)

graph_worker_data_names <- c(
  "observed_graph_45",
  "observed_degree_sequence",
  "observed_edge_weights",
  "observed_edge_keys",
  "graph_rewire_iterations",
  "graph_null_seed_base",
  "number_of_graph_louvain_runs"
)

graph_cluster <- NULL

if (length(pending_graph_replicates) > 0L && number_of_workers > 1L) {
  graph_cluster <- parallel::makeCluster(number_of_workers)
  parallel::clusterEvalQ(graph_cluster, {
    library(igraph)
    NULL
  })
  parallel::clusterExport(
    graph_cluster,
    varlist = c(graph_worker_names, graph_worker_data_names),
    envir = environment()
  )
}

tryCatch(
  {
    while (length(pending_graph_replicates) > 0L) {
      batch_ids <- head(pending_graph_replicates, checkpoint_batch_size)

      batch_results <- if (is.null(graph_cluster)) {
        lapply(batch_ids, simulate_one_graph_null)
      } else {
        parallel::parLapplyLB(
          graph_cluster,
          batch_ids,
          simulate_one_graph_null
        )
      }

      names(batch_results) <- as.character(batch_ids)
      graph_null_results_list[names(batch_results)] <- batch_results
      graph_null_results_list <- graph_null_results_list[
        order(as.integer(names(graph_null_results_list)))
      ]

      saveRDS(
        list(
          Configuration = graph_null_configuration,
          Results = graph_null_results_list
        ),
        graph_null_checkpoint
      )

      pending_graph_replicates <- setdiff(
        seq_len(number_of_graph_null_replicates),
        as.integer(names(graph_null_results_list))
      )

      cat(
        "Graph null: ",
        length(graph_null_results_list),
        "/",
        number_of_graph_null_replicates,
        " replicates completed.\n",
        sep = ""
      )
    }
  },
  finally = {
    if (!is.null(graph_cluster)) {
      parallel::stopCluster(graph_cluster)
      graph_cluster <- NULL
    }
  }
)

graph_null_results <- do.call(rbind, graph_null_results_list)
row.names(graph_null_results) <- NULL

if (
  nrow(graph_null_results) != number_of_graph_null_replicates ||
  any(!is.finite(graph_null_results$Maximum_weighted_modularity))
) {
  stop("The aggregated degree-preserving graph-null results failed.")
}

graph_null_p <- empirical_upper_p(
  observed_graph_q_max,
  graph_null_results$Maximum_weighted_modularity
)

# ------------------------------------------------------------
# 11. Assemble inferential summary
# ------------------------------------------------------------

summarise_test <- function(
  test_name,
  status,
  observed_value,
  null_values,
  empirical_p,
  notes
) {
  null_values <- null_values[is.finite(null_values)]
  exceedances <- sum(null_values >= observed_value)
  binomial_interval <- stats::binom.test(
    exceedances,
    length(null_values),
    conf.level = 0.95
  )$conf.int

  tibble::tibble(
    Test = test_name,
    Status = status,
    Observed_statistic = observed_value,
    Null_mean = mean(null_values),
    Null_SD = stats::sd(null_values),
    Null_percentile_2_5 = unname(stats::quantile(null_values, 0.025)),
    Null_median = stats::median(null_values),
    Null_percentile_97_5 = unname(stats::quantile(null_values, 0.975)),
    Observed_minus_null_mean = observed_value - mean(null_values),
    Standardised_excess = standardised_excess(
      observed_value,
      null_values
    ),
    Null_exceedances = exceedances,
    Empirical_p = empirical_p,
    Monte_Carlo_p_CI_low = unname(binomial_interval[1]),
    Monte_Carlo_p_CI_high = unname(binomial_interval[2]),
    Null_replicates = length(null_values),
    Notes = notes
  )
}

modularity_tests <- dplyr::bind_rows(
  summarise_test(
    test_name = paste0(
      "Primary: Gaussian-copula null with full k-selection pipeline"
    ),
    status = "Primary inference",
    observed_value = observed_q_selected,
    null_values = null_selected_q,
    empirical_p = selection_adjusted_p,
    notes = paste0(
      "Each null replicate scans k = 5:80 and selects k using CH, ",
      "silhouette, Davies-Bouldin and ARI criteria"
    )
  ),
  summarise_test(
    test_name = "Gaussian-copula null at fixed k = 45",
    status = "Complementary fixed-k test",
    observed_value = observed_q_fixed_45,
    null_values = null_fixed_45_q,
    empirical_p = fixed_k_p,
    notes = "Does not by itself adjust for the prior k search"
  ),
  summarise_test(
    test_name = paste0(
      "Family-wise maximum standardised modularity across k = 5:80"
    ),
    status = "Complementary scan-wide test",
    observed_value = observed_maximum_z,
    null_values = null_maximum_z,
    empirical_p = global_scan_p,
    notes = paste0(
      "Standardises modularity separately at each graph density before ",
      "taking the maximum"
    )
  ),
  summarise_test(
    test_name = "Degree-preserving graph rewiring at k = 45",
    status = "Topological sensitivity analysis",
    observed_value = observed_graph_q_max,
    null_values = graph_null_results$Maximum_weighted_modularity,
    empirical_p = graph_null_p,
    notes = paste0(
      "Preserves degree sequence and global weight distribution, but not ",
      "node strengths or the original data geometry"
    )
  )
)

# ------------------------------------------------------------
# 12. Diagnostic figures
# ------------------------------------------------------------

data_null_distribution_plot_data <- dplyr::bind_rows(
  tibble::tibble(
    Test = "A  Selection-adjusted data null",
    Null_modularity = null_selected_q
  ),
  tibble::tibble(
    Test = "B  Fixed k = 45 data null",
    Null_modularity = null_fixed_45_q
  )
)

data_null_observed_lines <- tibble::tibble(
  Test = c(
    "A  Selection-adjusted data null",
    "B  Fixed k = 45 data null"
  ),
  Observed_modularity = c(
    observed_q_selected,
    observed_q_fixed_45
  )
)

data_null_distribution_plot <- ggplot2::ggplot(
  data_null_distribution_plot_data,
  ggplot2::aes(x = Null_modularity)
) +
  ggplot2::geom_histogram(
    bins = 35,
    fill = "grey78",
    colour = "white",
    linewidth = 0.25
  ) +
  ggplot2::geom_vline(
    data = data_null_observed_lines,
    ggplot2::aes(xintercept = Observed_modularity),
    inherit.aes = FALSE,
    colour = "#B22222",
    linewidth = 0.8
  ) +
  ggplot2::facet_wrap(~Test, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    x = "Weighted modularity under the Gaussian-copula null",
    y = "Null replicates",
    title = "Data-space null distributions",
    subtitle = "Red line: observed modularity"
  ) +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", hjust = 0)
  )

modularity_envelope_plot <- ggplot2::ggplot(
  per_k_null_summary,
  ggplot2::aes(x = k)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = Null_percentile_2_5,
      ymax = Null_percentile_97_5
    ),
    fill = "grey80"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = Null_mean),
    colour = "grey35",
    linewidth = 0.55
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = Observed_modularity),
    colour = "#1F4E79",
    linewidth = 0.7
  ) +
  ggplot2::geom_vline(
    xintercept = expected_selected_k,
    colour = "#B22222",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggplot2::scale_x_continuous(breaks = seq(5, 80, by = 5)) +
  ggplot2::labs(
    x = "Number of nearest neighbours (k)",
    y = "Weighted modularity",
    title = "Observed modularity against the data-space null envelope",
    subtitle = paste0(
      "Blue: observed; grey: null mean and 95% interval; red: k = 45"
    )
  ) +
  ggplot2::theme_classic(base_size = 11)

graph_null_distribution_plot <- ggplot2::ggplot(
  graph_null_results,
  ggplot2::aes(x = Maximum_weighted_modularity)
) +
  ggplot2::geom_histogram(
    bins = 35,
    fill = "grey78",
    colour = "white",
    linewidth = 0.25
  ) +
  ggplot2::geom_vline(
    xintercept = observed_graph_q_max,
    colour = "#B22222",
    linewidth = 0.8
  ) +
  ggplot2::labs(
    x = "Maximum weighted modularity",
    y = "Degree-preserving null graphs",
    title = "Topology-based null distribution at k = 45",
    subtitle = "Red line: observed graph"
  ) +
  ggplot2::theme_classic(base_size = 11)

# ------------------------------------------------------------
# 13. Export tables, figures and complete reproducibility object
# ------------------------------------------------------------

tests_csv <- file.path(
  tables_dir,
  "Objective1_ModularityNullTests.csv"
)

per_k_csv <- file.path(
  tables_dir,
  "Objective1_ModularityDataNull_by_k.csv"
)

selected_null_csv <- file.path(
  tables_dir,
  "Objective1_ModularityDataNull_selected_replicates.csv"
)

selected_k_distribution_csv <- file.path(
  tables_dir,
  "Objective1_ModularityDataNull_selected_k_distribution.csv"
)

correlation_audit_csv <- file.path(
  tables_dir,
  "Objective1_ModularityDataNull_correlation_audit.csv"
)

graph_null_csv <- file.path(
  tables_dir,
  "Objective1_ModularityGraphNull_replicates.csv"
)

data_null_pdf <- file.path(
  figures_dir,
  "FigureS_Modularity_DataNull_Distributions.pdf"
)

data_null_tiff <- file.path(
  figures_dir,
  "FigureS_Modularity_DataNull_Distributions.tiff"
)

envelope_pdf <- file.path(
  figures_dir,
  "FigureS_Modularity_DataNull_Envelope_by_k.pdf"
)

envelope_tiff <- file.path(
  figures_dir,
  "FigureS_Modularity_DataNull_Envelope_by_k.tiff"
)

graph_null_pdf <- file.path(
  figures_dir,
  "FigureS_Modularity_GraphNull_Distribution.pdf"
)

graph_null_tiff <- file.path(
  figures_dir,
  "FigureS_Modularity_GraphNull_Distribution.tiff"
)

complete_results_rds <- file.path(
  objects_dir,
  "objective1_modularity_null_test_results.rds"
)

session_info_file <- file.path(
  tables_dir,
  "Objective1_ModularityNullTest_sessionInfo.txt"
)

utils::write.csv(modularity_tests, tests_csv, row.names = FALSE)
utils::write.csv(per_k_null_summary, per_k_csv, row.names = FALSE)
utils::write.csv(data_null_selected, selected_null_csv, row.names = FALSE)
utils::write.csv(
  selected_k_distribution,
  selected_k_distribution_csv,
  row.names = FALSE
)
utils::write.csv(
  data_null_correlation_audit,
  correlation_audit_csv,
  row.names = FALSE
)
utils::write.csv(graph_null_results, graph_null_csv, row.names = FALSE)

ggplot2::ggsave(
  filename = data_null_pdf,
  plot = data_null_distribution_plot,
  width = 7,
  height = 7,
  units = "in"
)

ggplot2::ggsave(
  filename = data_null_tiff,
  plot = data_null_distribution_plot,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggplot2::ggsave(
  filename = envelope_pdf,
  plot = modularity_envelope_plot,
  width = 8,
  height = 5.5,
  units = "in"
)

ggplot2::ggsave(
  filename = envelope_tiff,
  plot = modularity_envelope_plot,
  width = 8,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggplot2::ggsave(
  filename = graph_null_pdf,
  plot = graph_null_distribution_plot,
  width = 7,
  height = 5.5,
  units = "in"
)

ggplot2::ggsave(
  filename = graph_null_tiff,
  plot = graph_null_distribution_plot,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

complete_results <- list(
  Status = "Formal modularity null-model validation",
  Input_MD5 = input_md5,
  Traceability_key = "Author + ID",
  Robust_normalisation = paste0(
    "Within Author using controls: (x - median) / (1.4826 x raw MAD)"
  ),
  Analytical_matrix = "All 89 controls jointly after cohort-wise normalisation",
  Data_null_configuration = data_null_configuration,
  Graph_null_configuration = graph_null_configuration,
  Copula_correlation = copula_correlation,
  Observed_scan = observed_by_k,
  Observed_selected = observed_selected,
  Saved_006_0_k45 = saved_result_45,
  Modularity_tests = modularity_tests,
  Per_k_null_summary = per_k_null_summary,
  Data_null_selected_replicates = data_null_selected,
  Data_null_selected_k_distribution = selected_k_distribution,
  Data_null_correlation_audit = data_null_correlation_audit,
  Null_maximum_standardised_modularity = null_maximum_z,
  Observed_maximum_standardised_modularity = observed_maximum_z,
  Observed_maximum_standardised_modularity_k = observed_maximum_z_k,
  Graph_null_replicates = graph_null_results
)

saveRDS(complete_results, complete_results_rds)
capture.output(utils::sessionInfo(), file = session_info_file)

# ------------------------------------------------------------
# 14. Verify outputs and print the final audit
# ------------------------------------------------------------

expected_output_files <- c(
  tests_csv,
  per_k_csv,
  selected_null_csv,
  selected_k_distribution_csv,
  correlation_audit_csv,
  graph_null_csv,
  data_null_pdf,
  data_null_tiff,
  envelope_pdf,
  envelope_tiff,
  graph_null_pdf,
  graph_null_tiff,
  complete_results_rds,
  session_info_file,
  data_null_checkpoint,
  graph_null_checkpoint
)

output_info <- file.info(expected_output_files)

if (
  any(!file.exists(expected_output_files)) ||
  any(is.na(output_info$size)) ||
  any(output_info$size <= 0)
) {
  stop("At least one expected modularity-test output is absent or empty.")
}

cat("\n============================================================\n")
cat("SCRIPT 006.1 — FORMAL MODULARITY NULL-MODEL TEST\n")
cat("============================================================\n")
cat("\nObserved data: 89 control fish x 4 robust variables.\n")
cat("Traceability: Author + ID; Author not used as a clustering variable.\n")
cat("Observed selected k: ", observed_selected_k, "\n", sep = "")
cat(
  "Observed communities: ",
  expected_selected_communities,
  " (",
  paste(expected_selected_sizes, collapse = "/"),
  ").\n",
  sep = ""
)
cat(
  "Observed weighted modularity at selected k: ",
  sprintf("%.6f", observed_q_selected),
  "\n",
  sep = ""
)
cat(
  "Encoded selection rule reproduced the saved k = 45 partition: ARI = ",
  sprintf("%.6f", observed_saved_ari),
  "\n",
  sep = ""
)

cat("\nNull-model tests:\n")
print(as.data.frame(modularity_tests), row.names = FALSE)

cat("\nMost frequent k/community outcomes under the data-space null:\n")
print(
  as.data.frame(utils::head(selected_k_distribution, 15L)),
  row.names = FALSE
)

cat("\nGaussian-copula dependence audit:\n")
print(
  data.frame(
    Metric = c(
      "Median maximum absolute Spearman difference",
      "95th percentile maximum absolute Spearman difference",
      "Median Spearman RMSE",
      "Median Pearson RMSE"
    ),
    Value = c(
      stats::median(
        data_null_correlation_audit$Maximum_absolute_Spearman_difference
      ),
      unname(stats::quantile(
        data_null_correlation_audit$Maximum_absolute_Spearman_difference,
        0.95
      )),
      stats::median(data_null_correlation_audit$RMSE_Spearman),
      stats::median(data_null_correlation_audit$RMSE_Pearson)
    )
  ),
  row.names = FALSE
)

cat(
  "\nMaximum observed standardised modularity occurred at k = ",
  observed_maximum_z_k,
  ".\n",
  sep = ""
)
cat(
  "Data-null replicates: ",
  number_of_data_null_replicates,
  "; graph-null replicates: ",
  number_of_graph_null_replicates,
  ".\n",
  sep = ""
)
cat(
  "Minimum attainable empirical p: ",
  sprintf("%.6f", 1 / (number_of_data_null_replicates + 1)),
  ".\n",
  sep = ""
)
cat(
  "Median graph-null edge overlap with the observed graph: ",
  sprintf(
    "%.4f",
    stats::median(graph_null_results$Edge_overlap_proportion)
  ),
  ".\n",
  sep = ""
)
cat(
  "Median graph-null strength correlation with the observed graph: ",
  sprintf(
    "%.4f",
    stats::median(
      graph_null_results$Strength_correlation_with_observed,
      na.rm = TRUE
    )
  ),
  ".\n",
  sep = ""
)
cat(
  "Workers used: ",
  number_of_workers,
  ". Checkpoints permit exact resumption after interruption.\n",
  sep = ""
)

cat("\nMain result table:\n")
cat(normalizePath(tests_csv, winslash = "/", mustWork = TRUE), "\n")
cat("\nAll integrity checks passed.\n")
cat("============================================================\n")
