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