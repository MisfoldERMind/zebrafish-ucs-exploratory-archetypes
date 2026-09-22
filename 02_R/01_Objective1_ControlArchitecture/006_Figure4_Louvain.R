# ============================================================
# 006_Figure4_Louvain.R
#
# Objective 1
# Descriptive PCA projection of the exploratory Louvain
# segmentation of the LAPCOM control population after formal
# null-model evaluation.
#
# Required execution order:
# 1. 005_Figure3_PCA.R
# 2. 006.0_Optimisation_k_Louvain.R
# 3. 006.1_ModularityNullTest_Louvain.R
# 4. This script
#
# Analytical dependency:
# - 006.0 supplies the observed k, membership and modularity.
# - 006.1 supplies the primary selection-adjusted inference.
# - This script does not optimise k or rerun Louvain.
#
# Current inference:
# The k = 45 partition contains two algorithmic segments
# (40/49 fish), but the primary Gaussian-copula null test does
# not support discrete behavioural communities. The PCA is used
# only to display this descriptive segmentation of a continuous
# locomotor-exploratory space.
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

required_packages <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "ggrepel",
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
library(ggrepel)
library(igraph)

# ------------------------------------------------------------
# 3. Input and output directories
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")
figures_dir <- file.path("03_Results", "Figures")

for (directory_i in c(objects_dir, tables_dir, figures_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

# ------------------------------------------------------------
# 4. Required inputs from 005, 006.0 and 006.1
# ------------------------------------------------------------

robust_data_file <- file.path(
  objects_dir,
  "objective1_control_robust_z.rds"
)

pca_scores_file <- file.path(
  objects_dir,
  "objective1_pca_scores_robust.rds"
)

pca_variance_file <- file.path(
  objects_dir,
  "objective1_pca_variance_robust.rds"
)

optimisation_results_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_results_robust.rds"
)

optimisation_memberships_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_memberships_robust.rds"
)

modularity_null_results_file <- file.path(
  objects_dir,
  "objective1_modularity_null_test_results.rds"
)

modularity_tests_csv_file <- file.path(
  tables_dir,
  "Objective1_ModularityNullTests.csv"
)

required_input_files <- c(
  robust_data_file,
  pca_scores_file,
  pca_variance_file,
  optimisation_results_file,
  optimisation_memberships_file,
  modularity_null_results_file,
  modularity_tests_csv_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 005_Figure3_PCA.R, ",
    "006.0_Optimisation_k_Louvain.R and ",
    "006.1_ModularityNullTest_Louvain.R before this script."
  )
}

# Prevent the figure from consuming a null-test result that predates
# either of the two 006.0 optimisation objects.

input_information <- file.info(required_input_files)

if (
  input_information[modularity_null_results_file, "mtime"] <
    max(input_information[
      c(optimisation_results_file, optimisation_memberships_file),
      "mtime"
    ])
) {
  stop(
    "The 006.1 null-model RDS predates at least one 006.0 object. ",
    "Rerun 006.1 before this script."
  )
}

# ------------------------------------------------------------
# 5. Load and validate the dependency objects
# ------------------------------------------------------------

control_robust <- readRDS(robust_data_file)
pca_scores <- readRDS(pca_scores_file)
pca_variance <- readRDS(pca_variance_file)
results_by_k <- readRDS(optimisation_results_file)
memberships_by_k <- readRDS(optimisation_memberships_file)
null_model_results <- readRDS(modularity_null_results_file)

modularity_tests_csv <- utils::read.csv(
  modularity_tests_csv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!is.data.frame(control_robust)) {
  stop("objective1_control_robust_z.rds is not a data frame.")
}

if (!is.data.frame(pca_scores)) {
  stop("objective1_pca_scores_robust.rds is not a data frame.")
}

if (!is.data.frame(pca_variance)) {
  stop("objective1_pca_variance_robust.rds is not a data frame.")
}

if (!is.data.frame(results_by_k)) {
  stop("The 006.0 optimisation-results RDS is not a data frame.")
}

if (!is.list(memberships_by_k)) {
  stop("The 006.0 optimisation-memberships RDS is not a list.")
}

if (!is.list(null_model_results)) {
  stop("The 006.1 modularity-null-test RDS is not a list.")
}

required_null_elements <- c(
  "Status",
  "Input_MD5",
  "Traceability_key",
  "Observed_selected",
  "Saved_006_0_k45",
  "Modularity_tests"
)

missing_null_elements <- setdiff(
  required_null_elements,
  names(null_model_results)
)

if (length(missing_null_elements) > 0L) {
  stop(
    "The 006.1 RDS lacks the following required elements:\n",
    paste(missing_null_elements, collapse = ", ")
  )
}

if (!identical(
  as.character(null_model_results$Status),
  "Formal modularity null-model validation"
)) {
  stop("The 006.1 RDS has an unexpected validation status.")
}

if (!identical(
  as.character(null_model_results$Traceability_key),
  "Author + ID"
)) {
  stop("The 006.1 RDS does not use Author + ID as its traceability key.")
}

# Confirm that 006.1 evaluated the same robust-data object that is
# being consumed here. In 006.1, Input_MD5 is intentionally stored
# as one unnamed scalar: the MD5 of objective1_control_robust_z.rds.
# Concordance with the two 006.0 objects is audited below through
# their modification times, saved selected-k row, imported
# membership, reconstructed graph and weighted modularity.

saved_robust_md5 <- as.character(
  unlist(null_model_results$Input_MD5, use.names = FALSE)
)

if (
  length(saved_robust_md5) != 1L ||
    is.na(saved_robust_md5) ||
    !grepl("^[[:xdigit:]]{32}$", saved_robust_md5)
) {
  stop("The 006.1 RDS does not contain a valid robust-data MD5 audit.")
}

current_robust_md5 <- as.character(
  unname(tools::md5sum(robust_data_file))
)

if (!identical(
  tolower(current_robust_md5),
  tolower(saved_robust_md5)
)) {
  stop(
    "The current ",
    basename(robust_data_file),
    " differs from the robust-data object evaluated by 006.1."
  )
}

modularity_tests <- null_model_results$Modularity_tests
observed_selected <- null_model_results$Observed_selected
saved_006_0_selected <- null_model_results$Saved_006_0_k45

if (!is.data.frame(modularity_tests)) {
  stop("Modularity_tests in the 006.1 RDS is not a data frame.")
}

if (!is.data.frame(observed_selected) || nrow(observed_selected) != 1L) {
  stop("Observed_selected in the 006.1 RDS is not a one-row data frame.")
}

if (!is.data.frame(saved_006_0_selected) || nrow(saved_006_0_selected) != 1L) {
  stop("Saved_006_0_k45 in the 006.1 RDS is not a one-row data frame.")
}

required_test_columns <- c(
  "Test",
  "Status",
  "Observed_statistic",
  "Empirical_p"
)

missing_test_columns <- setdiff(required_test_columns, names(modularity_tests))

if (length(missing_test_columns) > 0L) {
  stop(
    "The 006.1 modularity-test table lacks the following columns:\n",
    paste(missing_test_columns, collapse = ", ")
  )
}

missing_csv_test_columns <- setdiff(
  required_test_columns,
  names(modularity_tests_csv)
)

if (length(missing_csv_test_columns) > 0L) {
  stop(
    "The exported 006.1 test CSV lacks the following columns:\n",
    paste(missing_csv_test_columns, collapse = ", ")
  )
}

primary_test <- modularity_tests[
  modularity_tests$Status == "Primary inference",
  ,
  drop = FALSE
]

primary_test_csv <- modularity_tests_csv[
  modularity_tests_csv$Status == "Primary inference",
  ,
  drop = FALSE
]

if (nrow(primary_test) != 1L || nrow(primary_test_csv) != 1L) {
  stop("Exactly one primary inference must be present in each 006.1 output.")
}

for (column_i in required_test_columns) {
  if (!isTRUE(all.equal(
    primary_test[[column_i]],
    primary_test_csv[[column_i]],
    tolerance = 1e-12,
    check.attributes = FALSE
  ))) {
    stop(
      "The primary-test column '",
      column_i,
      "' differs between the 006.1 RDS and CSV."
    )
  }
}

primary_empirical_p <- as.numeric(primary_test$Empirical_p)
primary_observed_modularity <- as.numeric(primary_test$Observed_statistic)
primary_alpha <- 0.05

if (
  length(primary_empirical_p) != 1L ||
    !is.finite(primary_empirical_p) ||
    primary_empirical_p < 0 ||
    primary_empirical_p > 1
) {
  stop("The primary empirical p value in 006.1 is invalid.")
}

if (
  length(primary_observed_modularity) != 1L ||
    !is.finite(primary_observed_modularity)
) {
  stop("The primary observed modularity in 006.1 is invalid.")
}

evidence_of_discrete_communities <- primary_empirical_p < primary_alpha

if (evidence_of_discrete_communities) {
  stop(
    "The primary 006.1 conclusion now differs from the descriptive ",
    "continuous-space interpretation encoded in this script. Review the ",
    "analytical workflow before generating a biologically labelled figure."
  )
}

# ------------------------------------------------------------
# 6. Extract the selected solution from 006.1
# ------------------------------------------------------------

required_selected_columns <- c("k", "Communities")
missing_selected_columns <- setdiff(
  required_selected_columns,
  names(observed_selected)
)

if (length(missing_selected_columns) > 0L) {
  stop(
    "Observed_selected lacks the following columns:\n",
    paste(missing_selected_columns, collapse = ", ")
  )
}

selected_k <- as.integer(observed_selected$k[[1]])
selected_number_of_segments <- as.integer(
  observed_selected$Communities[[1]]
)

if (
  length(selected_k) != 1L ||
    is.na(selected_k) ||
    selected_k < 1L
) {
  stop("The selected k in the 006.1 RDS is invalid.")
}

if (
  length(selected_number_of_segments) != 1L ||
    is.na(selected_number_of_segments) ||
    selected_number_of_segments < 1L
) {
  stop("The selected number of segments in the 006.1 RDS is invalid.")
}

required_saved_006_0_columns <- c("k", "Clusters")
missing_saved_006_0_columns <- setdiff(
  required_saved_006_0_columns,
  names(saved_006_0_selected)
)

if (length(missing_saved_006_0_columns) > 0L) {
  stop(
    "Saved_006_0_k45 lacks the following columns:\n",
    paste(missing_saved_006_0_columns, collapse = ", ")
  )
}

if (as.integer(saved_006_0_selected$k[[1]]) != selected_k) {
  stop("006.1 does not report the same selected k for its observed and saved 006.0 objects.")
}

if (
  as.integer(saved_006_0_selected$Clusters[[1]]) !=
    selected_number_of_segments
) {
  stop(
    "006.1 does not report the same number of segments for its ",
    "observed and saved 006.0 objects."
  )
}

# The current post-validation workflow is explicitly the k = 45,
# two-segment exploratory solution. This guard prevents a stale
# figure script from silently consuming a different future result.

if (selected_k != 45L || selected_number_of_segments != 2L) {
  stop(
    "The current 006.1 result is not the expected k = 45, ",
    "two-segment solution. Review this figure script before proceeding."
  )
}

# ------------------------------------------------------------
# 7. Validate columns and Author + ID traceability
# ------------------------------------------------------------

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

expected_number_of_fish <- 89L

required_robust_columns <- c("Author", "ID", robust_variables)
missing_robust_columns <- setdiff(required_robust_columns, names(control_robust))

if (length(missing_robust_columns) > 0L) {
  stop(
    "The robust control dataset lacks the following columns:\n",
    paste(missing_robust_columns, collapse = ", ")
  )
}

required_pca_columns <- c("Author", "ID", "PC1", "PC2")
missing_pca_columns <- setdiff(required_pca_columns, names(pca_scores))

if (length(missing_pca_columns) > 0L) {
  stop(
    "The PCA-score object lacks the following columns:\n",
    paste(missing_pca_columns, collapse = ", ")
  )
}

required_variance_columns <- c("Component", "Percentage_of_variance")
missing_variance_columns <- setdiff(
  required_variance_columns,
  names(pca_variance)
)

if (length(missing_variance_columns) > 0L) {
  stop(
    "The PCA-variance object lacks the following columns:\n",
    paste(missing_variance_columns, collapse = ", ")
  )
}

required_optimisation_columns <- c(
  "k",
  "Clusters",
  "Weighted_modularity"
)

missing_optimisation_columns <- setdiff(
  required_optimisation_columns,
  names(results_by_k)
)

if (length(missing_optimisation_columns) > 0L) {
  stop(
    "The 006.0 optimisation table lacks the following columns:\n",
    paste(missing_optimisation_columns, collapse = ", ")
  )
}

make_traceability_key <- function(author, id) {
  paste(as.character(author), as.character(id), sep = " || ")
}

control_robust <- control_robust %>%
  dplyr::mutate(
    Author = as.character(Author),
    ID = as.character(ID),
    Traceability_ID = make_traceability_key(Author, ID)
  )

pca_scores <- pca_scores %>%
  dplyr::mutate(
    Author = as.character(Author),
    ID = as.character(ID),
    Traceability_ID = make_traceability_key(Author, ID)
  )

for (data_name_i in c("control_robust", "pca_scores")) {
  data_i <- get(data_name_i)

  if (
    anyNA(data_i$Author) ||
      anyNA(data_i$ID) ||
      any(data_i$Author == "") ||
      any(data_i$ID == "")
  ) {
    stop("Missing Author or ID values were detected in ", data_name_i, ".")
  }

  if (anyDuplicated(data_i$Traceability_ID) > 0L) {
    stop("Duplicated Author + ID keys were detected in ", data_name_i, ".")
  }
}

if (nrow(control_robust) != expected_number_of_fish) {
  stop(
    "The robust control dataset contains ",
    nrow(control_robust),
    " fish; ",
    expected_number_of_fish,
    " were expected."
  )
}

if (!setequal(control_robust$Traceability_ID, pca_scores$Traceability_ID)) {
  stop("The Author + ID keys in the robust and PCA datasets do not match.")
}

pca_scores <- pca_scores[
  match(control_robust$Traceability_ID, pca_scores$Traceability_ID),
  ,
  drop = FALSE
]

if (!identical(control_robust$Traceability_ID, pca_scores$Traceability_ID)) {
  stop("The PCA scores could not be aligned by Author + ID.")
}

non_numeric_robust_variables <- robust_variables[
  !vapply(control_robust[robust_variables], is.numeric, logical(1))
]

if (length(non_numeric_robust_variables) > 0L) {
  stop(
    "The following robust variables are not numeric:\n",
    paste(non_numeric_robust_variables, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 8. Audit the within-Author robust normalisation
# ------------------------------------------------------------

X <- control_robust %>%
  dplyr::select(dplyr::all_of(robust_variables)) %>%
  as.matrix()

storage.mode(X) <- "double"
rownames(X) <- control_robust$Traceability_ID

if (any(!is.finite(X))) {
  stop("The robust behavioural matrix contains missing or non-finite values.")
}

robust_audit <- dplyr::bind_rows(lapply(
  unique(control_robust$Author),
  function(author_i) {
    author_data <- control_robust[
      control_robust$Author == author_i,
      ,
      drop = FALSE
    ]

    dplyr::bind_rows(lapply(robust_variables, function(variable_i) {
      values_i <- author_data[[variable_i]]

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
    }))
  }
))

audit_tolerance <- 1e-6

if (any(abs(robust_audit$Median) > audit_tolerance)) {
  stop("At least one within-Author robust variable does not have median approximately zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > audit_tolerance)) {
  stop("At least one within-Author robust variable does not have scaled MAD approximately one.")
}

# ------------------------------------------------------------
# 9. Import the selected membership from 006.0
# ------------------------------------------------------------

results_by_k <- results_by_k %>%
  dplyr::mutate(
    k = as.integer(k),
    Clusters = as.integer(Clusters)
  ) %>%
  dplyr::arrange(k)

selected_result_006_0 <- results_by_k %>%
  dplyr::filter(.data$k == .env$selected_k)

if (nrow(selected_result_006_0) != 1L) {
  stop("The 006.0 optimisation table does not contain exactly one selected-k row.")
}

if (selected_result_006_0$Clusters[[1]] != selected_number_of_segments) {
  stop("006.0 and 006.1 disagree on the number of segments at the selected k.")
}

selected_membership_name <- paste0("k_", selected_k)

if (!selected_membership_name %in% names(memberships_by_k)) {
  stop(
    "The 006.0 membership RDS does not contain ",
    selected_membership_name,
    "."
  )
}

selected_membership_006_0 <- memberships_by_k[[selected_membership_name]]

if (!is.data.frame(selected_membership_006_0)) {
  stop("The selected membership object from 006.0 is not a data frame.")
}

required_membership_columns <- c("k", "Author", "ID", "Cluster")
missing_membership_columns <- setdiff(
  required_membership_columns,
  names(selected_membership_006_0)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The selected 006.0 membership lacks the following columns:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

selected_membership <- selected_membership_006_0 %>%
  dplyr::transmute(
    k = as.integer(k),
    Author = as.character(Author),
    ID = as.character(ID),
    Traceability_ID = make_traceability_key(Author, ID),
    Algorithmic_Segment = as.integer(Cluster)
  )

if (any(selected_membership$k != selected_k)) {
  stop("The imported 006.0 membership contains a different k value.")
}

if (
  anyNA(selected_membership$Traceability_ID) ||
    anyDuplicated(selected_membership$Traceability_ID) > 0L
) {
  stop("The imported 006.0 membership has missing or duplicated Author + ID keys.")
}

if (!setequal(
  selected_membership$Traceability_ID,
  control_robust$Traceability_ID
)) {
  stop("The selected 006.0 membership does not match the control fish.")
}

selected_membership <- selected_membership[
  match(control_robust$Traceability_ID, selected_membership$Traceability_ID),
  ,
  drop = FALSE
]

canonical_membership <- selected_membership$Algorithmic_Segment
names(canonical_membership) <- selected_membership$Traceability_ID

if (length(unique(canonical_membership)) != selected_number_of_segments) {
  stop("The imported 006.0 membership has an unexpected number of segments.")
}

# ------------------------------------------------------------
# 10. Reconstruct the selected graph without rerunning Louvain
# ------------------------------------------------------------

if (selected_k >= nrow(X)) {
  stop("The selected k must be smaller than the number of fish.")
}

distance_matrix <- as.matrix(
  stats::dist(X, method = "euclidean", diag = TRUE, upper = TRUE)
)

if (any(!is.finite(distance_matrix)) || any(distance_matrix < 0)) {
  stop("The Euclidean distance matrix is invalid.")
}

diag(distance_matrix) <- Inf

number_of_fish <- nrow(X)
index_tie_breaker <- seq_len(number_of_fish)

nearest_neighbour_index <- matrix(
  NA_integer_,
  nrow = number_of_fish,
  ncol = selected_k
)

for (fish_i in seq_len(number_of_fish)) {
  neighbour_order <- order(
    distance_matrix[fish_i, ],
    index_tie_breaker,
    na.last = NA
  )

  if (length(neighbour_order) < selected_k) {
    stop("Fewer than k valid neighbours were available for fish ", fish_i, ".")
  }

  nearest_neighbour_index[fish_i, ] <- neighbour_order[seq_len(selected_k)]
}

from_index <- rep(seq_len(number_of_fish), each = selected_k)
to_index <- as.vector(t(nearest_neighbour_index))
neighbour_distance <- distance_matrix[cbind(from_index, to_index)]

edge_list_directed <- tibble::tibble(
  From_Index = from_index,
  To_Index = to_index,
  From_Traceability_ID = control_robust$Traceability_ID[from_index],
  To_Traceability_ID = control_robust$Traceability_ID[to_index],
  Neighbour_Rank = rep(seq_len(selected_k), times = number_of_fish),
  Distance = neighbour_distance,
  Similarity = 1 / (1 + neighbour_distance)
)

if (
  any(from_index == to_index) ||
    any(!is.finite(edge_list_directed$Similarity)) ||
    any(edge_list_directed$Similarity <= 0)
) {
  stop("Invalid directed kNN relations were generated.")
}

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
    from = control_robust$Traceability_ID[Undirected_From],
    to = control_robust$Traceability_ID[Undirected_To],
    .before = 1
  )

graph_knn <- igraph::graph_from_data_frame(
  d = edge_list_undirected %>%
    dplyr::select(from, to, weight, Distance, Directed_Relations),
  directed = FALSE,
  vertices = tibble::tibble(name = control_robust$Traceability_ID)
)

edge_weights <- igraph::E(graph_knn)$weight
graph_components <- igraph::components(graph_knn)

if (
  igraph::is_directed(graph_knn) ||
    igraph::vcount(graph_knn) != number_of_fish ||
    !igraph::is_connected(graph_knn) ||
    any(!is.finite(edge_weights)) ||
    any(edge_weights <= 0)
) {
  stop("The reconstructed selected-k graph failed its integrity checks.")
}

# The partition is not recalculated. Its modularity is evaluated
# only to verify consistency among 006.0, 006.1 and this script.

reconstructed_modularity <- igraph::modularity(
  x = graph_knn,
  membership = canonical_membership[igraph::V(graph_knn)$name],
  weights = edge_weights,
  directed = FALSE
)

modularity_006_0 <- as.numeric(
  selected_result_006_0$Weighted_modularity[[1]]
)

if (!isTRUE(all.equal(
  as.numeric(reconstructed_modularity),
  modularity_006_0,
  tolerance = 1e-10
))) {
  stop("The reconstructed modularity does not match the selected 006.0 result.")
}

if (!isTRUE(all.equal(
  as.numeric(reconstructed_modularity),
  primary_observed_modularity,
  tolerance = 1e-10
))) {
  stop("The reconstructed modularity does not match the primary 006.1 statistic.")
}

# ------------------------------------------------------------
# 11. Segment tables and inference status
# ------------------------------------------------------------

segment_levels_numeric <- sort(unique(canonical_membership))
segment_levels <- paste("Segment", segment_levels_numeric)

segment_label_map <- stats::setNames(
  segment_levels,
  as.character(segment_levels_numeric)
)

control_segmented <- control_robust %>%
  dplyr::mutate(
    Algorithmic_Segment = factor(
      segment_label_map[as.character(
        canonical_membership[Traceability_ID]
      )],
      levels = segment_levels
    )
  )

if (anyNA(control_segmented$Algorithmic_Segment)) {
  stop("At least one fish lacks an imported algorithmic segment.")
}

segment_membership <- control_segmented %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Algorithmic_Segment
  ) %>%
  dplyr::arrange(
    Algorithmic_Segment,
    Author,
    suppressWarnings(as.numeric(ID)),
    ID
  )

segment_sizes <- segment_membership %>%
  dplyr::count(Algorithmic_Segment, name = "n") %>%
  dplyr::mutate(Percentage = 100 * n / sum(n))

if (!identical(sort(segment_sizes$n), c(40L, 49L))) {
  stop("The selected exploratory segmentation does not contain 40 and 49 fish.")
}

inference_status <- list(
  Analysis = "Exploratory weighted kNN-Louvain segmentation",
  Selected_k = selected_k,
  Number_of_algorithmic_segments = selected_number_of_segments,
  Segment_sizes = sort(segment_sizes$n),
  Observed_weighted_modularity = as.numeric(reconstructed_modularity),
  Primary_test = as.character(primary_test$Test),
  Primary_empirical_p = primary_empirical_p,
  Alpha = primary_alpha,
  Evidence_of_discrete_communities = evidence_of_discrete_communities,
  Interpretation = paste0(
    "Descriptive algorithmic segmentation of a continuous ",
    "locomotor-exploratory space; not evidence of discrete ",
    "behavioural communities."
  ),
  Permitted_use = paste0(
    "Descriptive or supplementary visualisation of the partition ",
    "returned by 006.0."
  ),
  Prohibited_use = paste0(
    "Biological profile labels, normative classification, inferential ",
    "comparisons between segments, or downstream assignment of stressed fish."
  )
)

graph_summary <- tibble::tibble(
  Number_of_fish = number_of_fish,
  Number_of_behavioural_variables = ncol(X),
  Traceability_key = "Author + ID",
  Normalisation = "Within Author: (x - median) / (1.4826 x raw MAD)",
  Distance = "Euclidean",
  Similarity = "1 / (1 + D)",
  Graph_type = "Weighted, undirected union kNN",
  Selected_k = selected_k,
  Vertices = igraph::vcount(graph_knn),
  Directed_kNN_relations = nrow(edge_list_directed),
  Undirected_edges = igraph::ecount(graph_knn),
  Graph_density = igraph::edge_density(graph_knn, loops = FALSE),
  Mean_degree = mean(igraph::degree(graph_knn)),
  Connected_components = graph_components$no,
  Number_of_algorithmic_segments = selected_number_of_segments,
  Weighted_modularity = as.numeric(reconstructed_modularity),
  Primary_empirical_p = primary_empirical_p,
  Evidence_of_discrete_communities = evidence_of_discrete_communities
)

# ------------------------------------------------------------
# 12. Combine PCA coordinates with the imported segmentation
# ------------------------------------------------------------

plot_data <- pca_scores %>%
  dplyr::left_join(
    segment_membership %>%
      dplyr::select(Traceability_ID, Algorithmic_Segment),
    by = "Traceability_ID"
  )

if (anyNA(plot_data$Algorithmic_Segment)) {
  stop("At least one PCA coordinate lacks an imported algorithmic segment.")
}

pca_centroids <- plot_data %>%
  dplyr::group_by(Algorithmic_Segment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  )

pc1_row <- pca_variance %>%
  dplyr::filter(Component == "PC1")

pc2_row <- pca_variance %>%
  dplyr::filter(Component == "PC2")

if (nrow(pc1_row) != 1L || nrow(pc2_row) != 1L) {
  stop("PC1 or PC2 variance could not be uniquely identified.")
}

percent1 <- round(as.numeric(pc1_row$Percentage_of_variance), 1)
percent2 <- round(as.numeric(pc2_row$Percentage_of_variance), 1)

if (!is.finite(percent1) || !is.finite(percent2)) {
  stop("PC1 or PC2 explained variance is not finite.")
}

# ------------------------------------------------------------
# 13. Generate the descriptive supplementary figure
# ------------------------------------------------------------

segment_palette <- stats::setNames(
  c("#63B8A6", "#F07A5A"),
  segment_levels
)

figure_louvain_descriptive <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = PC1, y = PC2)
) +
  ggplot2::geom_point(
    ggplot2::aes(colour = Algorithmic_Segment),
    size = 2.6,
    alpha = 0.85
  ) +
  ggplot2::geom_point(
    data = pca_centroids,
    ggplot2::aes(x = PC1, y = PC2),
    inherit.aes = FALSE,
    shape = 4,
    size = 6,
    stroke = 1.5,
    colour = "black"
  ) +
  ggrepel::geom_label_repel(
    data = pca_centroids,
    ggplot2::aes(
      x = PC1,
      y = PC2,
      label = Algorithmic_Segment
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold",
    show.legend = FALSE,
    seed = 1234L
  ) +
  ggplot2::scale_colour_manual(
    values = segment_palette,
    breaks = segment_levels,
    drop = FALSE
  ) +
  ggplot2::labs(
    x = paste0("PC1 (", percent1, "%)"),
    y = paste0("PC2 (", percent2, "%)"),
    colour = "Exploratory segment",
    caption = paste0(
      "Descriptive PCA projection only; no evidence of discrete communities ",
      "(selection-adjusted p = ",
      formatC(primary_empirical_p, format = "f", digits = 3),
      ")."
    )
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(colour = "black"),
    legend.position = "right",
    plot.caption = ggplot2::element_text(
      size = 9,
      colour = "black",
      hjust = 0
    )
  )

print(figure_louvain_descriptive)

# ------------------------------------------------------------
# 14. Output paths
# ------------------------------------------------------------

pdf_output <- file.path(
  figures_dir,
  "FigureS_Louvain_ExploratorySegmentation.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "FigureS_Louvain_ExploratorySegmentation.tiff"
)

membership_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_Membership.csv"
)

segment_sizes_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_Sizes.csv"
)

pca_centroids_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_PC_Centroids.csv"
)

graph_summary_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_GraphSummary.csv"
)

dependency_audit_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_DependencyAudit.csv"
)

directed_relations_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_DirectedKNNRelations.csv"
)

undirected_edges_csv <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_WeightedEdges.csv"
)

# ------------------------------------------------------------
# 15. Export the figure and audit tables
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = pdf_output,
  plot = figure_louvain_descriptive,
  width = 7,
  height = 6.3,
  units = "in"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = figure_louvain_descriptive,
  width = 7,
  height = 6.3,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

dependency_audit <- tibble::tibble(
  Dependency = c(
    "005 robust data",
    "005 PCA scores",
    "005 PCA variance",
    "006.0 optimisation results",
    "006.0 optimisation memberships",
    "006.1 complete null-model results",
    "006.1 modularity-test CSV"
  ),
  File = required_input_files,
  MD5 = unname(tools::md5sum(required_input_files)),
  Modified = as.character(file.info(required_input_files)$mtime)
)

utils::write.csv(segment_membership, membership_csv, row.names = FALSE)
utils::write.csv(segment_sizes, segment_sizes_csv, row.names = FALSE)
utils::write.csv(pca_centroids, pca_centroids_csv, row.names = FALSE)
utils::write.csv(graph_summary, graph_summary_csv, row.names = FALSE)
utils::write.csv(dependency_audit, dependency_audit_csv, row.names = FALSE)
utils::write.csv(edge_list_directed, directed_relations_csv, row.names = FALSE)
utils::write.csv(edge_list_undirected, undirected_edges_csv, row.names = FALSE)

# ------------------------------------------------------------
# 16. Save explicitly descriptive objects
# ------------------------------------------------------------

segmented_data_rds <- file.path(
  objects_dir,
  "objective1_control_louvain_exploratory_segmentation.rds"
)

membership_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_membership.rds"
)

graph_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph.rds"
)

graph_summary_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph_summary.rds"
)

plot_data_rds <- file.path(
  objects_dir,
  "objective1_pca_with_louvain_exploratory_segmentation.rds"
)

inference_status_rds <- file.path(
  objects_dir,
  "objective1_louvain_inference_status.rds"
)

figure_rds <- file.path(
  objects_dir,
  "objective1_figureS_louvain_exploratory_segmentation.rds"
)

session_info_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segmentation_sessionInfo.txt"
)

saveRDS(control_segmented, segmented_data_rds)
saveRDS(segment_membership, membership_rds)
saveRDS(graph_knn, graph_rds)
saveRDS(graph_summary, graph_summary_rds)
saveRDS(plot_data, plot_data_rds)
saveRDS(inference_status, inference_status_rds)
saveRDS(figure_louvain_descriptive, figure_rds)

capture.output(utils::sessionInfo(), file = session_info_file)

# Deliberately not written:
# - a new Louvain model;
# - a validated cluster-profile object;
# - higher-/lower-exploration labels;
# - downstream classification objects for stressed fish.

# ------------------------------------------------------------
# 17. Verify outputs and print the final audit
# ------------------------------------------------------------

expected_output_files <- c(
  pdf_output,
  tiff_output,
  membership_csv,
  segment_sizes_csv,
  pca_centroids_csv,
  graph_summary_csv,
  dependency_audit_csv,
  directed_relations_csv,
  undirected_edges_csv,
  segmented_data_rds,
  membership_rds,
  graph_rds,
  graph_summary_rds,
  plot_data_rds,
  inference_status_rds,
  figure_rds,
  session_info_file
)

output_information <- file.info(expected_output_files)

if (
  any(!file.exists(expected_output_files)) ||
    any(is.na(output_information$size)) ||
    any(output_information$size <= 0)
) {
  stop("At least one expected descriptive Louvain output is absent or empty.")
}

cat("\n============================================================\n")
cat("DESCRIPTIVE LOUVAIN SEGMENTATION AFTER NULL-MODEL TEST\n")
cat("============================================================\n")
cat("\nControl fish: ", number_of_fish, "\n", sep = "")
cat("Traceability: Author + ID\n")
cat("Selected k imported from 006.1: ", selected_k, "\n", sep = "")
cat(
  "Algorithmic segments imported from 006.0: ",
  selected_number_of_segments,
  " (",
  paste(sort(segment_sizes$n), collapse = "/"),
  ")\n",
  sep = ""
)
cat(
  "Observed weighted modularity: ",
  sprintf("%.6f", reconstructed_modularity),
  "\n",
  sep = ""
)
cat(
  "Primary selection-adjusted empirical p: ",
  sprintf("%.9f", primary_empirical_p),
  "\n",
  sep = ""
)
cat(
  "Evidence of discrete communities: ",
  toupper(as.character(evidence_of_discrete_communities)),
  "\n",
  sep = ""
)
cat(
  "Interpretation: descriptive segmentation of a continuous ",
  "locomotor-exploratory space.\n"
)

cat("\nPrimary 006.1 result:\n")
print(primary_test, row.names = FALSE)

cat("\nWithin-Author robust-normalisation audit:\n")
print(robust_audit, n = Inf, width = Inf)

cat("\nSelected 006.0 result:\n")
print(selected_result_006_0, n = Inf, width = Inf)

cat("\nGraph summary:\n")
print(graph_summary, n = Inf, width = Inf)

cat("\nAlgorithmic segment sizes:\n")
print(segment_sizes, n = Inf, width = Inf)

cat(
  "\nDescriptive figure saved as:\n",
  normalizePath(pdf_output, winslash = "/", mustWork = TRUE),
  "\n",
  normalizePath(tiff_output, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

cat("\nAll dependency and integrity checks passed.\n")
cat("============================================================\n")
