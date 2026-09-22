# ============================================================
# 009_Table3_ArchetypeCentroids.R
#
# Objective 1
# Final normative archetype centroids for the LAPCOM control
# population.
#
# Definitive clustering configuration:
# - robust normalisation: (x - median) / (1.4826 x raw MAD)
# - Euclidean distance
# - similarity: 1 / (1 + D)
# - weighted, undirected union kNN graph
# - k = 56
# - two Louvain clusters
#
# The centroid of each archetype is the arithmetic mean of each
# robustly normalised behavioural variable within that cluster.
#
# Required upstream scripts:
# 005_Figure3_PCA.R
# 006_Figure4_Louvain.R
# 007_Figure5_ClusterProfiles.R
# ============================================================

# ------------------------------------------------------------
# 1. Project directory and packages
# ------------------------------------------------------------

base_dir <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)

required_packages <- c("dplyr", "tidyr", "tibble", "openxlsx")

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
library(tidyr)
library(tibble)
library(openxlsx)

# ------------------------------------------------------------
# 2. Paths and required input files
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")

if (!dir.exists(objects_dir)) {
  stop("Objects directory not found:\n", objects_dir)
}

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

control_file <- file.path(
  objects_dir,
  "objective1_control_robust_louvain.rds"
)

configuration_file <- file.path(
  objects_dir,
  "objective1_selected_louvain_configuration_robust.rds"
)

membership_file <- file.path(
  objects_dir,
  "objective1_selected_louvain_membership_robust.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_graph_summary_robust.rds"
)

normalisation_parameters_file <- file.path(
  objects_dir,
  "objective1_robust_normalisation_parameters.rds"
)

profile_wide_file <- file.path(
  objects_dir,
  "objective1_louvain_cluster_profiles_wide_robust.rds"
)

required_input_files <- c(
  control_file,
  configuration_file,
  membership_file,
  graph_summary_file,
  normalisation_parameters_file,
  profile_wide_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun scripts 005, 006 and 007 before this script."
  )
}

# ------------------------------------------------------------
# 3. Load input objects
# ------------------------------------------------------------

control_louvain <- readRDS(control_file)
selected_configuration <- readRDS(configuration_file)
selected_membership <- readRDS(membership_file)
graph_summary <- readRDS(graph_summary_file)
normalisation_parameters <- readRDS(normalisation_parameters_file)
profile_wide_reference <- readRDS(profile_wide_file)

if (!is.data.frame(control_louvain)) {
  stop(basename(control_file), " does not contain a data frame.")
}

if (!is.list(selected_configuration)) {
  stop(basename(configuration_file), " does not contain a list.")
}

if (!is.data.frame(selected_membership)) {
  stop(basename(membership_file), " does not contain a data frame.")
}

if (!is.data.frame(graph_summary) || nrow(graph_summary) != 1L) {
  stop(basename(graph_summary_file), " must contain a one-row data frame.")
}

if (!is.data.frame(normalisation_parameters)) {
  stop(basename(normalisation_parameters_file), " does not contain a data frame.")
}

if (!is.data.frame(profile_wide_reference)) {
  stop(basename(profile_wide_file), " does not contain a data frame.")
}

# ------------------------------------------------------------
# 4. Definitive analysis specifications
# ------------------------------------------------------------

expected_n <- 89L
expected_k <- 56L
expected_number_of_clusters <- 2L
expected_cluster_sizes <- c("1" = 41L, "2" = 48L)

raw_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

centroid_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

variable_labels <- c(
  distance = "Distance",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

audit_tolerance <- 1e-6
comparison_tolerance <- 1e-12

# ------------------------------------------------------------
# 5. Validate final configuration and graph summary
# ------------------------------------------------------------

required_configuration_fields <- c(
  "Number_of_Clusters",
  "k",
  "Normalisation",
  "Distance",
  "Similarity",
  "Graph"
)

missing_configuration_fields <- setdiff(
  required_configuration_fields,
  names(selected_configuration)
)

if (length(missing_configuration_fields) > 0L) {
  stop(
    "The selected configuration is missing:\n",
    paste(missing_configuration_fields, collapse = ", ")
  )
}

if (as.integer(selected_configuration$k) != expected_k) {
  stop("The selected configuration does not specify k = 56.")
}

if (
  as.integer(selected_configuration$Number_of_Clusters) !=
  expected_number_of_clusters
) {
  stop("The selected configuration does not specify two clusters.")
}

required_graph_columns <- c(
  "Number_of_fish",
  "Number_of_behavioural_variables",
  "k",
  "Connected",
  "Number_of_clusters",
  "Weighted_modularity"
)

missing_graph_columns <- setdiff(
  required_graph_columns,
  names(graph_summary)
)

if (length(missing_graph_columns) > 0L) {
  stop(
    "The graph summary is missing:\n",
    paste(missing_graph_columns, collapse = ", ")
  )
}

if (as.integer(graph_summary$Number_of_fish[[1]]) != expected_n) {
  stop("The graph summary does not contain 89 control fish.")
}

if (
  as.integer(graph_summary$Number_of_behavioural_variables[[1]]) !=
  length(robust_variables)
) {
  stop("The graph summary does not contain four behavioural variables.")
}

if (as.integer(graph_summary$k[[1]]) != expected_k) {
  stop("The graph summary does not correspond to k = 56.")
}

if (
  as.integer(graph_summary$Number_of_clusters[[1]]) !=
  expected_number_of_clusters
) {
  stop("The graph summary does not correspond to two clusters.")
}

if (!isTRUE(graph_summary$Connected[[1]])) {
  stop("The final kNN graph is not connected.")
}

if (!is.finite(graph_summary$Weighted_modularity[[1]])) {
  stop("The weighted modularity is not finite.")
}

# ------------------------------------------------------------
# 6. Validate and align fish IDs and cluster assignments
# ------------------------------------------------------------

required_control_columns <- c("ID", "Cluster", robust_variables)
missing_control_columns <- setdiff(
  required_control_columns,
  names(control_louvain)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The final Louvain dataset is missing:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_membership_columns <- c(
  "ID",
  "k",
  "Number_of_Clusters",
  "Cluster"
)

missing_membership_columns <- setdiff(
  required_membership_columns,
  names(selected_membership)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The selected-membership object is missing:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

cluster_to_integer <- function(x, object_name) {
  x_character <- as.character(x)
  x_integer <- suppressWarnings(as.integer(x_character))
  
  if (anyNA(x_integer)) {
    stop(
      "Non-integer cluster labels were detected in ",
      object_name,
      "."
    )
  }
  
  x_integer
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    ID = as.character(ID),
    Cluster_integer = cluster_to_integer(Cluster, "control_louvain")
  )

selected_membership <- selected_membership %>%
  dplyr::mutate(
    ID = as.character(ID),
    k = as.integer(k),
    Number_of_Clusters = as.integer(Number_of_Clusters),
    Cluster_integer = cluster_to_integer(Cluster, "selected_membership")
  )

if (
  anyNA(control_louvain$ID) ||
  any(control_louvain$ID == "") ||
  anyDuplicated(control_louvain$ID) > 0L
) {
  stop("Missing, empty, or duplicated IDs were detected in control_louvain.")
}

if (
  anyNA(selected_membership$ID) ||
  any(selected_membership$ID == "") ||
  anyDuplicated(selected_membership$ID) > 0L
) {
  stop("Missing, empty, or duplicated IDs were detected in selected_membership.")
}

if (nrow(control_louvain) != expected_n) {
  stop(
    "The final Louvain dataset contains ",
    nrow(control_louvain),
    " fish; 89 were expected."
  )
}

if (!setequal(control_louvain$ID, selected_membership$ID)) {
  stop("Fish IDs differ between the Louvain and membership objects.")
}

selected_membership <- selected_membership[
  match(control_louvain$ID, selected_membership$ID),
  ,
  drop = FALSE
]

if (!identical(control_louvain$ID, selected_membership$ID)) {
  stop("The selected membership could not be aligned by fish ID.")
}

if (
  any(selected_membership$k != expected_k) ||
  any(
    selected_membership$Number_of_Clusters !=
    expected_number_of_clusters
  )
) {
  stop("The membership object is not the final k = 56 two-cluster solution.")
}

if (
  !identical(
    control_louvain$Cluster_integer,
    selected_membership$Cluster_integer
  )
) {
  stop(
    "Cluster assignments in control_louvain do not match ",
    "the final k = 56 membership object."
  )
}

observed_cluster_labels <- sort(unique(control_louvain$Cluster_integer))

if (!identical(observed_cluster_labels, c(1L, 2L))) {
  stop("The final solution must contain cluster labels 1 and 2.")
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    Cluster = factor(
      Cluster_integer,
      levels = c(1L, 2L)
    )
  ) %>%
  dplyr::select(-Cluster_integer)

# ------------------------------------------------------------
# 7. Audit robust normalisation
# ------------------------------------------------------------

non_numeric_variables <- robust_variables[
  !vapply(control_louvain[robust_variables], is.numeric, logical(1))
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following robust variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

robust_matrix <- as.matrix(control_louvain[robust_variables])
storage.mode(robust_matrix) <- "double"

if (any(!is.finite(robust_matrix))) {
  stop("Missing or non-finite robust behavioural values were detected.")
}

robust_audit <- tibble::tibble(
  Variable = robust_variables,
  n = nrow(control_louvain),
  Median = vapply(
    control_louvain[robust_variables],
    stats::median,
    numeric(1),
    na.rm = TRUE
  ),
  Scaled_MAD = vapply(
    control_louvain[robust_variables],
    function(x) {
      stats::mad(
        x,
        center = stats::median(x, na.rm = TRUE),
        constant = 1.4826,
        na.rm = TRUE
      )
    },
    numeric(1)
  )
)

if (any(abs(robust_audit$Median) > audit_tolerance)) {
  stop("At least one robust variable does not have median approximately zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > audit_tolerance)) {
  stop("At least one robust variable does not have scaled MAD approximately one.")
}

required_parameter_columns <- c(
  "Variable",
  "MAD_raw",
  "MAD_scaled"
)

missing_parameter_columns <- setdiff(
  required_parameter_columns,
  names(normalisation_parameters)
)

if (length(missing_parameter_columns) > 0L) {
  stop(
    "The normalisation-parameter object is missing:\n",
    paste(missing_parameter_columns, collapse = ", ")
  )
}

parameter_order <- match(
  raw_variables,
  normalisation_parameters$Variable
)

if (anyNA(parameter_order)) {
  stop("Normalisation parameters were not found for all four variables.")
}

normalisation_parameters <- normalisation_parameters[
  parameter_order,
  ,
  drop = FALSE
]

expected_scaled_mad <- 1.4826 * normalisation_parameters$MAD_raw
scaled_mad_difference <- abs(
  normalisation_parameters$MAD_scaled - expected_scaled_mad
)

scaled_mad_tolerance <- audit_tolerance * pmax(
  1,
  abs(expected_scaled_mad)
)

if (any(scaled_mad_difference > scaled_mad_tolerance)) {
  stop(
    "The stored parameters do not satisfy ",
    "MAD_scaled = 1.4826 x MAD_raw."
  )
}

# ------------------------------------------------------------
# 8. Audit definitive cluster sizes
# ------------------------------------------------------------

cluster_sizes <- control_louvain %>%
  dplyr::count(Cluster, name = "n") %>%
  dplyr::mutate(
    Percentage = 100 * n / sum(n)
  ) %>%
  dplyr::arrange(Cluster)

observed_sizes <- stats::setNames(
  as.integer(cluster_sizes$n),
  as.character(cluster_sizes$Cluster)
)

if (!identical(observed_sizes, expected_cluster_sizes)) {
  stop(
    "Unexpected cluster sizes. Expected Cluster 1 = 41 and ",
    "Cluster 2 = 48."
  )
}

# ------------------------------------------------------------
# 9. Calculate final normative archetype centroids
# ------------------------------------------------------------

centroids <- control_louvain %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarise(
    n = dplyr::n(),
    distance = mean(distance_rz),
    crossing = mean(crossing_rz),
    top_entry = mean(top_entry_rz),
    top_time = mean(top_time_rz),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Cluster)

if (nrow(centroids) != expected_number_of_clusters) {
  stop("The centroid table does not contain exactly two rows.")
}

if (sum(centroids$n) != expected_n) {
  stop("Centroid sample sizes do not sum to 89.")
}

centroid_matrix <- as.matrix(centroids[centroid_variables])
storage.mode(centroid_matrix) <- "double"

if (any(!is.finite(centroid_matrix))) {
  stop("At least one archetype centroid is not finite.")
}

centroids_long <- centroids %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(centroid_variables),
    names_to = "Variable_Code",
    values_to = "Centroid_Mean_Robust_Z"
  ) %>%
  dplyr::mutate(
    Variable = unname(variable_labels[Variable_Code]),
    .after = Variable_Code
  ) %>%
  dplyr::select(
    Cluster,
    n,
    Variable_Code,
    Variable,
    Centroid_Mean_Robust_Z
  )

if (anyNA(centroids_long$Variable)) {
  stop("At least one centroid variable could not be labelled.")
}

# ------------------------------------------------------------
# 10. Cross-check against the profiles generated by script 007
# ------------------------------------------------------------

required_profile_columns <- c(
  "Cluster",
  "n",
  centroid_variables
)

missing_profile_columns <- setdiff(
  required_profile_columns,
  names(profile_wide_reference)
)

if (length(missing_profile_columns) > 0L) {
  stop(
    "The profile object generated by script 007 is missing:\n",
    paste(missing_profile_columns, collapse = ", ")
  )
}

profile_wide_reference <- profile_wide_reference %>%
  dplyr::mutate(
    Cluster = factor(
      cluster_to_integer(Cluster, "profile_wide_reference"),
      levels = c(1L, 2L)
    ),
    n = as.integer(n)
  ) %>%
  dplyr::arrange(Cluster)

if (
  !identical(
    as.character(centroids$Cluster),
    as.character(profile_wide_reference$Cluster)
  )
) {
  stop("Cluster order differs between scripts 007 and 009.")
}

if (!identical(as.integer(centroids$n), profile_wide_reference$n)) {
  stop("Cluster sizes differ between scripts 007 and 009.")
}

centroid_profile_difference <- abs(
  as.matrix(centroids[centroid_variables]) -
    as.matrix(profile_wide_reference[centroid_variables])
)

if (any(centroid_profile_difference > comparison_tolerance)) {
  stop(
    "The centroids calculated by script 009 do not match ",
    "the mean profiles generated by script 007."
  )
}

centroid_validation <- tibble::tibble(
  Check = c(
    "Final k",
    "Number of clusters",
    "Cluster 1 size",
    "Cluster 2 size",
    "Membership agreement with script 006",
    "Centroid agreement with script 007",
    "Maximum absolute centroid difference"
  ),
  Result = c(
    as.character(expected_k),
    as.character(expected_number_of_clusters),
    as.character(expected_cluster_sizes[["1"]]),
    as.character(expected_cluster_sizes[["2"]]),
    "Exact",
    "Exact within tolerance",
    format(
      max(centroid_profile_difference),
      scientific = TRUE,
      digits = 6
    )
  )
)

analysis_configuration <- tibble::tibble(
  Parameter = c(
    "Number of control fish",
    "Selected k",
    "Number of clusters",
    "Cluster 1 n",
    "Cluster 2 n",
    "Normalisation",
    "Distance",
    "Similarity",
    "Graph",
    "Centroid definition",
    "Weighted modularity"
  ),
  Value = c(
    as.character(expected_n),
    as.character(expected_k),
    as.character(expected_number_of_clusters),
    as.character(expected_cluster_sizes[["1"]]),
    as.character(expected_cluster_sizes[["2"]]),
    "(x - median) / (1.4826 x raw MAD)",
    "Euclidean",
    "1 / (1 + D)",
    "Weighted, undirected union kNN",
    "Arithmetic mean in four-dimensional robust behavioural space",
    as.character(graph_summary$Weighted_modularity[[1]])
  )
)

# ------------------------------------------------------------
# 11. Export Table 3 and R objects
# ------------------------------------------------------------

excel_output <- file.path(
  tables_dir,
  "Table3_ArchetypeCentroids.xlsx"
)

csv_output <- file.path(
  tables_dir,
  "Table3_ArchetypeCentroids.csv"
)

long_csv_output <- file.path(
  tables_dir,
  "Table3_ArchetypeCentroids_Long.csv"
)

cluster_size_output <- file.path(
  tables_dir,
  "Table3_ArchetypeClusterSizes.csv"
)

audit_output <- file.path(
  tables_dir,
  "Table3_ArchetypeCentroids_Audit.csv"
)

configuration_output <- file.path(
  tables_dir,
  "Table3_ArchetypeCentroids_Configuration.csv"
)

centroids_rds <- file.path(
  objects_dir,
  "control_archetype_centroids.rds"
)

centroids_long_rds <- file.path(
  objects_dir,
  "objective1_control_archetype_centroids_long_robust.rds"
)

centroid_audit_rds <- file.path(
  objects_dir,
  "objective1_control_archetype_centroids_audit_robust.rds"
)

openxlsx::write.xlsx(
  x = list(
    Archetype_centroids = centroids,
    Long_format = centroids_long,
    Cluster_sizes = cluster_sizes,
    RobustZ_audit = robust_audit,
    Validation = centroid_validation,
    Configuration = analysis_configuration
  ),
  file = excel_output,
  overwrite = TRUE
)

utils::write.csv(
  centroids,
  file = csv_output,
  row.names = FALSE
)

utils::write.csv(
  centroids_long,
  file = long_csv_output,
  row.names = FALSE
)

utils::write.csv(
  cluster_sizes,
  file = cluster_size_output,
  row.names = FALSE
)

utils::write.csv(
  centroid_validation,
  file = audit_output,
  row.names = FALSE
)

utils::write.csv(
  analysis_configuration,
  file = configuration_output,
  row.names = FALSE
)

saveRDS(centroids, centroids_rds)
saveRDS(centroids_long, centroids_long_rds)

saveRDS(
  list(
    Centroids = centroids,
    Centroids_long = centroids_long,
    Cluster_sizes = cluster_sizes,
    Robust_audit = robust_audit,
    Validation = centroid_validation,
    Configuration = analysis_configuration
  ),
  centroid_audit_rds
)

# ------------------------------------------------------------
# 12. Verify exported files
# ------------------------------------------------------------

expected_output_files <- c(
  excel_output,
  csv_output,
  long_csv_output,
  cluster_size_output,
  audit_output,
  configuration_output,
  centroids_rds,
  centroids_long_rds,
  centroid_audit_rds
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following output files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 13. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("TABLE 3 - FINAL NORMATIVE ARCHETYPE CENTROIDS\n")
cat("============================================================\n")
cat("\nNumber of control fish: ", expected_n, "\n", sep = "")
cat("Selected k: ", expected_k, "\n", sep = "")
cat(
  "Number of Louvain clusters: ",
  expected_number_of_clusters,
  "\n",
  sep = ""
)

cat("\nRobust-normalisation audit:\n")
print(robust_audit, n = Inf, width = Inf)

cat("\nCluster sizes:\n")
print(cluster_sizes, n = Inf, width = Inf)

cat("\nNormative archetype centroids:\n")
print(centroids, n = Inf, width = Inf)

cat("\nCentroid validation:\n")
print(centroid_validation, n = Inf, width = Inf)

cat(
  "\nCentroid scale:\n",
  "Arithmetic mean of robust z-scores calculated as ",
  "(x - median) / (1.4826 x raw MAD).\n",
  sep = ""
)

cat(
  "\nExcel workbook saved at:\n",
  normalizePath(excel_output, winslash = "/", mustWork = FALSE),
  "\n",
  sep = ""
)

cat(
  "Centroid R object saved at:\n",
  normalizePath(centroids_rds, winslash = "/", mustWork = FALSE),
  "\n",
  sep = ""
)

cat("\n============================================================\n")