# ============================================================
# 007_Figure5_ClusterProfiles.R
#
# Objective 1
# Behavioural profiles of the final Louvain clusters in the
# LAPCOM control population.
#
# Required upstream script: 006_Figure4_Louvain.R
# Definitive configuration: weighted undirected union kNN,
# k = 56, two clusters, robust z-score using MAD x 1.4826.
# ============================================================

# ------------------------------------------------------------
# 1. Project and packages
# ------------------------------------------------------------

base_dir <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)

required_packages <- c("dplyr", "tidyr", "tibble", "ggplot2")
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
library(ggplot2)

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

louvain_data_file <- file.path(
  objects_dir,
  "objective1_control_robust_louvain.rds"
)

selected_configuration_file <- file.path(
  objects_dir,
  "objective1_selected_louvain_configuration_robust.rds"
)

selected_membership_file <- file.path(
  objects_dir,
  "objective1_selected_louvain_membership_robust.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_graph_summary_robust.rds"
)

required_input_files <- c(
  louvain_data_file,
  selected_configuration_file,
  selected_membership_file,
  graph_summary_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 006_Figure4_Louvain.R before this script."
  )
}

# ------------------------------------------------------------
# 3. Load and validate upstream objects
# ------------------------------------------------------------

control_louvain <- readRDS(louvain_data_file)
selected_configuration <- readRDS(selected_configuration_file)
selected_membership <- readRDS(selected_membership_file)
graph_summary <- readRDS(graph_summary_file)

if (!is.data.frame(control_louvain)) {
  stop("The final Louvain object is not a data frame.")
}

if (!is.list(selected_configuration)) {
  stop("The selected-configuration object is not a list.")
}

if (!is.data.frame(selected_membership)) {
  stop("The selected-membership object is not a data frame.")
}

if (!is.data.frame(graph_summary) || nrow(graph_summary) != 1L) {
  stop("The graph-summary object must be a one-row data frame.")
}

expected_k <- 56L
expected_clusters <- 2L
expected_fish <- 89L
expected_sizes <- c(41L, 48L)

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

required_configuration_fields <- c(
  "Number_of_Clusters", "k", "Normalisation", "Distance",
  "Similarity", "Graph", "Louvain_Seed"
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

if (as.integer(selected_configuration$Number_of_Clusters) != expected_clusters) {
  stop("The selected configuration does not specify two clusters.")
}

required_graph_columns <- c(
  "Number_of_fish", "k", "Connected", "Number_of_clusters",
  "Weighted_modularity"
)

missing_graph_columns <- setdiff(required_graph_columns, names(graph_summary))

if (length(missing_graph_columns) > 0L) {
  stop(
    "The graph summary is missing:\n",
    paste(missing_graph_columns, collapse = ", ")
  )
}

if (
  as.integer(graph_summary$Number_of_fish) != expected_fish ||
  as.integer(graph_summary$k) != expected_k ||
  as.integer(graph_summary$Number_of_clusters) != expected_clusters ||
  !isTRUE(graph_summary$Connected)
) {
  stop("The graph summary is inconsistent with the final k = 56 solution.")
}

if (!is.finite(graph_summary$Weighted_modularity)) {
  stop("The weighted modularity is not finite.")
}

# ------------------------------------------------------------
# 4. Validate and align memberships
# ------------------------------------------------------------

required_louvain_columns <- c("ID", "Cluster", robust_variables)
missing_louvain_columns <- setdiff(
  required_louvain_columns,
  names(control_louvain)
)

if (length(missing_louvain_columns) > 0L) {
  stop(
    "The Louvain dataset is missing:\n",
    paste(missing_louvain_columns, collapse = ", ")
  )
}

required_membership_columns <- c("ID", "k", "Number_of_Clusters", "Cluster")
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
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  result <- suppressWarnings(as.integer(x))
  
  if (anyNA(result)) {
    stop("Invalid cluster labels were detected in ", object_name, ".")
  }
  
  result
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    ID = as.character(ID),
    Cluster_integer = cluster_to_integer(Cluster, "control_louvain")
  )

selected_membership <- selected_membership %>%
  dplyr::transmute(
    ID = as.character(ID),
    k = as.integer(k),
    Number_of_Clusters = as.integer(Number_of_Clusters),
    Cluster_integer = cluster_to_integer(Cluster, "selected_membership")
  )

if (
  anyNA(control_louvain$ID) || any(control_louvain$ID == "") ||
  anyDuplicated(control_louvain$ID) > 0L
) {
  stop("Missing, empty, or duplicated IDs were detected in control_louvain.")
}

if (
  anyNA(selected_membership$ID) || any(selected_membership$ID == "") ||
  anyDuplicated(selected_membership$ID) > 0L
) {
  stop("Missing, empty, or duplicated IDs were detected in selected_membership.")
}

if (!setequal(control_louvain$ID, selected_membership$ID)) {
  stop("Fish IDs differ between the two upstream objects.")
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
  any(selected_membership$Number_of_Clusters != expected_clusters)
) {
  stop("The membership object is not the final k = 56, two-cluster solution.")
}

if (!identical(
  control_louvain$Cluster_integer,
  selected_membership$Cluster_integer
)) {
  stop("Cluster assignments differ between the two upstream objects.")
}

cluster_levels <- sort(unique(control_louvain$Cluster_integer))

if (!identical(cluster_levels, c(1L, 2L))) {
  stop("The final cluster labels are not exactly 1 and 2.")
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    Cluster = factor(Cluster_integer, levels = cluster_levels)
  ) %>%
  dplyr::select(-Cluster_integer)

# ------------------------------------------------------------
# 5. Audit the final data and robust normalisation
# ------------------------------------------------------------

if (nrow(control_louvain) != expected_fish) {
  stop("The final Louvain dataset does not contain 89 fish.")
}

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

if (
  any(abs(robust_audit$Median) > 1e-6) ||
  any(abs(robust_audit$Scaled_MAD - 1) > 1e-6)
) {
  stop("The robust-normalisation audit failed.")
}

cluster_sizes <- control_louvain %>%
  dplyr::count(Cluster, name = "n") %>%
  dplyr::mutate(Percentage = 100 * n / sum(n)) %>%
  dplyr::arrange(Cluster)

if (!identical(sort(cluster_sizes$n), expected_sizes)) {
  stop("The final clusters do not contain 41 and 48 fish.")
}

# ------------------------------------------------------------
# 6. Prepare individual and summary profile tables
# ------------------------------------------------------------

variable_keys <- c(
  distance_rz = "distance",
  crossing_rz = "crossing",
  top_entry_rz = "top_entry",
  top_time_rz = "top_time"
)

variable_labels <- c(
  distance = "Distance",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

profile_individual_long <- control_louvain %>%
  dplyr::select(ID, Cluster, dplyr::all_of(robust_variables)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(robust_variables),
    names_to = "Robust_Variable",
    values_to = "Robust_Z_Score"
  ) %>%
  dplyr::mutate(
    Variable_Key = unname(variable_keys[Robust_Variable]),
    Variable = factor(
      unname(variable_labels[Variable_Key]),
      levels = unname(variable_labels),
      ordered = TRUE
    )
  ) %>%
  dplyr::arrange(Cluster, Variable, ID)

if (
  anyNA(profile_individual_long$Variable_Key) ||
  anyNA(profile_individual_long$Variable)
) {
  stop("At least one behavioural variable could not be labelled.")
}

if (nrow(profile_individual_long) != expected_fish * length(robust_variables)) {
  stop("The individual long-format profile table has an incorrect size.")
}

cluster_profile_summary <- profile_individual_long %>%
  dplyr::group_by(Cluster, Robust_Variable, Variable_Key, Variable) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean_Robust_Z_Score = mean(Robust_Z_Score),
    SD_Robust_Z_Score = stats::sd(Robust_Z_Score),
    SE_Robust_Z_Score = SD_Robust_Z_Score / sqrt(n),
    Median_Robust_Z_Score = stats::median(Robust_Z_Score),
    Q1_Robust_Z_Score = as.numeric(
      stats::quantile(Robust_Z_Score, probs = 0.25, names = FALSE)
    ),
    Q3_Robust_Z_Score = as.numeric(
      stats::quantile(Robust_Z_Score, probs = 0.75, names = FALSE)
    ),
    IQR_Robust_Z_Score = stats::IQR(Robust_Z_Score),
    Minimum_Robust_Z_Score = min(Robust_Z_Score),
    Maximum_Robust_Z_Score = max(Robust_Z_Score),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Cluster, Variable)

if (nrow(cluster_profile_summary) != expected_clusters * length(robust_variables)) {
  stop("The cluster-profile summary has an incorrect number of rows.")
}

summary_columns <- c(
  "Mean_Robust_Z_Score", "SD_Robust_Z_Score", "SE_Robust_Z_Score",
  "Median_Robust_Z_Score", "Q1_Robust_Z_Score", "Q3_Robust_Z_Score",
  "IQR_Robust_Z_Score", "Minimum_Robust_Z_Score",
  "Maximum_Robust_Z_Score"
)

if (any(!is.finite(as.matrix(cluster_profile_summary[summary_columns])))) {
  stop("At least one cluster-profile statistic is not finite.")
}

# Preserve the structures and object names used by the original script.
cluster_profile_long <- cluster_profile_summary %>%
  dplyr::select(
    Cluster, n, Variable_Key, Variable, Mean_Robust_Z_Score
  )

cluster_profile_wide <- control_louvain %>%
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

# ------------------------------------------------------------
# 7. Generate Figure 5
# ------------------------------------------------------------

legend_labels <- stats::setNames(
  paste0(
    "Cluster ", as.character(cluster_sizes$Cluster),
    " (n = ", cluster_sizes$n, ")"
  ),
  as.character(cluster_sizes$Cluster)
)

figure_5 <- ggplot2::ggplot(
  cluster_profile_long,
  ggplot2::aes(
    x = Variable,
    y = Mean_Robust_Z_Score,
    group = Cluster,
    colour = Cluster
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    linetype = "dashed",
    colour = "grey50"
  ) +
  ggplot2::geom_line(linewidth = 1.15) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_colour_discrete(labels = legend_labels) +
  ggplot2::labs(
    x = NULL,
    y = "Mean robust z-score",
    colour = "Cluster"
  ) +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.text.x = ggplot2::element_text(
      colour = "black",
      angle = 20,
      hjust = 1
    ),
    axis.text.y = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.position = "right"
  )

print(figure_5)

# ------------------------------------------------------------
# 8. Export figure, tables, and R objects
# ------------------------------------------------------------

pdf_output <- file.path(figures_dir, "Figure5_ClusterProfiles.pdf")
tiff_output <- file.path(figures_dir, "Figure5_ClusterProfiles.tiff")

wide_output <- file.path(tables_dir, "Figure5_ClusterProfiles_Wide.csv")
long_output <- file.path(tables_dir, "Figure5_ClusterProfiles_Long.csv")
summary_output <- file.path(tables_dir, "Figure5_ClusterProfiles_Summary.csv")
individual_output <- file.path(
  tables_dir,
  "Figure5_ClusterProfiles_IndividualLong.csv"
)
cluster_size_output <- file.path(tables_dir, "Figure5_ClusterSizes.csv")
configuration_output <- file.path(
  tables_dir,
  "Figure5_ClusterProfiles_Configuration.csv"
)

figure_configuration <- tibble::tibble(
  Number_of_control_fish = expected_fish,
  k = expected_k,
  Number_of_clusters = expected_clusters,
  Cluster_1_n = cluster_sizes$n[as.character(cluster_sizes$Cluster) == "1"],
  Cluster_2_n = cluster_sizes$n[as.character(cluster_sizes$Cluster) == "2"],
  Normalisation = "(x - median) / (1.4826 x raw MAD)",
  Profile_statistic = "Arithmetic mean of robust z-scores",
  Weighted_modularity = as.numeric(graph_summary$Weighted_modularity)
)

ggplot2::ggsave(
  filename = pdf_output,
  plot = figure_5,
  width = 8,
  height = 5.5,
  units = "in"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = figure_5,
  width = 8,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

utils::write.csv(cluster_profile_wide, wide_output, row.names = FALSE)
utils::write.csv(cluster_profile_long, long_output, row.names = FALSE)
utils::write.csv(cluster_profile_summary, summary_output, row.names = FALSE)
utils::write.csv(profile_individual_long, individual_output, row.names = FALSE)
utils::write.csv(cluster_sizes, cluster_size_output, row.names = FALSE)
utils::write.csv(figure_configuration, configuration_output, row.names = FALSE)

wide_rds <- file.path(
  objects_dir,
  "objective1_louvain_cluster_profiles_wide_robust.rds"
)
long_rds <- file.path(
  objects_dir,
  "objective1_louvain_cluster_profiles_long_robust.rds"
)
summary_rds <- file.path(
  objects_dir,
  "objective1_louvain_cluster_profiles_summary_robust.rds"
)
individual_rds <- file.path(
  objects_dir,
  "objective1_louvain_cluster_profiles_individual_robust.rds"
)
figure_rds <- file.path(
  objects_dir,
  "objective1_figure5_cluster_profiles_robust.rds"
)
session_info_file <- file.path(
  objects_dir,
  "objective1_figure5_cluster_profiles_sessionInfo.txt"
)

saveRDS(cluster_profile_wide, wide_rds)
saveRDS(cluster_profile_long, long_rds)
saveRDS(cluster_profile_summary, summary_rds)
saveRDS(profile_individual_long, individual_rds)
saveRDS(figure_5, figure_rds)
capture.output(utils::sessionInfo(), file = session_info_file)

expected_output_files <- c(
  pdf_output, tiff_output, wide_output, long_output, summary_output,
  individual_output, cluster_size_output, configuration_output,
  wide_rds, long_rds, summary_rds, individual_rds, figure_rds,
  session_info_file
)

missing_output_files <- expected_output_files[!file.exists(expected_output_files)]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected output files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 9. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("FIGURE 5 - FINAL LOUVAIN CLUSTER PROFILES\n")
cat("============================================================\n")
cat("\nNumber of control fish: ", nrow(control_louvain), "\n", sep = "")
cat("Selected k: ", expected_k, "\n", sep = "")
cat("Number of clusters: ", expected_clusters, "\n", sep = "")

cat("\nRobust-normalisation audit:\n")
print(robust_audit, n = Inf, width = Inf)

cat("\nCluster sizes:\n")
print(cluster_sizes, n = Inf, width = Inf)

cat("\nMean robust behavioural profiles:\n")
print(cluster_profile_wide, n = Inf, width = Inf)

cat("\nDetailed cluster-profile summary:\n")
print(cluster_profile_summary, n = Inf, width = Inf)

cat(
  "\nFigure saved as:\n",
  normalizePath(pdf_output, winslash = "/", mustWork = FALSE),
  "\n",
  normalizePath(tiff_output, winslash = "/", mustWork = FALSE),
  "\n",
  sep = ""
)

cat("\n============================================================\n")