# ============================================================
# 008_Table2_ClusterStatistics.R
#
# Objective 1
# Statistical characterisation of the final Louvain clusters
# in the LAPCOM control population.
#
# Final clustering configuration:
# - Robust normalisation: (x - median) / (1.4826 x raw MAD)
# - Weighted, undirected union kNN graph
# - Euclidean distance
# - Similarity = 1 / (1 + D)
# - k = 56
# - Two Louvain clusters
#
# Because exactly two clusters are compared, the primary
# inferential analysis is the Wilcoxon rank-sum test rather than
# Dunn post hoc comparisons. Rank-biserial correlation is used
# as the directional effect size.
#
# Important interpretation:
# These tests characterise clusters derived from the same four
# behavioural variables. Therefore, p-values are descriptive of
# the resulting partition and are not independent confirmatory
# tests of an externally specified grouping.
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

library(dplyr)
library(tibble)
library(openxlsx)

# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")

if (!dir.exists(objects_dir)) {
  stop("Objects directory not found:\n", objects_dir)
}

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

louvain_data_file <- file.path(
  objects_dir,
  "objective1_control_robust_louvain.rds"
)

membership_file <- file.path(
  objects_dir,
  "objective1_louvain_membership_robust.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_graph_summary_robust.rds"
)

required_input_files <- c(
  louvain_data_file,
  membership_file,
  graph_summary_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 006_Figure4_Louvain.R before this script."
  )
}

# ------------------------------------------------------------
# 4. Load and verify the final Louvain objects
# ------------------------------------------------------------

control_louvain <- readRDS(louvain_data_file)
cluster_membership_reference <- readRDS(membership_file)
graph_summary <- readRDS(graph_summary_file)

if (!is.data.frame(control_louvain)) {
  stop(basename(louvain_data_file), " does not contain a data frame.")
}

if (!is.data.frame(cluster_membership_reference)) {
  stop(basename(membership_file), " does not contain a data frame.")
}

if (!is.data.frame(graph_summary)) {
  stop(basename(graph_summary_file), " does not contain a data frame.")
}

expected_n <- 89L
expected_k <- 56L
expected_number_of_clusters <- 2L
expected_cluster_sizes <- c(41L, 48L)

required_graph_columns <- c(
  "Number_of_fish",
  "k",
  "Number_of_clusters"
)

missing_graph_columns <- setdiff(
  required_graph_columns,
  names(graph_summary)
)

if (length(missing_graph_columns) > 0L) {
  stop(
    "The graph-summary object is missing the following columns:\n",
    paste(missing_graph_columns, collapse = ", ")
  )
}

if (nrow(graph_summary) != 1L) {
  stop("The graph-summary object must contain exactly one row.")
}

if (as.integer(graph_summary$Number_of_fish[[1]]) != expected_n) {
  stop("The graph summary does not correspond to 89 control fish.")
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

# ------------------------------------------------------------
# 5. Define and verify behavioural variables
# ------------------------------------------------------------

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

variable_labels <- c(
  distance_rz = "Distance",
  crossing_rz = "Line crossings",
  top_entry_rz = "Upper-zone entries",
  top_time_rz = "Upper-zone time"
)

required_control_columns <- c(
  "ID",
  "Cluster",
  robust_variables
)

missing_control_columns <- setdiff(
  required_control_columns,
  names(control_louvain)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The final Louvain dataset is missing the following columns:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_membership_columns <- c("ID", "Cluster")

missing_membership_columns <- setdiff(
  required_membership_columns,
  names(cluster_membership_reference)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The reference membership object is missing the following columns:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

non_numeric_variables <- robust_variables[
  !vapply(
    control_louvain[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following robust behavioural variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    ID = as.character(ID),
    Cluster = factor(as.character(Cluster), levels = c("1", "2"))
  )

cluster_membership_reference <- cluster_membership_reference %>%
  dplyr::transmute(
    ID = as.character(ID),
    Cluster = factor(as.character(Cluster), levels = c("1", "2"))
  )

# ------------------------------------------------------------
# 6. Audit IDs, memberships and robust normalisation
# ------------------------------------------------------------

if (nrow(control_louvain) != expected_n) {
  stop(
    "The final Louvain dataset contains ",
    nrow(control_louvain),
    " fish; 89 were expected."
  )
}

if (anyNA(control_louvain$ID) || any(control_louvain$ID == "")) {
  stop("Missing or empty fish IDs were detected in control_louvain.")
}

if (anyDuplicated(control_louvain$ID) > 0L) {
  stop("Duplicated fish IDs were detected in control_louvain.")
}

if (
  anyNA(cluster_membership_reference$ID) ||
  any(cluster_membership_reference$ID == "")
) {
  stop("Missing or empty fish IDs were detected in the membership object.")
}

if (anyDuplicated(cluster_membership_reference$ID) > 0L) {
  stop("Duplicated fish IDs were detected in the membership object.")
}

if (!setequal(control_louvain$ID, cluster_membership_reference$ID)) {
  stop(
    "Fish IDs differ between control_louvain and the reference membership object."
  )
}

cluster_membership_reference <- cluster_membership_reference[
  match(control_louvain$ID, cluster_membership_reference$ID),
  ,
  drop = FALSE
]

if (!identical(control_louvain$ID, cluster_membership_reference$ID)) {
  stop("The reference membership object could not be aligned by fish ID.")
}

if (anyNA(control_louvain$Cluster)) {
  stop("Invalid or missing cluster labels were detected in control_louvain.")
}

if (anyNA(cluster_membership_reference$Cluster)) {
  stop("Invalid or missing cluster labels were detected in the membership object.")
}

if (
  !identical(
    as.character(control_louvain$Cluster),
    as.character(cluster_membership_reference$Cluster)
  )
) {
  stop(
    "Cluster assignments in control_louvain do not match the final k = 56 membership object."
  )
}

behavioural_matrix <- as.matrix(
  control_louvain[robust_variables]
)

storage.mode(behavioural_matrix) <- "double"

if (any(!is.finite(behavioural_matrix))) {
  stop(
    "Missing or non-finite values were detected in the robust behavioural variables."
  )
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

audit_tolerance <- 1e-6

if (any(abs(robust_audit$Median) > audit_tolerance)) {
  stop("At least one robust variable does not have median approximately zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > audit_tolerance)) {
  stop("At least one robust variable does not have scaled MAD approximately one.")
}

cluster_sizes <- control_louvain %>%
  dplyr::count(Cluster, name = "n") %>%
  dplyr::mutate(Percentage = 100 * n / sum(n)) %>%
  dplyr::arrange(Cluster)

if (nrow(cluster_sizes) != expected_number_of_clusters) {
  stop("The final dataset does not contain exactly two clusters.")
}

if (!identical(sort(as.integer(cluster_sizes$n)), expected_cluster_sizes)) {
  stop("The final clusters do not contain 41 and 48 fish.")
}

# ------------------------------------------------------------
# 7. Helper functions
# ------------------------------------------------------------

format_p_value <- function(p_value) {
  format.pval(
    p_value,
    digits = 4,
    eps = 2.2e-16
  )
}

significance_code <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.0001 ~ "****",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

rank_biserial_magnitude <- function(effect_size) {
  absolute_effect <- abs(effect_size)
  
  dplyr::case_when(
    absolute_effect < 0.10 ~ "negligible",
    absolute_effect < 0.30 ~ "small",
    absolute_effect < 0.50 ~ "moderate",
    TRUE ~ "large"
  )
}

# ------------------------------------------------------------
# 8. Descriptive statistics by cluster
# ------------------------------------------------------------

descriptive_results_list <- lapply(
  robust_variables,
  function(variable_i) {
    control_louvain %>%
      dplyr::group_by(Cluster) %>%
      dplyr::summarise(
        n = dplyr::n(),
        Mean = mean(.data[[variable_i]], na.rm = TRUE),
        SD = stats::sd(.data[[variable_i]], na.rm = TRUE),
        Median = stats::median(.data[[variable_i]], na.rm = TRUE),
        Q1 = stats::quantile(
          .data[[variable_i]],
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        ),
        Q3 = stats::quantile(
          .data[[variable_i]],
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        ),
        IQR = stats::IQR(
          .data[[variable_i]],
          na.rm = TRUE,
          type = 7
        ),
        Minimum = min(.data[[variable_i]], na.rm = TRUE),
        Maximum = max(.data[[variable_i]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Variable_Code = variable_i,
        Variable = unname(variable_labels[[variable_i]]),
        .before = 1
      )
  }
)

cluster_descriptive_statistics <- dplyr::bind_rows(
  descriptive_results_list
) %>%
  dplyr::arrange(
    match(Variable_Code, robust_variables),
    Cluster
  )

expected_descriptive_rows <-
  length(robust_variables) * expected_number_of_clusters

if (nrow(cluster_descriptive_statistics) != expected_descriptive_rows) {
  stop(
    "The descriptive table contains ",
    nrow(cluster_descriptive_statistics),
    " rows; ",
    expected_descriptive_rows,
    " were expected."
  )
}

# ------------------------------------------------------------
# 9. Wilcoxon rank-sum tests and rank-biserial effect sizes
# ------------------------------------------------------------

comparison_results_list <- lapply(
  robust_variables,
  function(variable_i) {
    x_cluster_1 <- control_louvain[
      control_louvain$Cluster == "1",
      variable_i,
      drop = TRUE
    ]
    
    x_cluster_2 <- control_louvain[
      control_louvain$Cluster == "2",
      variable_i,
      drop = TRUE
    ]
    
    n_cluster_1 <- length(x_cluster_1)
    n_cluster_2 <- length(x_cluster_2)
    
    if (
      n_cluster_1 != cluster_sizes$n[cluster_sizes$Cluster == "1"] ||
      n_cluster_2 != cluster_sizes$n[cluster_sizes$Cluster == "2"]
    ) {
      stop("Cluster sizes were not preserved for variable ", variable_i, ".")
    }
    
    wilcoxon_result <- stats::wilcox.test(
      x = x_cluster_1,
      y = x_cluster_2,
      alternative = "two.sided",
      exact = FALSE,
      correct = FALSE
    )
    
    pooled_values <- c(x_cluster_1, x_cluster_2)
    pooled_ranks <- rank(pooled_values, ties.method = "average")
    
    rank_sum_cluster_1 <- sum(
      pooled_ranks[seq_len(n_cluster_1)]
    )
    
    u_cluster_1 <-
      rank_sum_cluster_1 - n_cluster_1 * (n_cluster_1 + 1) / 2
    
    u_cluster_2 <-
      n_cluster_1 * n_cluster_2 - u_cluster_1
    
    rank_biserial <-
      2 * u_cluster_1 / (n_cluster_1 * n_cluster_2) - 1
    
    if (!is.finite(rank_biserial)) {
      stop("A non-finite rank-biserial effect size was generated.")
    }
    
    if (rank_biserial < -1 - 1e-12 || rank_biserial > 1 + 1e-12) {
      stop("Rank-biserial correlation fell outside [-1, 1].")
    }
    
    rank_biserial <- max(-1, min(1, rank_biserial))
    
    tibble::tibble(
      Variable_Code = variable_i,
      Variable = unname(variable_labels[[variable_i]]),
      Group_1 = "Cluster 1",
      Group_2 = "Cluster 2",
      n_Cluster_1 = n_cluster_1,
      n_Cluster_2 = n_cluster_2,
      Median_Cluster_1 = stats::median(x_cluster_1),
      Median_Cluster_2 = stats::median(x_cluster_2),
      Median_Difference_1_minus_2 =
        stats::median(x_cluster_1) - stats::median(x_cluster_2),
      Wilcoxon_W = as.numeric(wilcoxon_result$statistic),
      U_Cluster_1 = as.numeric(u_cluster_1),
      U_Cluster_2 = as.numeric(u_cluster_2),
      P_Value = as.numeric(wilcoxon_result$p.value),
      Rank_Biserial = as.numeric(rank_biserial),
      Effect_Direction = ifelse(
        rank_biserial > 0,
        "Cluster 1 > Cluster 2",
        ifelse(
          rank_biserial < 0,
          "Cluster 1 < Cluster 2",
          "No directional difference"
        )
      ),
      Effect_Magnitude = rank_biserial_magnitude(rank_biserial),
      Test = "Wilcoxon rank-sum (Mann-Whitney)",
      Exact = FALSE,
      Continuity_Correction = FALSE
    )
  }
)

cluster_comparison_results <- dplyr::bind_rows(
  comparison_results_list
) %>%
  dplyr::mutate(
    P_Adjusted_Holm = stats::p.adjust(P_Value, method = "holm"),
    P_Value_Formatted = vapply(
      P_Value,
      format_p_value,
      character(1)
    ),
    P_Adjusted_Holm_Formatted = vapply(
      P_Adjusted_Holm,
      format_p_value,
      character(1)
    ),
    Adjusted_Significance = significance_code(P_Adjusted_Holm)
  ) %>%
  dplyr::arrange(match(Variable_Code, robust_variables))

if (nrow(cluster_comparison_results) != length(robust_variables)) {
  stop("The number of two-cluster comparison results is incorrect.")
}

numeric_result_columns <- c(
  "Wilcoxon_W",
  "U_Cluster_1",
  "U_Cluster_2",
  "P_Value",
  "P_Adjusted_Holm",
  "Rank_Biserial"
)

if (
  any(
    !is.finite(
      as.matrix(cluster_comparison_results[numeric_result_columns])
    )
  )
) {
  stop("At least one statistical result is non-finite.")
}

# ------------------------------------------------------------
# 10. Kruskal-Wallis equivalence audit for two groups
# ------------------------------------------------------------

kruskal_audit_list <- lapply(
  robust_variables,
  function(variable_i) {
    analysis_formula <- stats::reformulate(
      termlabels = "Cluster",
      response = variable_i
    )
    
    kruskal_result <- stats::kruskal.test(
      formula = analysis_formula,
      data = control_louvain
    )
    
    wilcoxon_p <- cluster_comparison_results$P_Value[
      cluster_comparison_results$Variable_Code == variable_i
    ]
    
    tibble::tibble(
      Variable_Code = variable_i,
      Variable = unname(variable_labels[[variable_i]]),
      Kruskal_H = as.numeric(kruskal_result$statistic),
      Degrees_of_Freedom = as.numeric(kruskal_result$parameter),
      Kruskal_P_Value = as.numeric(kruskal_result$p.value),
      Wilcoxon_P_Value = as.numeric(wilcoxon_p),
      Absolute_P_Difference = abs(
        as.numeric(kruskal_result$p.value) - as.numeric(wilcoxon_p)
      )
    )
  }
)

kruskal_equivalence_audit <- dplyr::bind_rows(
  kruskal_audit_list
) %>%
  dplyr::arrange(match(Variable_Code, robust_variables))

if (nrow(kruskal_equivalence_audit) != length(robust_variables)) {
  stop("The Kruskal-Wallis audit is incomplete.")
}

# ------------------------------------------------------------
# 11. Analysis configuration and interpretation notes
# ------------------------------------------------------------

analysis_configuration <- tibble::tibble(
  Parameter = c(
    "Number of control fish",
    "Selected k",
    "Number of Louvain clusters",
    "Cluster sizes",
    "Normalisation",
    "Distance",
    "Similarity",
    "Graph",
    "Primary comparison",
    "Multiple-testing adjustment",
    "Effect size"
  ),
  Value = c(
    as.character(expected_n),
    as.character(expected_k),
    as.character(expected_number_of_clusters),
    paste(expected_cluster_sizes, collapse = " and "),
    "(x - median) / (1.4826 x raw MAD)",
    "Euclidean",
    "1 / (1 + D)",
    "Weighted, undirected union kNN",
    "Wilcoxon rank-sum; asymptotic, no continuity correction",
    "Holm across the four behavioural variables",
    "Rank-biserial correlation; positive values indicate Cluster 1 > Cluster 2"
  )
)

analysis_notes <- tibble::tibble(
  Note = c(
    paste0(
      "The clusters were derived from the same four behavioural variables ",
      "analysed here. The resulting p-values describe the separation induced ",
      "by the clustering solution and must not be interpreted as independent ",
      "confirmatory evidence for externally specified groups."
    ),
    paste0(
      "Dunn post hoc comparisons were not performed because the final ",
      "solution contains exactly two clusters. The Wilcoxon rank-sum test ",
      "is the direct non-parametric comparison for two independent groups."
    ),
    paste0(
      "The Kruskal-Wallis results are retained only as a two-group ",
      "equivalence audit and are not treated as an additional independent test."
    )
  )
)

# ------------------------------------------------------------
# 12. Output paths
# ------------------------------------------------------------

excel_output <- file.path(
  tables_dir,
  "Table2_ClusterStatistics.xlsx"
)

cluster_sizes_csv <- file.path(
  tables_dir,
  "Table2_ClusterSizes.csv"
)

descriptive_csv <- file.path(
  tables_dir,
  "Table2_ClusterDescriptiveStatistics.csv"
)

comparison_csv <- file.path(
  tables_dir,
  "Table2_WilcoxonRankSum_RankBiserial.csv"
)

kruskal_audit_csv <- file.path(
  tables_dir,
  "Table2_KruskalWallis_EquivalenceAudit.csv"
)

configuration_csv <- file.path(
  tables_dir,
  "Table2_AnalysisConfiguration.csv"
)

notes_csv <- file.path(
  tables_dir,
  "Table2_InterpretationNotes.csv"
)

combined_results_rds <- file.path(
  objects_dir,
  "objective1_cluster_statistics_robust.rds"
)

descriptive_rds <- file.path(
  objects_dir,
  "objective1_cluster_descriptive_statistics_robust.rds"
)

wilcoxon_rds <- file.path(
  objects_dir,
  "objective1_cluster_wilcoxon_results_robust.rds"
)

rank_biserial_rds <- file.path(
  objects_dir,
  "objective1_cluster_rank_biserial_effect_sizes_robust.rds"
)

session_info_file <- file.path(
  objects_dir,
  "objective1_cluster_statistics_sessionInfo.txt"
)

# ------------------------------------------------------------
# 13. Export tables
# ------------------------------------------------------------

openxlsx::write.xlsx(
  x = list(
    Cluster_sizes = cluster_sizes,
    Descriptive_statistics = cluster_descriptive_statistics,
    Wilcoxon_and_effect_size = cluster_comparison_results,
    Kruskal_equivalence_audit = kruskal_equivalence_audit,
    RobustZ_audit = robust_audit,
    Analysis_configuration = analysis_configuration,
    Interpretation_notes = analysis_notes
  ),
  file = excel_output,
  overwrite = TRUE
)

utils::write.csv(
  cluster_sizes,
  file = cluster_sizes_csv,
  row.names = FALSE
)

utils::write.csv(
  cluster_descriptive_statistics,
  file = descriptive_csv,
  row.names = FALSE
)

utils::write.csv(
  cluster_comparison_results,
  file = comparison_csv,
  row.names = FALSE
)

utils::write.csv(
  kruskal_equivalence_audit,
  file = kruskal_audit_csv,
  row.names = FALSE
)

utils::write.csv(
  analysis_configuration,
  file = configuration_csv,
  row.names = FALSE
)

utils::write.csv(
  analysis_notes,
  file = notes_csv,
  row.names = FALSE
)

# ------------------------------------------------------------
# 14. Save R objects
# ------------------------------------------------------------

combined_results <- list(
  Configuration = analysis_configuration,
  Notes = analysis_notes,
  Robust_Audit = robust_audit,
  Cluster_Sizes = cluster_sizes,
  Descriptive_Statistics = cluster_descriptive_statistics,
  Wilcoxon_and_Rank_Biserial = cluster_comparison_results,
  Kruskal_Equivalence_Audit = kruskal_equivalence_audit
)

saveRDS(combined_results, combined_results_rds)
saveRDS(cluster_descriptive_statistics, descriptive_rds)
saveRDS(cluster_comparison_results, wilcoxon_rds)
saveRDS(
  cluster_comparison_results %>%
    dplyr::select(
      Variable_Code,
      Variable,
      Group_1,
      Group_2,
      Rank_Biserial,
      Effect_Direction,
      Effect_Magnitude
    ),
  rank_biserial_rds
)

capture.output(
  utils::sessionInfo(),
  file = session_info_file
)

# ------------------------------------------------------------
# 15. Verify exported files
# ------------------------------------------------------------

expected_output_files <- c(
  excel_output,
  cluster_sizes_csv,
  descriptive_csv,
  comparison_csv,
  kruskal_audit_csv,
  configuration_csv,
  notes_csv,
  combined_results_rds,
  descriptive_rds,
  wilcoxon_rds,
  rank_biserial_rds,
  session_info_file
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
# 16. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("TABLE 2 - FINAL TWO-CLUSTER STATISTICS\n")
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

cat("\nDescriptive statistics:\n")
print(cluster_descriptive_statistics, n = Inf, width = Inf)

cat("\nWilcoxon rank-sum tests and rank-biserial effect sizes:\n")
print(cluster_comparison_results, n = Inf, width = Inf)

cat("\nKruskal-Wallis two-group equivalence audit:\n")
print(kruskal_equivalence_audit, n = Inf, width = Inf)

cat("\nInterpretation note:\n")
cat(analysis_notes$Note[[1]], "\n")

cat(
  "\nExcel workbook saved at:\n",
  normalizePath(excel_output, winslash = "/", mustWork = FALSE),
  "\n",
  sep = ""
)

cat("\n============================================================\n")