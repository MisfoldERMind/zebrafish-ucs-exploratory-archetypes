# ============================================================
# 008_TableS_ExploratorySegmentSummaries.R
#
# Objective 1
# Supplementary descriptive table for the exploratory Louvain
# segmentation of the LAPCOM control population.
#
# Required execution order:
# 1. 005_Figure3_PCA.R
# 2. 006.0_Optimisation_k_Louvain.R
# 3. 006.1_ModularityNullTest_Louvain.R
# 4. corrected 006_Figure4_Louvain.R
# 5. 007_FigureS_ExploratorySegmentSummaries.R
# 6. this script
#
# Analytical status:
# - k = 45 and the 40/49 membership are imported; Louvain is not rerun.
# - The primary selection-adjusted modularity test from 006.1 is
#   non-significant (p approximately 0.393).
# - There is no evidence of discrete behavioural communities.
# - The two labels are algorithmic segments of a continuous
#   locomotor-exploratory space, not validated biological categories.
# - The same four variables constructed and describe the partition.
#   Accordingly, no inferential comparison, p value, confidence interval,
#   effect size or significance code between segments is calculated.
# - This output is supplementary and must not be treated as manuscript
#   Table 2, a normative atlas or a classifier for stressed fish.
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
# 2. Input and output directories
# ------------------------------------------------------------

objects_dir <- file.path("03_Results", "Objects")
tables_dir <- file.path("03_Results", "Tables")

for (directory_i in c(objects_dir, tables_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

# ------------------------------------------------------------
# 3. Required inputs from 006.0, 006.1, corrected 006 and 007
# ------------------------------------------------------------

# Corrected 006 integrated outputs.

segmented_data_file <- file.path(
  objects_dir,
  "objective1_control_louvain_exploratory_segmentation.rds"
)

integrated_membership_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_membership.rds"
)

inference_status_file <- file.path(
  objects_dir,
  "objective1_louvain_inference_status.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph_summary.rds"
)

corrected_006_dependency_audit_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_DependencyAudit.csv"
)

# Direct 006.0 and 006.1 sources recorded by 007.

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

# Outputs produced and validated by 007.

summary_007_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries.rds"
)

individual_007_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries_individual.rds"
)

configuration_007_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries_configuration.rds"
)

segment_sizes_007_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_Sizes.csv"
)

robust_audit_007_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_RobustAudit.csv"
)

dependency_audit_007_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_DependencyAudit.csv"
)

source_files_consumed_by_007 <- c(
  segmented_data_file,
  integrated_membership_file,
  inference_status_file,
  graph_summary_file,
  corrected_006_dependency_audit_file,
  optimisation_results_file,
  optimisation_memberships_file,
  modularity_null_results_file,
  modularity_tests_csv_file
)

outputs_from_007 <- c(
  summary_007_file,
  individual_007_file,
  configuration_007_file,
  segment_sizes_007_file,
  robust_audit_007_file,
  dependency_audit_007_file
)

required_input_files <- c(
  source_files_consumed_by_007,
  outputs_from_007
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 006.0, 006.1, corrected 006 and corrected 007 before this script."
  )
}

# The three 007 RDS files are written after its dependency audit. If one
# predates that audit, the 007 output set is mixed or stale.

input_information <- file.info(required_input_files)
latest_007_rds_files <- c(
  summary_007_file,
  individual_007_file,
  configuration_007_file
)

if (
  min(input_information[latest_007_rds_files, "mtime"]) <
    input_information[dependency_audit_007_file, "mtime"]
) {
  stop(
    "At least one 007 RDS object predates its dependency audit. ",
    "Rerun corrected 007 before this script."
  )
}

# ------------------------------------------------------------
# 4. Load dependency objects
# ------------------------------------------------------------

control_segmented <- readRDS(segmented_data_file)
integrated_membership <- readRDS(integrated_membership_file)
inference_status <- readRDS(inference_status_file)
graph_summary <- readRDS(graph_summary_file)

descriptive_summary_007 <- readRDS(summary_007_file)
individual_long_007 <- readRDS(individual_007_file)
configuration_007 <- readRDS(configuration_007_file)

segment_sizes_007 <- utils::read.csv(
  segment_sizes_007_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

robust_audit_007 <- utils::read.csv(
  robust_audit_007_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

dependency_audit_007 <- utils::read.csv(
  dependency_audit_007_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

modularity_tests <- utils::read.csv(
  modularity_tests_csv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!is.data.frame(control_segmented)) {
  stop("The corrected 006 segmented-data RDS is not a data frame.")
}

if (!is.data.frame(integrated_membership)) {
  stop("The corrected 006 membership RDS is not a data frame.")
}

if (!is.list(inference_status)) {
  stop("The corrected 006 inference-status RDS is not a list.")
}

if (!is.data.frame(graph_summary) || nrow(graph_summary) != 1L) {
  stop("The corrected 006 graph-summary RDS must be a one-row data frame.")
}

for (object_name_i in c(
  "descriptive_summary_007",
  "individual_long_007",
  "configuration_007",
  "segment_sizes_007",
  "robust_audit_007",
  "dependency_audit_007",
  "modularity_tests"
)) {
  if (!is.data.frame(get(object_name_i))) {
    stop(object_name_i, " is not a data frame.")
  }
}

if (nrow(configuration_007) != 1L) {
  stop("The 007 configuration RDS must contain exactly one row.")
}

# ------------------------------------------------------------
# 5. Verify that 007 consumed the current 006.0/006.1/006 files
# ------------------------------------------------------------

required_dependency_audit_columns <- c(
  "Dependency",
  "File",
  "MD5",
  "Modified"
)

missing_dependency_audit_columns <- setdiff(
  required_dependency_audit_columns,
  names(dependency_audit_007)
)

if (length(missing_dependency_audit_columns) > 0L) {
  stop(
    "The 007 dependency audit lacks:\n",
    paste(missing_dependency_audit_columns, collapse = ", ")
  )
}

audited_basenames <- basename(as.character(dependency_audit_007$File))

for (source_file_i in source_files_consumed_by_007) {
  source_basename_i <- basename(source_file_i)
  matching_rows_i <- which(audited_basenames == source_basename_i)

  if (length(matching_rows_i) != 1L) {
    stop(
      "The 007 dependency audit does not contain exactly one row for ",
      source_basename_i,
      "."
    )
  }

  saved_md5_i <- tolower(trimws(as.character(
    dependency_audit_007$MD5[matching_rows_i]
  )))
  current_md5_i <- tolower(as.character(unname(
    tools::md5sum(source_file_i)
  )))

  if (
    length(saved_md5_i) != 1L ||
      !grepl("^[[:xdigit:]]{32}$", saved_md5_i) ||
      !identical(saved_md5_i, current_md5_i)
  ) {
    stop(
      source_basename_i,
      " differs from the version consumed by 007. ",
      "Rerun corrected 007 before this script."
    )
  }
}

# ------------------------------------------------------------
# 6. Validate the inferential conclusion from 006.1
# ------------------------------------------------------------

expected_k <- 45L
expected_number_of_segments <- 2L
expected_number_of_fish <- 89L
expected_segment_sizes <- c(40L, 49L)
expected_number_of_authors <- 4L
primary_alpha <- 0.05

required_status_elements <- c(
  "Selected_k",
  "Number_of_algorithmic_segments",
  "Segment_sizes",
  "Observed_weighted_modularity",
  "Primary_test",
  "Primary_empirical_p",
  "Alpha",
  "Evidence_of_discrete_communities",
  "Interpretation",
  "Permitted_use",
  "Prohibited_use"
)

missing_status_elements <- setdiff(
  required_status_elements,
  names(inference_status)
)

if (length(missing_status_elements) > 0L) {
  stop(
    "The corrected 006 inference-status RDS lacks:\n",
    paste(missing_status_elements, collapse = ", ")
  )
}

selected_k <- as.integer(inference_status$Selected_k)
selected_number_of_segments <- as.integer(
  inference_status$Number_of_algorithmic_segments
)
saved_segment_sizes <- sort(as.integer(unlist(
  inference_status$Segment_sizes,
  use.names = FALSE
)))
observed_weighted_modularity <- as.numeric(
  inference_status$Observed_weighted_modularity
)
primary_empirical_p <- as.numeric(inference_status$Primary_empirical_p)
saved_alpha <- as.numeric(inference_status$Alpha)
evidence_of_discrete_communities <- as.logical(
  inference_status$Evidence_of_discrete_communities
)

if (
  length(selected_k) != 1L ||
    is.na(selected_k) ||
    selected_k != expected_k
) {
  stop("The corrected 006 inference status does not specify k = 45.")
}

if (
  length(selected_number_of_segments) != 1L ||
    is.na(selected_number_of_segments) ||
    selected_number_of_segments != expected_number_of_segments
) {
  stop("The corrected 006 inference status does not specify two segments.")
}

if (!identical(saved_segment_sizes, expected_segment_sizes)) {
  stop("The corrected 006 inference status does not contain sizes 40/49.")
}

if (
  length(observed_weighted_modularity) != 1L ||
    !is.finite(observed_weighted_modularity)
) {
  stop("The corrected 006 inference status contains invalid modularity.")
}

if (
  length(primary_empirical_p) != 1L ||
    !is.finite(primary_empirical_p) ||
    primary_empirical_p < 0 ||
    primary_empirical_p > 1
) {
  stop("The corrected 006 inference status contains an invalid primary p value.")
}

if (
  length(saved_alpha) != 1L ||
    !is.finite(saved_alpha) ||
    !isTRUE(all.equal(saved_alpha, primary_alpha, tolerance = 1e-12))
) {
  stop("The corrected 006 inference status does not use alpha = 0.05.")
}

if (
  length(evidence_of_discrete_communities) != 1L ||
    is.na(evidence_of_discrete_communities)
) {
  stop("The discrete-community evidence flag is invalid.")
}

if (
  evidence_of_discrete_communities ||
    primary_empirical_p < primary_alpha
) {
  stop(
    "The 006.1 conclusion now differs from the continuous-space ",
    "interpretation encoded in this script. Review the workflow before ",
    "producing this supplementary table."
  )
}

required_test_columns <- c(
  "Test",
  "Status",
  "Observed_statistic",
  "Empirical_p"
)

missing_test_columns <- setdiff(
  required_test_columns,
  names(modularity_tests)
)

if (length(missing_test_columns) > 0L) {
  stop(
    "The 006.1 modularity-test CSV lacks:\n",
    paste(missing_test_columns, collapse = ", ")
  )
}

primary_test <- modularity_tests[
  modularity_tests$Status == "Primary inference",
  ,
  drop = FALSE
]

if (nrow(primary_test) != 1L) {
  stop("Exactly one primary inference must be present in the 006.1 CSV.")
}

if (!isTRUE(all.equal(
  as.numeric(primary_test$Empirical_p),
  primary_empirical_p,
  tolerance = 1e-12
))) {
  stop("The primary empirical p value differs between 006 and 006.1.")
}

if (!isTRUE(all.equal(
  as.numeric(primary_test$Observed_statistic),
  observed_weighted_modularity,
  tolerance = 1e-10
))) {
  stop("The observed modularity differs between 006 and 006.1.")
}

if (!identical(
  as.character(primary_test$Test),
  as.character(inference_status$Primary_test)
)) {
  stop("The primary test identity differs between 006 and 006.1.")
}

required_graph_columns <- c(
  "Number_of_fish",
  "Traceability_key",
  "Selected_k",
  "Number_of_algorithmic_segments",
  "Weighted_modularity",
  "Primary_empirical_p",
  "Evidence_of_discrete_communities"
)

missing_graph_columns <- setdiff(
  required_graph_columns,
  names(graph_summary)
)

if (length(missing_graph_columns) > 0L) {
  stop(
    "The corrected 006 graph summary lacks:\n",
    paste(missing_graph_columns, collapse = ", ")
  )
}

if (
  as.integer(graph_summary$Number_of_fish[[1]]) != expected_number_of_fish ||
    as.character(graph_summary$Traceability_key[[1]]) != "Author + ID" ||
    as.integer(graph_summary$Selected_k[[1]]) != selected_k ||
    as.integer(graph_summary$Number_of_algorithmic_segments[[1]]) !=
      selected_number_of_segments
) {
  stop("The corrected 006 graph summary has an unexpected configuration.")
}

if (!isTRUE(all.equal(
  as.numeric(graph_summary$Weighted_modularity[[1]]),
  observed_weighted_modularity,
  tolerance = 1e-10
))) {
  stop("Weighted modularity differs between corrected 006 outputs.")
}

if (!isTRUE(all.equal(
  as.numeric(graph_summary$Primary_empirical_p[[1]]),
  primary_empirical_p,
  tolerance = 1e-12
))) {
  stop("The primary empirical p value differs between corrected 006 outputs.")
}

graph_evidence_flag <- as.logical(
  graph_summary$Evidence_of_discrete_communities[[1]]
)

if (
  length(graph_evidence_flag) != 1L ||
    is.na(graph_evidence_flag) ||
    graph_evidence_flag
) {
  stop("The corrected 006 graph summary does not encode the null conclusion.")
}

# Confirm that the 007 configuration records the same conclusion.

required_configuration_007_columns <- c(
  "Analysis_status",
  "Number_of_control_fish",
  "Traceability_key",
  "Selected_k",
  "Number_of_algorithmic_segments",
  "Segment_sizes",
  "Observed_weighted_modularity",
  "Primary_selection_adjusted_empirical_p",
  "Alpha",
  "Evidence_of_discrete_communities",
  "Variables_used_to_construct_partition",
  "Same_variables_described_in_figure",
  "Inferential_comparisons_between_segments",
  "Error_bars_or_confidence_intervals"
)

missing_configuration_007_columns <- setdiff(
  required_configuration_007_columns,
  names(configuration_007)
)

if (length(missing_configuration_007_columns) > 0L) {
  stop(
    "The 007 configuration RDS lacks:\n",
    paste(missing_configuration_007_columns, collapse = ", ")
  )
}

if (
  as.character(configuration_007$Analysis_status[[1]]) !=
    "Supplementary descriptive characterisation only" ||
  as.integer(configuration_007$Number_of_control_fish[[1]]) !=
    expected_number_of_fish ||
    as.character(configuration_007$Traceability_key[[1]]) != "Author + ID" ||
    as.integer(configuration_007$Selected_k[[1]]) != selected_k ||
    as.integer(configuration_007$Number_of_algorithmic_segments[[1]]) !=
      selected_number_of_segments ||
    as.character(configuration_007$Segment_sizes[[1]]) !=
      paste(expected_segment_sizes, collapse = "/") ||
    isTRUE(as.logical(
      configuration_007$Evidence_of_discrete_communities[[1]]
    )) ||
    !isTRUE(as.logical(
      configuration_007$Same_variables_described_in_figure[[1]]
    )) ||
    isTRUE(as.logical(
      configuration_007$Inferential_comparisons_between_segments[[1]]
    )) ||
    isTRUE(as.logical(
      configuration_007$Error_bars_or_confidence_intervals[[1]]
    ))
) {
  stop("The 007 configuration is incompatible with descriptive-only use.")
}

if (!isTRUE(all.equal(
  as.numeric(configuration_007$Observed_weighted_modularity[[1]]),
  observed_weighted_modularity,
  tolerance = 1e-10
))) {
  stop("Observed modularity differs between 006 and 007.")
}

if (!isTRUE(all.equal(
  as.numeric(
    configuration_007$Primary_selection_adjusted_empirical_p[[1]]
  ),
  primary_empirical_p,
  tolerance = 1e-12
))) {
  stop("The primary empirical p value differs between 006 and 007.")
}

if (!isTRUE(all.equal(
  as.numeric(configuration_007$Alpha[[1]]),
  primary_alpha,
  tolerance = 1e-12
))) {
  stop("The alpha threshold differs between 006 and 007.")
}

# ------------------------------------------------------------
# 7. Validate Author + ID traceability and segment membership
# ------------------------------------------------------------

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

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

variables_recorded_by_007 <- trimws(strsplit(
  as.character(
    configuration_007$Variables_used_to_construct_partition[[1]]
  ),
  split = ";",
  fixed = TRUE
)[[1]])

if (!identical(variables_recorded_by_007, robust_variables)) {
  stop("The variables recorded by 007 differ from the four partition variables.")
}

required_segmented_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Algorithmic_Segment",
  robust_variables
)

missing_segmented_columns <- setdiff(
  required_segmented_columns,
  names(control_segmented)
)

if (length(missing_segmented_columns) > 0L) {
  stop(
    "The corrected 006 segmented dataset lacks:\n",
    paste(missing_segmented_columns, collapse = ", ")
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
  names(integrated_membership)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The corrected 006 membership object lacks:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

make_traceability_key <- function(author, id) {
  paste(trimws(as.character(author)), trimws(as.character(id)), sep = " || ")
}

normalise_segment_label <- function(x, object_name) {
  label <- trimws(as.character(x))
  segment_number <- suppressWarnings(
    as.integer(sub("^Segment[[:space:]]+", "", label))
  )

  valid_labels <- paste(
    "Segment",
    seq_len(expected_number_of_segments)
  )

  if (
    anyNA(segment_number) ||
      any(!label %in% valid_labels)
  ) {
    stop("Invalid algorithmic-segment labels were detected in ", object_name, ".")
  }

  factor(label, levels = valid_labels)
}

control_segmented <- control_segmented %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Algorithmic_Segment = normalise_segment_label(
      Algorithmic_Segment,
      "control_segmented"
    )
  )

integrated_membership <- integrated_membership %>%
  dplyr::transmute(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Algorithmic_Segment = normalise_segment_label(
      Algorithmic_Segment,
      "integrated_membership"
    )
  )

for (data_name_i in c("control_segmented", "integrated_membership")) {
  data_i <- get(data_name_i)

  if (
    anyNA(data_i[c("Author", "ID", "Traceability_ID")]) ||
      any(data_i$Author == "") ||
      any(data_i$ID == "") ||
      any(data_i$Traceability_ID == "")
  ) {
    stop("Missing or empty traceability values were detected in ", data_name_i, ".")
  }

  expected_keys_i <- make_traceability_key(data_i$Author, data_i$ID)

  if (!identical(data_i$Traceability_ID, expected_keys_i)) {
    stop("Stored Author + ID keys are invalid in ", data_name_i, ".")
  }

  if (
    anyDuplicated(data_i[c("Author", "ID")]) > 0L ||
      anyDuplicated(data_i$Traceability_ID) > 0L
  ) {
    stop("Duplicated Author + ID keys were detected in ", data_name_i, ".")
  }
}

if (nrow(control_segmented) != expected_number_of_fish) {
  stop("The corrected 006 segmented dataset does not contain 89 fish.")
}

if (nrow(integrated_membership) != expected_number_of_fish) {
  stop("The corrected 006 membership object does not contain 89 fish.")
}

if (!setequal(
  control_segmented$Traceability_ID,
  integrated_membership$Traceability_ID
)) {
  stop("Author + ID keys differ between the corrected 006 data and membership.")
}

integrated_membership <- integrated_membership[
  match(
    control_segmented$Traceability_ID,
    integrated_membership$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (!identical(
  control_segmented$Traceability_ID,
  integrated_membership$Traceability_ID
)) {
  stop("The corrected 006 membership could not be aligned by Author + ID.")
}

if (!identical(
  as.character(control_segmented$Algorithmic_Segment),
  as.character(integrated_membership$Algorithmic_Segment)
)) {
  stop("Segment assignments differ between the corrected 006 objects.")
}

if (dplyr::n_distinct(control_segmented$Author) != expected_number_of_authors) {
  stop("The corrected 006 dataset does not contain the expected four Authors.")
}

non_numeric_variables <- robust_variables[
  !vapply(control_segmented[robust_variables], is.numeric, logical(1))
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following robust variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

robust_matrix <- as.matrix(control_segmented[robust_variables])
storage.mode(robust_matrix) <- "double"

if (any(!is.finite(robust_matrix))) {
  stop("Missing or non-finite robust behavioural values were detected.")
}

segment_sizes <- control_segmented %>%
  dplyr::count(Algorithmic_Segment, name = "n") %>%
  dplyr::mutate(Percentage = 100 * n / sum(n)) %>%
  dplyr::arrange(Algorithmic_Segment)

if (!identical(sort(segment_sizes$n), expected_segment_sizes)) {
  stop("The imported exploratory segmentation does not contain 40 and 49 fish.")
}

required_segment_size_columns <- c(
  "Algorithmic_Segment",
  "n",
  "Percentage"
)

missing_segment_size_columns <- setdiff(
  required_segment_size_columns,
  names(segment_sizes_007)
)

if (length(missing_segment_size_columns) > 0L) {
  stop(
    "The 007 segment-size CSV lacks:\n",
    paste(missing_segment_size_columns, collapse = ", ")
  )
}

segment_sizes_007 <- segment_sizes_007 %>%
  dplyr::transmute(
    Algorithmic_Segment = normalise_segment_label(
      Algorithmic_Segment,
      "segment_sizes_007"
    ),
    n = as.integer(n),
    Percentage = as.numeric(Percentage)
  ) %>%
  dplyr::arrange(Algorithmic_Segment)

if (
  !identical(as.character(
    segment_sizes$Algorithmic_Segment
  ), as.character(segment_sizes_007$Algorithmic_Segment)) ||
    !identical(as.integer(segment_sizes$n), as.integer(segment_sizes_007$n)) ||
    !isTRUE(all.equal(
      segment_sizes$Percentage,
      segment_sizes_007$Percentage,
      tolerance = 1e-12
    ))
) {
  stop("Segment sizes differ between corrected 006 and 007.")
}

# ------------------------------------------------------------
# 8. Recompute and verify the within-Author robust audit
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

if (
  nrow(robust_audit) !=
    expected_number_of_authors * length(robust_variables)
) {
  stop("The within-Author robust-normalisation audit has an unexpected size.")
}

if (any(!is.finite(as.matrix(
  robust_audit[c("Median", "Scaled_MAD")]
)))) {
  stop("The within-Author robust-normalisation audit contains non-finite values.")
}

if (any(abs(robust_audit$Median) > 1e-6)) {
  stop("At least one within-Author robust median differs from zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > 1e-6)) {
  stop("At least one within-Author scaled MAD differs from one.")
}

required_robust_audit_columns <- c(
  "Author",
  "Variable",
  "n",
  "Median",
  "Scaled_MAD"
)

missing_robust_audit_columns <- setdiff(
  required_robust_audit_columns,
  names(robust_audit_007)
)

if (length(missing_robust_audit_columns) > 0L) {
  stop(
    "The 007 robust-audit CSV lacks:\n",
    paste(missing_robust_audit_columns, collapse = ", ")
  )
}
