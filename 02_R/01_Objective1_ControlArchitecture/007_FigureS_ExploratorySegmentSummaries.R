# ============================================================
# 007_FigureS_ExploratorySegmentSummaries.R
#
# Objective 1
# Supplementary descriptive summaries of the exploratory
# kNN-Louvain segmentation of the LAPCOM control population.
#
# Required upstream scripts, in order:
#   005_Figure3_PCA.R
#   006.0_Optimisation_k_Louvain.R
#   006.1_ModularityNullTest_Louvain.R
#   006_Figure4_Louvain.R (post-null-model corrected version)
#
# The k = 45 partition contains two algorithmic segments of
# 40 and 49 fish. The formal selection-adjusted null-model test
# did not support discrete behavioural communities. Therefore,
# this script:
#   - imports the segmentation; it never reruns Louvain;
#   - uses Author + ID as the traceability key;
#   - verifies robust normalisation separately within Author;
#   - provides descriptive summaries only;
#   - performs no inferential comparisons between segments;
#   - uses no biological, higher/lower, or phenotype labels;
#   - does not create a main-text Figure 5 or classification object.
#
# The displayed variables are the same variables used to build
# the partition. Their segment-wise summaries are consequently a
# circular description of the algorithmic split, not independent
# evidence of discrete behavioural categories.
# ============================================================

# ------------------------------------------------------------
# 1. Project and packages
# ------------------------------------------------------------

base_dir <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

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
# 2. Input and output directories
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
# 3. Required inputs from 006.0, 006.1 and corrected 006
# ------------------------------------------------------------

# Outputs integrated and validated by the corrected 006 script.

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

# Direct 006.0 dependencies used to verify the imported solution.

optimisation_results_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_results_robust.rds"
)

optimisation_memberships_file <- file.path(
  objects_dir,
  "objective1_louvain_optimisation_memberships_robust.rds"
)

# Direct 006.1 dependency used to verify the inferential status.

modularity_tests_csv_file <- file.path(
  tables_dir,
  "Objective1_ModularityNullTests.csv"
)

modularity_null_results_file <- file.path(
  objects_dir,
  "objective1_modularity_null_test_results.rds"
)

integrated_object_files <- c(
  segmented_data_file,
  integrated_membership_file,
  inference_status_file,
  graph_summary_file
)

integrated_input_files <- c(
  integrated_object_files,
  corrected_006_dependency_audit_file
)

source_input_files <- c(
  optimisation_results_file,
  optimisation_memberships_file,
  modularity_null_results_file,
  modularity_tests_csv_file
)

required_input_files <- c(integrated_input_files, source_input_files)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 006.0, 006.1 and the corrected 006 script before this script."
  )
}

# In the corrected 006 script, its dependency audit is written before
# its integrated RDS outputs. An integrated object that predates the
# audit therefore indicates a mixed or stale set of corrected 006 files.

input_information <- file.info(required_input_files)

if (
  min(input_information[integrated_object_files, "mtime"]) <
    input_information[corrected_006_dependency_audit_file, "mtime"]
) {
  stop(
    "At least one corrected 006 object predates its dependency audit. ",
    "Rerun the corrected 006 script before this script."
  )
}

# ------------------------------------------------------------
# 4. Load dependency objects
# ------------------------------------------------------------

control_segmented <- readRDS(segmented_data_file)
integrated_membership <- readRDS(integrated_membership_file)
inference_status <- readRDS(inference_status_file)
graph_summary <- readRDS(graph_summary_file)
results_by_k <- readRDS(optimisation_results_file)
memberships_by_k <- readRDS(optimisation_memberships_file)

modularity_tests <- utils::read.csv(
  modularity_tests_csv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

corrected_006_dependency_audit <- utils::read.csv(
  corrected_006_dependency_audit_file,
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

if (!is.data.frame(results_by_k)) {
  stop("The 006.0 optimisation-results RDS is not a data frame.")
}

if (!is.list(memberships_by_k)) {
  stop("The 006.0 optimisation-memberships RDS is not a list.")
}

if (!is.data.frame(modularity_tests)) {
  stop("The 006.1 modularity-test CSV is not a data frame.")
}

if (!is.data.frame(corrected_006_dependency_audit)) {
  stop("The corrected 006 dependency audit is not a data frame.")
}

required_dependency_audit_columns <- c("Dependency", "File", "MD5", "Modified")
missing_dependency_audit_columns <- setdiff(
  required_dependency_audit_columns,
  names(corrected_006_dependency_audit)
)

if (length(missing_dependency_audit_columns) > 0L) {
  stop(
    "The corrected 006 dependency audit lacks:\n",
    paste(missing_dependency_audit_columns, collapse = ", ")
  )
}

# Confirm by MD5, rather than modification time alone, that the direct
# 006.0 and 006.1 files are exactly those consumed by corrected 006.

audited_basenames <- basename(as.character(
  corrected_006_dependency_audit$File
))

for (source_file_i in source_input_files) {
  source_basename_i <- basename(source_file_i)
  matching_audit_rows_i <- which(audited_basenames == source_basename_i)

  if (length(matching_audit_rows_i) != 1L) {
    stop(
      "The corrected 006 dependency audit does not contain exactly one row for ",
      source_basename_i,
      "."
    )
  }

  saved_md5_i <- tolower(trimws(as.character(
    corrected_006_dependency_audit$MD5[matching_audit_rows_i]
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
      " differs from the version consumed by corrected 006. ",
      "Rerun corrected 006 before this script."
    )
  }
}

# ------------------------------------------------------------
# 5. Validate the 006.1 inferential conclusion
# ------------------------------------------------------------

expected_k <- 45L
expected_number_of_segments <- 2L
expected_number_of_fish <- 89L
expected_segment_sizes <- c(40L, 49L)
expected_number_of_authors <- 4L
primary_alpha <- 0.05

required_status_elements <- c(
  "Analysis",
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
  length(selected_k) != 1L || is.na(selected_k) ||
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
  stop("The corrected 006 inference status does not contain segment sizes 40/49.")
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
    "The primary 006.1 conclusion now differs from the descriptive ",
    "continuous-space interpretation encoded in this script. Review the ",
    "workflow before producing segment summaries."
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

# ------------------------------------------------------------
# 6. Validate corrected 006 data and Author + ID traceability
# ------------------------------------------------------------

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

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

required_integrated_membership_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Algorithmic_Segment"
)

missing_integrated_membership_columns <- setdiff(
  required_integrated_membership_columns,
  names(integrated_membership)
)

if (length(missing_integrated_membership_columns) > 0L) {
  stop(
    "The corrected 006 membership object lacks:\n",
    paste(missing_integrated_membership_columns, collapse = ", ")
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

  if (
    anyNA(segment_number) ||
      any(!label %in% paste("Segment", seq_len(expected_number_of_segments)))
  ) {
    stop("Invalid algorithmic-segment labels were detected in ", object_name, ".")
  }

  factor(
    paste("Segment", segment_number),
    levels = paste("Segment", seq_len(expected_number_of_segments))
  )
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

# ------------------------------------------------------------
# 7. Verify the imported membership directly against 006.0
# ------------------------------------------------------------

required_results_columns <- c("k", "Clusters")
missing_results_columns <- setdiff(
  required_results_columns,
  names(results_by_k)
)

if (length(missing_results_columns) > 0L) {
  stop(
    "The 006.0 optimisation-results object lacks:\n",
    paste(missing_results_columns, collapse = ", ")
  )
}

selected_result_006_0 <- results_by_k[
  as.integer(results_by_k$k) == selected_k,
  ,
  drop = FALSE
]

if (nrow(selected_result_006_0) != 1L) {
  stop("The 006.0 results do not contain exactly one row for k = 45.")
}

if (
  as.integer(selected_result_006_0$Clusters[[1]]) !=
    selected_number_of_segments
) {
  stop("006.0 and corrected 006 disagree on the number of segments at k = 45.")
}

selected_membership_name <- paste0("k_", selected_k)

if (!selected_membership_name %in% names(memberships_by_k)) {
  stop("The 006.0 membership list lacks ", selected_membership_name, ".")
}

selected_membership_006_0 <- memberships_by_k[[selected_membership_name]]

if (!is.data.frame(selected_membership_006_0)) {
  stop("The 006.0 k = 45 membership is not a data frame.")
}

required_006_0_membership_columns <- c("k", "Author", "ID", "Cluster")
missing_006_0_membership_columns <- setdiff(
  required_006_0_membership_columns,
  names(selected_membership_006_0)
)

if (length(missing_006_0_membership_columns) > 0L) {
  stop(
    "The 006.0 k = 45 membership lacks:\n",
    paste(missing_006_0_membership_columns, collapse = ", ")
  )
}

selected_membership_006_0 <- selected_membership_006_0 %>%
  dplyr::transmute(
    k = as.integer(k),
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = make_traceability_key(Author, ID),
    Algorithmic_Segment = factor(
      paste("Segment", as.integer(Cluster)),
      levels = paste("Segment", seq_len(expected_number_of_segments))
    )
  )

if (
  anyNA(selected_membership_006_0) ||
    any(selected_membership_006_0$k != selected_k) ||
    anyDuplicated(selected_membership_006_0$Traceability_ID) > 0L
) {
  stop("The 006.0 k = 45 membership failed its integrity checks.")
}

if (!setequal(
  selected_membership_006_0$Traceability_ID,
  control_segmented$Traceability_ID
)) {
  stop("The 006.0 and corrected 006 Author + ID keys do not match.")
}

selected_membership_006_0 <- selected_membership_006_0[
  match(
    control_segmented$Traceability_ID,
    selected_membership_006_0$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (!identical(
  as.character(selected_membership_006_0$Algorithmic_Segment),
  as.character(control_segmented$Algorithmic_Segment)
)) {
  stop("The imported corrected 006 segments differ from the 006.0 k = 45 membership.")
}

# ------------------------------------------------------------
# 8. Verify graph and modularity status from corrected 006
# ------------------------------------------------------------

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
  stop("The corrected 006 graph summary does not encode the non-significant conclusion.")
}

# ------------------------------------------------------------
# 9. Audit robust normalisation separately within Author
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
))

if (
  nrow(robust_audit) !=
    expected_number_of_authors * length(robust_variables)
) {
  stop("The within-Author robust-normalisation audit has an unexpected size.")
}

if (any(!is.finite(as.matrix(robust_audit[c("Median", "Scaled_MAD")]))) ) {
  stop("The within-Author robust-normalisation audit contains non-finite values.")
}

if (any(abs(robust_audit$Median) > 1e-6)) {
  stop("At least one within-Author robust median differs from zero.")
}

if (any(abs(robust_audit$Scaled_MAD - 1) > 1e-6)) {
  stop("At least one within-Author scaled MAD differs from one.")
}

# ------------------------------------------------------------
# 10. Prepare descriptive individual and summary tables
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

individual_long <- control_segmented %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Algorithmic_Segment,
    dplyr::all_of(robust_variables)
  ) %>%
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
  dplyr::arrange(
    Algorithmic_Segment,
    Author,
    suppressWarnings(as.numeric(ID)),
    ID,
    Variable
  )

if (
  anyNA(individual_long$Variable_Key) ||
    anyNA(individual_long$Variable)
) {
  stop("At least one behavioural variable could not be labelled.")
}

if (
  nrow(individual_long) !=
    expected_number_of_fish * length(robust_variables)
) {
  stop("The descriptive individual table has an incorrect number of rows.")
}

descriptive_summary <- individual_long %>%
  dplyr::group_by(
    Algorithmic_Segment,
    Robust_Variable,
    Variable_Key,
    Variable
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean_Robust_Z_Score = mean(Robust_Z_Score),
    SD_Robust_Z_Score = stats::sd(Robust_Z_Score),
    Median_Robust_Z_Score = stats::median(Robust_Z_Score),
    Q1_Robust_Z_Score = as.numeric(stats::quantile(
      Robust_Z_Score,
      probs = 0.25,
      names = FALSE
    )),
    Q3_Robust_Z_Score = as.numeric(stats::quantile(
      Robust_Z_Score,
      probs = 0.75,
      names = FALSE
    )),
    IQR_Robust_Z_Score = stats::IQR(Robust_Z_Score),
    Minimum_Robust_Z_Score = min(Robust_Z_Score),
    Maximum_Robust_Z_Score = max(Robust_Z_Score),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Algorithmic_Segment, Variable)

if (
  nrow(descriptive_summary) !=
    expected_number_of_segments * length(robust_variables)
) {
  stop("The descriptive segment summary has an incorrect number of rows.")
}

descriptive_columns <- c(
  "Mean_Robust_Z_Score",
  "SD_Robust_Z_Score",
  "Median_Robust_Z_Score",
  "Q1_Robust_Z_Score",
  "Q3_Robust_Z_Score",
  "IQR_Robust_Z_Score",
  "Minimum_Robust_Z_Score",
  "Maximum_Robust_Z_Score"
)

if (any(!is.finite(as.matrix(descriptive_summary[descriptive_columns])))) {
  stop("At least one descriptive segment statistic is not finite.")
}

mean_summary_wide <- descriptive_summary %>%
  dplyr::select(
    Algorithmic_Segment,
    n,
    Variable_Key,
    Mean_Robust_Z_Score
  ) %>%
  tidyr::pivot_wider(
    names_from = Variable_Key,
    values_from = Mean_Robust_Z_Score
  ) %>%
  dplyr::arrange(Algorithmic_Segment)

# ------------------------------------------------------------
# 11. Generate the descriptive supplementary figure
# ------------------------------------------------------------

segment_levels <- levels(control_segmented$Algorithmic_Segment)

segment_palette <- stats::setNames(
  c("#63B8A6", "#F07A5A"),
  segment_levels
)

legend_labels <- stats::setNames(
  paste0(
    segment_sizes$Algorithmic_Segment,
    " (n = ",
    segment_sizes$n,
    ")"
  ),
  as.character(segment_sizes$Algorithmic_Segment)
)

figure_caption <- paste(
  base::strwrap(
    paste0(
      "Descriptive summary of the algorithmic partition only; individual ",
      "traces show within-segment variation. The same variables constructed ",
      "the partition, and no inferential comparison was performed ",
      "(selection-adjusted p = ",
      formatC(primary_empirical_p, format = "f", digits = 3),
      ")."
    ),
    width = 105
  ),
  collapse = "\n"
)

figure_segment_summaries <- ggplot2::ggplot(
  individual_long,
  ggplot2::aes(
    x = Variable,
    y = Robust_Z_Score,
    group = Traceability_ID,
    colour = Algorithmic_Segment
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    linetype = "dashed",
    colour = "grey50"
  ) +
  ggplot2::geom_line(
    linewidth = 0.35,
    alpha = 0.13,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    size = 0.8,
    alpha = 0.16,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    data = descriptive_summary,
    ggplot2::aes(
      x = Variable,
      y = Mean_Robust_Z_Score,
      group = Algorithmic_Segment,
      colour = Algorithmic_Segment
    ),
    inherit.aes = FALSE,
    linewidth = 1.25
  ) +
  ggplot2::geom_point(
    data = descriptive_summary,
    ggplot2::aes(
      x = Variable,
      y = Mean_Robust_Z_Score,
      colour = Algorithmic_Segment
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ggplot2::scale_colour_manual(
    values = segment_palette,
    breaks = segment_levels,
    labels = legend_labels,
    drop = FALSE
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Within-Author robust z-score",
    colour = "Exploratory segment",
    caption = figure_caption
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
    legend.position = "right",
    plot.caption = ggplot2::element_text(
      colour = "grey25",
      size = 9,
      hjust = 0,
      margin = ggplot2::margin(t = 8)
    )
  )

print(figure_segment_summaries)

# ------------------------------------------------------------
# 12. Define explicitly descriptive outputs
# ------------------------------------------------------------

pdf_output <- file.path(
  figures_dir,
  "FigureS_Louvain_ExploratorySegmentSummaries.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "FigureS_Louvain_ExploratorySegmentSummaries.tiff"
)

summary_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries.csv"
)

individual_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_IndividualLong.csv"
)

mean_wide_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_MeanWide.csv"
)

segment_size_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_Sizes.csv"
)

robust_audit_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_RobustAudit.csv"
)

configuration_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_Configuration.csv"
)

dependency_audit_output <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentSummaries_DependencyAudit.csv"
)

figure_configuration <- tibble::tibble(
  Analysis_status = "Supplementary descriptive characterisation only",
  Number_of_control_fish = expected_number_of_fish,
  Traceability_key = "Author + ID",
  Normalisation = "Within Author: (x - median) / (1.4826 x raw MAD)",
  Selected_k = selected_k,
  Number_of_algorithmic_segments = selected_number_of_segments,
  Segment_sizes = paste(expected_segment_sizes, collapse = "/"),
  Observed_weighted_modularity = observed_weighted_modularity,
  Primary_selection_adjusted_empirical_p = primary_empirical_p,
  Alpha = primary_alpha,
  Evidence_of_discrete_communities = FALSE,
  Variables_used_to_construct_partition = paste(
    robust_variables,
    collapse = "; "
  ),
  Same_variables_described_in_figure = TRUE,
  Inferential_comparisons_between_segments = FALSE,
  Error_bars_or_confidence_intervals = FALSE,
  Summary_statistic = "Arithmetic mean of within-Author robust z-scores",
  Individual_variation_displayed = TRUE,
  Interpretation = paste0(
    "Circular descriptive summary of an algorithmic partition of a ",
    "continuous locomotor-exploratory space; not independent evidence ",
    "of discrete behavioural categories."
  ),
  Permitted_use = "Supplementary descriptive visualisation only",
  Prohibited_use = paste0(
    "Biological labels; inferential segment comparisons; normative ",
    "classification; assignment of stressed fish."
  )
)

dependency_audit <- tibble::tibble(
  Dependency = c(
    "Corrected 006 segmented data",
    "Corrected 006 integrated membership",
    "Corrected 006 inference status",
    "Corrected 006 graph summary",
    "Corrected 006 dependency audit",
    "006.0 optimisation results",
    "006.0 optimisation memberships",
    "006.1 complete null-model results",
    "006.1 modularity-test CSV"
  ),
  File = required_input_files,
  MD5 = unname(tools::md5sum(required_input_files)),
  Modified = as.character(file.info(required_input_files)$mtime)
)

# ------------------------------------------------------------
# 13. Export figure, tables and descriptive R objects
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = pdf_output,
  plot = figure_segment_summaries,
  width = 9,
  height = 6,
  units = "in"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = figure_segment_summaries,
  width = 9,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

utils::write.csv(descriptive_summary, summary_output, row.names = FALSE)
utils::write.csv(individual_long, individual_output, row.names = FALSE)
utils::write.csv(mean_summary_wide, mean_wide_output, row.names = FALSE)
utils::write.csv(segment_sizes, segment_size_output, row.names = FALSE)
utils::write.csv(robust_audit, robust_audit_output, row.names = FALSE)
utils::write.csv(figure_configuration, configuration_output, row.names = FALSE)
utils::write.csv(dependency_audit, dependency_audit_output, row.names = FALSE)

summary_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries.rds"
)

individual_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries_individual.rds"
)

figure_rds <- file.path(
  objects_dir,
  "objective1_figureS_louvain_exploratory_segment_summaries.rds"
)

configuration_rds <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries_configuration.rds"
)

session_info_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_segment_summaries_sessionInfo.txt"
)

saveRDS(descriptive_summary, summary_rds)
saveRDS(individual_long, individual_rds)
saveRDS(figure_segment_summaries, figure_rds)
saveRDS(figure_configuration, configuration_rds)
capture.output(utils::sessionInfo(), file = session_info_file)

# Deliberately not written:
# - a validated biological-category object;
# - a new Louvain model or membership;
# - inferential comparisons, p values, confidence intervals, or effect sizes
#   between algorithmic segments;
# - higher-/lower-exploration or phenotype labels;
# - any object for assigning stressed fish to segments.

# Historical outputs from the superseded script are neither overwritten
# nor deleted automatically. If present, they must not be used as current
# results.

historical_output_files <- c(
  file.path(figures_dir, "Figure5_ClusterProfiles.pdf"),
  file.path(figures_dir, "Figure5_ClusterProfiles.tiff"),
  file.path(tables_dir, "Figure5_ClusterProfiles_Wide.csv"),
  file.path(tables_dir, "Figure5_ClusterProfiles_Long.csv"),
  file.path(tables_dir, "Figure5_ClusterProfiles_Summary.csv"),
  file.path(tables_dir, "Figure5_ClusterProfiles_IndividualLong.csv"),
  file.path(tables_dir, "Figure5_ClusterSizes.csv"),
  file.path(tables_dir, "Figure5_ClusterProfiles_Configuration.csv")
)

existing_historical_outputs <- historical_output_files[
  file.exists(historical_output_files)
]

expected_output_files <- c(
  pdf_output,
  tiff_output,
  summary_output,
  individual_output,
  mean_wide_output,
  segment_size_output,
  robust_audit_output,
  configuration_output,
  dependency_audit_output,
  summary_rds,
  individual_rds,
  figure_rds,
  configuration_rds,
  session_info_file
)

output_information <- file.info(expected_output_files)

if (
  any(!file.exists(expected_output_files)) ||
    any(is.na(output_information$size)) ||
    any(output_information$size <= 0)
) {
  stop("At least one expected descriptive output is absent or empty.")
}

# ------------------------------------------------------------
# 14. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SCRIPT 007 - EXPLORATORY SEGMENT SUMMARIES\n")
cat("============================================================\n")
cat("\nStatus: supplementary descriptive characterisation only.\n")
cat("Control fish: ", nrow(control_segmented), "\n", sep = "")
cat("Traceability: Author + ID\n")
cat("Selected k imported from 006.0/006: ", selected_k, "\n", sep = "")
cat(
  "Algorithmic segments: ",
  selected_number_of_segments,
  " (",
  paste(sort(segment_sizes$n), collapse = "/"),
  ").\n",
  sep = ""
)
cat(
  "Observed weighted modularity: ",
  sprintf("%.6f", observed_weighted_modularity),
  "\n",
  sep = ""
)
cat(
  "Primary selection-adjusted empirical p: ",
  sprintf("%.6f", primary_empirical_p),
  "\n",
  sep = ""
)
cat("Evidence of discrete communities: FALSE\n")
cat("Inferential comparisons between segments: NONE\n")
cat(
  "Interpretation: circular descriptive summary of the exploratory ",
  "partition of a continuous behavioural space.\n"
)

cat("\nWithin-Author robust-normalisation audit:\n")
print(robust_audit, n = Inf, width = Inf)

cat("\nAlgorithmic segment sizes:\n")
print(segment_sizes, n = Inf, width = Inf)

cat("\nDescriptive variable summaries:\n")
print(descriptive_summary, n = Inf, width = Inf)

cat(
  "\nSupplementary figure saved as:\n",
  normalizePath(pdf_output, winslash = "/", mustWork = TRUE),
  "\n",
  normalizePath(tiff_output, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

if (length(existing_historical_outputs) > 0L) {
  cat(
    "\nWARNING: superseded Figure5_ClusterProfiles outputs remain on disk.\n",
    "They were not modified or deleted and must not be treated as current results:\n",
    paste(existing_historical_outputs, collapse = "\n"),
    "\n",
    sep = ""
  )
}

cat("\nAll integrity checks passed.\n")
cat("============================================================\n")
