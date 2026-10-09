# ============================================================
# 009_Table3_ContinuousBehaviouralReference.R
#
# Objective 1
# Table 3: continuous behavioural reference for the LAPCOM
# control population.
#
# Required upstream scripts:
#   005_Figure3_PCA.R
#   006.0_Optimisation_k_Louvain.R
#   006.1_ModularityNullTest_Louvain.R
#   006_Figure4_Louvain.R (post-null-model corrected version)
#
# The selection-adjusted modularity test did not support discrete
# behavioural communities. Consequently, this script replaces the
# historical table of "normative archetype centroids" with a
# continuous PCA reference. It:
#   - uses Author + ID as the traceability key;
#   - verifies robust normalisation separately within Author;
#   - verifies the stored PCA centre, loadings, variance and scores;
#   - reports component loadings and control-score distributions;
#   - preserves a projection-ready continuous reference;
#   - creates no clusters, archetypes, thresholds or classifications.
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
# 3. Required continuous and inferential inputs
# ------------------------------------------------------------

robust_data_file <- file.path(
  objects_dir,
  "objective1_control_robust_z.rds"
)

pca_model_file <- file.path(
  objects_dir,
  "objective1_pca_model_robust.rds"
)

pca_scores_file <- file.path(
  objects_dir,
  "objective1_pca_scores_robust.rds"
)

pca_loadings_file <- file.path(
  objects_dir,
  "objective1_pca_loadings_robust.rds"
)

pca_variance_file <- file.path(
  objects_dir,
  "objective1_pca_variance_robust.rds"
)

inference_status_file <- file.path(
  objects_dir,
  "objective1_louvain_inference_status.rds"
)

modularity_null_results_file <- file.path(
  objects_dir,
  "objective1_modularity_null_test_results.rds"
)

modularity_tests_csv_file <- file.path(
  tables_dir,
  "Objective1_ModularityNullTests.csv"
)

corrected_006_dependency_audit_file <- file.path(
  tables_dir,
  "Objective1_Louvain_ExploratorySegmentation_DependencyAudit.csv"
)

required_input_files <- c(
  robust_data_file,
  pca_model_file,
  pca_scores_file,
  pca_loadings_file,
  pca_variance_file,
  inference_status_file,
  modularity_null_results_file,
  modularity_tests_csv_file,
  corrected_006_dependency_audit_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 005, 006.0, 006.1 and the corrected 006 script before 009."
  )
}

input_information <- file.info(required_input_files)

if (
  any(is.na(input_information$size)) ||
    any(input_information$size <= 0)
) {
  stop("At least one required input file is empty or inaccessible.")
}

# In corrected 006, the dependency audit is written before its
# integrated inference-status RDS. An older status object indicates
# a stale or mixed set of outputs.

if (
  input_information[inference_status_file, "mtime"] <
    input_information[corrected_006_dependency_audit_file, "mtime"]
) {
  stop(
    "The corrected 006 inference-status object predates its dependency ",
    "audit. Rerun the corrected 006 script before 009."
  )
}

# ------------------------------------------------------------
# 4. Load input objects
# ------------------------------------------------------------

control_robust <- readRDS(robust_data_file)
pca_model <- readRDS(pca_model_file)
pca_scores <- readRDS(pca_scores_file)
pca_loadings <- readRDS(pca_loadings_file)
pca_variance <- readRDS(pca_variance_file)
inference_status <- readRDS(inference_status_file)
null_model_results <- readRDS(modularity_null_results_file)

modularity_tests_csv <- utils::read.csv(
  modularity_tests_csv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

corrected_006_dependency_audit <- utils::read.csv(
  corrected_006_dependency_audit_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!is.data.frame(control_robust)) {
  stop("The robust-control RDS is not a data frame.")
}

if (!inherits(pca_model, "prcomp")) {
  stop("The PCA-model RDS does not contain a prcomp object.")
}

for (object_name_i in c(
  "pca_scores",
  "pca_loadings",
  "pca_variance",
  "modularity_tests_csv",
  "corrected_006_dependency_audit"
)) {
  if (!is.data.frame(get(object_name_i))) {
    stop(object_name_i, " is not a data frame.")
  }
}

if (!is.list(inference_status)) {
  stop("The corrected 006 inference-status RDS is not a list.")
}

if (!is.list(null_model_results)) {
  stop("The 006.1 null-model RDS is not a list.")
}

# ------------------------------------------------------------
# 5. Fixed analysis specifications
# ------------------------------------------------------------

expected_number_of_fish <- 89L
expected_number_of_authors <- 4L
expected_selected_k <- 45L
expected_number_of_segments <- 2L
expected_segment_sizes <- c(40L, 49L)
alpha <- 0.05

raw_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- paste0(raw_variables, "_rz")
components <- paste0("PC", seq_along(raw_variables))

variable_labels <- c(
  distance = "Distance",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

normalisation_tolerance <- 1e-6
reconstruction_tolerance <- 1e-10
comparison_tolerance <- 1e-12

# ------------------------------------------------------------
# 6. Verify the null-model conclusion and dependency identity
# ------------------------------------------------------------

required_inference_fields <- c(
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
  "Prohibited_use"
)

missing_inference_fields <- setdiff(
  required_inference_fields,
  names(inference_status)
)

if (length(missing_inference_fields) > 0L) {
  stop(
    "The corrected 006 inference-status object lacks:\n",
    paste(missing_inference_fields, collapse = ", ")
  )
}

primary_empirical_p <- as.numeric(inference_status$Primary_empirical_p)
observed_weighted_modularity <- as.numeric(
  inference_status$Observed_weighted_modularity
)

if (
  length(primary_empirical_p) != 1L ||
    !is.finite(primary_empirical_p) ||
    primary_empirical_p < 0 ||
    primary_empirical_p > 1
) {
  stop("The primary empirical p value in corrected 006 is invalid.")
}

if (
  length(observed_weighted_modularity) != 1L ||
    !is.finite(observed_weighted_modularity)
) {
  stop("The observed weighted modularity in corrected 006 is invalid.")
}

if (
  as.integer(inference_status$Selected_k) != expected_selected_k ||
    as.integer(inference_status$Number_of_algorithmic_segments) !=
      expected_number_of_segments ||
    !identical(
      sort(as.integer(inference_status$Segment_sizes)),
      expected_segment_sizes
    )
) {
  stop(
    "The corrected 006 inference status is not the expected k = 45, ",
    "40/49 exploratory segmentation."
  )
}

if (
  isTRUE(as.logical(inference_status$Evidence_of_discrete_communities)) ||
    primary_empirical_p < alpha
) {
  stop(
    "The current inferential result no longer supports the continuous-only ",
    "reference encoded in this script. Review the analytical workflow."
  )
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
    "The 006.1 null-model RDS lacks:\n",
    paste(missing_null_elements, collapse = ", ")
  )
}

if (!identical(
  as.character(null_model_results$Status),
  "Formal modularity null-model validation"
)) {
  stop("The 006.1 null-model RDS has an unexpected status.")
}

if (!identical(
  as.character(null_model_results$Traceability_key),
  "Author + ID"
)) {
  stop("The 006.1 null-model RDS does not use Author + ID traceability.")
}

modularity_tests_rds <- null_model_results$Modularity_tests

if (!is.data.frame(modularity_tests_rds)) {
  stop("Modularity_tests in the 006.1 RDS is not a data frame.")
}

required_test_columns <- c(
  "Test",
  "Status",
  "Observed_statistic",
  "Empirical_p"
)

for (object_name_i in c("modularity_tests_rds", "modularity_tests_csv")) {
  missing_columns_i <- setdiff(
    required_test_columns,
    names(get(object_name_i))
  )

  if (length(missing_columns_i) > 0L) {
    stop(
      object_name_i,
      " lacks:\n",
      paste(missing_columns_i, collapse = ", ")
    )
  }
}

primary_test_rds <- modularity_tests_rds[
  modularity_tests_rds$Status == "Primary inference",
  required_test_columns,
  drop = FALSE
]

primary_test_csv <- modularity_tests_csv[
  modularity_tests_csv$Status == "Primary inference",
  required_test_columns,
  drop = FALSE
]

if (nrow(primary_test_rds) != 1L || nrow(primary_test_csv) != 1L) {
  stop("Exactly one primary inference must be present in each 006.1 output.")
}

for (column_i in required_test_columns) {
  if (!isTRUE(all.equal(
    primary_test_rds[[column_i]],
    primary_test_csv[[column_i]],
    tolerance = comparison_tolerance,
    check.attributes = FALSE
  ))) {
    stop(
      "Primary-test column '",
      column_i,
      "' differs between the 006.1 RDS and CSV."
    )
  }
}

if (
  !isTRUE(all.equal(
    as.numeric(primary_test_rds$Empirical_p),
    primary_empirical_p,
    tolerance = comparison_tolerance
  )) ||
    !isTRUE(all.equal(
      as.numeric(primary_test_rds$Observed_statistic),
      observed_weighted_modularity,
      tolerance = comparison_tolerance
    ))
) {
  stop("The primary 006.1 result differs from corrected 006.")
}

required_dependency_audit_columns <- c(
  "Dependency",
  "File",
  "MD5",
  "Modified"
)

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

# Confirm that every source shared with corrected 006 is byte-identical
# to the object audited by corrected 006.

shared_audited_files <- c(
  robust_data_file,
  pca_scores_file,
  pca_variance_file,
  modularity_null_results_file,
  modularity_tests_csv_file
)

audited_basenames <- basename(
  as.character(corrected_006_dependency_audit$File)
)

for (file_i in shared_audited_files) {
  matching_rows_i <- which(audited_basenames == basename(file_i))

  if (length(matching_rows_i) != 1L) {
    stop(
      "The corrected 006 dependency audit does not contain exactly one row for ",
      basename(file_i),
      "."
    )
  }

  saved_md5_i <- tolower(trimws(as.character(
    corrected_006_dependency_audit$MD5[matching_rows_i]
  )))

  current_md5_i <- tolower(as.character(
    unname(tools::md5sum(file_i))
  ))

  if (!identical(saved_md5_i, current_md5_i)) {
    stop(
      basename(file_i),
      " differs from the file consumed by corrected 006."
    )
  }
}

saved_robust_md5 <- tolower(as.character(
  unlist(null_model_results$Input_MD5, use.names = FALSE)
))

current_robust_md5 <- tolower(as.character(
  unname(tools::md5sum(robust_data_file))
))

if (
  length(saved_robust_md5) != 1L ||
    !identical(saved_robust_md5, current_robust_md5)
) {
  stop("The robust-control object differs from the object evaluated by 006.1.")
}

# ------------------------------------------------------------
# 7. Validate Author + ID traceability and analysis variables
# ------------------------------------------------------------

required_control_columns <- c(
  "Author",
  "ID",
  "Treatment",
  raw_variables,
  robust_variables
)

missing_control_columns <- setdiff(
  required_control_columns,
  names(control_robust)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The robust-control dataset lacks:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_score_columns <- c("Author", "ID", components)
missing_score_columns <- setdiff(required_score_columns, names(pca_scores))

if (length(missing_score_columns) > 0L) {
  stop(
    "The PCA-score object lacks:\n",
    paste(missing_score_columns, collapse = ", ")
  )
}

make_traceability_key <- function(author, id) {
  paste(trimws(as.character(author)), trimws(as.character(id)), sep = " || ")
}

control_robust <- control_robust %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = trimws(as.character(Treatment)),
    Traceability_ID = make_traceability_key(Author, ID)
  )

pca_scores <- pca_scores %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = make_traceability_key(Author, ID)
  )

for (data_name_i in c("control_robust", "pca_scores")) {
  data_i <- get(data_name_i)

  if (
    anyNA(data_i[c("Author", "ID", "Traceability_ID")]) ||
      any(data_i$Author == "") ||
      any(data_i$ID == "") ||
      any(data_i$Traceability_ID == "")
  ) {
    stop("Missing or empty traceability values were detected in ", data_name_i, ".")
  }

  if (
    anyDuplicated(data_i[c("Author", "ID")]) > 0L ||
      anyDuplicated(data_i$Traceability_ID) > 0L
  ) {
    stop("Duplicated Author + ID keys were detected in ", data_name_i, ".")
  }
}

if (nrow(control_robust) != expected_number_of_fish) {
  stop(
    "The robust-control dataset contains ",
    nrow(control_robust),
    " fish; 89 were expected."
  )
}

if (nrow(pca_scores) != expected_number_of_fish) {
  stop(
    "The PCA-score object contains ",
    nrow(pca_scores),
    " fish; 89 were expected."
  )
}

if (!all(control_robust$Treatment == "Control")) {
  stop("The robust-control object contains non-Control observations.")
}

author_levels <- sort(unique(control_robust$Author))

if (length(author_levels) != expected_number_of_authors) {
  stop("The robust-control object does not contain the expected four Authors.")
}

if (!setequal(control_robust$Traceability_ID, pca_scores$Traceability_ID)) {
  stop("Author + ID keys differ between the robust and PCA-score objects.")
}

pca_scores <- pca_scores[
  match(control_robust$Traceability_ID, pca_scores$Traceability_ID),
  ,
  drop = FALSE
]

if (!identical(control_robust$Traceability_ID, pca_scores$Traceability_ID)) {
  stop("The PCA scores could not be aligned by Author + ID.")
}

numeric_analysis_variables <- c(raw_variables, robust_variables)
non_numeric_control_variables <- numeric_analysis_variables[
  !vapply(control_robust[numeric_analysis_variables], is.numeric, logical(1))
]

if (length(non_numeric_control_variables) > 0L) {
  stop(
    "The following control variables are not numeric:\n",
    paste(non_numeric_control_variables, collapse = ", ")
  )
}

non_numeric_scores <- components[
  !vapply(pca_scores[components], is.numeric, logical(1))
]

if (length(non_numeric_scores) > 0L) {
  stop(
    "The following PCA-score columns are not numeric:\n",
    paste(non_numeric_scores, collapse = ", ")
  )
}

control_matrix_all <- as.matrix(
  control_robust[c(raw_variables, robust_variables)]
)
score_matrix_stored <- as.matrix(pca_scores[components])

storage.mode(control_matrix_all) <- "double"
storage.mode(score_matrix_stored) <- "double"

if (
  any(!is.finite(control_matrix_all)) ||
    any(!is.finite(score_matrix_stored))
) {
  stop("Missing or non-finite behavioural or PCA-score values were detected.")
}

# ------------------------------------------------------------
# 8. Reconstruct and audit within-Author robust normalisation
# ------------------------------------------------------------

normalisation_parameters <- dplyr::bind_rows(lapply(
  author_levels,
  function(author_i) {
    author_data_i <- control_robust[
      control_robust$Author == author_i,
      ,
      drop = FALSE
    ]

    dplyr::bind_rows(lapply(
      seq_along(raw_variables),
      function(variable_position_i) {
        raw_variable_i <- raw_variables[variable_position_i]
        robust_variable_i <- robust_variables[variable_position_i]
        raw_values_i <- author_data_i[[raw_variable_i]]
        stored_robust_i <- author_data_i[[robust_variable_i]]
        median_i <- stats::median(raw_values_i)
        mad_raw_i <- stats::median(abs(raw_values_i - median_i))
        mad_scaled_i <- 1.4826 * mad_raw_i

        if (!is.finite(mad_scaled_i) || mad_scaled_i <= 0) {
          stop(
            "A non-positive scaled MAD was found for Author ",
            author_i,
            ", variable ",
            raw_variable_i,
            "."
          )
        }

        reconstructed_robust_i <- (raw_values_i - median_i) / mad_scaled_i

        tibble::tibble(
          Author = author_i,
          Variable_Code = raw_variable_i,
          Variable = unname(variable_labels[raw_variable_i]),
          Robust_Variable = robust_variable_i,
          n = length(raw_values_i),
          Median_Raw = median_i,
          MAD_Raw = mad_raw_i,
          MAD_Scaled = mad_scaled_i,
          Median_Robust_Z = stats::median(stored_robust_i),
          Scaled_MAD_Robust_Z = stats::mad(
            stored_robust_i,
            center = stats::median(stored_robust_i),
            constant = 1.4826
          ),
          Maximum_Absolute_Reconstruction_Difference = max(
            abs(stored_robust_i - reconstructed_robust_i)
          )
        )
      }
    ))
  }
)) %>%
  dplyr::arrange(Author, match(Variable_Code, raw_variables))

if (
  nrow(normalisation_parameters) !=
    expected_number_of_authors * length(raw_variables)
) {
  stop("The within-Author normalisation audit has an unexpected size.")
}

normalisation_numeric_columns <- c(
  "Median_Raw",
  "MAD_Raw",
  "MAD_Scaled",
  "Median_Robust_Z",
  "Scaled_MAD_Robust_Z",
  "Maximum_Absolute_Reconstruction_Difference"
)

if (any(!is.finite(as.matrix(
  normalisation_parameters[normalisation_numeric_columns]
)))) {
  stop("The within-Author normalisation audit contains non-finite values.")
}

if (any(abs(normalisation_parameters$Median_Robust_Z) > normalisation_tolerance)) {
  stop("At least one within-Author robust median differs from zero.")
}

if (any(abs(
  normalisation_parameters$Scaled_MAD_Robust_Z - 1
) > normalisation_tolerance)) {
  stop("At least one within-Author scaled MAD differs from one.")
}

maximum_normalisation_reconstruction_difference <- max(
  normalisation_parameters$Maximum_Absolute_Reconstruction_Difference
)

if (
  maximum_normalisation_reconstruction_difference >
    reconstruction_tolerance
) {
  stop(
    "Stored robust z-scores cannot be reconstructed from the raw values ",
    "and within-Author control parameters."
  )
}

# ------------------------------------------------------------
# 9. Verify the stored PCA model, loadings, variance and scores
# ------------------------------------------------------------

if (!isFALSE(pca_model$scale)) {
  stop(
    "The stored PCA model must not apply additional SD scaling after ",
    "within-Author robust scaling."
  )
}

if (
  !is.numeric(pca_model$center) ||
    length(pca_model$center) != length(raw_variables) ||
    is.null(names(pca_model$center)) ||
    !all(raw_variables %in% names(pca_model$center))
) {
  stop("The stored PCA model does not contain the expected four-variable centre.")
}

if (
  is.null(pca_model$rotation) ||
    is.null(pca_model$sdev) ||
    is.null(pca_model$x)
) {
  stop("The stored PCA model lacks rotation, sdev or retained scores.")
}

if (
  !all(raw_variables %in% rownames(pca_model$rotation)) ||
    !all(components %in% colnames(pca_model$rotation))
) {
  stop("The stored PCA rotation does not contain the expected variables and components.")
}

required_loading_columns <- c("Variable", components)
missing_loading_columns <- setdiff(
  required_loading_columns,
  names(pca_loadings)
)

if (length(missing_loading_columns) > 0L) {
  stop(
    "The PCA-loading object lacks:\n",
    paste(missing_loading_columns, collapse = ", ")
  )
}

pca_loadings <- pca_loadings %>%
  dplyr::mutate(Variable = trimws(as.character(Variable)))

if (
  anyNA(pca_loadings$Variable) ||
    any(pca_loadings$Variable == "") ||
    anyDuplicated(pca_loadings$Variable) > 0L ||
    !setequal(pca_loadings$Variable, raw_variables)
) {
  stop("The PCA-loading object has invalid or unexpected variable identifiers.")
}

pca_loadings <- pca_loadings[
  match(raw_variables, pca_loadings$Variable),
  ,
  drop = FALSE
]

rotation_matrix <- pca_model$rotation[raw_variables, components, drop = FALSE]
stored_loading_matrix <- as.matrix(pca_loadings[components])
storage.mode(rotation_matrix) <- "double"
storage.mode(stored_loading_matrix) <- "double"

if (any(!is.finite(stored_loading_matrix))) {
  stop("The stored PCA-loading matrix contains non-finite values.")
}

maximum_loading_difference <- max(abs(
  rotation_matrix - stored_loading_matrix
))

if (maximum_loading_difference > comparison_tolerance) {
  stop("The stored PCA loadings differ from the PCA-model rotation.")
}

orthogonality_difference <- max(abs(
  crossprod(rotation_matrix) - diag(length(components))
))

if (orthogonality_difference > reconstruction_tolerance) {
  stop("The PCA rotation is not orthonormal within tolerance.")
}

robust_matrix <- as.matrix(control_robust[robust_variables])
storage.mode(robust_matrix) <- "double"
rownames(robust_matrix) <- control_robust$Traceability_ID

pca_centre <- as.numeric(pca_model$center[raw_variables])
names(pca_centre) <- raw_variables

if (any(!is.finite(pca_centre))) {
  stop("The stored PCA centre contains non-finite values.")
}

expected_pca_centre <- colMeans(robust_matrix)

maximum_pca_centre_difference <- max(abs(
  pca_centre - expected_pca_centre
))

if (maximum_pca_centre_difference > reconstruction_tolerance) {
  stop("The stored PCA centre differs from the mean of the robust input matrix.")
}

pca_centre_table <- tibble::tibble(
  Variable_Code = raw_variables,
  Variable = unname(variable_labels[raw_variables]),
  PCA_Centre = unname(pca_centre)
)

score_matrix_reconstructed <- sweep(
  robust_matrix,
  MARGIN = 2,
  STATS = pca_centre,
  FUN = "-"
) %*% rotation_matrix
colnames(score_matrix_reconstructed) <- components

maximum_score_reconstruction_difference <- max(abs(
  score_matrix_reconstructed - score_matrix_stored
))

if (
  maximum_score_reconstruction_difference >
    reconstruction_tolerance
) {
  stop("Stored PCA scores cannot be reconstructed from the robust data and loadings.")
}

pca_model_score_matrix <- as.matrix(
  pca_model$x[, components, drop = FALSE]
)
storage.mode(pca_model_score_matrix) <- "double"

if (!identical(dim(pca_model_score_matrix), dim(score_matrix_stored))) {
  stop("The retained PCA-model scores have an unexpected dimension.")
}

maximum_model_score_difference <- max(abs(
  pca_model_score_matrix - score_matrix_stored
))

if (maximum_model_score_difference > reconstruction_tolerance) {
  stop("The exported PCA scores differ from the scores retained in the PCA model.")
}

required_variance_columns <- c(
  "Component",
  "Eigenvalue",
  "Percentage_of_variance",
  "Cumulative_percentage"
)

missing_variance_columns <- setdiff(
  required_variance_columns,
  names(pca_variance)
)

if (length(missing_variance_columns) > 0L) {
  stop(
    "The PCA-variance object lacks:\n",
    paste(missing_variance_columns, collapse = ", ")
  )
}

pca_variance <- pca_variance %>%
  dplyr::transmute(
    Component = trimws(as.character(Component)),
    Eigenvalue = as.numeric(Eigenvalue),
    Percentage_of_variance = as.numeric(Percentage_of_variance),
    Cumulative_percentage = as.numeric(Cumulative_percentage)
  )

if (
  anyNA(pca_variance$Component) ||
    anyDuplicated(pca_variance$Component) > 0L ||
    !setequal(pca_variance$Component, components)
) {
  stop("The PCA-variance object has invalid or unexpected component labels.")
}

pca_variance <- pca_variance[
  match(components, pca_variance$Component),
  ,
  drop = FALSE
]

if (any(!is.finite(as.matrix(
  pca_variance[c(
    "Eigenvalue",
    "Percentage_of_variance",
    "Cumulative_percentage"
  )]
)))) {
  stop("The PCA-variance table contains non-finite values.")
}

eigenvalues_reconstructed <- as.numeric(pca_model$sdev)^2
percentage_reconstructed <- 100 * eigenvalues_reconstructed /
  sum(eigenvalues_reconstructed)
cumulative_reconstructed <- cumsum(percentage_reconstructed)

if (length(eigenvalues_reconstructed) != length(components)) {
  stop("The PCA model does not contain exactly four component variances.")
}

maximum_eigenvalue_difference <- max(abs(
  pca_variance$Eigenvalue - eigenvalues_reconstructed
))

maximum_percentage_difference <- max(abs(
  pca_variance$Percentage_of_variance - percentage_reconstructed
))

maximum_cumulative_difference <- max(abs(
  pca_variance$Cumulative_percentage - cumulative_reconstructed
))

if (
  maximum_eigenvalue_difference > comparison_tolerance ||
    maximum_percentage_difference > reconstruction_tolerance ||
    maximum_cumulative_difference > reconstruction_tolerance
) {
  stop("The stored PCA-variance table cannot be reconstructed from the PCA model.")
}

# ------------------------------------------------------------
# 10. Construct the continuous behavioural reference
# ------------------------------------------------------------

score_distribution <- dplyr::bind_rows(lapply(
  components,
  function(component_i) {
    values_i <- pca_scores[[component_i]]

    tibble::tibble(
      Component = component_i,
      n = length(values_i),
      Mean = mean(values_i),
      SD = stats::sd(values_i),
      Median = stats::median(values_i),
      Q1 = as.numeric(stats::quantile(values_i, 0.25, names = FALSE)),
      Q3 = as.numeric(stats::quantile(values_i, 0.75, names = FALSE)),
      IQR = stats::IQR(values_i),
      Minimum = min(values_i),
      Maximum = max(values_i)
    )
  }
))

if (
  nrow(score_distribution) != length(components) ||
    any(score_distribution$n != expected_number_of_fish) ||
    any(!is.finite(as.matrix(score_distribution[
      setdiff(names(score_distribution), "Component")
    ])))
) {
  stop("The PCA-score reference distribution is invalid.")
}

loadings_long <- pca_loadings %>%
  dplyr::select(Variable, dplyr::all_of(components)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(components),
    names_to = "Component",
    values_to = "Loading"
  ) %>%
  dplyr::mutate(
    Variable_Code = Variable,
    Variable = unname(variable_labels[Variable_Code]),
    Component = factor(Component, levels = components),
    Variable_Code = factor(Variable_Code, levels = raw_variables)
  ) %>%
  dplyr::arrange(Component, Variable_Code) %>%
  dplyr::mutate(
    Component = as.character(Component),
    Variable_Code = as.character(Variable_Code)
  ) %>%
  dplyr::select(Component, Variable_Code, Variable, Loading)

if (
  nrow(loadings_long) != length(components) * length(raw_variables) ||
    anyNA(loadings_long$Variable) ||
    any(!is.finite(loadings_long$Loading))
) {
  stop("The long-format PCA-loading table is invalid.")
}

loadings_wide <- loadings_long %>%
  dplyr::select(Component, Variable_Code, Loading) %>%
  tidyr::pivot_wider(
    names_from = Variable_Code,
    values_from = Loading,
    names_prefix = "Loading_"
  )

continuous_reference_full <- pca_variance %>%
  dplyr::left_join(loadings_wide, by = "Component") %>%
  dplyr::left_join(score_distribution, by = "Component") %>%
  dplyr::mutate(
    Orientation = "Stored PCA orientation; component signs are arbitrary"
  ) %>%
  dplyr::select(
    Component,
    Eigenvalue,
    Percentage_of_variance,
    Cumulative_percentage,
    Loading_distance,
    Loading_crossing,
    Loading_top_entry,
    Loading_top_time,
    n,
    Mean,
    SD,
    Median,
    Q1,
    Q3,
    IQR,
    Minimum,
    Maximum,
    Orientation
  )

if (
  nrow(continuous_reference_full) != length(components) ||
    anyNA(continuous_reference_full)
) {
  stop("The continuous PCA reference table is incomplete.")
}

continuous_reference_display <- continuous_reference_full %>%
  dplyr::transmute(
    Component,
    `Variance explained (%)` = round(Percentage_of_variance, 2),
    `Cumulative variance (%)` = round(Cumulative_percentage, 2),
    `Distance loading` = round(Loading_distance, 4),
    `Line-crossing loading` = round(Loading_crossing, 4),
    `Upper-entry loading` = round(Loading_top_entry, 4),
    `Upper-time loading` = round(Loading_top_time, 4),
    `Control score, median` = round(Median, 4),
    `Control score, Q1` = round(Q1, 4),
    `Control score, Q3` = round(Q3, 4)
  )

# ------------------------------------------------------------
# 11. Validation, status and interpretation tables
# ------------------------------------------------------------

validation_table <- tibble::tibble(
  Check = c(
    "Control fish",
    "Author cohorts",
    "Traceability key",
    "Within-Author robust medians",
    "Within-Author scaled MADs",
    "Maximum robust-z reconstruction difference",
    "Maximum stored PCA-centre difference",
    "Maximum stored-loading difference",
    "Maximum score reconstruction difference",
    "Maximum retained-model score difference",
    "Maximum PCA eigenvalue difference",
    "Maximum PCA percentage difference",
    "Maximum PCA cumulative-percentage difference",
    "Maximum rotation orthogonality difference",
    "Primary selection-adjusted empirical p",
    "Evidence of discrete communities",
    "Segment-derived statistics in Table 3"
  ),
  Result = c(
    as.character(expected_number_of_fish),
    as.character(expected_number_of_authors),
    "Author + ID",
    "All approximately zero",
    "All approximately one",
    format(maximum_normalisation_reconstruction_difference, scientific = TRUE),
    format(maximum_pca_centre_difference, scientific = TRUE),
    format(maximum_loading_difference, scientific = TRUE),
    format(maximum_score_reconstruction_difference, scientific = TRUE),
    format(maximum_model_score_difference, scientific = TRUE),
    format(maximum_eigenvalue_difference, scientific = TRUE),
    format(maximum_percentage_difference, scientific = TRUE),
    format(maximum_cumulative_difference, scientific = TRUE),
    format(orthogonality_difference, scientific = TRUE),
    sprintf("%.9f", primary_empirical_p),
    "FALSE",
    "NONE"
  )
)

analysis_status <- tibble::tibble(
  Parameter = c(
    "Table designation",
    "Analytical status",
    "Number of control fish",
    "Traceability",
    "Control cohorts",
    "Behavioural variables",
    "Normalisation",
    "PCA fitting",
    "Reference dimensionality",
    "Primary modularity p",
    "Evidence of discrete communities",
    "Use of Louvain segments",
    "Permitted downstream use",
    "Prohibited downstream use"
  ),
  Value = c(
    "Table 3 - Continuous behavioural reference",
    "Continuous multivariate reference; no discrete behavioural archetypes",
    as.character(expected_number_of_fish),
    "Author + ID",
    as.character(expected_number_of_authors),
    paste(unname(variable_labels), collapse = "; "),
    "Within Author controls: (x - median) / (1.4826 x raw MAD)",
    "All 89 controls jointly after cohort-wise normalisation; mean centring applied; no additional SD scaling",
    "Four orthogonal continuous component scores",
    sprintf("%.9f", primary_empirical_p),
    "FALSE",
    "None",
    "Projection of observations onto fixed PCA axes as continuous scores",
    "Archetype, cluster, resilience/susceptibility or other categorical assignment"
  )
)

projection_specification <- tibble::tibble(
  Step = c(
    "1. Traceability",
    "2. Cohort reference",
    "3. Robust transformation",
    "4. PCA mean centring",
    "5. Continuous projection",
    "6. Interpretation"
  ),
  Specification = c(
    "Retain Author + ID; Author is a cohort identifier, not a behavioural predictor.",
    "For each Author and variable, use the median and scaled MAD estimated from that Author's controls.",
    "Robust z = (raw value - control median) / (1.4826 x control raw MAD).",
    "Subtract the stored pooled-control PCA centre from the four-variable robust-z row vector.",
    "PC scores = centred robust-z row vector multiplied by the fixed 4 x 4 PCA rotation.",
    "Analyse scores continuously. PCA signs are arbitrary and must remain fixed to this stored orientation."
  )
)

interpretation_notes <- tibble::tibble(
  Note = c(
    paste0(
      "The selection-adjusted modularity test was not significant ",
      "(p = ",
      sprintf("%.6f", primary_empirical_p),
      "); therefore no discrete control archetypes are estimated."
    ),
    paste0(
      "Table 3 reports the continuous PCA architecture: explained variance, ",
      "variable loadings and descriptive control-score distributions."
    ),
    paste0(
      "Component signs are mathematically arbitrary. The stored orientation ",
      "is frozen only to ensure reproducible projection and interpretation."
    ),
    paste0(
      "The control-score quartiles are descriptive reference summaries, not ",
      "diagnostic thresholds or categories."
    ),
    paste0(
      "No Louvain membership, segment centroid, nearest-centroid rule, ",
      "inferential segment comparison or stressed-fish classification is used."
    )
  )
)

# ------------------------------------------------------------
# 12. Output paths and historical outputs
# ------------------------------------------------------------

excel_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference.xlsx"
)

display_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_Display.csv"
)

full_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_Full.csv"
)

loadings_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_LoadingsLong.csv"
)

score_distribution_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_ScoreDistribution.csv"
)

normalisation_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_CohortNormalisation.csv"
)

validation_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_Validation.csv"
)

status_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_Status.csv"
)

notes_csv_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_Notes.csv"
)

dependency_audit_output <- file.path(
  tables_dir,
  "Table3_ContinuousBehaviouralReference_DependencyAudit.csv"
)

continuous_reference_rds <- file.path(
  objects_dir,
  "objective1_continuous_behavioural_reference.rds"
)

session_info_file <- file.path(
  objects_dir,
  "objective1_continuous_behavioural_reference_sessionInfo.txt"
)

historical_output_files <- c(
  file.path(tables_dir, "Table3_ArchetypeCentroids.xlsx"),
  file.path(tables_dir, "Table3_ArchetypeCentroids.csv"),
  file.path(tables_dir, "Table3_ArchetypeCentroids_Long.csv"),
  file.path(tables_dir, "Table3_ArchetypeClusterSizes.csv"),
  file.path(tables_dir, "Table3_ArchetypeCentroids_Audit.csv"),
  file.path(tables_dir, "Table3_ArchetypeCentroids_Configuration.csv"),
  file.path(objects_dir, "control_archetype_centroids.rds"),
  file.path(objects_dir, "objective1_control_archetype_centroids_long_robust.rds"),
  file.path(objects_dir, "objective1_control_archetype_centroids_audit_robust.rds")
)

existing_historical_outputs <- historical_output_files[
  file.exists(historical_output_files)
]

historical_md5_before <- if (length(existing_historical_outputs) > 0L) {
  tools::md5sum(existing_historical_outputs)
} else {
  character(0)
}

dependency_audit <- tibble::tibble(
  Dependency = c(
    "005 robust control data",
    "005 PCA model",
    "005 PCA scores",
    "005 PCA loadings",
    "005 PCA variance",
    "Corrected 006 inference status",
    "006.1 complete null-model results",
    "006.1 modularity-test CSV",
    "Corrected 006 dependency audit"
  ),
  File = required_input_files,
  MD5 = unname(tools::md5sum(required_input_files)),
  Modified = as.character(file.info(required_input_files)$mtime)
)

# ------------------------------------------------------------
# 13. Export workbook, CSV tables and continuous reference RDS
# ------------------------------------------------------------

workbook <- openxlsx::createWorkbook(
  creator = "LAPCOM UCS Resilience project"
)

workbook_sheets <- list(
  Continuous_reference = continuous_reference_display,
  Full_precision = continuous_reference_full,
  Loadings_long = loadings_long,
  PCA_centre = pca_centre_table,
  Score_distribution = score_distribution,
  Cohort_normalisation = normalisation_parameters,
  Projection_specification = projection_specification,
  Analysis_status = analysis_status,
  Validation = validation_table,
  Interpretation_notes = interpretation_notes,
  Dependency_audit = dependency_audit
)

header_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#2F6F73",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom",
  borderColour = "#1F4C4F"
)

body_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  valign = "top"
)

wrap_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  valign = "top",
  wrapText = TRUE
)

for (sheet_name_i in names(workbook_sheets)) {
  sheet_data_i <- workbook_sheets[[sheet_name_i]]

  openxlsx::addWorksheet(
    workbook,
    sheetName = sheet_name_i,
    gridLines = FALSE
  )

  openxlsx::writeData(
    workbook,
    sheet = sheet_name_i,
    x = sheet_data_i,
    startRow = 1,
    startCol = 1,
    headerStyle = header_style,
    withFilter = TRUE
  )

  if (nrow(sheet_data_i) > 0L && ncol(sheet_data_i) > 0L) {
    openxlsx::addStyle(
      workbook,
      sheet = sheet_name_i,
      style = body_style,
      rows = 2:(nrow(sheet_data_i) + 1L),
      cols = seq_len(ncol(sheet_data_i)),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet = sheet_name_i,
    firstActiveRow = 2
  )

  openxlsx::setColWidths(
    workbook,
    sheet = sheet_name_i,
    cols = seq_len(ncol(sheet_data_i)),
    widths = "auto"
  )
}

openxlsx::setColWidths(
  workbook,
  sheet = "Continuous_reference",
  cols = seq_len(ncol(continuous_reference_display)),
  widths = 19
)

for (sheet_name_i in c(
  "Projection_specification",
  "Analysis_status",
  "Interpretation_notes"
)) {
  sheet_data_i <- workbook_sheets[[sheet_name_i]]

  openxlsx::addStyle(
    workbook,
    sheet = sheet_name_i,
    style = wrap_style,
    rows = 2:(nrow(sheet_data_i) + 1L),
    cols = seq_len(ncol(sheet_data_i)),
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::setColWidths(
  workbook,
  sheet = "Projection_specification",
  cols = c(1, 2),
  widths = c(24, 100)
)

openxlsx::setColWidths(
  workbook,
  sheet = "Analysis_status",
  cols = c(1, 2),
  widths = c(32, 100)
)

openxlsx::setColWidths(
  workbook,
  sheet = "Interpretation_notes",
  cols = 1,
  widths = 110
)

openxlsx::saveWorkbook(
  workbook,
  file = excel_output,
  overwrite = TRUE
)

utils::write.csv(
  continuous_reference_display,
  file = display_csv_output,
  row.names = FALSE
)

utils::write.csv(
  continuous_reference_full,
  file = full_csv_output,
  row.names = FALSE
)

utils::write.csv(
  loadings_long,
  file = loadings_csv_output,
  row.names = FALSE
)

utils::write.csv(
  score_distribution,
  file = score_distribution_csv_output,
  row.names = FALSE
)

utils::write.csv(
  normalisation_parameters,
  file = normalisation_csv_output,
  row.names = FALSE
)

utils::write.csv(
  validation_table,
  file = validation_csv_output,
  row.names = FALSE
)

utils::write.csv(
  analysis_status,
  file = status_csv_output,
  row.names = FALSE
)

utils::write.csv(
  interpretation_notes,
  file = notes_csv_output,
  row.names = FALSE
)

utils::write.csv(
  dependency_audit,
  file = dependency_audit_output,
  row.names = FALSE
)

continuous_reference_object <- list(
  Status = "Continuous PCA reference; no discrete behavioural archetypes",
  Traceability_Key = "Author + ID",
  Number_of_Control_Fish = expected_number_of_fish,
  Number_of_Authors = expected_number_of_authors,
  Raw_Variables = raw_variables,
  Robust_Variables = robust_variables,
  Normalisation_Parameters = normalisation_parameters,
  PCA_Model = pca_model,
  PCA_Centre = pca_centre_table,
  Fixed_Rotation = rotation_matrix,
  PCA_Variance = pca_variance,
  PCA_Loadings_Long = loadings_long,
  Control_Score_Distribution = score_distribution,
  Continuous_Reference_Table = continuous_reference_full,
  Projection_Specification = projection_specification,
  Inference_Status = analysis_status,
  Primary_Selection_Adjusted_Empirical_P = primary_empirical_p,
  Evidence_of_Discrete_Communities = FALSE,
  Segment_Membership_Included = FALSE,
  Classification_Thresholds_Included = FALSE,
  Validation = validation_table,
  Dependency_Audit = dependency_audit
)

saveRDS(continuous_reference_object, continuous_reference_rds)

capture.output(
  utils::sessionInfo(),
  file = session_info_file
)

# Deliberately not written:
# - Louvain memberships or segment centroids;
# - normative archetypes or biological profile labels;
# - nearest-centroid or other categorical assignment rules;
# - cut-offs for resilience, susceptibility, anxiety or exploration;
# - inferential comparisons between algorithmic segments.

# ------------------------------------------------------------
# 14. Verify exported files and historical-file preservation
# ------------------------------------------------------------

expected_output_files <- c(
  excel_output,
  display_csv_output,
  full_csv_output,
  loadings_csv_output,
  score_distribution_csv_output,
  normalisation_csv_output,
  validation_csv_output,
  status_csv_output,
  notes_csv_output,
  dependency_audit_output,
  continuous_reference_rds,
  session_info_file
)

output_information <- file.info(expected_output_files)

if (
  any(!file.exists(expected_output_files)) ||
    any(is.na(output_information$size)) ||
    any(output_information$size <= 0)
) {
  stop("At least one expected continuous-reference output is absent or empty.")
}

expected_sheet_names <- names(workbook_sheets)
observed_sheet_names <- openxlsx::getSheetNames(excel_output)

if (!identical(observed_sheet_names, expected_sheet_names)) {
  stop("The exported workbook does not contain the expected worksheets.")
}

display_csv_check <- utils::read.csv(
  display_csv_output,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (
  nrow(display_csv_check) != nrow(continuous_reference_display) ||
    !identical(names(display_csv_check), names(continuous_reference_display))
) {
  stop("The exported display CSV does not match the in-memory table structure.")
}

continuous_reference_check <- readRDS(continuous_reference_rds)

if (
  !is.list(continuous_reference_check) ||
    !identical(
      as.character(continuous_reference_check$Status),
      "Continuous PCA reference; no discrete behavioural archetypes"
    ) ||
    !identical(
      as.logical(continuous_reference_check$Evidence_of_Discrete_Communities),
      FALSE
    ) ||
    !identical(
      as.logical(continuous_reference_check$Segment_Membership_Included),
      FALSE
    ) ||
    !identical(
      as.logical(continuous_reference_check$Classification_Thresholds_Included),
      FALSE
    )
) {
  stop("The exported continuous-reference RDS failed its status audit.")
}

if (length(existing_historical_outputs) > 0L) {
  historical_md5_after <- tools::md5sum(existing_historical_outputs)

  if (!identical(
    unname(historical_md5_before),
    unname(historical_md5_after)
  )) {
    stop("At least one historical archetype-centroid output was modified.")
  }
}

# ------------------------------------------------------------
# 15. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SCRIPT 009 - TABLE 3 CONTINUOUS BEHAVIOURAL REFERENCE\n")
cat("============================================================\n")
cat("\nStatus: continuous multivariate reference only.\n")
cat("Control fish: ", expected_number_of_fish, "\n", sep = "")
cat("Author cohorts: ", expected_number_of_authors, "\n", sep = "")
cat("Traceability: Author + ID\n")
cat("Continuous dimensions: ", length(components), "\n", sep = "")
cat(
  "Primary selection-adjusted empirical p: ",
  sprintf("%.6f", primary_empirical_p),
  "\n",
  sep = ""
)
cat("Evidence of discrete communities: FALSE\n")
cat("Louvain segments used in Table 3: NONE\n")
cat("Archetypes or classification thresholds created: NONE\n")

cat("\nWithin-Author normalisation and reconstruction audit:\n")
print(normalisation_parameters, n = Inf, width = Inf)

cat("\nTable 3 - continuous behavioural reference:\n")
print(continuous_reference_display, n = Inf, width = Inf)

cat("\nValidation:\n")
print(validation_table, n = Inf, width = Inf)

cat(
  "\nContinuous-reference workbook saved as:\n",
  normalizePath(excel_output, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

cat(
  "Continuous projection reference saved as:\n",
  normalizePath(continuous_reference_rds, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

if (length(existing_historical_outputs) > 0L) {
  cat(
    "\nWARNING: superseded Table3_ArchetypeCentroids outputs remain on disk.\n",
    "They were not modified or deleted and must not be treated as current results:\n",
    paste(existing_historical_outputs, collapse = "\n"),
    "\n",
    sep = ""
  )
}

cat("\nAll dependency and integrity checks passed.\n")
cat("============================================================\n")
