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