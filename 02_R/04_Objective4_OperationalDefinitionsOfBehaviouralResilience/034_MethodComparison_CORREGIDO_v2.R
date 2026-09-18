# ============================================================
# 034_MethodComparison_CORREGIDO_v2.R
#
# Objective 4
# Comparison of five operational definitions
# of behavioural resilience
#
# CORRECTED ANALYTICAL FOUNDATION
# - Stress only, n = 89
# - Traceability = Author + ID / Traceability_ID
# - All five classifications are inherited from the corrected
#   Objective 4 objects already validated independently:
#     1. Swaminathan-like
#     2. PCA + quantiles
#     3. GMM
#     4. Two-Step
#     5. Louvain
# - No classification method is recalculated here.
# - The script compares the individual assignments produced by
#   the five methods.
# ============================================================

script_version <- paste(
  "034_MethodComparison_CORREGIDO_v2",
  "| 2026-08-10",
  "| Author + ID traceability and corrected five-method objects"
)

cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 0. Project directory, packages, seed, outputs
# ------------------------------------------------------------

project_root <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

if (!dir.exists(project_root)) {
  stop("Project directory not found:\n", project_root)
}

setwd(project_root)

required_packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "mclust",
  "irr",
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

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(mclust)
  library(irr)
  library(openxlsx)
})

set.seed(1234)

expected_stress_n <- 89L
expected_authors <- c("1", "2", "3", "4")
numeric_tolerance <- 1e-10

allowed_labels <- c(
  "Susceptible",
  "Intermediate",
  "Resilient"
)

methods <- c(
  "Swaminathan",
  "PCA_Quantiles",
  "GMM",
  "TwoStep",
  "Louvain"
)

output_directories <- c(
  "03_Results/Figures",
  "03_Results/Tables",
  "03_Results/Objects"
)

for (directory_i in output_directories) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

objects_dir <- "03_Results/Objects"
tables_dir <- "03_Results/Tables"
figures_dir <- "03_Results/Figures"

# ------------------------------------------------------------
# 1. Corrected input objects
# ------------------------------------------------------------

swaminathan_file <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

pca_quantile_file <- file.path(
  objects_dir,
  "objective4_pca_quantile_classification.rds"
)

gmm_file <- file.path(
  objects_dir,
  "objective4_gmm_classification.rds"
)

twostep_file <- file.path(
  objects_dir,
  "objective4_twostep_classification.rds"
)

louvain_file <- file.path(
  objects_dir,
  "objective4_louvain_classification.rds"
)

required_input_files <- c(
  swaminathan_file,
  pca_quantile_file,
  gmm_file,
  twostep_file,
  louvain_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following corrected classification objects were not found:\n",
    paste(missing_input_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2. Helper: standardise one corrected classification object
# ------------------------------------------------------------

prepare_classification <- function(
  object,
  object_name,
  class_variable,
  output_class_name,
  treatment_filter = FALSE
) {

  if (!is.data.frame(object)) {
    stop(
      object_name,
      " does not contain a data frame."
    )
  }

  required_columns <- c(
    "Author",
    "ID",
    "Traceability_ID",
    "Behavioural_Index",
    class_variable
  )

  if (treatment_filter) {
    required_columns <- c(
      required_columns,
      "Treatment"
    )
  }

  missing_columns <- setdiff(
    required_columns,
    names(object)
  )

  if (length(missing_columns) > 0L) {
    stop(
      object_name,
      " is missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (treatment_filter) {
    object <- object %>%
      dplyr::filter(
        Treatment == "Stress"
      )
  }

  result <- object %>%
    dplyr::transmute(
      Author =
        trimws(
          as.character(Author)
        ),

      ID =
        trimws(
          as.character(ID)
        ),

      Traceability_ID =
        trimws(
          as.character(Traceability_ID)
        ),

      Behavioural_Index =
        as.numeric(Behavioural_Index),

      Classification =
        trimws(
          as.character(
            .data[[class_variable]]
          )
        )
    ) %>%
    dplyr::rename(
      !!output_class_name :=
        Classification
    ) %>%
    dplyr::arrange(
      Traceability_ID
    )

  if (nrow(result) != expected_stress_n) {
    stop(
      object_name,
      " does not contain exactly 89 Stress fish."
    )
  }

  if (
    anyNA(result$Author) ||
    anyNA(result$ID) ||
    anyNA(result$Traceability_ID) ||
    any(result$Author == "") ||
    any(result$ID == "") ||
    any(result$Traceability_ID == "")
  ) {
    stop(
      "Missing or empty identifiers detected in ",
      object_name,
      "."
    )
  }

  if (
    anyDuplicated(
      result[c("Author", "ID")]
    ) > 0L ||
    anyDuplicated(
      result$Traceability_ID
    ) > 0L
  ) {
    stop(
      "Duplicated Author + ID traceability keys detected in ",
      object_name,
      "."
    )
  }

  reconstructed_traceability <- paste(
    result$Author,
    result$ID,
    sep = " || "
  )

  if (
    !identical(
      result$Traceability_ID,
      reconstructed_traceability
    )
  ) {
    stop(
      "Traceability_ID does not exactly match Author + ID in ",
      object_name,
      "."
    )
  }

  if (
    !identical(
      sort(unique(result$Author)),
      expected_authors
    )
  ) {
    stop(
      object_name,
      " does not contain Authors 1, 2, 3, and 4."
    )
  }

  if (
    anyNA(
      result[[output_class_name]]
    )
  ) {
    stop(
      "Missing classifications detected in ",
      object_name,
      "."
    )
  }

  invalid_labels <- setdiff(
    unique(
      result[[output_class_name]]
    ),
    allowed_labels
  )

  if (length(invalid_labels) > 0L) {
    stop(
      "Invalid classification labels detected in ",
      object_name,
      ": ",
      paste(invalid_labels, collapse = ", ")
    )
  }

  if (
    any(
      !is.finite(
        result$Behavioural_Index
      )
    )
  ) {
    stop(
      "Missing or non-finite Behavioural_Index values detected in ",
      object_name,
      "."
    )
  }

  result
}

# ------------------------------------------------------------
# 3. Load and standardise the five corrected classifications
# ------------------------------------------------------------

swaminathan <- prepare_classification(
  object = readRDS(swaminathan_file),
  object_name = "Swaminathan",
  class_variable = "Swaminathan_class",
  output_class_name = "Swaminathan",
  treatment_filter = TRUE
)

pca_quantile <- prepare_classification(
  object = readRDS(pca_quantile_file),
  object_name = "PCA + quantiles",
  class_variable = "PCA_Quantile_Class",
  output_class_name = "PCA_Quantiles"
)

gmm <- prepare_classification(
  object = readRDS(gmm_file),
  object_name = "GMM",
  class_variable = "GMM_Class",
  output_class_name = "GMM"
)

twostep <- prepare_classification(
  object = readRDS(twostep_file),
  object_name = "Two-Step",
  class_variable = "TwoStep_Class",
  output_class_name = "TwoStep"
)

louvain <- prepare_classification(
  object = readRDS(louvain_file),
  object_name = "Louvain",
  class_variable = "Louvain_Class",
  output_class_name = "Louvain"
)

classification_objects <- list(
  Swaminathan = swaminathan,
  PCA_Quantiles = pca_quantile,
  GMM = gmm,
  TwoStep = twostep,
  Louvain = louvain
)

# ------------------------------------------------------------
# 4. Cross-object traceability and Behavioural_Index audit
# ------------------------------------------------------------

reference_keys <- swaminathan$Traceability_ID
reference_index <- swaminathan$Behavioural_Index

index_difference_by_method <- c(
  Swaminathan = 0
)

for (
  object_name in setdiff(
    names(classification_objects),
    "Swaminathan"
  )
) {

  object_i <- classification_objects[[object_name]]

  if (
    !identical(
      reference_keys,
      object_i$Traceability_ID
    )
  ) {
    stop(
      "The individual set/order in ",
      object_name,
      " does not exactly match the Swaminathan reference set."
    )
  }

  index_difference_by_method[object_name] <- max(
    abs(
      reference_index -
        object_i$Behavioural_Index
    )
  )
}

if (
  any(!is.finite(index_difference_by_method)) ||
  any(index_difference_by_method > numeric_tolerance)
) {
  stop(
    "The Behavioural_Index is not identical across the corrected ",
    "Objective 4 classification objects."
  )
}

# ------------------------------------------------------------
# 5. Merge classifications by full traceability key
# ------------------------------------------------------------

method_data <- swaminathan %>%
  dplyr::rename(
    Behavioural_Index_reference =
      Behavioural_Index
  ) %>%
  dplyr::left_join(
    pca_quantile %>%
      dplyr::select(
        Author,
        ID,
        Traceability_ID,
        PCA_Quantiles
      ),
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  ) %>%
  dplyr::left_join(
    gmm %>%
      dplyr::select(
        Author,
        ID,
        Traceability_ID,
        GMM
      ),
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  ) %>%
  dplyr::left_join(
    twostep %>%
      dplyr::select(
        Author,
        ID,
        Traceability_ID,
        TwoStep
      ),
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  ) %>%
  dplyr::left_join(
    louvain %>%
      dplyr::select(
        Author,
        ID,
        Traceability_ID,
        Louvain
      ),
    by = c(
      "Author",
      "ID",
      "Traceability_ID"
    )
  ) %>%
  dplyr::rename(
    Behavioural_Index =
      Behavioural_Index_reference
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

# ------------------------------------------------------------
# 6. Audit integrated dataset
# ------------------------------------------------------------

if (nrow(method_data) != expected_stress_n) {
  stop(
    "Expected 89 stressed individuals, found ",
    nrow(method_data),
    "."
  )
}

if (
  anyDuplicated(
    method_data[c("Author", "ID")]
  ) > 0L ||
  anyDuplicated(
    method_data$Traceability_ID
  ) > 0L
) {
  stop(
    "Duplicated Author + ID traceability keys detected in the integrated dataset."
  )
}

if (
  anyNA(
    method_data[, methods]
  )
) {
  stop(
    "Missing method classifications detected in the integrated dataset."
  )
}

invalid_labels_integrated <- unique(
  unlist(
    lapply(
      method_data[, methods],
      function(x) {
        setdiff(
          unique(as.character(x)),
          allowed_labels
        )
      }
    )
  )
)

invalid_labels_integrated <- invalid_labels_integrated[
  !is.na(invalid_labels_integrated)
]

if (length(invalid_labels_integrated) > 0L) {
  stop(
    "Invalid classification labels detected: ",
    paste(
      invalid_labels_integrated,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# 7. Helper functions for pairwise agreement
# ------------------------------------------------------------

exact_label_agreement <- function(
  x,
  y
) {
  mean(
    x == y
  )
}

safe_kappa <- function(
  x,
  y
) {

  # All methods use the same conceptual classification system:
  # Susceptible / Intermediate / Resilient.
  #
  # Some methods do not generate an Intermediate category.
  # This does NOT make Cohen's kappa inapplicable. Both raters
  # are therefore represented using the same three predefined
  # category levels, with zero occupancy where appropriate.

  x_factor <- factor(
    x,
    levels = allowed_labels
  )

  y_factor <- factor(
    y,
    levels = allowed_labels
  )

  result <- tryCatch(
    irr::kappa2(
      data.frame(
        rater1 = x_factor,
        rater2 = y_factor
      ),
      weight = "unweighted"
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {

    # Manual fallback for Cohen's unweighted kappa.
    tab_full <- table(
      x_factor,
      y_factor
    )

    n <- sum(tab_full)

    p_o <- sum(diag(tab_full)) / n

    p_e <- sum(
      rowSums(tab_full) *
        colSums(tab_full)
    ) / (n^2)

    if (
      !is.finite(p_e) ||
      abs(1 - p_e) < .Machine$double.eps
    ) {
      return(
        data.frame(
          Kappa = NA_real_,
          Kappa_p = NA_real_,
          Kappa_applicable = FALSE
        )
      )
    }

    return(
      data.frame(
        Kappa =
          (p_o - p_e) /
          (1 - p_e),

        Kappa_p =
          NA_real_,

        Kappa_applicable =
          TRUE
      )
    )
  }

  data.frame(
    Kappa =
      unname(
        result$value
      ),

    Kappa_p =
      unname(
        result$p.value
      ),

    Kappa_applicable =
      TRUE
  )
}

cramers_v <- function(tab) {

  chi <- suppressWarnings(
    stats::chisq.test(
      tab,
      correct = FALSE
    )
  )

  n <- sum(tab)

  k <- min(
    nrow(tab) - 1,
    ncol(tab) - 1
  )

  if (
    k <= 0 ||
    n <= 0
  ) {
    return(
      NA_real_
    )
  }

  sqrt(
    as.numeric(
      chi$statistic
    ) /
      (n * k)
  )
}

# ------------------------------------------------------------
# 8. Pairwise comparisons
# ------------------------------------------------------------

method_pairs <- combn(
  methods,
  2,
  simplify = FALSE
)

pairwise_results <- vector(
  mode = "list",
  length = length(method_pairs)
)

contingency_export <- list()

for (i in seq_along(method_pairs)) {

  method_a <- method_pairs[[i]][1]
  method_b <- method_pairs[[i]][2]

  x <- as.character(
    method_data[[method_a]]
  )

  y <- as.character(
    method_data[[method_b]]
  )

  # Full common 3 x 3 table for transparent classification comparison.
  tab_full <- table(
    factor(
      x,
      levels = allowed_labels
    ),
    factor(
      y,
      levels = allowed_labels
    )
  )

  # For chi-square/Fisher calculations, remove empty margins.
  tab_observed <- tab_full[
    rowSums(tab_full) > 0,
    colSums(tab_full) > 0,
    drop = FALSE
  ]

  chi_result <- suppressWarnings(
    stats::chisq.test(
      tab_observed,
      correct = FALSE
    )
  )

  expected_counts <- chi_result$expected

  minimum_expected_count <- min(
    expected_counts
  )

  proportion_expected_below_5 <- mean(
    expected_counts < 5
  )

  if (
    nrow(tab_observed) == 2L &&
    ncol(tab_observed) == 2L
  ) {

    fisher_result <- stats::fisher.test(
      tab_observed
    )

    fisher_p <- fisher_result$p.value
    fisher_method <- "Exact Fisher test"

  } else {

    fisher_result <- stats::fisher.test(
      tab_observed,
      simulate.p.value = TRUE,
      B = 99999
    )

    fisher_p <- fisher_result$p.value
    fisher_method <- "Monte Carlo Fisher test"
  }

  kappa_result <- safe_kappa(
    x,
    y
  )

  pairwise_results[[i]] <- data.frame(
    Method_1 =
      method_a,

    Method_2 =
      method_b,

    Categories_Method_1 =
      length(unique(x)),

    Categories_Method_2 =
      length(unique(y)),

    Exact_agreement_n =
      sum(x == y),

    Exact_agreement_percent =
      100 *
      exact_label_agreement(
        x,
        y
      ),

    ARI =
      mclust::adjustedRandIndex(
        x,
        y
      ),

    Chi_square =
      unname(
        chi_result$statistic
      ),

    Chi_square_df =
      unname(
        chi_result$parameter
      ),

    Chi_square_p =
      chi_result$p.value,

    Minimum_expected_count =
      minimum_expected_count,

    Proportion_expected_below_5 =
      proportion_expected_below_5,

    Fisher_method =
      fisher_method,

    Fisher_p =
      fisher_p,

    Cramers_V =
      cramers_v(
        tab_observed
      ),

    Kappa =
      kappa_result$Kappa,

    Kappa_p =
      kappa_result$Kappa_p,

    Kappa_applicable =
      kappa_result$Kappa_applicable,

    stringsAsFactors = FALSE
  )

  sheet_name <- paste0(
    method_a,
    "_vs_",
    method_b
  )

  contingency_export[[sheet_name]] <- data.frame(
    Class_Method_1 =
      rownames(
        tab_full
      ),
    as.data.frame.matrix(
      tab_full
    ),
    check.names = FALSE,
    row.names = NULL
  )
}

pairwise_table <- dplyr::bind_rows(
  pairwise_results
)

# ------------------------------------------------------------
# 9. Build symmetric agreement matrices
# ------------------------------------------------------------

create_symmetric_matrix <- function(
  pairwise_data,
  value_column,
  diagonal_value = 1
) {

  mat <- matrix(
    NA_real_,
    nrow = length(methods),
    ncol = length(methods),
    dimnames = list(
      methods,
      methods
    )
  )

  diag(mat) <- diagonal_value

  for (
    i in seq_len(
      nrow(pairwise_data)
    )
  ) {

    m1 <- pairwise_data$Method_1[i]
    m2 <- pairwise_data$Method_2[i]

    value <- pairwise_data[[value_column]][i]

    mat[m1, m2] <- value
    mat[m2, m1] <- value
  }

  mat
}

ari_matrix <- create_symmetric_matrix(
  pairwise_table,
  "ARI"
)

exact_agreement_matrix <- create_symmetric_matrix(
  pairwise_table,
  "Exact_agreement_percent",
  diagonal_value = 100
)

cramers_v_matrix <- create_symmetric_matrix(
  pairwise_table,
  "Cramers_V"
)

kappa_matrix <- create_symmetric_matrix(
  pairwise_table,
  "Kappa"
)

# ------------------------------------------------------------
# 10. Consensus scores
# ------------------------------------------------------------

method_data <- method_data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(