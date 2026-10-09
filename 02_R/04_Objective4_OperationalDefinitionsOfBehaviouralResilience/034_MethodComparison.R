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

project_root <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

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
    n_Resilient =
      sum(
        c_across(
          dplyr::all_of(methods)
        ) == "Resilient"
      ),

    n_Susceptible =
      sum(
        c_across(
          dplyr::all_of(methods)
        ) == "Susceptible"
      ),

    n_Intermediate =
      sum(
        c_across(
          dplyr::all_of(methods)
        ) == "Intermediate"
      ),

    Resilience_Consensus_Score =
      n_Resilient /
      length(methods),

    Susceptibility_Consensus_Score =
      n_Susceptible /
      length(methods),

    Intermediate_Consensus_Score =
      n_Intermediate /
      length(methods)
  ) %>%
  dplyr::ungroup()

if (
  any(
    method_data$n_Resilient +
      method_data$n_Susceptible +
      method_data$n_Intermediate !=
      length(methods)
  )
) {
  stop(
    "Consensus counts do not sum to five methods."
  )
}

# ------------------------------------------------------------
# 11. Consensus categories
# ------------------------------------------------------------

method_data <- method_data %>%
  dplyr::mutate(
    Consensus_Category =
      dplyr::case_when(
        n_Susceptible == 5L ~
          "Unanimous susceptible",

        n_Resilient == 5L ~
          "Unanimous resilient",

        n_Susceptible == 4L ~
          "High-consensus susceptible",

        n_Resilient == 4L ~
          "High-consensus resilient",

        TRUE ~
          "Ambiguous/intermediate"
      ),

    Consensus_Category = factor(
      Consensus_Category,
      levels = c(
        "Unanimous susceptible",
        "High-consensus susceptible",
        "Ambiguous/intermediate",
        "High-consensus resilient",
        "Unanimous resilient"
      )
    )
  )

# ------------------------------------------------------------
# 12. Individual consensus tables
# ------------------------------------------------------------

unanimous_susceptible <- method_data %>%
  dplyr::filter(
    n_Susceptible == 5L
  )

high_consensus_susceptible <- method_data %>%
  dplyr::filter(
    n_Susceptible == 4L
  )

ambiguous_individuals <- method_data %>%
  dplyr::filter(
    Consensus_Category ==
      "Ambiguous/intermediate"
  )

high_consensus_resilient <- method_data %>%
  dplyr::filter(
    n_Resilient == 4L
  )

unanimous_resilient <- method_data %>%
  dplyr::filter(
    n_Resilient == 5L
  )

# ------------------------------------------------------------
# 13. Long-format dataset and method sizes
# ------------------------------------------------------------

method_long <- method_data %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Behavioural_Index,
    dplyr::all_of(methods)
  ) %>%
  tidyr::pivot_longer(
    cols =
      dplyr::all_of(methods),

    names_to =
      "Method",

    values_to =
      "Classification"
  ) %>%
  dplyr::mutate(
    Method = factor(
      Method,
      levels = methods
    ),

    Classification = factor(
      Classification,
      levels = allowed_labels
    )
  )

method_size_table <- method_long %>%
  dplyr::count(
    Method,
    Classification,
    name = "n",
    .drop = FALSE
  ) %>%
  dplyr::group_by(
    Method
  ) %>%
  dplyr::mutate(
    Percentage =
      100 *
      n /
      sum(n)
  ) %>%
  dplyr::ungroup()

method_size_wide <- method_size_table %>%
  dplyr::select(
    Method,
    Classification,
    n
  ) %>%
  tidyr::pivot_wider(
    names_from =
      Classification,

    values_from =
      n,

    values_fill =
      0
  )

# ------------------------------------------------------------
# 14. Heatmap helper
# ------------------------------------------------------------

matrix_to_long <- function(
  matrix_object,
  value_name
) {

  as.data.frame(
    as.table(
      matrix_object
    )
  ) %>%
    dplyr::rename(
      Method_1 = Var1,
      Method_2 = Var2,
      !!value_name := Freq
    ) %>%
    dplyr::mutate(
      Method_1 = factor(
        Method_1,
        levels = methods
      ),

      Method_2 = factor(
        Method_2,
        levels = rev(methods)
      )
    )
}

ari_long <- matrix_to_long(
  ari_matrix,
  "ARI"
) %>%
  dplyr::mutate(
    Text_colour = ifelse(
      ARI >= 0.60,
      "white",
      "black"
    )
  )

exact_agreement_long <- matrix_to_long(
  exact_agreement_matrix,
  "Exact_Agreement"
) %>%
  dplyr::mutate(
    Text_colour = ifelse(
      Exact_Agreement >= 70,
      "white",
      "black"
    )
  )

cramers_long <- matrix_to_long(
  cramers_v_matrix,
  "Cramers_V"
) %>%
  dplyr::mutate(
    Text_colour = ifelse(
      Cramers_V >= 0.60,
      "white",
      "black"
    )
  )

kappa_long <- matrix_to_long(
  kappa_matrix,
  "Kappa"
) %>%
  dplyr::mutate(
    Text_colour = ifelse(
      Kappa >= 0.60,
      "white",
      "black"
    )
  )

# ------------------------------------------------------------
# 15. Heatmap constructor
# ------------------------------------------------------------

make_heatmap <- function(
  data,
  value_column,
  label_digits,
  legend_title,
  limits,
  text_threshold,
  filename_stub
) {

  data <- data %>%
    dplyr::mutate(
      Text_colour =
        ifelse(
          .data[[value_column]] >=
            text_threshold,
          "white",
          "black"
        )
    )

  label_format <- paste0(
    "%.",
    label_digits,
    "f"
  )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = Method_1,
      y = Method_2,
      fill = .data[[value_column]]
    )
  ) +
    ggplot2::geom_tile(
      colour = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf(
          label_format,
          .data[[value_column]]
        ),
        colour = Text_colour
      ),
      family = "Arial",
      size = 4,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "black",
      limits = limits
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = legend_title
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(
      base_size = 13,
      base_family = "Arial"
    ) +
    ggplot2::theme(
      panel.grid =
        ggplot2::element_blank(),

      axis.text.x =
        ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
    )

  print(p)

  ggplot2::ggsave(
    filename = file.path(
      figures_dir,
      paste0(
        filename_stub,
        ".pdf"
      )
    ),
    plot = p,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    units = "in",
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(
      figures_dir,
      paste0(
        filename_stub,
        ".tiff"
      )
    ),
    plot = p,
    width = 7,
    height = 6,
    units = "in",
    dpi = 600,
    compression = "lzw",
    bg = "white"
  )

  p
}

# ------------------------------------------------------------
# 16. Agreement heatmaps
# ------------------------------------------------------------

p_ari <- make_heatmap(
  data = ari_long,
  value_column = "ARI",
  label_digits = 2,
  legend_title = "ARI",
  limits = c(
    min(
      ari_long$ARI,
      na.rm = TRUE
    ),
    1
  ),
  text_threshold = 0.60,
  filename_stub = "Objective4_ARI_Heatmap"
)

p_exact_agreement <- make_heatmap(
  data = exact_agreement_long,
  value_column = "Exact_Agreement",
  label_digits = 1,
  legend_title = "Exact agreement (%)",
  limits = c(
    min(
      exact_agreement_long$Exact_Agreement,
      na.rm = TRUE
    ),
    100
  ),
  text_threshold = 70,
  filename_stub = "Objective4_ExactAgreement_Heatmap"
)

p_cramers <- make_heatmap(
  data = cramers_long,
  value_column = "Cramers_V",
  label_digits = 2,
  legend_title = "Cramer's V",
  limits = c(
    min(
      cramers_long$Cramers_V,
      na.rm = TRUE
    ),
    1
  ),
  text_threshold = 0.60,
  filename_stub = "Objective4_CramersV_Heatmap"
)

p_kappa <- make_heatmap(
  data = kappa_long,
  value_column = "Kappa",
  label_digits = 2,
  legend_title = "Cohen's kappa",
  limits = c(
    min(
      kappa_long$Kappa,
      na.rm = TRUE
    ),
    1
  ),
  text_threshold = 0.60,
  filename_stub = "Objective4_Kappa_Heatmap"
)

# ------------------------------------------------------------
# 17. Consensus-score data and figure
# ------------------------------------------------------------

consensus_long <- method_data %>%
  dplyr::select(
    Author,
    ID,
    Traceability_ID,
    Resilience_Consensus_Score,
    Susceptibility_Consensus_Score
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      Resilience_Consensus_Score,
      Susceptibility_Consensus_Score
    ),

    names_to =
      "Consensus_Type",

    values_to =
      "Consensus_Score"
  )

consensus_plot_data <- consensus_long %>%
  dplyr::count(
    Consensus_Type,
    Consensus_Score,
    name = "n"
  ) %>%
  tidyr::complete(
    Consensus_Type,

    Consensus_Score =
      seq(
        0,
        1,
        by = 0.2
      ),

    fill = list(
      n = 0
    )
  )

p_consensus <- ggplot2::ggplot(
  consensus_plot_data,
  ggplot2::aes(
    x = factor(
      Consensus_Score,
      levels = seq(
        0,
        1,
        by = 0.2
      )
    ),
    y = n,
    fill = Consensus_Type
  )
) +
  ggplot2::geom_col(
    position =
      ggplot2::position_dodge(
        width = 0.8
      ),
    width = 0.7,
    colour = "black"
  ) +
  ggplot2::labs(
    x = "Consensus score",
    y = "Number of individuals",
    fill = NULL
  ) +
  ggplot2::theme_classic(
    base_size = 14,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    legend.position = "top"
  )

print(p_consensus)

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "Objective4_ConsensusScores.pdf"
  ),
  plot = p_consensus,
  device = grDevices::cairo_pdf,
  width = 7.5,
  height = 5.5,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "Objective4_ConsensusScores.tiff"
  ),
  plot = p_consensus,
  width = 7.5,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ------------------------------------------------------------
# 18. Export Excel workbook
# ------------------------------------------------------------

export_list <- c(
  list(
    Pairwise_results =
      pairwise_table,

    ARI_matrix =
      data.frame(
        Method =
          rownames(
            ari_matrix
          ),
        ari_matrix,
        check.names = FALSE
      ),

    ExactAgreement_matrix =
      data.frame(
        Method =
          rownames(
            exact_agreement_matrix
          ),
        exact_agreement_matrix,
        check.names = FALSE
      ),

    CramersV_matrix =
      data.frame(
        Method =
          rownames(
            cramers_v_matrix
          ),
        cramers_v_matrix,
        check.names = FALSE
      ),

    Kappa_matrix =
      data.frame(
        Method =
          rownames(
            kappa_matrix
          ),
        kappa_matrix,
        check.names = FALSE
      ),

    Method_sizes_long =
      method_size_table,

    Method_sizes_wide =
      method_size_wide,

    Individual_consensus =
      method_data,

    Consensus_plot_data =
      consensus_plot_data,

    Unanimous_susceptible =
      unanimous_susceptible,

    High_consensus_susceptible =
      high_consensus_susceptible,

    Ambiguous_intermediate =
      ambiguous_individuals,

    High_consensus_resilient =
      high_consensus_resilient,

    Unanimous_resilient =
      unanimous_resilient
  ),
  contingency_export
)

comparison_xlsx <- file.path(
  tables_dir,
  "Objective4_FiveMethodComparison.xlsx"
)

openxlsx::write.xlsx(
  export_list,
  file = comparison_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 19. Save objects
# ------------------------------------------------------------

saveRDS(
  method_data,
  file.path(
    objects_dir,
    "objective4_five_method_classifications.rds"
  )
)

saveRDS(
  pairwise_table,
  file.path(
    objects_dir,
    "objective4_pairwise_method_comparison.rds"
  )
)

saveRDS(
  ari_matrix,
  file.path(
    objects_dir,
    "objective4_ari_matrix.rds"
  )
)

saveRDS(
  exact_agreement_matrix,
  file.path(
    objects_dir,
    "objective4_exact_agreement_matrix.rds"
  )
)

saveRDS(
  cramers_v_matrix,
  file.path(
    objects_dir,
    "objective4_cramers_v_matrix.rds"
  )
)

saveRDS(
  kappa_matrix,
  file.path(
    objects_dir,
    "objective4_kappa_matrix.rds"
  )
)

saveRDS(
  method_size_table,
  file.path(
    objects_dir,
    "objective4_five_method_size_table_long.rds"
  )
)

saveRDS(
  method_size_wide,
  file.path(
    objects_dir,
    "objective4_five_method_size_table_wide.rds"
  )
)

saveRDS(
  method_long,
  file.path(
    objects_dir,
    "objective4_method_classifications_long.rds"
  )
)

saveRDS(
  consensus_plot_data,
  file.path(
    objects_dir,
    "objective4_consensus_plot_data.rds"
  )
)

capture.output(
  utils::sessionInfo(),
  file = file.path(
    objects_dir,
    "objective4_method_comparison_sessionInfo.txt"
  )
)

# ------------------------------------------------------------
# 20. Confirm outputs
# ------------------------------------------------------------

expected_outputs <- c(
  comparison_xlsx,

  file.path(
    figures_dir,
    "Objective4_ARI_Heatmap.pdf"
  ),

  file.path(
    figures_dir,
    "Objective4_ARI_Heatmap.tiff"
  ),

  file.path(
    figures_dir,
    "Objective4_ExactAgreement_Heatmap.pdf"
  ),

  file.path(
    figures_dir,
    "Objective4_ExactAgreement_Heatmap.tiff"
  ),

  file.path(
    figures_dir,
    "Objective4_CramersV_Heatmap.pdf"
  ),

  file.path(
    figures_dir,
    "Objective4_CramersV_Heatmap.tiff"
  ),

  file.path(
    figures_dir,
    "Objective4_Kappa_Heatmap.pdf"
  ),

  file.path(
    figures_dir,
    "Objective4_Kappa_Heatmap.tiff"
  ),

  file.path(
    figures_dir,
    "Objective4_ConsensusScores.pdf"
  ),

  file.path(
    figures_dir,
    "Objective4_ConsensusScores.tiff"
  )
)

missing_outputs <- expected_outputs[
  !file.exists(expected_outputs)
]

if (length(missing_outputs) > 0L) {
  stop(
    "The following Objective 4 comparison outputs were not created:\n",
    paste(missing_outputs, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 21. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 4 - FIVE-METHOD COMPARISON\n")
cat("============================================================\n")

cat("\nIntegrated dataset dimensions:\n")
print(
  dim(method_data)
)

cat("\nFish per Author:\n")
print(
  table(
    method_data$Author,
    useNA = "ifany"
  )
)

cat("\nDuplicate Author + ID combinations:\n")
print(
  sum(
    duplicated(
      method_data[c("Author", "ID")]
    )
  )
)

cat("\nDuplicate Traceability_ID values:\n")
print(
  sum(
    duplicated(
      method_data$Traceability_ID
    )
  )
)

cat("\nMaximum absolute Behavioural_Index difference across methods:\n")
print(
  index_difference_by_method
)

cat("\nClass sizes by method:\n")

for (m in methods) {
  cat(
    "\n",
    m,
    "\n",
    sep = ""
  )

  print(
    table(
      factor(
        method_data[[m]],
        levels = allowed_labels
      ),
      useNA = "ifany"
    )
  )
}

cat("\nPairwise method comparison:\n")
print(
  tibble::as_tibble(
    pairwise_table
  ),
  n = Inf,
  width = Inf
)

cat("\nARI matrix:\n")
print(
  round(
    ari_matrix,
    3
  )
)

cat("\nExact label agreement matrix (%):\n")
print(
  round(
    exact_agreement_matrix,
    1
  )
)

cat("\nCramer's V matrix:\n")
print(
  round(
    cramers_v_matrix,
    3
  )
)

cat("\nCohen's kappa matrix:\n")
print(
  round(
    kappa_matrix,
    3
  )
)

cat("\nFive-method size table:\n")
print(
  method_size_wide,
  n = Inf,
  width = Inf
)

cat("\nConsensus-category distribution:\n")
print(
  table(
    method_data$Consensus_Category,
    useNA = "ifany"
  )
)

cat("\nResilience-consensus score distribution:\n")
print(
  table(
    method_data$Resilience_Consensus_Score,
    useNA = "ifany"
  )
)

cat("\nSusceptibility-consensus score distribution:\n")
print(
  table(
    method_data$Susceptibility_Consensus_Score,
    useNA = "ifany"
  )
)

cat("\nIntermediate-consensus score distribution:\n")
print(
  table(
    method_data$Intermediate_Consensus_Score,
    useNA = "ifany"
  )
)

cat(
  "\nNo classification method was recalculated in this script.\n",
  "All five individual classifications were inherited from the ",
  "corrected Objective 4 objects and merged by Author + ID / Traceability_ID.\n",
  sep = ""
)

cat("\nOutputs saved successfully.\n")
cat("\n============================================================\n")
