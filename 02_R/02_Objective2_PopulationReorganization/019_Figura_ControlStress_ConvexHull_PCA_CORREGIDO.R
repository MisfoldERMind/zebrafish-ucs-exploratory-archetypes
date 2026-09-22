# ============================================================
# 019_Figura_ControlStress_ConvexHull_PCA.R
#
# Objective 2
# Control versus Stress projection with descriptive convex hulls
#
# The PCA coordinate system is inherited directly from the
# corrected Objective 1 outputs:
#   - objective1_control_robust_z.rds
#   - objective1_pca_scores_robust.rds
#   - objective1_pca_variance_robust.rds
#
# The four behavioural variables were robustly normalised within
# Author using the corresponding Control median and scaled raw MAD.
# Individual continuity is verified by Author + ID.
#
# The affine transformation that maps the four corrected robust
# behavioural variables onto the Objective 1 PC1-PC2 coordinates is
# reconstructed from the 89 Control fish and then applied unchanged
# to the 89 Stress fish.
#
# This avoids refitting a PCA with different centring or scaling
# conventions and guarantees continuity with the final Objective 1
# representation.
#
# Convex hulls are descriptive boundaries only. They are not
# confidence regions and provide no inferential test.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

project_root <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(project_root)) {
  stop("Project directory not found:\n", project_root)
}

setwd(project_root)

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c("dplyr", "tibble", "ggplot2")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tibble)
library(ggplot2)

# ------------------------------------------------------------
# 3. Fixed settings
# ------------------------------------------------------------

required_treatments <- c("Control", "Stress")
expected_control_n <- 89L
expected_stress_n <- 89L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"
expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- paste0(behavioural_variables, "_rz")
pc_variables <- c("PC1", "PC2")

numeric_tolerance <- 1e-10
projection_tolerance <- 1e-8

# ------------------------------------------------------------
# 4. Paths
# ------------------------------------------------------------

processed_dir <- file.path(project_root, "01_Data", "Processed")
objects_dir <- file.path(project_root, "03_Results", "Objects")
tables_dir <- file.path(project_root, "03_Results", "Tables")
figures_dir <- file.path(project_root, "03_Results", "Figures")

for (directory_i in c(objects_dir, tables_dir, figures_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

population_file <- file.path(
  processed_dir,
  "population_control_stress_robust_z.rds"
)

objective1_control_file <- file.path(
  objects_dir,
  "objective1_control_robust_z.rds"
)

objective1_scores_file <- file.path(
  objects_dir,
  "objective1_pca_scores_robust.rds"
)

objective1_variance_file <- file.path(
  objects_dir,
  "objective1_pca_variance_robust.rds"
)

required_input_files <- c(
  population_file,
  objective1_control_file,
  objective1_scores_file,
  objective1_variance_file
)

missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun the corrected Objective 1 PCA script and Script 011 first."
  )
}

projection_model_output <- file.path(
  objects_dir,
  "objective2_control_reference_pca_projection_robust.rds"
)

scores_output <- file.path(
  objects_dir,
  "objective2_control_stress_pca_scores_robust.rds"
)

hulls_output <- file.path(
  objects_dir,
  "objective2_control_stress_pca_hulls_robust.rds"
)

centroids_output <- file.path(
  objects_dir,
  "objective2_control_stress_pca_centroids_robust.rds"
)

results_output <- file.path(
  objects_dir,
  "objective2_control_stress_pca_results_robust.rds"
)

variance_csv_output <- file.path(
  tables_dir,
  "Objective2_ControlReference_PCA_ExplainedVariance.csv"
)

coefficients_csv_output <- file.path(
  tables_dir,
  "Objective2_ControlReference_PCA_ProjectionCoefficients.csv"
)

centroids_csv_output <- file.path(
  tables_dir,
  "Objective2_ControlStress_PCA_Centroids.csv"
)

hull_summary_csv_output <- file.path(
  tables_dir,
  "Objective2_ControlStress_PCA_ConvexHull_Summary.csv"
)

figure_pdf_output <- file.path(
  figures_dir,
  "Objective2_ControlStress_ConvexHull_PCA.pdf"
)

figure_tiff_output <- file.path(
  figures_dir,
  "Objective2_ControlStress_ConvexHull_PCA.tiff"
)

figure_png_output <- file.path(
  figures_dir,
  "Objective2_ControlStress_ConvexHull_PCA.png"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_control_stress_pca_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load and validate corrected upstream objects
# ------------------------------------------------------------

population <- readRDS(population_file)
objective1_control <- readRDS(objective1_control_file)
objective1_scores <- readRDS(objective1_scores_file)
objective1_variance <- readRDS(objective1_variance_file)

if (!is.data.frame(population)) {
  stop(basename(population_file), " is not a data frame.")
}

if (!is.data.frame(objective1_control)) {
  stop(basename(objective1_control_file), " is not a data frame.")
}

if (!is.data.frame(objective1_scores)) {
  stop(basename(objective1_scores_file), " is not a data frame.")
}

if (!is.data.frame(objective1_variance)) {
  stop(basename(objective1_variance_file), " is not a data frame.")
}

required_population_columns <- c(
  "Author",
  "ID",
  "Treatment",
  robust_variables
)

missing_population_columns <- setdiff(
  required_population_columns,
  names(population)
)

if (length(missing_population_columns) > 0L) {
  stop(
    "The corrected Objective 2 population is missing:\n",
    paste(missing_population_columns, collapse = ", ")
  )
}

required_control_columns <- c(
  "Author",
  "ID",
  "Treatment",
  robust_variables
)

missing_control_columns <- setdiff(
  required_control_columns,
  names(objective1_control)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The corrected Objective 1 Control object is missing:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_score_columns <- c(
  "Author",
  "ID",
  pc_variables
)

missing_score_columns <- setdiff(
  required_score_columns,
  names(objective1_scores)
)

if (length(missing_score_columns) > 0L) {
  stop(
    "The corrected Objective 1 PCA-score object is missing:\n",
    paste(missing_score_columns, collapse = ", ")
  )
}

required_variance_columns <- c(
  "Component",
  "Percentage_of_variance"
)

missing_variance_columns <- setdiff(
  required_variance_columns,
  names(objective1_variance)
)

if (length(missing_variance_columns) > 0L) {
  stop(
    "The corrected Objective 1 variance object is missing:\n",
    paste(missing_variance_columns, collapse = ", ")
  )
}

if (
  !all(
    vapply(
      population[robust_variables],
      is.numeric,
      logical(1)
    )
  )
) {
  stop("The Objective 2 robust behavioural variables must be numeric.")
}

if (
  !all(
    vapply(
      objective1_control[robust_variables],
      is.numeric,
      logical(1)
    )
  )
) {
  stop("The Objective 1 robust behavioural variables must be numeric.")
}

if (
  !all(
    vapply(
      objective1_scores[pc_variables],
      is.numeric,
      logical(1)
    )
  )
) {
  stop("The corrected Objective 1 PC1-PC2 scores must be numeric.")
}

make_traceability_key <- function(author, id) {
  paste(
    trimws(as.character(author)),
    trimws(as.character(id)),
    sep = " || "
  )
}

population <- population %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Traceability_ID = make_traceability_key(Author, ID),
    Analysis_ID = paste(
      as.character(Treatment),
      Author,
      ID,
      sep = "_"
    )
  )

objective1_control <- objective1_control %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = as.character(Treatment),
    Traceability_ID = make_traceability_key(Author, ID)
  )

objective1_scores <- objective1_scores %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = make_traceability_key(Author, ID)
  )

for (data_name_i in c(
  "population",
  "objective1_control",
  "objective1_scores"
)) {
  data_i <- get(data_name_i)

  if (
    anyNA(
      data_i[
        c(
          "Author",
          "ID",
          "Traceability_ID"
        )
      ]
    ) ||
    any(data_i$Author == "") ||
    any(data_i$ID == "") ||
    any(data_i$Traceability_ID == "")
  ) {
    stop(
      "Missing or empty Author + ID traceability values were ",
      "detected in ",
      data_name_i,
      "."
    )
  }

  if (
    anyDuplicated(
      data_i[
        c(
          "Author",
          "ID"
        )
      ]
    ) > 0L ||
    anyDuplicated(
      data_i$Traceability_ID
    ) > 0L
  ) {
    stop(
      "Duplicated Author + ID keys were detected in ",
      data_name_i,
      "."
    )
  }
}

if (
  anyNA(population$Treatment) ||
  anyNA(population$Analysis_ID) ||
  any(population$Analysis_ID == "") ||
  anyDuplicated(population$Analysis_ID) > 0L
) {
  stop(
    "The Objective 2 treatment labels or Analysis_ID values are invalid."
  )
}

observed_treatments <- unique(
  as.character(
    population$Treatment
  )
)

if (!setequal(
  observed_treatments,
  required_treatments
)) {
  stop(
    "The Objective 2 population must contain exactly Control and Stress."
  )
}

author_levels <- sort(
  unique(
    population$Author
  )
)

if (
  length(author_levels) != expected_number_of_authors ||
  !identical(
    author_levels,
    sort(
      unique(
        objective1_control$Author
      )
    )
  )
) {
  stop(
    "The corrected Objective 1 and Objective 2 objects do not contain ",
    "the same four Author cohorts."
  )
}

group_count_table <- table(
  population$Treatment,
  useNA = "ifany"
)

group_count_values <- as.integer(
  group_count_table[
    required_treatments
  ]
)

names(group_count_values) <- required_treatments

if (
  anyNA(group_count_values) ||
  group_count_values[["Control"]] != expected_control_n ||
  group_count_values[["Stress"]] != expected_stress_n
) {
  stop("Expected 89 Control and 89 Stress fish.")
}

author_treatment_counts <- population %>%
  dplyr::count(
    Author,
    Treatment,
    name = "n",
    .drop = FALSE
  ) %>%
  dplyr::arrange(
    Author,
    Treatment
  )

if (
  nrow(author_treatment_counts) !=
    expected_number_of_authors *
      length(required_treatments) ||
  any(author_treatment_counts$n <= 0L) ||
  sum(author_treatment_counts$n) !=
    expected_control_n +
      expected_stress_n
) {
  stop(
    "The Author-by-Treatment count audit is invalid."
  )
}

if (
  nrow(objective1_control) != expected_control_n ||
  nrow(objective1_scores) != expected_control_n
) {
  stop(
    "The corrected Objective 1 objects must each contain 89 Control fish."
  )
}

if (
  !all(
    objective1_control$Treatment == "Control"
  )
) {
  stop(
    "The corrected Objective 1 Control object contains non-Control fish."
  )
}

if (
  !setequal(
    objective1_control$Traceability_ID,
    objective1_scores$Traceability_ID
  )
) {
  stop(
    "The Objective 1 Control and PCA-score Author + ID keys do not match."
  )
}

control_population <- population[
  population$Treatment == "Control",
  ,
  drop = FALSE
]

stress_population <- population[
  population$Treatment == "Stress",
  ,
  drop = FALSE
]

if (
  !setequal(
    control_population$Traceability_ID,
    objective1_control$Traceability_ID
  )
) {
  stop(
    "The 89 Control Author + ID keys in Objective 2 do not reproduce ",
    "the corrected Objective 1 reference population."
  )
}

objective1_control <- objective1_control[
  match(
    control_population$Traceability_ID,
    objective1_control$Traceability_ID
  ),
  ,
  drop = FALSE
]

objective1_scores <- objective1_scores[
  match(
    control_population$Traceability_ID,
    objective1_scores$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  anyNA(objective1_control$Traceability_ID) ||
  anyNA(objective1_scores$Traceability_ID) ||
  !identical(
    control_population$Traceability_ID,
    objective1_control$Traceability_ID
  ) ||
  !identical(
    control_population$Traceability_ID,
    objective1_scores$Traceability_ID
  )
) {
  stop(
    "The corrected Objective 1 objects could not be aligned by Author + ID."
  )
}

control_matrix <- as.matrix(
  control_population[
    ,
    robust_variables,
    drop = FALSE
  ]
)

stress_matrix <- as.matrix(
  stress_population[
    ,
    robust_variables,
    drop = FALSE
  ]
)

objective1_control_matrix <- as.matrix(
  objective1_control[
    ,
    robust_variables,
    drop = FALSE
  ]
)

objective1_score_matrix <- as.matrix(
  objective1_scores[
    ,
    pc_variables,
    drop = FALSE
  ]
)

storage.mode(control_matrix) <- "double"
storage.mode(stress_matrix) <- "double"
storage.mode(objective1_control_matrix) <- "double"
storage.mode(objective1_score_matrix) <- "double"

rownames(control_matrix) <- control_population$Traceability_ID
rownames(stress_matrix) <- stress_population$Traceability_ID
rownames(objective1_control_matrix) <-
  objective1_control$Traceability_ID
rownames(objective1_score_matrix) <-
  objective1_scores$Traceability_ID

if (
  any(!is.finite(control_matrix)) ||
  any(!is.finite(stress_matrix)) ||
  any(!is.finite(objective1_control_matrix)) ||
  any(!is.finite(objective1_score_matrix))
) {
  stop("Non-finite values were detected in the PCA inputs.")
}

control_data_comparison <- all.equal(
  unname(control_matrix),
  unname(objective1_control_matrix),
  tolerance = 1e-12,
  check.attributes = FALSE
)

if (!isTRUE(control_data_comparison)) {
  stop(
    "The Objective 2 Control values do not reproduce the corrected ",
    "Objective 1 robust data.\nComparison result:\n",
    paste(control_data_comparison, collapse = "\n")
  )
}

# Audit the completed robust normalisation separately within Author.
# The stored robust coordinates are not recalculated or altered here.

normalisation_audit <- dplyr::bind_rows(
  lapply(
    author_levels,
    function(author_i) {

      author_control_matrix <- as.matrix(
        control_population[
          control_population$Author == author_i,
          robust_variables,
          drop = FALSE
        ]
      )

      storage.mode(author_control_matrix) <- "double"

      if (
        nrow(author_control_matrix) == 0L ||
        any(!is.finite(author_control_matrix))
      ) {
        stop(
          "Invalid Control reference data were detected for Author ",
          author_i,
          "."
        )
      }

      tibble::tibble(
        Author = author_i,
        Robust_variable = robust_variables,
        Control_median = apply(
          author_control_matrix,
          2,
          stats::median
        ),
        Control_scaled_MAD = apply(
          author_control_matrix,
          2,
          stats::mad,
          constant = 1.4826
        )
      )
    }
  )
)

if (
  nrow(normalisation_audit) !=
    expected_number_of_authors *
      length(robust_variables) ||
  any(!is.finite(normalisation_audit$Control_median)) ||
  any(!is.finite(normalisation_audit$Control_scaled_MAD))
) {
  stop(
    "The Author-specific robust-normalisation audit is invalid."
  )
}

if (
  any(
    abs(
      normalisation_audit$Control_median
    ) > numeric_tolerance
  )
) {
  stop(
    "At least one Author-specific Control median is not zero ",
    "within tolerance."
  )
}

if (
  any(
    abs(
      normalisation_audit$Control_scaled_MAD -
        1
    ) > numeric_tolerance
  )
) {
  stop(
    "At least one Author-specific Control scaled MAD is not one ",
    "within tolerance."
  )
}

# ------------------------------------------------------------
# 6. Reconstruct the definitive Objective 1 projection
# ------------------------------------------------------------

# PCA scores are an affine linear transformation of the original
# variables. Reconstructing that map from the definitive Control
# scores avoids imposing a new centring or scaling convention.

control_design_matrix <- cbind(
  Intercept = 1,
  control_matrix
)

stress_design_matrix <- cbind(
  Intercept = 1,
  stress_matrix
)

if (qr(control_design_matrix)[["rank"]] != ncol(control_design_matrix)) {
  stop("The Control projection design matrix is not full rank.")
}

projection_coefficients <- qr.solve(
  control_design_matrix,
  objective1_score_matrix,
  tol = 1e-12
)

if (
  !is.matrix(projection_coefficients) ||
  !identical(
    dim(projection_coefficients),
    c(length(robust_variables) + 1L, length(pc_variables))
  ) ||
  any(!is.finite(projection_coefficients))
) {
  stop("The reconstructed PCA projection coefficients are invalid.")
}

rownames(projection_coefficients) <- c("Intercept", robust_variables)
colnames(projection_coefficients) <- pc_variables

control_scores_reconstructed <- control_design_matrix %*%
  projection_coefficients

stress_scores_projected <- stress_design_matrix %*%
  projection_coefficients

colnames(control_scores_reconstructed) <- pc_variables
colnames(stress_scores_projected) <- pc_variables

maximum_control_projection_error <- max(
  abs(control_scores_reconstructed - objective1_score_matrix)
)

if (
  !is.finite(maximum_control_projection_error) ||
  maximum_control_projection_error > projection_tolerance
) {
  stop(
    "The affine projection does not reproduce the definitive Objective 1 ",
    "Control PCA scores. Maximum error = ",
    format(maximum_control_projection_error, scientific = TRUE)
  )
}

# ------------------------------------------------------------
# 7. Retrieve definitive explained variance
# ------------------------------------------------------------

objective1_variance <- objective1_variance %>%
  dplyr::mutate(
    Component = as.character(Component),
    Percentage_of_variance = as.numeric(Percentage_of_variance)
  )

if (
  anyNA(objective1_variance[["Component"]]) ||
  any(objective1_variance[["Component"]] == "") ||
  anyNA(objective1_variance[["Percentage_of_variance"]]) ||
  any(!is.finite(objective1_variance[["Percentage_of_variance"]])) ||
  any(objective1_variance[["Percentage_of_variance"]] < 0) ||
  any(objective1_variance[["Percentage_of_variance"]] > 100) ||
  anyDuplicated(objective1_variance[["Component"]]) > 0L
) {
  stop("The definitive Objective 1 explained-variance table is invalid.")
}

pc1_row <- objective1_variance[
  objective1_variance[["Component"]] == "PC1",
  ,
  drop = FALSE
]

pc2_row <- objective1_variance[
  objective1_variance[["Component"]] == "PC2",
  ,
  drop = FALSE
]

if (nrow(pc1_row) != 1L || nrow(pc2_row) != 1L) {
  stop("PC1 or PC2 could not be identified uniquely in Objective 1.")
}

pc1_percent <- as.numeric(pc1_row[["Percentage_of_variance"]])
pc2_percent <- as.numeric(pc2_row[["Percentage_of_variance"]])
pc12_percent <- pc1_percent + pc2_percent

variance_table <- objective1_variance %>%
  dplyr::arrange(
    match(
      Component,
      paste0("PC", seq_len(nrow(objective1_variance)))
    )
  ) %>%
  dplyr::mutate(
    Cumulative_variance_percent = cumsum(Percentage_of_variance),
    Source = "Corrected Objective 1 PCA"
  )

# ------------------------------------------------------------
# 8. Assemble Control and Stress scores
# ------------------------------------------------------------

control_scores <- tibble::tibble(
  Analysis_ID = control_population$Analysis_ID,
  Traceability_ID = control_population$Traceability_ID,
  Author = control_population$Author,
  ID = control_population$ID,
  Treatment = factor(
    "Control",
    levels = required_treatments
  ),
  PC1 = as.numeric(
    control_scores_reconstructed[
      ,
      "PC1"
    ]
  ),
  PC2 = as.numeric(
    control_scores_reconstructed[
      ,
      "PC2"
    ]
  )
)

stress_scores <- tibble::tibble(
  Analysis_ID = stress_population$Analysis_ID,
  Traceability_ID = stress_population$Traceability_ID,
  Author = stress_population$Author,
  ID = stress_population$ID,
  Treatment = factor(
    "Stress",
    levels = required_treatments
  ),
  PC1 = as.numeric(
    stress_scores_projected[
      ,
      "PC1"
    ]
  ),
  PC2 = as.numeric(
    stress_scores_projected[
      ,
      "PC2"
    ]
  )
)

pca_scores <- dplyr::bind_rows(control_scores, stress_scores)

if (
  nrow(pca_scores) !=
    expected_control_n +
      expected_stress_n ||
  anyDuplicated(
    pca_scores$Analysis_ID
  ) > 0L ||
  anyDuplicated(
    pca_scores$Traceability_ID
  ) > 0L ||
  any(
    !is.finite(
      pca_scores$PC1
    )
  ) ||
  any(
    !is.finite(
      pca_scores$PC2
    )
  )
) {
  stop("The combined Control-Stress PCA-score table is invalid.")
}

group_counts <- pca_scores %>%
  dplyr::count(Treatment, name = "n") %>%
  dplyr::arrange(Treatment)

pca_centroids <- pca_scores %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Treatment)

if (
  nrow(pca_centroids) != 2L ||
  !identical(
    as.character(pca_centroids[["Treatment"]]),
    required_treatments
  )
) {
  stop("The PCA centroid table is invalid.")
}

centroid_difference <- c(
  PC1 = pca_centroids[["PC1"]][2] - pca_centroids[["PC1"]][1],
  PC2 = pca_centroids[["PC2"]][2] - pca_centroids[["PC2"]][1]
)

centroid_displacement_pc12 <- sqrt(sum(centroid_difference^2))

# ------------------------------------------------------------
# 9. Descriptive convex hulls
# ------------------------------------------------------------

calculate_polygon_area <- function(x, y) {
  if (
    length(x) != length(y) ||
    length(x) < 3L ||
    any(!is.finite(x)) ||
    any(!is.finite(y))
  ) {
    stop("Invalid coordinates were supplied to calculate hull area.")
  }

  next_index <- c(seq.int(2L, length(x)), 1L)

  area <- 0.5 * abs(
    sum(x * y[next_index] - y * x[next_index])
  )

  if (!is.finite(area) || area <= 0) {
    stop("A non-positive convex-hull area was calculated.")
  }

  as.numeric(area)
}

hull_list <- vector("list", length(required_treatments))
hull_summary_list <- vector("list", length(required_treatments))

for (group_index in seq_along(required_treatments)) {
  treatment_i <- required_treatments[group_index]

  group_scores_i <- pca_scores[
    as.character(pca_scores[["Treatment"]]) == treatment_i,
    ,
    drop = FALSE
  ]

  unique_coordinates_i <- unique(
    group_scores_i[, c("PC1", "PC2"), drop = FALSE]
  )

  if (nrow(unique_coordinates_i) < 3L) {
    stop(treatment_i, " has fewer than three unique PC1-PC2 coordinates.")
  }

  hull_indices_i <- grDevices::chull(
    group_scores_i[["PC1"]],
    group_scores_i[["PC2"]]
  )

  if (length(hull_indices_i) < 3L || anyDuplicated(hull_indices_i) > 0L) {
    stop(treatment_i, " produced an invalid convex hull.")
  }

  hull_i <- group_scores_i[
    hull_indices_i,
    c(
      "Analysis_ID",
      "Traceability_ID",
      "Author",
      "ID",
      "Treatment",
      "PC1",
      "PC2"
    ),
    drop = FALSE
  ]

  hull_i[["Hull_order"]] <- seq_len(nrow(hull_i))

  hull_area_i <- calculate_polygon_area(
    hull_i[["PC1"]],
    hull_i[["PC2"]]
  )

  hull_list[[group_index]] <- hull_i

  hull_summary_list[[group_index]] <- tibble::tibble(
    Treatment = factor(treatment_i, levels = required_treatments),
    Group_n = nrow(group_scores_i),
    Hull_vertices = nrow(hull_i),
    Hull_area_PC1_PC2 = hull_area_i,
    Interpretation = "Descriptive boundary only; not a confidence region"
  )
}

pca_hulls <- dplyr::bind_rows(hull_list) %>%
  dplyr::arrange(Treatment, Hull_order)

hull_summary <- dplyr::bind_rows(hull_summary_list) %>%
  dplyr::arrange(Treatment)

if (
  nrow(hull_summary) != 2L ||
  any(!is.finite(hull_summary[["Hull_area_PC1_PC2"]])) ||
  any(hull_summary[["Hull_area_PC1_PC2"]] <= 0)
) {
  stop("The convex-hull summary is invalid.")
}

# ------------------------------------------------------------
# 10. Projection-coefficient table
# ------------------------------------------------------------

coefficient_table <- as.data.frame(
  projection_coefficients,
  check.names = FALSE
) %>%
  tibble::rownames_to_column(var = "Term") %>%
  tibble::as_tibble()

projection_audit <- tibble::tibble(
  Control_n = expected_control_n,
  Stress_n = expected_stress_n,
  Number_of_input_variables = length(robust_variables),
  Projected_components = paste(pc_variables, collapse = ", "),
  Maximum_Control_score_reconstruction_error =
    maximum_control_projection_error,
  PC1_variance_percent = pc1_percent,
  PC2_variance_percent = pc2_percent,
  PC1_PC2_cumulative_percent = pc12_percent,
  PC1_PC2_centroid_displacement = centroid_displacement_pc12
)

# ------------------------------------------------------------
# 11. Figure
# ------------------------------------------------------------

pc1_label <- paste0(
  "PC1 (",
  format(round(pc1_percent, 1), nsmall = 1),
  "%)"
)

pc2_label <- paste0(
  "PC2 (",
  format(round(pc2_percent, 1), nsmall = 1),
  "%)"
)

p_hull <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = pca_hulls,
    mapping = ggplot2::aes(
      x = PC1,
      y = PC2,
      group = Treatment,
      fill = Treatment,
      colour = Treatment
    ),
    alpha = 0.16,
    linewidth = 0.75
  ) +
  ggplot2::geom_point(
    data = pca_scores,
    mapping = ggplot2::aes(
      x = PC1,
      y = PC2,
      colour = Treatment
    ),
    size = 2.0,
    alpha = 0.68
  ) +
  ggplot2::geom_point(
    data = pca_centroids,
    mapping = ggplot2::aes(
      x = PC1,
      y = PC2,
      colour = Treatment
    ),
    shape = 4,
    size = 5.0,
    stroke = 1.4
  ) +
  ggplot2::labs(
    x = pc1_label,
    y = pc2_label,
    colour = NULL,
    fill = NULL
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_classic(
    base_size = 12
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    legend.position = "top",
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

print(p_hull)

# ------------------------------------------------------------
# 12. Save outputs
# ------------------------------------------------------------


projection_model <- list(
  Objective = "Objective 2",
  Reference = "Corrected Objective 1 PC1-PC2 coordinate system",
  Traceability = expected_traceability,
  Normalisation = expected_normalisation,
  Input_variables = robust_variables,
  Output_components = pc_variables,
  Coefficients = projection_coefficients,
  Control_score_reconstruction_error =
    maximum_control_projection_error,
  Objective1_control_file = objective1_control_file,
  Objective1_scores_file = objective1_scores_file,
  Objective1_variance_file = objective1_variance_file
)

results_object <- list(
  Objective = "Objective 2",
  Comparison_design = "Independent Control and Stress populations",
  Population_file = population_file,
  Projection_model = projection_model,
  Explained_variance = variance_table,
  Author_Treatment_counts = author_treatment_counts,
  Traceability = expected_traceability,
  Normalisation = expected_normalisation,
  Normalisation_audit = normalisation_audit,
  Scores = pca_scores,
  Centroids = pca_centroids,
  Centroid_difference_Stress_minus_Control = centroid_difference,
  Centroid_displacement_PC1_PC2 = centroid_displacement_pc12,
  Convex_hulls = pca_hulls,
  Convex_hull_summary = hull_summary,
  Projection_audit = projection_audit,
  Plot = p_hull
)

saveRDS(projection_model, projection_model_output)
saveRDS(pca_scores, scores_output)
saveRDS(pca_hulls, hulls_output)
saveRDS(pca_centroids, centroids_output)
saveRDS(results_object, results_output)

utils::write.csv(
  variance_table,
  variance_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  coefficient_table,
  coefficients_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  pca_centroids,
  centroids_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  hull_summary,
  hull_summary_csv_output,
  row.names = FALSE,
  na = ""
)

ggplot2::ggsave(
  filename = figure_pdf_output,
  plot = p_hull,
  width = 7.5,
  height = 6.5,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_tiff_output,
  plot = p_hull,
  width = 7.5,
  height = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_png_output,
  plot = p_hull,
  width = 7.5,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 13. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  projection_model_output,
  scores_output,
  hulls_output,
  centroids_output,
  results_output,
  variance_csv_output,
  coefficients_csv_output,
  centroids_csv_output,
  hull_summary_csv_output,
  figure_pdf_output,
  figure_tiff_output,
  figure_png_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected output files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 14. Legacy outputs
# ------------------------------------------------------------

legacy_files <- c(
  file.path(processed_dir, "population_control_stress_z.rds"),
  file.path(objects_dir, "objective2_control_reference_pca.rds"),
  file.path(objects_dir, "objective2_control_stress_pca_scores.rds"),
  file.path(objects_dir, "objective2_control_stress_hulls.rds"),
  file.path(objects_dir, "objective2_control_stress_pca_centroids.rds")
)

existing_legacy_files <- legacy_files[file.exists(legacy_files)]

if (length(existing_legacy_files) > 0L) {
  warning(
    "Legacy PCA files from the former workflow were detected. ",
    "They were not used or overwritten:\n",
    paste(existing_legacy_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 15. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - DEFINITIVE CONTROL-REFERENCE PCA PROJECTION\n")
cat("============================================================\n")

cat(
  "\nThe PC1-PC2 coordinate system and explained variance were ",
  "inherited directly from the definitive Objective 1 outputs.\n",
  sep = ""
)

cat("\nIndependent populations:\n")
print(group_counts, n = Inf, width = Inf)

cat(
  "\nCorrected upstream data:\n",
  "Traceability: ",
  expected_traceability,
  "\nNormalisation: ",
  expected_normalisation,
  "\n",
  sep = ""
)

cat("\nPopulation sizes by Author and treatment:\n")
print(author_treatment_counts, n = Inf, width = Inf)

cat("\nAuthor-specific Control-reference robust-normalisation audit:\n")
print(normalisation_audit, n = Inf, width = Inf)

cat("\nCorrected Objective 1 explained variance:\n")
print(variance_table, n = Inf, width = Inf)

cat("\nProjection audit:\n")
print(projection_audit, n = Inf, width = Inf)

cat("\nPCA group centroids:\n")
print(pca_centroids, n = Inf, width = Inf)

cat("\nDescriptive convex-hull summary:\n")
print(hull_summary, n = Inf, width = Inf)

cat(
  "\nInterpretive constraint: convex hulls describe the outermost ",
  "observations in each independent population. They are sensitive ",
  "to peripheral observations and are not confidence regions.\n",
  sep = ""
)

cat(
  "\nThis population-level visualisation does not use Louvain ",
  "memberships because it represents the complete Control and Stress ",
  "populations.\n",
  sep = ""
)

cat("\nDefinitive corrected outputs saved as:\n")

for (output_file in expected_output_files) {
  cat(
    normalizePath(output_file, winslash = "/", mustWork = FALSE),
    "\n",
    sep = ""
  )
}

cat("\n============================================================\n")
