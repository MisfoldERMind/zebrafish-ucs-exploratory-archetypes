# ============================================================
# 020_StressArchitecture_Setup_CORREGIDO.R
#
# Objective 3
# Internal behavioural architecture of the UCS-exposed population
#
# Scientific question:
#   What internal behavioural structure emerges within the
#   UCS-exposed population?
#
# Operational question:
#   What structure does the displaced UCS-exposed population adopt
#   internally within the Control-referenced behavioural space?
#
# Purpose:
#   Prepare and audit the definitive Objective 3 Stress dataset.
#   This script does NOT perform PCA, construct a kNN graph, run
#   Louvain community detection, or assign resilient-like /
#   susceptible-like labels.
#
# Definitive upstream dependency:
#   Corrected 011_Project_Control_Stress.R
#
# Mandatory inherited coordinate system:
#   For each Author x behavioural variable:
#
#     rz = (x - Median_control_within_Author) /
#          (1.4826 * MAD_raw_control_within_Author)
#
#   The same Author-specific Control parameters are applied unchanged
#   to Control and Stress fish from that Author.
#
# Traceability:
#   Author + ID
#
# Objective 3 analytical population:
#   Stress only (n = 89), while retaining Author for provenance.
#
# IMPORTANT:
#   - No global normalisation.
#   - No Stress-specific normalisation.
#   - No scale().
#   - No new SD scaling.
#   - No re-estimation of the robust coordinate system.
# ============================================================

script_version <- paste(
  "020_StressArchitecture_Setup_CORREGIDO",
  "| 2026-08-07",
  "| Author-specific robust-normalisation and Author+ID audit"
)

cat("\nRunning: ", script_version, "\n", sep = "")

# ------------------------------------------------------------
# 1. Project directory
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

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c("dplyr", "tibble")

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

# ------------------------------------------------------------
# 3. Definitive analytical configuration
# ------------------------------------------------------------

required_treatments <- c("Control", "Stress")

expected_control_n <- 89L
expected_stress_n <- 89L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"

expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

expected_reference_population <- "Control within each Author"

expected_normalisation_method <-
  "Author-specific control-referenced robust z-score"

expected_comparison_design <-
  "Independent Control and Stress populations"

mad_scale_constant <- 1.4826

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- paste0(
  behavioural_variables,
  "_rz"
)

legacy_variables <- c(
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z"
)

recalculation_tolerance <- 1e-10
control_anchor_tolerance <- 1e-8

# ------------------------------------------------------------
# 4. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(
  project_root,
  "01_Data",
  "Processed"
)

objects_dir <- file.path(
  project_root,
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  project_root,
  "03_Results",
  "Tables"
)

if (!dir.exists(processed_dir)) {
  stop("Processed-data directory not found:\n", processed_dir)
}

for (directory_i in c(objects_dir, tables_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
  }
}

population_input <- file.path(
  processed_dir,
  "population_control_stress_robust_z.rds"
)

stress_reference_input <- file.path(
  processed_dir,
  "stress_robust_z_control_reference.rds"
)

normalisation_parameters_input <- file.path(
  objects_dir,
  "objective2_control_reference_robust_normalisation_parameters.rds"
)

normalisation_audit_input <- file.path(
  objects_dir,
  "objective2_control_reference_robust_normalisation_audit.rds"
)

required_input_files <- c(
  population_input,
  stress_reference_input,
  normalisation_parameters_input,
  normalisation_audit_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive corrected Objective 2 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun the corrected 011_Project_Control_Stress.R first."
  )
}

stress_output <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

setup_output <- file.path(
  objects_dir,
  "objective3_stress_architecture_setup.rds"
)

normalisation_validation_output <- file.path(
  tables_dir,
  "Objective3_StressArchitecture_NormalisationValidation.csv"
)

stress_summary_output <- file.path(
  tables_dir,
  "Objective3_StressArchitecture_RobustSummary.csv"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_stress_architecture_setup_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load definitive corrected Objective 2 objects
# ------------------------------------------------------------

population <- readRDS(population_input)
stress_reference <- readRDS(stress_reference_input)
normalisation_object <- readRDS(normalisation_parameters_input)
objective2_audit <- readRDS(normalisation_audit_input)

if (!is.data.frame(population)) {
  stop(basename(population_input), " does not contain a data frame.")
}

if (!is.data.frame(stress_reference)) {
  stop(basename(stress_reference_input), " does not contain a data frame.")
}

if (!is.list(normalisation_object)) {
  stop(
    basename(normalisation_parameters_input),
    " does not contain the corrected Script 011 normalisation list."
  )
}

if (!is.list(objective2_audit)) {
  stop(
    basename(normalisation_audit_input),
    " does not contain the corrected Script 011 audit list."
  )
}

# ------------------------------------------------------------
# 6. Validate corrected Objective 2 metadata
# ------------------------------------------------------------

required_normalisation_fields <- c(
  "Reference_population",
  "Comparison_design",
  "Method",
  "Formula",
  "MAD_scale_constant",
  "Control_n",
  "Stress_n",
  "Authors",
  "Behavioural_variables",
  "Robust_variables",
  "Parameters"
)

missing_normalisation_fields <- setdiff(
  required_normalisation_fields,
  names(normalisation_object)
)

if (length(missing_normalisation_fields) > 0L) {
  stop(
    "The corrected Script 011 normalisation object is missing:\n",
    paste(missing_normalisation_fields, collapse = ", ")
  )
}

if (!identical(
  as.character(normalisation_object[["Reference_population"]]),
  expected_reference_population
)) {
  stop("The inherited reference population is not Control within each Author.")
}

if (!identical(
  as.character(normalisation_object[["Comparison_design"]]),
  expected_comparison_design
)) {
  stop("The inherited Objective 2 comparison design is unexpected.")
}

if (!identical(
  as.character(normalisation_object[["Method"]]),
  expected_normalisation_method
)) {
  stop(
    "The inherited normalisation method is not the corrected ",
    "Author-specific Control-referenced robust transformation."
  )
}

if (!isTRUE(all.equal(
  as.numeric(normalisation_object[["MAD_scale_constant"]]),
  mad_scale_constant,
  tolerance = 0
))) {
  stop("The inherited MAD scale constant is not 1.4826.")
}

if (
  as.integer(normalisation_object[["Control_n"]]) != expected_control_n ||
  as.integer(normalisation_object[["Stress_n"]]) != expected_stress_n
) {
  stop(
    "The inherited normalisation object does not contain ",
    "89 Control and 89 Stress fish."
  )
}

if (!identical(
  as.character(normalisation_object[["Behavioural_variables"]]),
  behavioural_variables
)) {
  stop(
    "The inherited behavioural variables do not match the definitive ",
    "four-variable architecture."
  )
}

if (!identical(
  as.character(normalisation_object[["Robust_variables"]]),
  robust_variables
)) {
  stop(
    "The inherited robust-variable names do not match the definitive ",
    "Objective 3 coordinates."
  )
}

# ------------------------------------------------------------
# 7. Validate corrected Objective 2 audit flags
# ------------------------------------------------------------

required_audit_fields <- c(
  "Group_counts",
  "Cohort_group_counts",
  "Normalisation_parameters",
  "Normalisation_audit",
  "Population_summary",
  "Raw_control_reference_match_by_Author_ID",
  "Objective1_normalised_control_match_by_Author_ID",
  "Objective1_parameter_match"
)

missing_audit_fields <- setdiff(
  required_audit_fields,
  names(objective2_audit)
)

if (length(missing_audit_fields) > 0L) {
  stop(
    "The corrected Script 011 audit object is missing:\n",
    paste(missing_audit_fields, collapse = ", ")
  )
}

if (
  !isTRUE(objective2_audit[["Raw_control_reference_match_by_Author_ID"]]) ||
  !isTRUE(objective2_audit[["Objective1_normalised_control_match_by_Author_ID"]]) ||
  !isTRUE(objective2_audit[["Objective1_parameter_match"]])
) {
  stop(
    "The corrected Objective 2 audit does not confirm all required ",
    "Objective 1 -> Objective 2 dependencies."
  )
}

# ------------------------------------------------------------
# 8. Validate population columns and Author + ID traceability
# ------------------------------------------------------------

required_columns <- c(
  "Author",
  "ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_population_columns <- setdiff(
  required_columns,
  names(population)
)

missing_stress_columns <- setdiff(
  required_columns,
  names(stress_reference)
)

if (length(missing_population_columns) > 0L) {
  stop(
    "The corrected combined population is missing:\n",
    paste(missing_population_columns, collapse = ", ")
  )
}

if (length(missing_stress_columns) > 0L) {
  stop(
    "The corrected standalone Stress object is missing:\n",
    paste(missing_stress_columns, collapse = ", ")
  )
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
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

stress_reference <- stress_reference %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

for (object_i in c("population", "stress_reference")) {
  data_i <- get(object_i)

  if (
    anyNA(
      data_i[
        c(
          "Author",
          "ID",
          "Treatment",
          "Traceability_ID"
        )
      ]
    ) ||
    any(data_i$Author == "") ||
    any(data_i$ID == "") ||
    anyDuplicated(data_i$Traceability_ID) > 0L
  ) {
    stop(
      object_i,
      " has invalid or duplicated Author + ID traceability."
    )
  }
}

if (dplyr::n_distinct(population$Author) != expected_number_of_authors) {
  stop(
    "The combined population does not contain the expected four Authors."
  )
}

population_authors <- sort(
  unique(population$Author)
)

stored_authors <- sort(
  as.character(
    normalisation_object[["Authors"]]
  )
)

if (!identical(
  population_authors,
  stored_authors
)) {
  stop(
    "The Authors in the corrected population do not match those stored ",
    "in the corrected Script 011 normalisation object."
  )
}

# ------------------------------------------------------------
# 9. Validate treatment groups and cohort counts
# ------------------------------------------------------------

if (anyNA(population$Treatment)) {
  stop("Missing or unexpected treatment labels were detected.")
}

treatment_values <- unique(
  as.character(population$Treatment)
)

if (!setequal(
  treatment_values,
  required_treatments
)) {
  stop(
    "The corrected population must contain only Control and Stress."
  )
}

group_counts <- population %>%
  dplyr::count(
    Treatment,
    name = "n",
    .drop = FALSE
  ) %>%
  dplyr::arrange(
    Treatment
  )

if (
  nrow(group_counts) != 2L ||
  !identical(
    as.character(group_counts$Treatment),
    required_treatments
  ) ||
  !identical(
    as.integer(group_counts$n),
    c(expected_control_n, expected_stress_n)
  )
) {
  stop("Expected exactly 89 Control and 89 Stress fish.")
}

cohort_group_counts <- population %>%
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

stored_cohort_group_counts <-
  objective2_audit[["Cohort_group_counts"]] %>%
  dplyr::mutate(
    Author = as.character(Author),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    )
  ) %>%
  dplyr::arrange(
    Author,
    Treatment
  )

cohort_count_comparison <- all.equal(
  as.data.frame(cohort_group_counts),
  as.data.frame(stored_cohort_group_counts),
  tolerance = 0,
  check.attributes = FALSE
)

if (!isTRUE(cohort_count_comparison)) {
  stop(
    "The Author x Treatment counts do not reproduce the corrected ",
    "Script 011 audit."
  )
}

# ------------------------------------------------------------
# 10. Audit raw and robust behavioural variables
# ------------------------------------------------------------

all_numeric_variables <- c(
  behavioural_variables,
  robust_variables
)

for (object_i in c("population", "stress_reference")) {
  data_i <- get(object_i)

  non_numeric_variables <- all_numeric_variables[
    !vapply(
      data_i[all_numeric_variables],
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric_variables) > 0L) {
    stop(
      object_i,
      " contains non-numeric analytical variables:\n",
      paste(non_numeric_variables, collapse = ", ")
    )
  }

  numeric_matrix <- as.matrix(
    data_i[all_numeric_variables]
  )

  storage.mode(numeric_matrix) <- "double"

  if (any(!is.finite(numeric_matrix))) {
    stop(
      object_i,
      " contains missing or non-finite analytical values."
    )
  }
}

# ------------------------------------------------------------
# 11. Validate the 16 Author x variable normalisation parameters
# ------------------------------------------------------------

normalisation_parameters <-
  normalisation_object[["Parameters"]]

if (!is.data.frame(normalisation_parameters)) {
  stop("normalisation_object$Parameters is not a data frame.")
}

required_parameter_columns <- c(
  "Author",
  "Variable",
  "n",
  "Median",
  "MAD_raw",
  "MAD_scaled"
)

missing_parameter_columns <- setdiff(
  required_parameter_columns,
  names(normalisation_parameters)
)

if (length(missing_parameter_columns) > 0L) {
  stop(
    "The corrected Author-specific parameter table is missing:\n",
    paste(missing_parameter_columns, collapse = ", ")
  )
}

normalisation_parameters <-
  normalisation_parameters %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    Variable = as.character(Variable)
  ) %>%
  dplyr::arrange(
    Author,
    match(
      Variable,
      behavioural_variables
    )
  )

expected_parameter_rows <-
  expected_number_of_authors *
  length(behavioural_variables)

if (
  nrow(normalisation_parameters) != expected_parameter_rows ||
  any(
    duplicated(
      normalisation_parameters[
        c("Author", "Variable")
      ]
    )
  ) ||
  !setequal(
    normalisation_parameters$Author,
    population_authors
  ) ||
  !setequal(
    normalisation_parameters$Variable,
    behavioural_variables
  )
) {
  stop(
    "The corrected normalisation table must contain exactly the ",
    "16 unique Author x variable parameter sets."
  )
}

parameter_numeric_columns <- c(
  "n",
  "Median",
  "MAD_raw",
  "MAD_scaled"
)

if (
  any(
    !vapply(
      normalisation_parameters[
        parameter_numeric_columns
      ],
      is.numeric,
      logical(1)
    )
  )
) {
  stop(
    "At least one corrected normalisation-parameter column is not numeric."
  )
}

parameter_matrix <- as.matrix(
  normalisation_parameters[
    parameter_numeric_columns
  ]
)

storage.mode(parameter_matrix) <- "double"

if (
  any(!is.finite(parameter_matrix)) ||
  any(normalisation_parameters$MAD_raw <= 0) ||
  any(normalisation_parameters$MAD_scaled <= 0)
) {
  stop(
    "At least one Author-specific normalisation parameter is invalid."
  )
}

if (
  any(
    abs(
      normalisation_parameters$MAD_scaled -
        mad_scale_constant *
        normalisation_parameters$MAD_raw
    ) > recalculation_tolerance
  )
) {
  stop(
    "MAD_scaled is not equal to 1.4826 x MAD_raw for at least one ",
    "Author x variable parameter set."
  )
}

stored_audit_parameters <-
  objective2_audit[["Normalisation_parameters"]] %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    Variable = as.character(Variable)
  ) %>%
  dplyr::arrange(
    Author,
    match(
      Variable,
      behavioural_variables
    )
  )

parameter_object_comparison <- all.equal(
  as.data.frame(normalisation_parameters),
  as.data.frame(stored_audit_parameters),
  tolerance = recalculation_tolerance,
  check.attributes = FALSE
)

if (!isTRUE(parameter_object_comparison)) {
  stop(
    "The normalisation parameters in the corrected Script 011 ",
    "normalisation and audit objects do not match."
  )
}

# ------------------------------------------------------------
# 12. Reconstruct and verify robust coordinates by Author
# ------------------------------------------------------------

validation_rows <- vector(
  mode = "list",
  length =
    expected_number_of_authors *
    length(required_treatments) *
    length(behavioural_variables)
)

validation_row_index <- 0L

for (author_i in population_authors) {
  for (variable_i in behavioural_variables) {
    robust_variable_i <- paste0(
      variable_i,
      "_rz"
    )

    parameter_row <- normalisation_parameters[
      normalisation_parameters$Author == author_i &
        normalisation_parameters$Variable == variable_i,
      ,
      drop = FALSE
    ]

    if (nrow(parameter_row) != 1L) {
      stop(
        "Exactly one parameter row is required for Author ",
        author_i,
        " and variable ",
        variable_i,
        "."
      )
    }

    author_rows <- population$Author == author_i

    expected_values <- (
      population[[variable_i]][author_rows] -
        parameter_row$Median[[1]]
    ) / parameter_row$MAD_scaled[[1]]

    observed_values <-
      population[[robust_variable_i]][author_rows]

    if (
      length(expected_values) != length(observed_values) ||
      any(!is.finite(expected_values)) ||
      any(!is.finite(observed_values))
    ) {
      stop(
        "Invalid reconstruction for Author ",
        author_i,
        ", variable ",
        variable_i,
        "."
      )
    }

    maximum_author_difference <- max(
      abs(
        observed_values -
          expected_values
      )
    )

    if (
      maximum_author_difference >
        recalculation_tolerance
    ) {
      stop(
        "The stored ",
        robust_variable_i,
        " values do not reproduce the corrected Author-specific ",
        "Control-referenced transformation for Author ",
        author_i,
        "."
      )
    }

    for (group_i in required_treatments) {
      group_rows <-
        population$Author == author_i &
        as.character(population$Treatment) == group_i

      group_values <-
        population[[robust_variable_i]][group_rows]

      if (length(group_values) == 0L) {
        stop(
          "No observations found for Author ",
          author_i,
          ", Treatment ",
          group_i,
          "."
        )
      }

      group_median <- stats::median(
        group_values
      )

      validation_row_index <-
        validation_row_index + 1L

      validation_rows[[validation_row_index]] <- tibble::tibble(
        Author = author_i,
        Group = group_i,
        Variable = variable_i,
        Robust_variable =
          robust_variable_i,
        n = length(group_values),
        Maximum_absolute_recalculation_difference =
          maximum_author_difference,
        Median_rz = group_median,
        MAD_scaled_rz = stats::mad(
          group_values,
          center = group_median,
          constant = mad_scale_constant
        )
      )
    }
  }
}

normalisation_validation <-
  dplyr::bind_rows(validation_rows) %>%
  dplyr::arrange(
    Author,
    factor(
      Group,
      levels = required_treatments
    ),
    match(
      Variable,
      behavioural_variables
    )
  )

expected_validation_rows <-
  expected_number_of_authors *
  length(required_treatments) *
  length(behavioural_variables)

if (
  nrow(normalisation_validation) !=
    expected_validation_rows
) {
  stop(
    "The Objective 3 normalisation audit must contain ",
    expected_validation_rows,
    " Author x Group x Variable rows."
  )
}

if (
  any(
    normalisation_validation[[
      "Maximum_absolute_recalculation_difference"
    ]] > recalculation_tolerance
  )
) {
  stop(
    "At least one robust coordinate fails Author-specific reconstruction."
  )
}

control_anchor_audit <-
  normalisation_validation %>%
  dplyr::filter(
    Group == "Control"
  )

if (
  any(
    abs(control_anchor_audit$Median_rz) >
      control_anchor_tolerance
  )
) {
  stop(
    "At least one Author-specific transformed Control median is not ",
    "zero within tolerance."
  )
}

if (
  any(
    abs(
      control_anchor_audit$MAD_scaled_rz -
        1
    ) > control_anchor_tolerance
  )
) {
  stop(
    "At least one Author-specific transformed Control scaled MAD is ",
    "not one within tolerance."
  )
}
