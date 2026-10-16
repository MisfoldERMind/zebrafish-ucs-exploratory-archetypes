# ============================================================
# 021_Table1_StressDescriptiveStatistics_CORREGIDO.R
#
# Objective 3
# Internal behavioural architecture of the UCS-exposed population
# Table 1: descriptive statistics of the Stress population
#
# Scientific question:
#   What internal behavioural structure emerges within the
#   UCS-exposed population?
#
# Purpose:
#   Describe the definitive four-dimensional Stress behavioural
#   space before de novo PCA / Louvain community detection.
#
# Required upstream script:
#   020_StressArchitecture_Setup_CORREGIDO_v3.R
#
# Definitive analytical scale:
#   Author-specific Control-referenced robust coordinates inherited
#   unchanged from Objectives 1 and 2:
#
#     rz = (x - Median_control_within_Author) /
#          (1.4826 * MAD_raw_control_within_Author)
#
# Traceability:
#   Author + ID
#
# IMPORTANT:
#   - No global normalisation.
#   - No Stress-specific normalisation.
#   - No scale().
#   - No community detection.
#   - No resilient-like / susceptible-like labels.
# ============================================================

script_version <- paste(
  "021_Table1_StressDescriptiveStatistics_CORREGIDO",
  "| 2026-08-07",
  "| Author-specific robust-coordinate audit"
)

cat(
  "\nRunning: ",
  script_version,
  "\n",
  sep = ""
)


# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

project_root <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

if (!dir.exists(project_root)) {
  stop(
    "Project directory not found:\n",
    project_root
  )
}

setwd(project_root)


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
# 3. Fixed analytical configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"

expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

expected_reference_population <-
  "Control within each Author"

mad_scale_constant <- 1.4826

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

behavioural_labels <- c(
  "Distance travelled",
  "Line crossings",
  "Upper-zone entries",
  "Upper-zone time"
)

variable_dictionary <- tibble::tibble(
  Variable = behavioural_labels,
  Raw_variable = behavioural_variables,
  Robust_variable = robust_variables
)

recalculation_tolerance <- 1e-10
summary_tolerance <- 1e-12


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

for (directory_i in c(
  processed_dir,
  objects_dir,
  tables_dir
)) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

stress_input <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

setup_input <- file.path(
  objects_dir,
  "objective3_stress_architecture_setup.rds"
)

required_input_files <- c(
  stress_input,
  setup_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive Objective 3 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun 020_StressArchitecture_Setup_CORREGIDO_v3.R first."
  )
}

xlsx_output <- file.path(
  tables_dir,
  "Objective3_Stress_DescriptiveStatistics.xlsx"
)

csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_DescriptiveStatistics.csv"
)

object_output <- file.path(
  objects_dir,
  "objective3_stress_descriptive_statistics.rds"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_stress_descriptive_statistics_sessionInfo.txt"
)


# ------------------------------------------------------------
# 5. Load definitive Objective 3 inputs
# ------------------------------------------------------------

stress <- readRDS(
  stress_input
)

setup_object <- readRDS(
  setup_input
)

if (!is.data.frame(stress)) {
  stop(
    basename(stress_input),
    " does not contain a data frame."
  )
}

if (!is.list(setup_object)) {
  stop(
    basename(setup_input),
    " does not contain the expected Objective 3 setup list."
  )
}


# ------------------------------------------------------------
# 6. Audit the analytical Stress population
# ------------------------------------------------------------

required_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_columns <- setdiff(
  required_columns,
  names(stress)
)

if (length(missing_columns) > 0L) {
  stop(
    "The definitive Objective 3 dataset is missing required columns:\n",
    paste(missing_columns, collapse = ", ")
  )
}

stress <- stress %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    ID = trimws(
      as.character(ID)
    ),
    Traceability_ID = trimws(
      as.character(Traceability_ID)
    ),
    Treatment = factor(
      as.character(Treatment),
      levels = "Stress"
    )
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

expected_traceability_id <- paste(
  stress$Author,
  stress$ID,
  sep = " || "
)

if (
  nrow(stress) != expected_stress_n ||
  anyNA(
    stress[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  any(stress$Author == "") ||
  any(stress$ID == "") ||
  any(stress$Traceability_ID == "") ||
  anyDuplicated(stress$Traceability_ID) > 0L ||
  !identical(
    stress$Traceability_ID,
    expected_traceability_id
  ) ||
  !all(
    as.character(stress$Treatment) == "Stress"
  )
) {
  stop(
    "The Objective 3 Stress dataset fails the Author + ID / ",
    "Stress-only population audit."
  )
}

population_authors <- sort(
  unique(
    stress$Author
  )
)

if (
  length(population_authors) != expected_number_of_authors
) {
  stop(
    "The Stress dataset does not retain the expected four Authors."
  )
}

analytical_variables <- c(
  behavioural_variables,
  robust_variables
)

non_numeric_variables <- analytical_variables[
  !vapply(
    stress[analytical_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following analytical variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

analytical_matrix <- as.matrix(
  stress[
    analytical_variables
  ]
)

storage.mode(
  analytical_matrix
) <- "double"

if (any(!is.finite(analytical_matrix))) {
  stop(
    "Missing or non-finite values were detected in the raw or ",
    "robust behavioural variables."
  )
}


# ------------------------------------------------------------
# 7. Audit dependence on corrected Script 020
# ------------------------------------------------------------

required_setup_elements <- c(
  "Scientific_question",
  "Operational_question",
  "Stress_n",
  "Analytical_population",
  "Authors",
  "Traceability",
  "Behavioural_variables",
  "Robust_variables",
  "Normalisation",
  "Normalisation_formula",
  "Reference_space",
  "Normalisation_parameters",
  "Author_group_variable_normalisation_validation",
  "Stress_robust_summary",
  "Downstream_constraint"
)

missing_setup_elements <- setdiff(
  required_setup_elements,
  names(setup_object)
)

if (length(missing_setup_elements) > 0L) {
  stop(
    "The corrected Objective 3 setup object is missing:\n",
    paste(missing_setup_elements, collapse = ", ")
  )
}

if (
  as.integer(
    setup_object[["Stress_n"]]
  ) != expected_stress_n
) {
  stop(
    "The setup object does not document Stress n = 89."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Analytical_population"]]
    ),
    "Stress only"
  )
) {
  stop(
    "The setup object does not define Stress as the analytical population."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Traceability"]]
    ),
    expected_traceability
  )
) {
  stop(
    "The setup object does not preserve Author + ID traceability."
  )
}

if (
  !identical(
    sort(
      as.character(
        setup_object[["Authors"]]
      )
    ),
    population_authors
  )
) {
  stop(
    "The Authors in the Stress dataset do not match the corrected setup."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Behavioural_variables"]]
    ),
    behavioural_variables
  ) ||
  !identical(
    as.character(
      setup_object[["Robust_variables"]]
    ),
    robust_variables
  )
) {
  stop(
    "The behavioural variables do not match the corrected Objective 3 setup."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Normalisation"]]
    ),
    expected_normalisation
  )
) {
  stop(
    "The corrected Objective 3 setup does not document the expected ",
    "Author-specific robust coordinate system."
  )
}


# ------------------------------------------------------------
# 8. Validate the 16 Author x variable reference parameters
# ------------------------------------------------------------

normalisation_parameters <-
  setup_object[["Normalisation_parameters"]]

if (!is.data.frame(normalisation_parameters)) {
  stop(
    "setup_object$Normalisation_parameters is not a data frame."
  )
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
    "The inherited Author-specific parameter table is missing:\n",
    paste(missing_parameter_columns, collapse = ", ")
  )
}

normalisation_parameters <-
  normalisation_parameters %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    Variable = as.character(
      Variable
    )
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
        c(
          "Author",
          "Variable"
        )
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
    "The corrected setup must contain exactly 16 unique ",
    "Author x variable parameter sets."
  )
}

if (
  any(
    !vapply(
      normalisation_parameters[
        c(
          "n",
          "Median",
          "MAD_raw",
          "MAD_scaled"
        )
      ],
      is.numeric,
      logical(1)
    )
  )
) {
  stop(
    "At least one Author-specific parameter column is not numeric."
  )
}

parameter_matrix <- as.matrix(
  normalisation_parameters[
    c(
      "n",
      "Median",
      "MAD_raw",
      "MAD_scaled"
    )
  ]
)

storage.mode(
  parameter_matrix
) <- "double"

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
    "MAD_scaled does not reproduce 1.4826 x MAD_raw for at least one ",
    "Author x variable parameter set."
  )
}


# ------------------------------------------------------------
# 9. Audit inherited Author x Group x Variable validation
# ------------------------------------------------------------

normalisation_validation <-
  setup_object[[
    "Author_group_variable_normalisation_validation"
  ]]

if (!is.data.frame(normalisation_validation)) {
  stop(
    "The inherited Author x Group x Variable validation is not a data frame."
  )
}

required_validation_columns <- c(
  "Author",
  "Group",
  "Variable",
  "Robust_variable",
  "n",
  "Maximum_absolute_recalculation_difference",
  "Median_rz",
  "MAD_scaled_rz"
)

missing_validation_columns <- setdiff(
  required_validation_columns,
  names(normalisation_validation)
)

if (length(missing_validation_columns) > 0L) {
  stop(
    "The inherited normalisation audit is missing:\n",
    paste(missing_validation_columns, collapse = ", ")
  )
}

normalisation_validation <- normalisation_validation %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    Group = as.character(
      Group
    ),
    Variable = as.character(
      Variable
    ),
    Robust_variable = as.character(
      Robust_variable
    )
  )

expected_validation_rows <-
  expected_number_of_authors *
  2L *
  length(behavioural_variables)

if (
  nrow(normalisation_validation) != expected_validation_rows ||
  any(
    duplicated(
      normalisation_validation[
        c(
          "Author",
          "Group",
          "Variable"
        )
      ]
    )
  ) ||
  !setequal(
    normalisation_validation$Author,
    population_authors
  ) ||
  !setequal(
    normalisation_validation$Group,
    c(
      "Control",
      "Stress"
    )
  ) ||
  !setequal(
    normalisation_validation$Variable,
    behavioural_variables
  )
) {
  stop(
    "The inherited corrected normalisation audit must contain exactly ",
    "32 Author x Group x Variable rows."
  )
}

if (
  !is.numeric(
    normalisation_validation[[
      "Maximum_absolute_recalculation_difference"
    ]]
  ) ||
  any(
    !is.finite(
      normalisation_validation[[
        "Maximum_absolute_recalculation_difference"
      ]]
    )
  ) ||
  any(
    normalisation_validation[[
      "Maximum_absolute_recalculation_difference"
    ]] > recalculation_tolerance
  )
) {
  stop(
    "The corrected setup does not confirm exact Author-specific ",
    "reproduction of the inherited robust coordinates."
  )
}


# ------------------------------------------------------------
# 10. Reconstruct Stress robust scores using each fish's Author
# ------------------------------------------------------------

normalisation_recalculation_rows <- vector(
  mode = "list",
  length =
    expected_number_of_authors *
    length(behavioural_variables)
)

audit_index <- 0L

for (author_i in population_authors) {
  author_rows <- stress$Author == author_i

  if (!any(author_rows)) {
    stop(
      "No Stress observations found for Author ",
      author_i,
      "."
    )
  }

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
        "Exactly one normalisation parameter row is required for Author ",
        author_i,
        " and variable ",
        variable_i,
        "."
      )
    }

    expected_values <- (
      stress[[variable_i]][author_rows] -
        parameter_row$Median[[1]]
    ) / parameter_row$MAD_scaled[[1]]

    observed_values <-
      stress[[robust_variable_i]][author_rows]

    maximum_difference <- max(
      abs(
        observed_values -
          expected_values
      )
    )

    audit_index <- audit_index + 1L

    normalisation_recalculation_rows[[
      audit_index
    ]] <- tibble::tibble(
      Author = author_i,
      Variable = variable_i,
      Robust_variable = robust_variable_i,
      n = sum(author_rows),
      Maximum_absolute_recalculation_difference =
        maximum_difference
    )
  }
}

normalisation_recalculation_audit <-
  dplyr::bind_rows(
    normalisation_recalculation_rows
  ) %>%
  dplyr::arrange(
    Author,
    match(
      Variable,
      behavioural_variables
    )
  )

if (
  nrow(normalisation_recalculation_audit) !=
    expected_number_of_authors *
    length(behavioural_variables) ||
  any(
    normalisation_recalculation_audit[[
      "Maximum_absolute_recalculation_difference"
    ]] > recalculation_tolerance
  )
) {
  stop(
    "At least one Stress robust coordinate does not reproduce the ",
    "Author-specific Control-referenced transformation."
  )
}


# ------------------------------------------------------------
# 11. Calculate full-precision descriptive statistics
# ------------------------------------------------------------

summary_rows <- vector(
  mode = "list",
  length = length(robust_variables)
)

for (variable_index in seq_along(
  robust_variables
)) {
  robust_variable_i <-
    robust_variables[[variable_index]]

  values_i <- stress[[
    robust_variable_i
  ]]

  q1_i <- as.numeric(
    stats::quantile(
      values_i,
      probs = 0.25,
      names = FALSE,
      type = 7
    )
  )

  q3_i <- as.numeric(
    stats::quantile(
      values_i,
      probs = 0.75,
      names = FALSE,
      type = 7
    )
  )

  summary_rows[[
    variable_index
  ]] <- tibble::tibble(
    Variable =
      behavioural_labels[[variable_index]],
    Raw_variable =
      behavioural_variables[[variable_index]],
    Robust_variable =
      robust_variable_i,
    n = length(values_i),
    Mean = mean(values_i),
    SD = stats::sd(values_i),
    Median = stats::median(values_i),
    Q1 = q1_i,
    Q3 = q3_i,
    IQR = stats::IQR(values_i),
    MAD_scaled = stats::mad(
      values_i,
      constant = mad_scale_constant
    ),
    Minimum = min(values_i),
    Maximum = max(values_i)
  )
}

stress_descriptive <- dplyr::bind_rows(
  summary_rows
)

if (
  nrow(stress_descriptive) !=
    length(robust_variables) ||
  !identical(
    as.character(
      stress_descriptive$Robust_variable
    ),
    robust_variables
  ) ||
  any(
    stress_descriptive$n !=
      expected_stress_n
  )
) {
  stop(
    "The descriptive table failed its dimension or ordering audit."
  )
}

numeric_summary_columns <- c(
  "Mean",
  "SD",
  "Median",
  "Q1",
  "Q3",
  "IQR",
  "MAD_scaled",
  "Minimum",
  "Maximum"
)

if (
  any(
    !vapply(
      stress_descriptive[
        numeric_summary_columns
      ],
      is.numeric,
      logical(1)
    )
  )
) {
  stop(
    "At least one descriptive statistic column is not numeric."
  )
}

summary_numeric_matrix <- as.matrix(
  stress_descriptive[
    numeric_summary_columns
  ]
)

storage.mode(
  summary_numeric_matrix
) <- "double"

if (
  any(
    !is.finite(
      summary_numeric_matrix
    )
  )
) {
  stop(
    "The descriptive table contains non-finite statistics."
  )
}


# ------------------------------------------------------------
# 12. Verify exact agreement with corrected Script 020 summary
# ------------------------------------------------------------

setup_summary <-
  setup_object[[
    "Stress_robust_summary"
  ]]

if (!is.data.frame(setup_summary)) {
  stop(
    "setup_object$Stress_robust_summary is not a data frame."
  )
}

required_setup_summary_columns <- c(
  "Variable",
  "n",
  "Mean",
  "SD",
  "Median",
  "Q1",
  "Q3",
  "IQR",
  "MAD_scaled",
  "Minimum",
  "Maximum"
)

missing_setup_summary_columns <- setdiff(
  required_setup_summary_columns,
  names(setup_summary)
)

if (
  length(
    missing_setup_summary_columns
  ) > 0L
) {
  stop(
    "The corrected setup summary is missing:\n",
    paste(
      missing_setup_summary_columns,
      collapse = ", "
    )
  )
}

setup_summary <- setup_summary[
  match(
    robust_variables,
    as.character(
      setup_summary$Variable
    )
  ),
  required_setup_summary_columns,
  drop = FALSE
]

if (
  nrow(setup_summary) !=
    length(robust_variables) ||
  anyNA(
    setup_summary$Variable
  ) ||
  !identical(
    as.character(
      setup_summary$Variable
    ),
    robust_variables
  )
) {
  stop(
    "The corrected setup summary does not contain one correctly ",
    "ordered row per robust variable."
  )
}

current_summary_for_comparison <-
  stress_descriptive %>%
  dplyr::transmute(
    Variable = Robust_variable,
    n = n,
    Mean = Mean,
    SD = SD,
    Median = Median,
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR,
    MAD_scaled = MAD_scaled,
    Minimum = Minimum,
    Maximum = Maximum
  )

if (
  !identical(
    as.integer(
      current_summary_for_comparison$n
    ),
    as.integer(
      setup_summary$n
    )
  )
) {
  stop(
    "The descriptive sample sizes do not match corrected Script 020."
  )
}

summary_comparison_columns <- setdiff(
  required_setup_summary_columns,
  c(
    "Variable",
    "n"
  )
)

summary_maximum_differences <- vapply(
  summary_comparison_columns,
  function(column_i) {
    max(
      abs(
        current_summary_for_comparison[[
          column_i
        ]] -
          setup_summary[[
            column_i
          ]]
      )
    )
  },
  numeric(1)
)

if (
  any(
    !is.finite(
      summary_maximum_differences
    )
  ) ||
  any(
    summary_maximum_differences >
      summary_tolerance
  )
) {
  stop(
    "The descriptive statistics do not reproduce the definitive ",
    "corrected Script 020 summary."
  )
}


# ------------------------------------------------------------
# 13. Prepare Excel display tables
# ------------------------------------------------------------

excel_table <- stress_descriptive %>%
  dplyr::select(
    Variable,
    Robust_variable,
    n,
    Mean,
    SD,
    Median,
    Q1,
    Q3,
    IQR,
    MAD_scaled,
    Minimum,
    Maximum
  ) %>%
  dplyr::rename(
    `Analytical variable` =
      Robust_variable,
    `Scaled MAD` =
      MAD_scaled
  )

author_recalculation_table <-
  normalisation_recalculation_audit


# ------------------------------------------------------------
# 14. Save tables and reproducibility objects
# ------------------------------------------------------------

utils::write.csv(
  stress_descriptive,
  csv_output,
  row.names = FALSE,
  na = ""
)

workbook <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  workbook,
  "Stress descriptive"
)

openxlsx::writeData(
  workbook,
  sheet = "Stress descriptive",
  x = excel_table
)

openxlsx::addWorksheet(
  workbook,
  "Author recalculation audit"
)

openxlsx::writeData(
  workbook,
  sheet = "Author recalculation audit",
  x = author_recalculation_table
)

openxlsx::saveWorkbook(
  workbook,
  file = xlsx_output,
  overwrite = TRUE
)

descriptive_object <- list(
  Objective = "Objective 3",
  Table =
    "Stress descriptive statistics before de novo community detection",
  Scientific_question =
    setup_object[["Scientific_question"]],
  Operational_question =
    setup_object[["Operational_question"]],
  Analytical_population =
    "Stress only",
  Stress_n =
    nrow(stress),
  Authors =
    population_authors,
  Traceability =
    expected_traceability,
  Reference_space =
    setup_object[["Reference_space"]],
  Normalisation =
    setup_object[["Normalisation"]],
  Normalisation_formula =
    setup_object[["Normalisation_formula"]],
  Behavioural_variables =
    behavioural_variables,
  Robust_variables =
    robust_variables,
  Variable_dictionary =
    variable_dictionary,
  Full_precision_table =
    stress_descriptive,
  Author_specific_normalisation_recalculation_audit =
    normalisation_recalculation_audit,
  Setup_summary_maximum_absolute_differences =
    summary_maximum_differences,
  Source_stress_file =
    stress_input,
  Source_setup_file =
    setup_input,
  Excel_file =
    xlsx_output,
  CSV_file =
    csv_output
)

saveRDS(
  descriptive_object,
  object_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)


# ------------------------------------------------------------
# 15. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  xlsx_output,
  csv_output,
  object_output,
  session_info_output
)

missing_output_files <-
  expected_output_files[
    !file.exists(
      expected_output_files
    )
  ]

if (
  length(
    missing_output_files
  ) > 0L
) {
  stop(
    "The following expected outputs were not created:\n",
    paste(
      missing_output_files,
      collapse = "\n"
    )
  )
}


# ------------------------------------------------------------
# 16. Final console audit
# ------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "OBJECTIVE 3 - CORRECTED STRESS DESCRIPTIVE STATISTICS\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nAnalytical population: Stress only (n = ",
  nrow(stress),
  ")\n",
  sep = ""
)

cat(
  "Traceability: ",
  expected_traceability,
  "\n",
  sep = ""
)

cat(
  "Authors/cohorts retained: ",
  paste(
    population_authors,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Analytical scale: Author-specific Control-referenced robust ",
  "coordinates inherited unchanged from corrected Objectives 1 and 2.\n",
  sep = ""
)

cat(
  "Normalisation: ",
  expected_normalisation,
  "\n",
  sep = ""
)

cat(
  "\nAuthor-specific robust-score recalculation audit:\n"
)

print(
  normalisation_recalculation_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nMaximum robust-score recalculation difference: ",
  format(
    max(
      normalisation_recalculation_audit[[
        "Maximum_absolute_recalculation_difference"
      ]]
    ),
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Maximum difference from the definitive corrected Script 020 summary: ",
  format(
    max(
      summary_maximum_differences
    ),
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nFull-precision descriptive statistics:\n"
)

print(
  stress_descriptive,
  n = Inf,
  width = Inf
)

cat(
  "\nInterpretive constraint: these statistics describe the ",
  "pre-clustering Stress space only. They do not identify, define, ",
  "or validate resilient-like or susceptible-like phenotypes.\n",
  sep = ""
)

cat(
  "\nDownstream constraint: PCA and Louvain must use the same four ",
  "*_rz coordinates without additional scaling or Stress-specific ",
  "normalisation.\n",
  sep = ""
)

cat(
  "\nOutputs saved as:\n"
)

for (
  output_file in expected_output_files
) {
  cat(
    normalizePath(
      output_file,
      winslash = "/",
      mustWork = FALSE
    ),
    "\n",
    sep = ""
  )
}

cat(
  "\n============================================================\n"
)
