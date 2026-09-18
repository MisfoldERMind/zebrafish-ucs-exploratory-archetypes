# ============================================================
# 001_load_lapcom_data.R
# Load and prepare LAPCOM database
# ============================================================

library(readxl)
library(dplyr)

# ------------------------------------------------------------
# 1. Define the Objective 1-4 raw database explicitly
# ------------------------------------------------------------

excel_file <- file.path(
  "01_Data",
  "Raw",
  "Resultados revisados AP.xlsx"
)

if (!file.exists(excel_file)) {
  stop(
    "The LAPCOM raw database was not found:\n",
    excel_file
  )
}

# ------------------------------------------------------------
# 2. Load the historical LAPCOM database
# ------------------------------------------------------------

datos <- readxl::read_excel(
  path = excel_file,
  sheet = "Página1"
)

# ------------------------------------------------------------
# 3. Verify the source structure
# ------------------------------------------------------------

required_columns <- c(
  "Author",
  "ID",
  "Treatment",
  "Distance AP",
  "Crossing AP",
  "Top_entry AP",
  "Top_time AP"
)

missing_columns <- setdiff(
  required_columns,
  names(datos)
)

if (length(missing_columns) > 0L) {
  stop(
    "The following required columns are missing:\n",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Rename the four behavioural variables
# ------------------------------------------------------------

datos <- datos %>%
  dplyr::rename(
    distance = `Distance AP`,
    crossing = `Crossing AP`,
    top_entry = `Top_entry AP`,
    top_time = `Top_time AP`
  ) %>%
  dplyr::mutate(
    Treatment = trimws(as.character(Treatment))
  )

# ------------------------------------------------------------
# 5. Audit identifiers, treatments, and cohorts
# ------------------------------------------------------------

if (anyNA(datos$Author)) {
  stop("Missing values were detected in Author/cohort.")
}

if (anyNA(datos$ID)) {
  stop("Missing values were detected in ID.")
}

if (anyDuplicated(datos$ID) > 0L) {
  duplicated_ids <- unique(
    datos$ID[duplicated(datos$ID)]
  )

  stop(
    "Duplicated IDs were detected:\n",
    paste(duplicated_ids, collapse = ", ")
  )
}

expected_treatments <- c(
  "Control",
  "Stress"
)

observed_treatments <- sort(
  unique(datos$Treatment)
)

if (!setequal(observed_treatments, expected_treatments)) {
  stop(
    "Unexpected Treatment values were detected. Observed values:\n",
    paste(observed_treatments, collapse = ", ")
  )
}

observed_cohorts <- sort(
  unique(datos$Author)
)

if (length(observed_cohorts) != 4L) {
  stop(
    "Exactly four Author/cohort levels were expected. Observed: ",
    length(observed_cohorts)
  )
}

cohort_treatment_counts <- datos %>%
  dplyr::count(
    Author,
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(
    Author,
    Treatment
  )

expected_combinations <- length(observed_cohorts) *
  length(expected_treatments)

if (nrow(cohort_treatment_counts) != expected_combinations) {
  stop(
    "At least one cohort does not contain both Control and Stress fish."
  )
}

# ------------------------------------------------------------
# 6. Audit the behavioural variables
# ------------------------------------------------------------

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

non_numeric_variables <- behavioural_variables[
  !vapply(
    datos[behavioural_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following behavioural variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

behavioural_matrix <- as.matrix(
  datos[, behavioural_variables, drop = FALSE]
)

if (any(!is.finite(behavioural_matrix))) {
  stop(
    "Missing or non-finite values were detected in the behavioural variables."
  )
}

# ------------------------------------------------------------
# 7. Verify the expected database size
# ------------------------------------------------------------

expected_total_n <- 178L
expected_control_n <- 89L
expected_stress_n <- 89L

observed_control_n <- sum(
  datos$Treatment == "Control"
)

observed_stress_n <- sum(
  datos$Treatment == "Stress"
)

if (nrow(datos) != expected_total_n) {
  stop(
    "The database should contain ",
    expected_total_n,
    " individuals. Observed: ",
    nrow(datos)
  )
}

if (observed_control_n != expected_control_n) {
  stop(
    "The database should contain ",
    expected_control_n,
    " Control fish. Observed: ",
    observed_control_n
  )
}

if (observed_stress_n != expected_stress_n) {
  stop(
    "The database should contain ",
    expected_stress_n,
    " Stress fish. Observed: ",
    observed_stress_n
  )
}

# ------------------------------------------------------------
# 8. Create the Control dataset
# ------------------------------------------------------------

control <- datos %>%
  dplyr::filter(
    Treatment == "Control"
  )

# ------------------------------------------------------------
# 9. Save the processed datasets
# ------------------------------------------------------------

processed_dir <- file.path(
  "01_Data",
  "Processed"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  datos,
  file.path(
    processed_dir,
    "LAPCOM_raw.rds"
  )
)

saveRDS(
  control,
  file.path(
    processed_dir,
    "control.rds"
  )
)

# ------------------------------------------------------------
# 10. Final audit reported in the console
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("LAPCOM DATABASE LOADED\n")
cat("============================================================\n")

cat("\nRaw database:\n")
cat(excel_file, "\n")

cat("\nTotal individuals:", nrow(datos), "\n")
cat("Control individuals:", observed_control_n, "\n")
cat("Stress individuals:", observed_stress_n, "\n")
cat("Number of cohorts:", length(observed_cohorts), "\n")

cat("\nCohort x Treatment counts:\n")
print(
  cohort_treatment_counts,
  n = Inf,
  width = Inf
)

cat("\nNo normalisation was performed in this script.\n")
cat(
  "The Objective 5 database was not loaded: ",
  "NTT FRANCISCO 22.05.25 - ANÁLISES AMANDA.xlsx\n",
  sep = ""
)

cat("\nSaved objects:\n")
cat("01_Data/Processed/LAPCOM_raw.rds\n")
cat("01_Data/Processed/control.rds\n")
cat("============================================================\n")