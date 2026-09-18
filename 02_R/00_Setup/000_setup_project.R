# ============================================================
# LAPCOM UCS Resilience
# Portable project initialisation and directory audit
# ============================================================

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 1. Locate the repository root
# ------------------------------------------------------------

find_repository_root <- function(start_path) {
  current_path <- normalizePath(
    start_path,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    root_markers_present <-
      file.exists(file.path(current_path, "README.md")) &&
      dir.exists(file.path(current_path, "02_R"))

    if (root_markers_present) {
      return(current_path)
    }

    parent_path <- dirname(current_path)

    if (identical(parent_path, current_path)) {
      return(NA_character_)
    }

    current_path <- parent_path
  }
}

explicit_root <- trimws(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = ""
  )
)

source_file <- tryCatch(
  normalizePath(
    sys.frame(1)$ofile,
    winslash = "/",
    mustWork = TRUE
  ),
  error = function(error_condition) {
    NA_character_
  }
)

candidate_start_paths <- unique(
  c(
    getwd(),
    if (!is.na(source_file)) dirname(source_file) else NULL
  )
)

if (nzchar(explicit_root)) {
  if (!dir.exists(explicit_root)) {
    stop(
      "LAPCOM_UCS_PROJECT_ROOT does not exist:\n",
      explicit_root
    )
  }

  base_dir <- normalizePath(
    explicit_root,
    winslash = "/",
    mustWork = TRUE
  )
} else {
  candidate_roots <- vapply(
    candidate_start_paths,
    find_repository_root,
    character(1)
  )

  candidate_roots <- unique(
    candidate_roots[
      !is.na(candidate_roots)
    ]
  )

  if (length(candidate_roots) == 0L) {
    stop(
      "The repository root could not be identified.\n",
      "Run this script from within the downloaded repository or set:\n",
      "Sys.setenv(LAPCOM_UCS_PROJECT_ROOT = ",
      "\"C:/path/to/zebrafish-ucs-exploratory-archetypes\")"
    )
  }

  base_dir <- candidate_roots[[1]]
}

setwd(base_dir)

# ------------------------------------------------------------
# 2. Define and create the required directory structure
# ------------------------------------------------------------

required_directories <- c(
  file.path("01_Data", "Raw"),
  file.path("01_Data", "Processed"),
  file.path("02_R", "00_Setup"),
  file.path("02_R", "01_Objective1_ControlArchitecture"),
  file.path("02_R", "02_Objective2_PopulationReorganization"),
  file.path("02_R", "03_Objective3_StressPhenotypes"),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience"
  ),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
    "SPSS_TwoStep",
    "Input"
  ),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
    "SPSS_TwoStep",
    "Output"
  ),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
    "SPSS_TwoStep",
    "Figures"
  ),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
    "SPSS_TwoStep",
    "Syntax"
  ),
  file.path(
    "02_R",
    "04_Objective4_OperationalDefinitionsOfBehaviouralResilience",
    "SPSS_TwoStep",
    "Exported_Classifications"
  ),
  file.path("03_Results", "Objects"),
  file.path("03_Results", "Tables"),
  file.path("03_Results", "Figures"),
  "07_Environment"
)

directories_created <- required_directories[
  !dir.exists(required_directories)
]

for (directory_path in required_directories) {
  dir.create(
    directory_path,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

directories_not_created <- required_directories[
  !dir.exists(required_directories)
]

if (length(directories_not_created) > 0L) {
  stop(
    "The following required directories could not be created:\n",
    paste(directories_not_created, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 3. Define reusable project paths
# ------------------------------------------------------------

project_paths <- list(
  root = base_dir,
  raw_data = file.path(base_dir, "01_Data", "Raw"),
  processed_data = file.path(base_dir, "01_Data", "Processed"),
  scripts = file.path(base_dir, "02_R"),
  result_objects = file.path(base_dir, "03_Results", "Objects"),
  result_tables = file.path(base_dir, "03_Results", "Tables"),
  result_figures = file.path(base_dir, "03_Results", "Figures"),
  environment = file.path(base_dir, "07_Environment")
)

# ------------------------------------------------------------
# 4. Verify the essential primary dataset
# ------------------------------------------------------------

raw_data_file <- file.path(
  project_paths$raw_data,
  "Resultados revisados AP.xlsx"
)

if (!file.exists(raw_data_file)) {
  stop(
    "The primary behavioural dataset was not found:\n",
    raw_data_file,
    "\n\nDownload the complete repository or place the unchanged ",
    "spreadsheet at the path shown above."
  )
}

raw_data_information <- file.info(raw_data_file)

if (
  is.na(raw_data_information$size) ||
  raw_data_information$size <= 0
) {
  stop(
    "The primary behavioural dataset exists but is empty or unreadable:\n",
    raw_data_file
  )
}

# ------------------------------------------------------------
# 5. Report the initialisation audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("LAPCOM UCS REPOSITORY INITIALISED\n")
cat("============================================================\n")
cat("Project root:\n", base_dir, "\n", sep = "")
cat(
  "Required directories verified: ",
  length(required_directories),
  "\n",
  sep = ""
)
cat(
  "Directories created during this run: ",
  length(directories_created),
  "\n",
  sep = ""
)

if (length(directories_created) > 0L) {
  cat(
    paste0("  - ", directories_created),
    sep = "\n"
  )
  cat("\n")
}

cat("Primary dataset verified:\n", raw_data_file, "\n", sep = "")
cat(
  "Primary dataset size: ",
  raw_data_information$size,
  " bytes\n",
  sep = ""
)
cat("\nNext script:\n")
cat("02_R/00_Setup/001_load_lapcom_data.R\n")
cat("============================================================\n")
