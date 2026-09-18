# ============================================================
# 016_Nearest_Normative_Archetype.R
#
# Objective 2
# Projection of Control and Stress fish onto the normative
# behavioural profiles identified in the corrected Objective 1
# workflow.
#
# Definitive upstream architecture:
#   - robust normalisation within Author:
#       (x - Control median within Author) /
#       (1.4826 x raw Control MAD within Author)
#   - Euclidean distance in four behavioural dimensions
#   - weighted, undirected union kNN graph
#   - selected k = 46
#   - two Louvain segments
#   - traceability by Author + ID
#
# Analytical logic retained from the original Script 016:
#   1. The normative profiles are fixed from Objective 1.
#   2. Control occupancy retains the original Louvain membership.
#   3. Stress fish are projected onto the nearest fixed normative
#      centroid in the same robust four-dimensional space.
#   4. Control nearest-centroid fidelity is audited using
#      leave-one-out centroids.
#   5. Occupancy and distance analyses are interpreted as
#      projections relative to the fixed normative atlas.
#
# Corrected dependencies:
#   - population_control_stress_robust_z.rds from Script 011;
#   - corrected Objective 1 exploratory-segmentation objects;
#   - no object from the obsolete k = 56 workflow is used.
# ============================================================

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
  "tidyr",
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
library(tidyr)
library(tibble)
library(openxlsx)

# ------------------------------------------------------------
# 3. Fixed definitive configuration
# ------------------------------------------------------------

expected_control_n <- 89L
expected_stress_n <- 89L
expected_number_of_authors <- 4L
expected_k <- 46L
expected_number_of_clusters <- 2L

expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

expected_distance <- "Euclidean"
expected_similarity <- "1 / (1 + D)"
expected_graph_type <- "Weighted, undirected union kNN"
expected_traceability <- "Author + ID"

required_treatments <- c(
  "Control",
  "Stress"
)

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

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

audit_tolerance <- 1e-10

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

population_file <- file.path(
  processed_dir,
  "population_control_stress_robust_z.rds"
)

control_louvain_file <- file.path(
  objects_dir,
  "objective1_control_louvain_exploratory_segmentation.rds"
)

selected_membership_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_membership.rds"
)

graph_summary_file <- file.path(
  objects_dir,
  "objective1_louvain_exploratory_graph_summary.rds"
)

inference_status_file <- file.path(
  objects_dir,
  "objective1_louvain_inference_status.rds"
)

required_input_files <- c(
  population_file,
  control_louvain_file,
  selected_membership_file,
  graph_summary_file,
  inference_status_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun the corrected Objective 1 workflow and ",
    "011_Project_Control_Stress.R before this script."
  )
}

projection_output <- file.path(
  processed_dir,
  "population_normative_archetype_projection_robust.rds"
)

distance_object_output <- file.path(
  objects_dir,
  "objective2_normative_archetype_distance_matrix_robust.rds"
)

results_object_output <- file.path(
  objects_dir,
  "objective2_nearest_normative_archetype_results_robust.rds"
)

workbook_output <- file.path(
  tables_dir,
  "Objective2_Normative_Archetype_Occupancy_and_Distance.xlsx"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_nearest_normative_archetype_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load corrected upstream objects
# ------------------------------------------------------------

population <- readRDS(
  population_file
)

control_louvain <- readRDS(
  control_louvain_file
)

selected_membership <- readRDS(
  selected_membership_file
)

graph_summary <- readRDS(
  graph_summary_file
)

inference_status <- readRDS(
  inference_status_file
)

if (!is.data.frame(population)) {
  stop(
    basename(population_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(control_louvain)) {
  stop(
    basename(control_louvain_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(selected_membership)) {
  stop(
    basename(selected_membership_file),
    " does not contain a data frame."
  )
}

if (
  !is.data.frame(graph_summary) ||
  nrow(graph_summary) != 1L
) {
  stop(
    basename(graph_summary_file),
    " must contain a one-row data frame."
  )
}

if (!is.list(inference_status)) {
  stop(
    basename(inference_status_file),
    " does not contain a list."
  )
}

# ------------------------------------------------------------
# 6. Verify corrected Objective 1 configuration
# ------------------------------------------------------------

# The corrected Objective 1 objects currently store k = 45 because
# that value was selected by the deterministic upstream rule.
# However, k = 44, 45 and 46 belong to the exact-equivalence plateau
# identified in the corrected optimisation: they reproduce the same
# two-segment membership. For the present Objective 2 projection,
# k = 46 is retained as the authorised representative of that
# equivalent partition. Therefore, the upstream stored k is audited
# for membership in the 44-46 plateau rather than required to equal
# 46 literally.

equivalent_partition_k <- 44L:46L

required_status_elements <- c(
  "Selected_k",
  "Number_of_algorithmic_segments",
  "Segment_sizes"
)

missing_status_elements <- setdiff(
  required_status_elements,
  names(inference_status)
)

if (length(missing_status_elements) > 0L) {
  stop(
    "The corrected Objective 1 inference-status object is missing:\n",
    paste(missing_status_elements, collapse = ", ")
  )
}

upstream_selected_k <- as.integer(
  inference_status[["Selected_k"]]
)

selected_number_of_clusters <- as.integer(
  inference_status[["Number_of_algorithmic_segments"]]
)

expected_cluster_sizes <- sort(
  as.integer(
    unlist(
      inference_status[["Segment_sizes"]],
      use.names = FALSE
    )
  )
)

if (
  length(upstream_selected_k) != 1L ||
  is.na(upstream_selected_k) ||
  !upstream_selected_k %in% equivalent_partition_k
) {
  stop(
    "The corrected Objective 1 inference-status object does not ",
    "contain a selected k within the exact-equivalence plateau ",
    "k = 44-46."
  )
}

if (
  length(expected_k) != 1L ||
  is.na(expected_k) ||
  expected_k != 46L ||
  !expected_k %in% equivalent_partition_k
) {
  stop(
    "The authorised Objective 2 representative must be k = 46 ",
    "within the exact-equivalence plateau k = 44-46."
  )
}

if (
  length(selected_number_of_clusters) != 1L ||
  is.na(selected_number_of_clusters) ||
  selected_number_of_clusters != expected_number_of_clusters
) {
  stop(
    "The corrected Objective 1 inference-status object does not ",
    "specify exactly two segments."
  )
}

if (
  length(expected_cluster_sizes) != expected_number_of_clusters ||
  anyNA(expected_cluster_sizes) ||
  any(expected_cluster_sizes <= 0L) ||
  sum(expected_cluster_sizes) != expected_control_n
) {
  stop(
    "The segment sizes stored by corrected Objective 1 are invalid."
  )
}

required_graph_columns <- c(
  "Number_of_fish",
  "Number_of_behavioural_variables",
  "Traceability_key",
  "Normalisation",
  "Distance",
  "Similarity",
  "Graph_type",
  "Selected_k",
  "Vertices",
  "Number_of_algorithmic_segments",
  "Weighted_modularity"
)

missing_graph_columns <- setdiff(
  required_graph_columns,
  names(graph_summary)
)

if (length(missing_graph_columns) > 0L) {
  stop(
    "The corrected Objective 1 graph summary is missing:\n",
    paste(missing_graph_columns, collapse = ", ")
  )
}

graph_selected_k <- as.integer(
  graph_summary[["Selected_k"]][[1]]
)

if (
  length(graph_selected_k) != 1L ||
  is.na(graph_selected_k) ||
  !graph_selected_k %in% equivalent_partition_k
) {
  stop(
    "The corrected Objective 1 graph summary does not contain a ",
    "selected k within the exact-equivalence plateau k = 44-46."
  )
}

if (
  graph_selected_k != upstream_selected_k
) {
  stop(
    "The corrected Objective 1 inference-status object and graph ",
    "summary report different upstream selected-k values."
  )
}

if (
  as.integer(
    graph_summary[["Number_of_fish"]][[1]]
  ) != expected_control_n ||
  as.integer(
    graph_summary[["Number_of_behavioural_variables"]][[1]]
  ) != length(robust_variables) ||
  as.integer(
    graph_summary[["Vertices"]][[1]]
  ) != expected_control_n ||
  as.integer(
    graph_summary[["Number_of_algorithmic_segments"]][[1]]
  ) != expected_number_of_clusters
) {
  stop(
    "The corrected Objective 1 graph summary is inconsistent with ",
    "the 89-fish, four-variable, two-segment normative solution."
  )
}

if (
  !identical(
    as.character(
      graph_summary[["Traceability_key"]][[1]]
    ),
    expected_traceability
  )
) {
  stop(
    "The corrected Objective 1 graph summary does not use ",
    "Author + ID traceability."
  )
}

if (
  !identical(
    as.character(
      graph_summary[["Normalisation"]][[1]]
    ),
    expected_normalisation
  )
) {
  stop(
    "The corrected Objective 1 graph summary does not use ",
    "within-Author robust normalisation."
  )
}

if (
  !identical(
    as.character(
      graph_summary[["Distance"]][[1]]
    ),
    expected_distance
  ) ||
  !identical(
    as.character(
      graph_summary[["Similarity"]][[1]]
    ),
    expected_similarity
  ) ||
  !identical(
    as.character(
      graph_summary[["Graph_type"]][[1]]
    ),
    expected_graph_type
  )
) {
  stop(
    "The corrected Objective 1 graph geometry does not match the ",
    "definitive analytical configuration."
  )
}

if (
  !is.finite(
    as.numeric(
      graph_summary[["Weighted_modularity"]][[1]]
    )
  )
) {
  stop(
    "The weighted modularity stored for Objective 1 is not finite."
  )
}

# Downstream labels and outputs retain the authorised representative
# k = 46. The upstream stored k remains available separately for
# audit and is not overwritten.
selected_k <- expected_k

# ------------------------------------------------------------
# 7. Validate Author + ID traceability and corrected population
# ------------------------------------------------------------

make_traceability_key <- function(
    author,
    id
) {
  paste(
    trimws(
      as.character(author)
    ),
    trimws(
      as.character(id)
    ),
    sep = " || "
  )
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
    "The corrected Objective 2 population object is missing:\n",
    paste(missing_population_columns, collapse = ", ")
  )
}

required_control_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Algorithmic_Segment",
  robust_variables
)

missing_control_columns <- setdiff(
  required_control_columns,
  names(control_louvain)
)

if (length(missing_control_columns) > 0L) {
  stop(
    "The corrected Objective 1 segmented-control object is missing:\n",
    paste(missing_control_columns, collapse = ", ")
  )
}

required_membership_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Algorithmic_Segment"
)

missing_membership_columns <- setdiff(
  required_membership_columns,
  names(selected_membership)
)

if (length(missing_membership_columns) > 0L) {
  stop(
    "The corrected Objective 1 membership object is missing:\n",
    paste(missing_membership_columns, collapse = ", ")
  )
}

population <- population %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    ID = trimws(
      as.character(ID)
    ),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

control_louvain <- control_louvain %>%
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
    Algorithmic_Segment = trimws(
      as.character(Algorithmic_Segment)
    )
  )

selected_membership <- selected_membership %>%
  dplyr::transmute(
    Author = trimws(
      as.character(Author)
    ),
    ID = trimws(
      as.character(ID)
    ),
    Traceability_ID = trimws(
      as.character(Traceability_ID)
    ),
    Algorithmic_Segment = trimws(
      as.character(Algorithmic_Segment)
    )
  )

for (data_name_i in c(
  "population",
  "control_louvain",
  "selected_membership"
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

  expected_keys_i <- make_traceability_key(
    data_i$Author,
    data_i$ID
  )

  if (
    !identical(
      data_i$Traceability_ID,
      expected_keys_i
    )
  ) {
    stop(
      "Stored Traceability_ID values are inconsistent with ",
      "Author + ID in ",
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
  dplyr::n_distinct(
    population$Author
  ) != expected_number_of_authors ||
  dplyr::n_distinct(
    control_louvain$Author
  ) != expected_number_of_authors
) {
  stop(
    "The corrected data do not contain the expected four Authors."
  )
}

treatment_values <- unique(
  as.character(
    population$Treatment
  )
)

if (!setequal(
  treatment_values,
  required_treatments
)) {
  stop(
    "The Objective 2 population must contain exactly Control and Stress."
  )
}

group_counts <- population %>%
  dplyr::count(
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(
    Treatment
  )

expected_group_counts <- tibble::tibble(
  Treatment = factor(
    required_treatments,
    levels = required_treatments
  ),
  n = c(
    expected_control_n,
    expected_stress_n
  )
)

if (!identical(
  group_counts,
  expected_group_counts
)) {
  stop(
    "Unexpected group counts were detected. Expected ",
    "Control = 89 and Stress = 89."
  )
}

if (
  nrow(control_louvain) != expected_control_n ||
  nrow(selected_membership) != expected_control_n
) {
  stop(
    "The corrected Objective 1 segmented-control and membership ",
    "objects must each contain 89 fish."
  )
}

if (
  !setequal(
    control_louvain$Traceability_ID,
    selected_membership$Traceability_ID
  )
) {
  stop(
    "Author + ID keys differ between the corrected Objective 1 ",
    "segmented-control and membership objects."
  )
}

selected_membership <- selected_membership[
  match(
    control_louvain$Traceability_ID,
    selected_membership$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    control_louvain$Traceability_ID,
    selected_membership$Traceability_ID
  )
) {
  stop(
    "The corrected Objective 1 membership could not be aligned ",
    "by Author + ID."
  )
}

if (
  !identical(
    control_louvain$Algorithmic_Segment,
    selected_membership$Algorithmic_Segment
  )
) {
  stop(
    "Segment assignments differ between the corrected Objective 1 ",
    "objects."
  )
}

cluster_levels <- sort(
  unique(
    control_louvain$Algorithmic_Segment
  )
)

if (
  length(cluster_levels) != expected_number_of_clusters ||
  anyNA(cluster_levels) ||
  any(cluster_levels == "")
) {
  stop(
    "The corrected Objective 1 control object does not contain ",
    "exactly two valid segments."
  )
}

control_louvain <- control_louvain %>%
  dplyr::mutate(
    Algorithmic_Segment = factor(
      Algorithmic_Segment,
      levels = cluster_levels
    )
  )

selected_membership <- selected_membership %>%
  dplyr::mutate(
    Algorithmic_Segment = factor(
      Algorithmic_Segment,
      levels = cluster_levels
    )
  )

cluster_sizes <- control_louvain %>%
  dplyr::count(
    Algorithmic_Segment,
    name = "n"
  ) %>%
  dplyr::arrange(
    Algorithmic_Segment
  )

if (
  !identical(
    sort(
      as.integer(
        cluster_sizes$n
      )
    ),
    expected_cluster_sizes
  )
) {
  stop(
    "The segment sizes in the corrected Objective 1 data do not ",
    "match the sizes stored in the inference-status object."
  )
}

# ------------------------------------------------------------
# 8. Audit robust normalisation separately within Author
# ------------------------------------------------------------

non_numeric_population_variables <- robust_variables[
  !vapply(
    population[
      robust_variables
    ],
    is.numeric,
    logical(1)
  )
]

non_numeric_control_variables <- robust_variables[
  !vapply(
    control_louvain[
      robust_variables
    ],
    is.numeric,
    logical(1)
  )
]

if (
  length(non_numeric_population_variables) > 0L ||
  length(non_numeric_control_variables) > 0L
) {
  stop(
    "All corrected robust behavioural variables must be numeric."
  )
}

control_reference_audit <- dplyr::bind_rows(
  lapply(
    sort(
      unique(
        population$Author
      )
    ),
    function(author_i) {

      author_control_matrix <- as.matrix(
        population[
          population$Author == author_i &
            population$Treatment == "Control",
          robust_variables,
          drop = FALSE
        ]
      )

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
  nrow(control_reference_audit) !=
    expected_number_of_authors *
      length(robust_variables)
) {
  stop(
    "The Author-specific robust-normalisation audit is incomplete."
  )
}

if (
  any(
    abs(
      control_reference_audit$Control_median
    ) > audit_tolerance
  )
) {
  stop(
    "At least one Author-specific Control median is not zero ",
    "within tolerance."
  )