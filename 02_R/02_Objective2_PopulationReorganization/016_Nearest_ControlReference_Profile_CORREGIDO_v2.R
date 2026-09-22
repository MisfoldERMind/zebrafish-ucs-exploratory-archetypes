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

project_root <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

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
}

if (
  any(
    abs(
      control_reference_audit$Control_scaled_MAD -
        1
    ) > audit_tolerance
  )
) {
  stop(
    "At least one Author-specific Control scaled MAD is not one ",
    "within tolerance."
  )
}

# ------------------------------------------------------------
# 9. Confirm exact continuity between Objectives 1 and 2
# ------------------------------------------------------------

control_population <- population %>%
  dplyr::filter(
    Treatment == "Control"
  )

if (
  !setequal(
    control_population$Traceability_ID,
    control_louvain$Traceability_ID
  )
) {
  stop(
    "The 89 Control Author + ID keys in Objective 2 do not ",
    "reproduce the corrected Objective 1 reference population."
  )
}

control_population <- control_population[
  match(
    control_louvain$Traceability_ID,
    control_population$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    control_population$Traceability_ID,
    control_louvain$Traceability_ID
  )
) {
  stop(
    "The Objective 2 Control data could not be aligned with ",
    "corrected Objective 1 by Author + ID."
  )
}

control_objective1_matrix <- as.matrix(
  control_louvain[
    robust_variables
  ]
)

control_objective2_matrix <- as.matrix(
  control_population[
    robust_variables
  ]
)

storage.mode(
  control_objective1_matrix
) <- "double"

storage.mode(
  control_objective2_matrix
) <- "double"

if (
  any(!is.finite(control_objective1_matrix)) ||
  any(!is.finite(control_objective2_matrix))
) {
  stop(
    "Non-finite robust values were detected in the aligned Control data."
  )
}

control_value_comparison <- all.equal(
  unname(
    control_objective1_matrix
  ),
  unname(
    control_objective2_matrix
  ),
  tolerance = 1e-12,
  check.attributes = FALSE
)

if (!isTRUE(
  control_value_comparison
)) {
  stop(
    "The robust Control values in Objective 2 do not reproduce ",
    "the corrected Objective 1 data.\nComparison result:\n",
    paste(
      control_value_comparison,
      collapse = "\n"
    )
  )
}

rownames(
  control_objective1_matrix
) <- control_louvain$Traceability_ID

# ------------------------------------------------------------
# 10. Reconstruct normative centroids from corrected k = 46 data
# ------------------------------------------------------------

centroids_recalculated <- control_louvain %>%
  dplyr::group_by(
    Algorithmic_Segment
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    distance = mean(
      distance_rz
    ),
    crossing = mean(
      crossing_rz
    ),
    top_entry = mean(
      top_entry_rz
    ),
    top_time = mean(
      top_time_rz
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Algorithmic_Segment
  )

if (
  nrow(centroids_recalculated) !=
    expected_number_of_clusters ||
  !identical(
    as.character(
      centroids_recalculated$Algorithmic_Segment
    ),
    cluster_levels
  ) ||
  !identical(
    as.integer(
      centroids_recalculated$n
    ),
    as.integer(
      cluster_sizes$n
    )
  )
) {
  stop(
    "The reconstructed normative centroids are inconsistent with ",
    "the corrected k = 46 membership."
  )
}

centroid_matrix <- as.matrix(
  centroids_recalculated[
    behavioural_variables
  ]
)

storage.mode(
  centroid_matrix
) <- "double"

rownames(
  centroid_matrix
) <- cluster_levels

colnames(
  centroid_matrix
) <- robust_variables

if (
  any(!is.finite(centroid_matrix))
) {
  stop(
    "The reconstructed normative centroid matrix contains ",
    "non-finite values."
  )
}

# ------------------------------------------------------------
# 11. Identify higher- and lower-exploration profiles
# ------------------------------------------------------------

centroid_overall_score <- rowMeans(
  centroid_matrix
)

higher_cluster <- names(
  which.max(
    centroid_overall_score
  )
)

lower_cluster <- names(
  which.min(
    centroid_overall_score
  )
)

if (
  length(higher_cluster) != 1L ||
  length(lower_cluster) != 1L ||
  identical(
    higher_cluster,
    lower_cluster
  )
) {
  stop(
    "The higher- and lower-exploration profiles could not be ",
    "identified uniquely."
  )
}

if (
  !all(
    centroid_matrix[
      higher_cluster,
      ,
      drop = TRUE
    ] >
      centroid_matrix[
        lower_cluster,
        ,
        drop = TRUE
      ]
  )
) {
  stop(
    "The higher-exploration profile is not higher across all four ",
    "behavioural dimensions."
  )
}

profile_by_cluster <- stats::setNames(
  c(
    "Higher-exploration",
    "Lower-exploration"
  ),
  c(
    higher_cluster,
    lower_cluster
  )
)

centroid_table <- centroids_recalculated %>%
  dplyr::mutate(
    Normative_Profile = unname(
      profile_by_cluster[
        as.character(
          Algorithmic_Segment
        )
      ]
    ),
    Overall_exploration_score = unname(
      centroid_overall_score[
        as.character(
          Algorithmic_Segment
        )
      ]
    ),
    k = expected_k,
    Number_of_Clusters =
      expected_number_of_clusters
  ) %>%
  dplyr::select(
    Algorithmic_Segment,
    Normative_Profile,
    n,
    distance,
    crossing,
    top_entry,
    top_time,
    Overall_exploration_score,
    k,
    Number_of_Clusters
  )

# ------------------------------------------------------------
# 12. Helper functions
# ------------------------------------------------------------

calculate_euclidean_distances <- function(
    observation_matrix,
    centroid_matrix_input
) {

  observation_matrix <- as.matrix(
    observation_matrix
  )

  centroid_matrix_input <- as.matrix(
    centroid_matrix_input
  )

  storage.mode(
    observation_matrix
  ) <- "double"

  storage.mode(
    centroid_matrix_input
  ) <- "double"

  if (
    ncol(observation_matrix) !=
      ncol(centroid_matrix_input)
  ) {
    stop(
      "Observation and centroid matrices have different dimensions."
    )
  }

  n_observations <- nrow(
    observation_matrix
  )

  n_centroids <- nrow(
    centroid_matrix_input
  )

  if (
    n_observations < 1L ||
    n_centroids < 1L
  ) {
    stop(
      "At least one observation and one centroid are required."
    )
  }

  if (
    is.null(
      rownames(
        centroid_matrix_input
      )
    ) ||
    any(
      rownames(
        centroid_matrix_input
      ) == ""
    )
  ) {
    stop(
      "The centroid matrix must have non-empty row names."
    )
  }

  output <- matrix(
    NA_real_,
    nrow = n_observations,
    ncol = n_centroids,
    dimnames = list(
      rownames(
        observation_matrix
      ),
      rownames(
        centroid_matrix_input
      )
    )
  )

  for (j in seq_len(
    n_centroids
  )) {

    centred_matrix_j <- sweep(
      observation_matrix,
      MARGIN = 2,
      STATS = centroid_matrix_input[
        j,
        ,
        drop = TRUE
      ],
      FUN = "-"
    )

    output[
      ,
      j
    ] <- sqrt(
      rowSums(
        centred_matrix_j^2
      )
    )
  }

  if (
    !identical(
      dim(output),
      c(
        n_observations,
        n_centroids
      )
    )
  ) {
    stop(
      "The Euclidean distance matrix has incorrect dimensions."
    )
  }

  if (
    any(!is.finite(output)) ||
    any(output < 0)
  ) {
    stop(
      "Invalid Euclidean distances to normative centroids were generated."
    )
  }

  output
}

nearest_cluster_from_distances <- function(
    distance_matrix_input,
    tie_tolerance = 1e-12
) {

  distance_matrix_input <- as.matrix(
    distance_matrix_input
  )

  if (
    ncol(distance_matrix_input) <
      2L
  ) {
    stop(
      "At least two normative centroids are required."
    )
  }

  ordered_distances <- t(
    apply(
      distance_matrix_input,
      1,
      sort
    )
  )

  margins <- (
    ordered_distances[
      ,
      2
    ] -
      ordered_distances[
        ,
        1
      ]
  )

  tied_rows <- which(
    margins <= tie_tolerance
  )

  if (
    length(tied_rows) > 0L
  ) {
    stop(
      "Exact or numerically unresolved ties were detected for ",
      length(tied_rows),
      " fish during nearest-centroid assignment."
    )
  }

  nearest_index <- max.col(
    -distance_matrix_input,
    ties.method = "first"
  )

  nearest_cluster <- colnames(
    distance_matrix_input
  )[
    nearest_index
  ]

  minimum_distance <- distance_matrix_input[
    cbind(
      seq_len(
        nrow(
          distance_matrix_input
        )
      ),
      nearest_index
    )
  ]

  list(
    Nearest_cluster = nearest_cluster,
    Minimum_distance = minimum_distance,
    Assignment_margin = margins
  )
}

adjusted_rand_index <- function(
    x,
    y
) {

  if (
    length(x) != length(y) ||
    anyNA(x) ||
    anyNA(y)
  ) {
    stop(
      "Invalid partitions were supplied to the ARI calculation."
    )
  }

  contingency <- table(
    x,
    y
  )

  choose_two <- function(z) {
    z * (z - 1) / 2
  }

  index <- sum(
    choose_two(
      contingency
    )
  )

  row_index <- sum(
    choose_two(
      rowSums(
        contingency
      )
    )
  )

  column_index <- sum(
    choose_two(
      colSums(
        contingency
      )
    )
  )

  total_pairs <- choose_two(
    sum(
      contingency
    )
  )

  expected_index <- (
    row_index *
      column_index /
      total_pairs
  )

  maximum_index <- (
    row_index +
      column_index
  ) / 2

  denominator <- (
    maximum_index -
      expected_index
  )

  if (
    abs(denominator) <=
      sqrt(
        .Machine$double.eps
      )
  ) {

    same_partition <- identical(
      outer(
        x,
        x,
        `==`
      ),
      outer(
        y,
        y,
        `==`
      )
    )

    return(
      if (same_partition) 1 else 0
    )
  }

  ari_value <- (
    index -
      expected_index
  ) /
    denominator

  if (!is.finite(
    ari_value
  )) {
    stop(
      "A non-finite ARI value was generated."
    )
  }

  as.numeric(
    ari_value
  )
}

calculate_rank_biserial <- function(
    control_values,
    stress_values
) {

  n_control <- length(
    control_values
  )

  n_stress <- length(
    stress_values
  )

  combined_values <- c(
    control_values,
    stress_values
  )

  combined_ranks <- rank(
    combined_values,
    ties.method = "average"
  )

  control_rank_sum <- sum(
    combined_ranks[
      seq_len(
        n_control
      )
    ]
  )

  U_control <- (
    control_rank_sum -
      n_control *
        (n_control + 1) / 2
  )

  rank_biserial_stress_minus_control <- (
    1 -
      2 *
        U_control /
        (
          n_control *
            n_stress
        )
  )

  c(
    U_control = U_control,
    Rank_biserial_Stress_minus_Control =
      rank_biserial_stress_minus_control
  )
}

single_row_distance_audit <- calculate_euclidean_distances(
  observation_matrix =
    control_objective1_matrix[
      1,
      ,
      drop = FALSE
    ],
  centroid_matrix_input =
    centroid_matrix
)

if (
  !identical(
    dim(
      single_row_distance_audit
    ),
    c(
      1L,
      expected_number_of_clusters
    )
  ) ||
  !identical(
    colnames(
      single_row_distance_audit
    ),
    cluster_levels
  )
) {
  stop(
    "The one-observation Euclidean-distance audit failed."
  )
}

# ------------------------------------------------------------
# 13. Project Stress fish onto fixed normative centroids
# ------------------------------------------------------------

stress_population <- population %>%
  dplyr::filter(
    Treatment == "Stress"
  )

stress_matrix <- as.matrix(
  stress_population[
    robust_variables
  ]
)

storage.mode(
  stress_matrix
) <- "double"

rownames(
  stress_matrix
) <- stress_population$Traceability_ID

if (
  any(!is.finite(stress_matrix))
) {
  stop(
    "Non-finite robust values were detected in the Stress population."
  )
}

stress_distance_matrix <- calculate_euclidean_distances(
  observation_matrix = stress_matrix,
  centroid_matrix_input = centroid_matrix
)

stress_nearest <- nearest_cluster_from_distances(
  stress_distance_matrix
)

# ------------------------------------------------------------
# 14. Leave-one-out projection audit for Control fish
# ------------------------------------------------------------

control_cluster <- as.character(
  control_louvain$Algorithmic_Segment
)

control_loo_distance_matrix <- matrix(
  NA_real_,
  nrow = expected_control_n,
  ncol = expected_number_of_clusters,
  dimnames = list(
    control_louvain$Traceability_ID,
    cluster_levels
  )
)

for (i in seq_len(
  expected_control_n
)) {

  centroid_matrix_i <- centroid_matrix

  own_cluster_i <- control_cluster[
    i
  ]

  own_cluster_rows_i <- (
    control_cluster ==
      own_cluster_i
  )

  own_cluster_rows_i[
    i
  ] <- FALSE

  if (
    sum(
      own_cluster_rows_i
    ) < 2L
  ) {
    stop(
      "Too few fish remain to calculate a leave-one-out centroid ",
      "for Control fish ",
      control_louvain$Traceability_ID[
        i
      ],
      "."
    )
  }

  centroid_matrix_i[
    own_cluster_i,
    ] <- colMeans(
      control_objective1_matrix[
        own_cluster_rows_i,
        ,
        drop = FALSE
      ]
    )

  control_loo_distance_matrix[
    i,
    ] <- calculate_euclidean_distances(
      observation_matrix =
        control_objective1_matrix[
          i,
          ,
          drop = FALSE
        ],
      centroid_matrix_input =
        centroid_matrix_i
    )[
      1,
      ,
      drop = TRUE
    ]
}

if (
  any(!is.finite(
    control_loo_distance_matrix
  )) ||
  any(
    control_loo_distance_matrix < 0
  )
) {
  stop(
    "Invalid leave-one-out Control distances were generated."
  )
}

control_nearest_loo <- nearest_cluster_from_distances(
  control_loo_distance_matrix
)

control_distance_to_assigned_cluster_loo <- vapply(
  seq_len(
    expected_control_n
  ),
  function(i) {
    control_loo_distance_matrix[
      i,
      control_cluster[
        i
      ]
    ]
  },
  numeric(1)
)

control_projection_accuracy <- mean(
  control_nearest_loo[[
    "Nearest_cluster"
  ]] ==
    control_cluster
)

control_projection_ari <- adjusted_rand_index(
  control_cluster,
  control_nearest_loo[[
    "Nearest_cluster"
  ]]
)

control_projection_confusion <- as.data.frame.matrix(
  table(
    Louvain_Segment =
      control_cluster,
    Nearest_Centroid_Segment_LOO =
      control_nearest_loo[[
        "Nearest_cluster"
      ]]
  )
) %>%
  tibble::rownames_to_column(
    var = "Louvain_Segment"
  ) %>%
  tibble::as_tibble()

control_projection_audit <- tibble::tibble(
  Reference_partition =
    "Corrected Objective 1 Louvain segments",
  Projection_rule =
    "Nearest leave-one-out normative centroid",
  k = expected_k,
  Number_of_Clusters =
    expected_number_of_clusters,
  Control_n = expected_control_n,
  Exact_assignment_accuracy =
    control_projection_accuracy,
  Adjusted_Rand_Index =
    control_projection_ari
)

# ------------------------------------------------------------
# 15. Construct final individual projection dataset
# ------------------------------------------------------------

distance_column_names <- paste0(
  "Distance_",
  gsub(
    "[^[:alnum:]]+",
    "_",
    cluster_levels
  )
)

control_distance_table <- as.data.frame(
  control_loo_distance_matrix,
  check.names = FALSE
)

stress_distance_table <- as.data.frame(
  stress_distance_matrix,
  check.names = FALSE
)

names(
  control_distance_table
) <- distance_column_names

names(
  stress_distance_table
) <- distance_column_names

control_projection <- control_population %>%
  dplyr::mutate(
    Control_Louvain_Cluster =
      control_cluster,
    Normative_Cluster =
      control_cluster,
    Normative_Profile =
      unname(
        profile_by_cluster[
          control_cluster
        ]
      ),
    Nearest_Centroid_Cluster =
      control_nearest_loo[[
        "Nearest_cluster"
      ]],
    Nearest_Centroid_Profile =
      unname(
        profile_by_cluster[
          control_nearest_loo[[
            "Nearest_cluster"
          ]]
        ]
      ),
    Minimum_Archetype_Distance =
      control_nearest_loo[[
        "Minimum_distance"
      ]],
    Distance_to_Assigned_Normative_Cluster =
      control_distance_to_assigned_cluster_loo,
    Assignment_Margin =
      control_nearest_loo[[
        "Assignment_margin"
      ]],
    Assignment_Method =
      "Original k = 46 Louvain membership",
    Distance_Reference_Method =
      "Leave-one-out normative centroids"
  ) %>%
  dplyr::bind_cols(
    control_distance_table
  )

stress_projection <- stress_population %>%
  dplyr::mutate(
    Control_Louvain_Cluster =
      NA_character_,
    Normative_Cluster =
      stress_nearest[[
        "Nearest_cluster"
      ]],
    Normative_Profile =
      unname(
        profile_by_cluster[
          stress_nearest[[
            "Nearest_cluster"
          ]]
        ]
      ),
    Nearest_Centroid_Cluster =
      stress_nearest[[
        "Nearest_cluster"
      ]],
    Nearest_Centroid_Profile =
      unname(
        profile_by_cluster[
          stress_nearest[[
            "Nearest_cluster"
          ]]
        ]
      ),
    Minimum_Archetype_Distance =
      stress_nearest[[
        "Minimum_distance"
      ]],
    Distance_to_Assigned_Normative_Cluster =
      stress_nearest[[
        "Minimum_distance"
      ]],
    Assignment_Margin =
      stress_nearest[[
        "Assignment_margin"
      ]],
    Assignment_Method =
      "Nearest fixed normative centroid",
    Distance_Reference_Method =
      "Complete corrected Objective 1 normative centroids"
  ) %>%
  dplyr::bind_cols(
    stress_distance_table
  )

population_projection <- dplyr::bind_rows(
  control_projection,
  stress_projection
) %>%
  dplyr::mutate(
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Normative_Cluster = factor(
      Normative_Cluster,
      levels = cluster_levels
    ),
    Normative_Profile = factor(
      Normative_Profile,
      levels = c(
        "Higher-exploration",
        "Lower-exploration"
      )
    ),
    Nearest_Centroid_Cluster = factor(
      Nearest_Centroid_Cluster,
      levels = cluster_levels
    ),
    Nearest_Centroid_Profile = factor(
      Nearest_Centroid_Profile,
      levels = c(
        "Higher-exploration",
        "Lower-exploration"
      )
    ),
    Analysis_ID = paste(
      Treatment,
      Author,
      ID,
      sep = "_"
    )
  )

if (
  nrow(population_projection) !=
    expected_control_n +
      expected_stress_n
) {
  stop(
    "The final projection dataset has an incorrect number of rows."
  )
}

if (
  anyNA(
    population_projection[[
      "Normative_Cluster"
    ]]
  ) ||
  anyNA(
    population_projection[[
      "Normative_Profile"
    ]]
  ) ||
  any(
    !is.finite(
      population_projection[[
        "Minimum_Archetype_Distance"
      ]]
    )
  )
) {
  stop(
    "The final projection dataset contains missing assignments ",
    "or non-finite distances."
  )
}

if (
  anyDuplicated(
    population_projection[[
      "Analysis_ID"
    ]]
  ) > 0L
) {
  stop(
    "The final Analysis_ID is not unique."
  )
}

# ------------------------------------------------------------
# 16. Normative-profile occupancy by treatment
# ------------------------------------------------------------

occupancy_long <- population_projection %>%
  dplyr::count(
    Treatment,
    Normative_Profile,
    name = "Count",
    .drop = FALSE
  ) %>%
  dplyr::group_by(
    Treatment
  ) %>%
  dplyr::mutate(
    Total = sum(
      Count
    ),
    Proportion =
      Count /
        Total,
    Percentage =
      100 *
        Proportion
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    Treatment,
    Normative_Profile
  )

occupancy_matrix <- xtabs(
  Count ~
    Treatment +
    Normative_Profile,
  data = occupancy_long
)

if (
  !identical(
    dim(
      occupancy_matrix
    ),
    c(
      2L,
      2L
    )
  )
) {
  stop(
    "The occupancy table is not 2 x 2."
  )
}

if (
  any(
    rowSums(
      occupancy_matrix
    ) !=
      c(
        expected_control_n,
        expected_stress_n
      )
  )
) {
  stop(
    "The occupancy counts do not reproduce the group sizes."
  )
}

if (
  !identical(
    rownames(
      occupancy_matrix
    ),
    required_treatments
  ) ||
  !identical(
    colnames(
      occupancy_matrix
    ),
    c(
      "Higher-exploration",
      "Lower-exploration"
    )
  )
) {
  stop(
    "The occupancy matrix does not preserve the expected ",
    "Control/Stress by profile structure."
  )
}

occupancy_matrix_inference <- occupancy_matrix[
  c(
    "Stress",
    "Control"
  ),
  c(
    "Higher-exploration",
    "Lower-exploration"
  ),
  drop = FALSE
]

fisher_occupancy <- stats::fisher.test(
  occupancy_matrix_inference,
  alternative = "two.sided",
  conf.int = TRUE
)

chi_square_occupancy <- suppressWarnings(
  stats::chisq.test(
    occupancy_matrix,
    correct = FALSE
  )
)

if (
  any(
    chi_square_occupancy$expected <
      5
  )
) {
  warning(
    "At least one expected occupancy count is below five. ",
    "Fisher's exact test remains the primary occupancy test."
  )
}

cramers_v <- sqrt(
  as.numeric(
    chi_square_occupancy$statistic
  ) /
    (
      sum(
        occupancy_matrix
      ) *
        min(
          nrow(
            occupancy_matrix
          ) -
            1,
          ncol(
            occupancy_matrix
          ) -
            1
        )
    )
)

control_high_count <- occupancy_matrix[
  "Control",
  "Higher-exploration"
]

stress_high_count <- occupancy_matrix[
  "Stress",
  "Higher-exploration"
]

control_high_proportion <- (
  control_high_count /
    expected_control_n
)

stress_high_proportion <- (
  stress_high_count /
    expected_stress_n
)

risk_difference_high <- (
  stress_high_proportion -
    control_high_proportion
)

risk_ratio_high <- if (
  control_high_proportion > 0
) {
  stress_high_proportion /
    control_high_proportion
} else {
  NA_real_
}

fisher_estimate <- if (
  length(
    fisher_occupancy$estimate
  ) == 1L
) {
  as.numeric(
    fisher_occupancy$estimate
  )
} else {
  NA_real_
}

fisher_confidence <- if (
  length(
    fisher_occupancy$conf.int
  ) == 2L
) {
  as.numeric(
    fisher_occupancy$conf.int
  )
} else {
  c(
    NA_real_,
    NA_real_
  )
}

occupancy_test_summary <- tibble::tibble(
  Comparison =
    "Stress versus Control",
  Outcome =
    "Occupancy of the higher-exploration normative profile",
  Control_Higher_n =
    as.integer(
      control_high_count
    ),
  Control_Total_n =
    expected_control_n,
  Control_Higher_Proportion =
    control_high_proportion,
  Stress_Higher_n =
    as.integer(
      stress_high_count
    ),
  Stress_Total_n =
    expected_stress_n,
  Stress_Higher_Proportion =
    stress_high_proportion,
  Risk_Difference_Stress_minus_Control =
    risk_difference_high,
  Risk_Ratio_Stress_over_Control =
    risk_ratio_high,
  Fisher_Odds_Ratio_Stress_over_Control =
    fisher_estimate,
  Fisher_CI95_low =
    fisher_confidence[
      1
    ],
  Fisher_CI95_high =
    fisher_confidence[
      2
    ],
  Fisher_Two_sided_p =
    fisher_occupancy$p.value,
  Pearson_Chi_square =
    as.numeric(
      chi_square_occupancy$statistic
    ),
  Pearson_df =
    as.numeric(
      chi_square_occupancy$parameter
    ),
  Pearson_p =
    chi_square_occupancy$p.value,
  Cramers_V =
    cramers_v
)

expected_frequency_matrix <- chi_square_occupancy[[
  "expected"
]]

if (
  !is.matrix(
    expected_frequency_matrix
  ) ||
  !identical(
    dim(
      expected_frequency_matrix
    ),
    c(
      2L,
      2L
    )
  )
) {
  stop(
    "The chi-square expected-frequency object is not a 2 x 2 matrix."
  )
}

occupancy_expected <- do.call(
  rbind,
  lapply(
    rownames(
      expected_frequency_matrix
    ),
    function(treatment_i) {
      do.call(
        rbind,
        lapply(
          colnames(
            expected_frequency_matrix
          ),
          function(profile_i) {
            data.frame(
              Treatment =
                treatment_i,
              Normative_Profile =
                profile_i,
              Expected_Count =
                as.numeric(
                  expected_frequency_matrix[
                    treatment_i,
                    profile_i
                  ]
                ),
              stringsAsFactors = FALSE
            )
          }
        )
      )
    }
  )
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = required_treatments
    ),
    Normative_Profile = factor(
      Normative_Profile,
      levels = c(
        "Higher-exploration",
        "Lower-exploration"
      )
    )
  ) %>%
  dplyr::arrange(
    Treatment,
    Normative_Profile
  )

if (
  nrow(
    occupancy_expected
  ) != 4L ||
  anyNA(
    occupancy_expected
  ) ||
  any(
    !is.finite(
      occupancy_expected[[
        "Expected_Count"
      ]]
    )
  )
) {
  stop(
    "The expected occupancy-frequency table is invalid."
  )
}

# ------------------------------------------------------------
# 17. Distance to nearest normative profile
# ------------------------------------------------------------

distance_summary <- population_projection %>%
  dplyr::group_by(
    Treatment
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean = mean(
      Minimum_Archetype_Distance
    ),
    SD = stats::sd(
      Minimum_Archetype_Distance
    ),
    Median = stats::median(
      Minimum_Archetype_Distance
    ),
    Q1 = as.numeric(
      stats::quantile(
        Minimum_Archetype_Distance,
        probs = 0.25,
        names = FALSE,
        type = 7
      )
    ),
    Q3 = as.numeric(
      stats::quantile(
        Minimum_Archetype_Distance,
        probs = 0.75,
        names = FALSE,
        type = 7
      )
    ),
    IQR = stats::IQR(
      Minimum_Archetype_Distance,
      type = 7
    ),
    Minimum = min(
      Minimum_Archetype_Distance
    ),
    Maximum = max(
      Minimum_Archetype_Distance
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Treatment
  )

control_minimum_distance <- population_projection %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::pull(
    Minimum_Archetype_Distance
  )

stress_minimum_distance <- population_projection %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::pull(
    Minimum_Archetype_Distance
  )

distance_wilcoxon <- stats::wilcox.test(
  x = control_minimum_distance,
  y = stress_minimum_distance,
  alternative = "two.sided",
  exact = FALSE,
  correct = FALSE,
  conf.int = FALSE
)

distance_rank_statistics <- calculate_rank_biserial(
  control_values =
    control_minimum_distance,
  stress_values =
    stress_minimum_distance
)

distance_wilcoxon_summary <- tibble::tibble(
  Comparison =
    "Stress versus Control",
  Outcome =
    "Euclidean distance to nearest normative profile",
  Control_distance_method =
    "Nearest leave-one-out normative centroid",
  Stress_distance_method =
    "Nearest complete normative centroid",
  Test =
    "Wilcoxon rank-sum test",
  Role = paste0(
    "Projection-based comparison conditional on the fixed ",
    "corrected Objective 1 atlas"
  ),
  W_or_U_for_Control =
    as.numeric(
      distance_wilcoxon$statistic
    ),
  Two_sided_p =
    distance_wilcoxon$p.value,
  Rank_biserial_Stress_minus_Control =
    unname(
      distance_rank_statistics[
        "Rank_biserial_Stress_minus_Control"
      ]
    )
)

if (
  !isTRUE(
    all.equal(
      as.numeric(
        distance_wilcoxon$statistic
      ),
      unname(
        distance_rank_statistics[
          "U_control"
        ]
      ),
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The manually reconstructed rank statistic does not match ",
    "wilcox.test()."
  )
}

# ------------------------------------------------------------
# 18. Integrated analysis summary
# ------------------------------------------------------------

analysis_summary <- tibble::tibble(
  Objective =
    "Objective 2",
  Comparison =
    "Control versus Stress",
  Design =
    "Two independent populations",
  Normative_reference =
    "Corrected Objective 1 weighted kNN-Louvain architecture",
  Traceability =
    expected_traceability,
  Normalisation =
    expected_normalisation,
  Distance_metric =
    expected_distance,
  Similarity =
    expected_similarity,
  Graph =
    expected_graph_type,
  k =
    expected_k,
  Number_of_normative_profiles =
    expected_number_of_clusters,
  Control_cluster_sizes =
    paste(
      expected_cluster_sizes,
      collapse = ", "
    ),
  Higher_exploration_cluster =
    higher_cluster,
  Lower_exploration_cluster =
    lower_cluster,
  Control_projection_accuracy_LOO =
    control_projection_accuracy,
  Control_projection_ARI_LOO =
    control_projection_ari,
  Occupancy_Fisher_p =
    fisher_occupancy$p.value,
  Occupancy_Cramers_V =
    cramers_v,
  Higher_profile_risk_difference_Stress_minus_Control =
    risk_difference_high,
  Nearest_distance_Wilcoxon_p =
    distance_wilcoxon$p.value,
  Nearest_distance_rank_biserial_Stress_minus_Control =
    unname(
      distance_rank_statistics[
        "Rank_biserial_Stress_minus_Control"
      ]
    )
)

# ------------------------------------------------------------
# 19. Save corrected outputs
# ------------------------------------------------------------

distance_object <- list(
  Objective =
    "Objective 2",
  Input_population_file =
    population_file,
  Input_control_louvain_file =
    control_louvain_file,
  Input_selected_membership_file =
    selected_membership_file,
  Input_graph_summary_file =
    graph_summary_file,
  Input_inference_status_file =
    inference_status_file,
  Fixed_configuration = list(
    Traceability =
      expected_traceability,
    Normalisation =
      expected_normalisation,
    Distance =
      expected_distance,
    Similarity =
      expected_similarity,
    Graph =
      expected_graph_type,
    k =
      expected_k,
    Number_of_Clusters =
      expected_number_of_clusters,
    Cluster_sizes =
      expected_cluster_sizes
  ),
  Cluster_levels =
    cluster_levels,
  Profile_by_cluster =
    profile_by_cluster,
  Normative_centroids =
    centroid_table,
  Control_leave_one_out_distance_matrix =
    control_loo_distance_matrix,
  Stress_distance_matrix =
    stress_distance_matrix
)

results_object <- list(
  Objective =
    "Objective 2",
  Comparison_design =
    "Independent Control and Stress populations",
  Normative_architecture =
    "Corrected Objective 1 k = 46 two-segment solution",
  Fixed_configuration =
    distance_object[[
      "Fixed_configuration"
    ]],
  Group_counts =
    group_counts,
  Cluster_sizes =
    cluster_sizes,
  Author_reference_audit =
    control_reference_audit,
  Centroids =
    centroid_table,
  Profile_by_cluster =
    profile_by_cluster,
  Control_projection_audit =
    control_projection_audit,
  Control_projection_confusion =
    control_projection_confusion,
  Individual_projection =
    population_projection,
  Occupancy_long =
    occupancy_long,
  Occupancy_matrix =
    occupancy_matrix,
  Occupancy_expected =
    occupancy_expected,
  Occupancy_Fisher =
    fisher_occupancy,
  Occupancy_Chi_square =
    chi_square_occupancy,
  Occupancy_test_summary =
    occupancy_test_summary,
  Distance_summary =
    distance_summary,
  Distance_Wilcoxon =
    distance_wilcoxon,
  Distance_Wilcoxon_summary =
    distance_wilcoxon_summary,
  Analysis_summary =
    analysis_summary
)

saveRDS(
  population_projection,
  projection_output
)

saveRDS(
  distance_object,
  distance_object_output
)

saveRDS(
  results_object,
  results_object_output
)

upstream_configuration <- tibble::tibble(
  Parameter = c(
    "Traceability",
    "Normalisation",
    "Distance",
    "Similarity",
    "Graph",
    "k",
    "Number of normative profiles",
    "Control segment sizes",
    "Weighted modularity"
  ),
  Value = c(
    expected_traceability,
    expected_normalisation,
    expected_distance,
    expected_similarity,
    expected_graph_type,
    as.character(
      expected_k
    ),
    as.character(
      expected_number_of_clusters
    ),
    paste(
      expected_cluster_sizes,
      collapse = "/"
    ),
    as.character(
      graph_summary[[
        "Weighted_modularity"
      ]][[
        1
      ]]
    )
  )
)

workbook_sheets <- list(
  Analysis_summary =
    analysis_summary,
  Upstream_configuration =
    upstream_configuration,
  Author_reference_audit =
    control_reference_audit,
  Normative_centroids =
    centroid_table,
  Control_projection_audit =
    control_projection_audit,
  Control_projection_confusion =
    control_projection_confusion,
  Occupancy =
    occupancy_long,
  Occupancy_expected =
    occupancy_expected,
  Occupancy_tests =
    occupancy_test_summary,
  Distance_summary =
    distance_summary,
  Distance_Wilcoxon =
    distance_wilcoxon_summary,
  Individual_projection =
    population_projection
)

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

for (
  sheet_i in names(
    workbook_sheets
  )
) {

  sheet_data_i <- workbook_sheets[[
    sheet_i
  ]]

  if (!is.data.frame(
    sheet_data_i
  )) {
    stop(
      "Workbook sheet ",
      sheet_i,
      " is not a data frame."
    )
  }

  openxlsx::addWorksheet(
    workbook,
    sheet_i
  )

  openxlsx::writeData(
    workbook,
    sheet = sheet_i,
    x = sheet_data_i
  )

  if (
    ncol(
      sheet_data_i
    ) > 0L
  ) {

    openxlsx::addStyle(
      workbook,
      sheet = sheet_i,
      style = header_style,
      rows = 1,
      cols = seq_len(
        ncol(
          sheet_data_i
        )
      ),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::setColWidths(
      workbook,
      sheet = sheet_i,
      cols = seq_len(
        ncol(
          sheet_data_i
        )
      ),
      widths = "auto"
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet = sheet_i,
    firstRow = TRUE
  )
}

openxlsx::saveWorkbook(
  workbook,
  workbook_output,
  overwrite = TRUE
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 20. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  projection_output,
  distance_object_output,
  results_object_output,
  workbook_output,
  session_info_output
)

missing_output_files <- expected_output_files[
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
    "The following expected output files were not created:\n",
    paste(
      missing_output_files,
      collapse = "\n"
    )
  )
}

# ------------------------------------------------------------
# 21. Identify obsolete outputs from former workflows
# ------------------------------------------------------------

legacy_files <- c(
  file.path(
    processed_dir,
    "population_control_stress_z.rds"
  ),
  file.path(
    objects_dir,
    "objective1_control_robust_louvain.rds"
  ),
  file.path(
    objects_dir,
    "objective1_selected_louvain_configuration_robust.rds"
  ),
  file.path(
    objects_dir,
    "objective1_selected_louvain_membership_robust.rds"
  ),
  file.path(
    objects_dir,
    "objective1_louvain_graph_summary_robust.rds"
  ),
  file.path(
    objects_dir,
    "control_archetype_centroids.rds"
  ),
  file.path(
    processed_dir,
    "population_nearest_normative_archetype.rds"
  ),
  file.path(
    tables_dir,
    "Table5_NormativeArchetypeOccupancy.xlsx"
  )
)

existing_legacy_files <- legacy_files[
  file.exists(
    legacy_files
  )
]

if (
  length(
    existing_legacy_files
  ) > 0L
) {
  warning(
    "Legacy files from former normalisation or k = 56 workflows ",
    "were detected. They were not used or overwritten:\n",
    paste(
      existing_legacy_files,
      collapse = "\n"
    )
  )
}

# ------------------------------------------------------------
# 22. Final console audit
# ------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "OBJECTIVE 2 - PROJECTION ONTO NORMATIVE PROFILES\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nCorrected Objective 1 dependency:\n",
  "Traceability: ",
  expected_traceability,
  "\nNormalisation: ",
  expected_normalisation,
  "\nDistance: ",
  expected_distance,
  "\nSimilarity: ",
  expected_similarity,
  "\nGraph: ",
  expected_graph_type,
  "\nk = ",
  expected_k,
  "\nNormative profiles = ",
  expected_number_of_clusters,
  "\nControl segment sizes = ",
  paste(
    expected_cluster_sizes,
    collapse = "/"
  ),
  "\n",
  sep = ""
)

cat(
  "\nIndependent populations:\n"
)

print(
  group_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nAuthor-specific Control-reference audit ",
  "(expected median = 0 and scaled MAD = 1):\n",
  sep = ""
)

print(
  control_reference_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nNormative profile centroids:\n"
)

print(
  centroid_table,
  n = Inf,
  width = Inf
)

cat(
  "\nControl leave-one-out projection audit:\n"
)

print(
  control_projection_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nControl Louvain versus nearest-centroid confusion matrix:\n"
)

print(
  control_projection_confusion,
  n = Inf,
  width = Inf
)

cat(
  "\nNormative-profile occupancy:\n"
)

print(
  occupancy_long,
  n = Inf,
  width = Inf
)

cat(
  "\nOccupancy comparison:\n"
)

print(
  occupancy_test_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nDistance to nearest normative profile:\n"
)

print(
  distance_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nProjection-based Wilcoxon comparison:\n"
)

print(
  distance_wilcoxon_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nInterpretive constraint: Control occupancy retains the ",
  "original corrected k = 46 Louvain membership, whereas Stress ",
  "occupancy is obtained by projection onto the nearest fixed ",
  "normative centroid. The Control leave-one-out audit quantifies ",
  "the fidelity of this centroid-based projection rule.\n",
  sep = ""
)

cat(
  "\nDefinitive corrected outputs saved as:\n",
  normalizePath(
    projection_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    distance_object_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    results_object_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    workbook_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    session_info_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)

cat(
  "\n============================================================\n"
)
