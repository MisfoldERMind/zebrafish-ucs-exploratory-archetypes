# ============================================================
# 017_Figure_Normative_Profile_Occupancy_Bar.R
#
# Objective 2
# Bar chart of normative-profile occupancy in two independent
# populations: Control and Stress
#
# Definitive upstream dependencies:
#   - Objective 1 normative architecture:
#       robust normalisation within Author
#       Author + ID traceability
#       Euclidean distance
#       weighted, undirected union kNN graph
#       authorised representative k = 46
#       two Louvain segments (49 and 40 controls)
#   - Objective 2 projection produced by the corrected
#       016_Nearest_Normative_Archetype.R
#
# This figure does not use an alluvial representation because
# Control and Stress are independent groups, not repeated
# measurements of the same fish.
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
  "tibble",
  "ggplot2"
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
library(ggplot2)

# ------------------------------------------------------------
# 3. Definitive analytical configuration
# ------------------------------------------------------------

expected_control_n <- 89L
expected_stress_n <- 89L
expected_k <- 46L
expected_number_of_profiles <- 2L
expected_control_segment_sizes <- c(
  40L,
  49L
)

required_treatments <- c(
  "Control",
  "Stress"
)

required_profiles <- c(
  "Higher-exploration",
  "Lower-exploration"
)

expected_traceability <- "Author + ID"
expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"
expected_distance <- "Euclidean"
expected_similarity <- "1 / (1 + D)"
expected_graph <- "Weighted, undirected union kNN"

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

figures_dir <- file.path(
  project_root,
  "03_Results",
  "Figures"
)

if (!dir.exists(objects_dir)) {
  dir.create(
    objects_dir,
    recursive = TRUE
  )
}

if (!dir.exists(figures_dir)) {
  dir.create(
    figures_dir,
    recursive = TRUE
  )
}

projection_file <- file.path(
  processed_dir,
  "population_normative_archetype_projection_robust.rds"
)

results_file <- file.path(
  objects_dir,
  "objective2_nearest_normative_archetype_results_robust.rds"
)

required_input_files <- c(
  projection_file,
  results_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun the corrected Script 016 before this script."
  )
}

plot_object_output <- file.path(
  objects_dir,
  "objective2_normative_profile_occupancy_bar_plot.rds"
)

figure_pdf_output <- file.path(
  figures_dir,
  "Figure12_Normative_Profile_Occupancy_Bar.pdf"
)

figure_tiff_output <- file.path(
  figures_dir,
  "Figure12_Normative_Profile_Occupancy_Bar.tiff"
)

figure_png_output <- file.path(
  figures_dir,
  "Figure12_Normative_Profile_Occupancy_Bar.png"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_normative_profile_occupancy_bar_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load corrected Script 016 outputs
# ------------------------------------------------------------

population_projection <- readRDS(
  projection_file
)

projection_results <- readRDS(
  results_file
)

if (!is.data.frame(population_projection)) {
  stop(
    basename(projection_file),
    " does not contain a data frame."
  )
}

if (!is.list(projection_results)) {
  stop(
    basename(results_file),
    " does not contain the expected result list."
  )
}

# ------------------------------------------------------------
# 6. Verify dependence on the corrected k = 46 architecture
# ------------------------------------------------------------

required_result_fields <- c(
  "Fixed_configuration",
  "Group_counts",
  "Cluster_sizes",
  "Profile_by_cluster",
  "Individual_projection",
  "Occupancy_long",
  "Occupancy_test_summary"
)

missing_result_fields <- setdiff(
  required_result_fields,
  names(projection_results)
)

if (length(missing_result_fields) > 0L) {
  stop(
    "The corrected Script 016 result object is missing:\n",
    paste(missing_result_fields, collapse = ", ")
  )
}

fixed_configuration <- projection_results[[
  "Fixed_configuration"
]]

if (!is.list(fixed_configuration)) {
  stop(
    "The corrected Script 016 fixed configuration is not a list."
  )
}

required_configuration_fields <- c(
  "Traceability",
  "Normalisation",
  "Distance",
  "Similarity",
  "Graph",
  "k",
  "Number_of_Clusters",
  "Cluster_sizes"
)

missing_configuration_fields <- setdiff(
  required_configuration_fields,
  names(fixed_configuration)
)

if (length(missing_configuration_fields) > 0L) {
  stop(
    "The corrected Script 016 fixed configuration is missing:\n",
    paste(missing_configuration_fields, collapse = ", ")
  )
}

stored_cluster_sizes <- sort(
  as.integer(
    unlist(
      fixed_configuration[["Cluster_sizes"]],
      use.names = FALSE
    )
  )
)

if (
  !identical(
    as.character(
      fixed_configuration[["Traceability"]]
    ),
    expected_traceability
  ) ||
  !identical(
    as.character(
      fixed_configuration[["Normalisation"]]
    ),
    expected_normalisation
  ) ||
  !identical(
    as.character(
      fixed_configuration[["Distance"]]
    ),
    expected_distance
  ) ||
  !identical(
    as.character(
      fixed_configuration[["Similarity"]]
    ),
    expected_similarity
  ) ||
  !identical(
    as.character(
      fixed_configuration[["Graph"]]
    ),
    expected_graph
  ) ||
  as.integer(
    fixed_configuration[["k"]]
  ) != expected_k ||
  as.integer(
    fixed_configuration[["Number_of_Clusters"]]
  ) != expected_number_of_profiles ||
  !identical(
    stored_cluster_sizes,
    sort(
      expected_control_segment_sizes
    )
  )
) {
  stop(
    "The Script 016 outputs do not correspond to the corrected ",
    "within-Author robust k = 46, two-profile architecture."
  )
}

cluster_sizes_stored <- projection_results[[
  "Cluster_sizes"
]]

profile_by_cluster <- projection_results[[
  "Profile_by_cluster"
]]

if (!is.data.frame(cluster_sizes_stored)) {
  stop(
    "Cluster_sizes in the corrected Script 016 result object is not ",
    "a data frame."
  )
}

required_cluster_size_columns <- c(
  "Algorithmic_Segment",
  "n"
)

missing_cluster_size_columns <- setdiff(
  required_cluster_size_columns,
  names(cluster_sizes_stored)
)

if (length(missing_cluster_size_columns) > 0L) {
  stop(
    "The corrected Script 016 Cluster_sizes table is missing:\n",
    paste(missing_cluster_size_columns, collapse = ", ")
  )
}

if (
  is.null(
    names(
      profile_by_cluster
    )
  ) ||
  length(
    profile_by_cluster
  ) != expected_number_of_profiles
) {
  stop(
    "Profile_by_cluster in the corrected Script 016 result object ",
    "is not a named two-profile mapping."
  )
}

cluster_sizes_stored <- cluster_sizes_stored %>%
  dplyr::transmute(
    Algorithmic_Segment = as.character(
      Algorithmic_Segment
    ),
    n = as.integer(
      n
    )
  ) %>%
  dplyr::arrange(
    Algorithmic_Segment
  )

if (
  nrow(cluster_sizes_stored) != expected_number_of_profiles ||
  anyNA(cluster_sizes_stored) ||
  any(cluster_sizes_stored$n <= 0L) ||
  sum(cluster_sizes_stored$n) != expected_control_n ||
  !identical(
    sort(
      cluster_sizes_stored$n
    ),
    sort(
      expected_control_segment_sizes
    )
  )
) {
  stop(
    "The corrected Script 016 segment sizes are invalid."
  )
}

if (
  !all(
    cluster_sizes_stored$Algorithmic_Segment %in%
      names(
        profile_by_cluster
      )
  )
) {
  stop(
    "The corrected Script 016 profile mapping does not cover all ",
    "stored algorithmic segments."
  )
}

expected_control_profile_counts <- cluster_sizes_stored %>%
  dplyr::mutate(
    Normative_Profile = unname(
      profile_by_cluster[
        Algorithmic_Segment
      ]
    )
  ) %>%
  dplyr::group_by(
    Normative_Profile
  ) %>%
  dplyr::summarise(
    Count = sum(
      n
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    match(
      Normative_Profile,
      required_profiles
    )
  )

if (
  nrow(expected_control_profile_counts) !=
    expected_number_of_profiles ||
  !setequal(
    expected_control_profile_counts$Normative_Profile,
    required_profiles
  )
) {
  stop(
    "The corrected Script 016 objects do not define the expected ",
    "higher- and lower-exploration profiles."
  )
}

expected_control_profile_count_vector <- stats::setNames(
  as.integer(
    expected_control_profile_counts$Count
  ),
  expected_control_profile_counts$Normative_Profile
)

# ------------------------------------------------------------
# 7. Validate the individual projection dataset
# ------------------------------------------------------------

required_projection_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Analysis_ID",
  "Treatment",
  "Normative_Profile"
)

missing_projection_columns <- setdiff(
  required_projection_columns,
  names(population_projection)
)

if (length(missing_projection_columns) > 0L) {
  stop(
    "The corrected projection dataset is missing:\n",
    paste(missing_projection_columns, collapse = ", ")
  )
}

population_projection <- population_projection %>%
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
    Analysis_ID = trimws(
      as.character(Analysis_ID)
    ),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Normative_Profile = factor(
      as.character(Normative_Profile),
      levels = required_profiles
    )
  )

expected_traceability_id <- paste(
  population_projection$Author,
  population_projection$ID,
  sep = " || "
)

expected_analysis_id <- paste(
  population_projection$Treatment,
  population_projection$Author,
  population_projection$ID,
  sep = "_"
)

if (
  anyNA(
    population_projection[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Analysis_ID"
      )
    ]
  ) ||
  any(population_projection$Author == "") ||
  any(population_projection$ID == "") ||
  any(population_projection$Traceability_ID == "") ||
  any(population_projection$Analysis_ID == "") ||
  !identical(
    population_projection$Traceability_ID,
    expected_traceability_id
  ) ||
  !identical(
    population_projection$Analysis_ID,
    expected_analysis_id
  ) ||
  anyDuplicated(
    population_projection$Traceability_ID
  ) > 0L ||
  anyDuplicated(
    population_projection$Analysis_ID
  ) > 0L
) {
  stop(
    "The corrected projection dataset does not preserve unique ",
    "Author + ID traceability."
  )
}

if (
  anyNA(population_projection[["Treatment"]]) ||
  anyNA(population_projection[["Normative_Profile"]])
) {
  stop(
    "Unexpected or missing treatment/profile labels were detected."
  )
}

group_counts <- population_projection %>%
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

if (!identical(group_counts, expected_group_counts)) {
  stop(
    "The projection dataset does not contain exactly 89 Control ",
    "and 89 Stress fish."
  )
}

stored_individual_projection <- projection_results[[
  "Individual_projection"
]]

if (!is.data.frame(stored_individual_projection)) {
  stop(
    "Individual_projection in the Script 016 result object is not ",
    "a data frame."
  )
}

if (nrow(stored_individual_projection) != nrow(population_projection)) {
  stop(
    "The standalone projection dataset and the Script 016 result ",
    "object contain different numbers of fish."
  )
}

required_stored_projection_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Analysis_ID",
  "Treatment",
  "Normative_Profile"
)

missing_stored_projection_columns <- setdiff(
  required_stored_projection_columns,
  names(stored_individual_projection)
)

if (length(missing_stored_projection_columns) > 0L) {
  stop(
    "Individual_projection in the Script 016 result object is missing:\n",
    paste(missing_stored_projection_columns, collapse = ", ")
  )
}

standalone_projection_key <- population_projection %>%
  dplyr::transmute(
    Author = as.character(Author),
    ID = as.character(ID),
    Traceability_ID = as.character(Traceability_ID),
    Analysis_ID = as.character(Analysis_ID),
    Treatment = as.character(Treatment),
    Normative_Profile = as.character(Normative_Profile)
  ) %>%
  dplyr::arrange(
    Analysis_ID
  )

stored_projection_key <- stored_individual_projection %>%
  dplyr::transmute(
    Author = as.character(Author),
    ID = as.character(ID),
    Traceability_ID = as.character(Traceability_ID),
    Analysis_ID = as.character(Analysis_ID),
    Treatment = as.character(Treatment),
    Normative_Profile = as.character(Normative_Profile)
  ) %>%
  dplyr::arrange(
    Analysis_ID
  )

projection_key_comparison <- all.equal(
  standalone_projection_key,
  stored_projection_key,
  tolerance = 0,
  check.attributes = FALSE
)

if (!isTRUE(projection_key_comparison)) {
  stop(
    "The standalone projection dataset does not reproduce the ",
    "individual assignments stored by Script 016."
  )
}

# ------------------------------------------------------------
# 8. Reconstruct and audit occupancy data
# ------------------------------------------------------------

figure_data <- population_projection %>%
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
    Total = sum(Count),
    Proportion = Count / Total,
    Percentage = 100 * Proportion
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    Treatment,
    Normative_Profile
  )

if (
  nrow(figure_data) != 4L ||
  any(figure_data[["Count"]] < 0L) ||
  any(!is.finite(figure_data[["Percentage"]]))
) {
  stop(
    "The reconstructed occupancy data are invalid."
  )
}

occupancy_totals <- figure_data %>%
  dplyr::group_by(
    Treatment
  ) %>%
  dplyr::summarise(
    Total_n = sum(Count),
    Total_percentage = sum(Percentage),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Treatment
  )

if (
  !identical(
    as.integer(occupancy_totals[["Total_n"]]),
    c(
      expected_control_n,
      expected_stress_n
    )
  ) ||
  any(
    abs(
      occupancy_totals[["Total_percentage"]] - 100
    ) > 1e-10
  )
) {
  stop(
    "Occupancy counts or percentages do not reproduce the group totals."
  )
}

control_occupancy <- figure_data %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::arrange(
    Normative_Profile
  )

control_count_vector <- stats::setNames(
  as.integer(
    control_occupancy[["Count"]]
  ),
  as.character(
    control_occupancy[["Normative_Profile"]]
  )
)

if (
  !identical(
    control_count_vector[
      required_profiles
    ],
    expected_control_profile_count_vector[
      required_profiles
    ]
  )
) {
  stop(
    "Control occupancy does not reproduce the corrected Objective 1 ",
    "profile sizes inherited through Script 016."
  )
}

stored_occupancy <- projection_results[[
  "Occupancy_long"
]]

if (!is.data.frame(stored_occupancy)) {
  stop(
    "Occupancy_long in the Script 016 result object is not a data frame."
  )
}

required_occupancy_columns <- c(
  "Treatment",
  "Normative_Profile",
  "Count"
)

missing_occupancy_columns <- setdiff(
  required_occupancy_columns,
  names(stored_occupancy)
)

if (length(missing_occupancy_columns) > 0L) {
  stop(
    "The stored occupancy table is missing:\n",
    paste(missing_occupancy_columns, collapse = ", ")
  )
}

stored_counts <- stored_occupancy %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Normative_Profile = as.character(Normative_Profile),
    Count = as.integer(Count)
  ) %>%
  dplyr::arrange(
    Treatment,
    Normative_Profile
  )

reconstructed_counts <- figure_data %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Normative_Profile = as.character(Normative_Profile),
    Count = as.integer(Count)
  ) %>%
  dplyr::arrange(
    Treatment,
    Normative_Profile
  )

occupancy_comparison <- all.equal(
  stored_counts,
  reconstructed_counts,
  tolerance = 0,
  check.attributes = FALSE
)

if (!isTRUE(occupancy_comparison)) {
  stop(
    "The figure occupancy counts do not reproduce the definitive ",
    "Script 016 occupancy results."
  )
}

# ------------------------------------------------------------
# 9. Retrieve the already established occupancy inference
# ------------------------------------------------------------

occupancy_test_summary <- projection_results[[
  "Occupancy_test_summary"
]]

if (
  !is.data.frame(occupancy_test_summary) ||
  nrow(occupancy_test_summary) != 1L
) {
  stop(
    "The Script 016 occupancy-test summary is not a one-row data frame."
  )
}

required_test_columns <- c(
  "Fisher_Two_sided_p",
  "Cramers_V"
)

missing_test_columns <- setdiff(
  required_test_columns,
  names(occupancy_test_summary)
)

if (length(missing_test_columns) > 0L) {
  stop(
    "The Script 016 occupancy-test summary is missing:\n",
    paste(missing_test_columns, collapse = ", ")
  )
}

fisher_p <- as.numeric(
  occupancy_test_summary[["Fisher_Two_sided_p"]][1]
)

cramers_v <- as.numeric(
  occupancy_test_summary[["Cramers_V"]][1]
)

if (
  !is.finite(fisher_p) ||
  fisher_p < 0 ||
  fisher_p > 1 ||
  !is.finite(cramers_v) ||
  cramers_v < 0 ||
  cramers_v > 1
) {
  stop(
    "The stored Fisher p-value or Cramer's V is invalid."
  )
}

# ------------------------------------------------------------
# 10. Prepare the independent-group bar chart
# ------------------------------------------------------------

figure_data <- figure_data %>%
  dplyr::mutate(
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Normative_Profile = factor(
      as.character(Normative_Profile),
      levels = required_profiles
    ),
    Segment_label = sprintf(
      "%.1f%%\n(n = %d)",
      Percentage,
      Count
    ),
    Segment_text_colour = dplyr::if_else(
      Normative_Profile == "Higher-exploration",
      "white",
      "black"
    )
  )

occupancy_plot <- ggplot2::ggplot(
  figure_data,
  ggplot2::aes(
    x = Treatment,
    y = Percentage,
    fill = Normative_Profile
  )
) +
  ggplot2::geom_col(
    width = 0.62,
    colour = "black",
    linewidth = 0.55
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = Segment_label,
      colour = Segment_text_colour
    ),
    position = ggplot2::position_stack(
      vjust = 0.5
    ),
    size = 3.6,
    lineheight = 0.95,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Higher-exploration" = "grey35",
      "Lower-exploration" = "grey80"
    ),
    breaks = required_profiles,
    labels = c(
      "Higher exploration",
      "Lower exploration"
    ),
    name = "Normative profile"
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) {
      paste0(x, "%")
    },
    expand = ggplot2::expansion(
      mult = c(0, 0)
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Population occupancy (%)"
  ) +
  ggplot2::theme_classic(
    base_size = 12,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    axis.text = ggplot2::element_text(
      colour = "black"
    ),
    axis.text.x = ggplot2::element_text(
      face = "bold",
      size = 11.5
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(
        r = 8
      )
    ),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      size = 11
    ),
    legend.text = ggplot2::element_text(
      size = 10.5
    ),
    plot.margin = ggplot2::margin(
      t = 8,
      r = 10,
      b = 8,
      l = 8
    )
  )

print(
  occupancy_plot
)

# ------------------------------------------------------------
# 11. Save the single definitive bar chart
# ------------------------------------------------------------

pdf_device <- if (
  isTRUE(
    base::capabilities()[["cairo"]]
  )
) {
  grDevices::cairo_pdf
} else {
  grDevices::pdf
}

ggplot2::ggsave(
  filename = figure_pdf_output,
  plot = occupancy_plot,
  width = 5.6,
  height = 5.8,
  units = "in",
  device = pdf_device,
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_tiff_output,
  plot = occupancy_plot,
  width = 5.6,
  height = 5.8,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_png_output,
  plot = occupancy_plot,
  width = 5.6,
  height = 5.8,
  units = "in",
  dpi = 300,
  bg = "white"
)

saveRDS(
  list(
    Plot = occupancy_plot,
    Figure_data = figure_data,
    Fisher_Two_sided_p = fisher_p,
    Cramers_V = cramers_v,
    Upstream_projection_file = projection_file,
    Upstream_results_file = results_file,
    Traceability = expected_traceability,
    Normalisation = expected_normalisation,
    k = expected_k,
    Number_of_profiles = expected_number_of_profiles,
    Control_profile_counts =
      expected_control_profile_count_vector
  ),
  plot_object_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 12. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  plot_object_output,
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
# 13. Identify obsolete alluvial outputs
# ------------------------------------------------------------

legacy_alluvial_files <- c(
  file.path(
    figures_dir,
    "Figure12_Alluvial_ArchetypeOccupancy.pdf"
  ),
  file.path(
    figures_dir,
    "Figure12_Alluvial_ArchetypeOccupancy.tiff"
  )
)

existing_legacy_alluvial_files <- legacy_alluvial_files[
  file.exists(legacy_alluvial_files)
]

if (length(existing_legacy_alluvial_files) > 0L) {
  warning(
    "Obsolete alluvial figures were detected. They were not used or ",
    "overwritten because the Control and Stress groups are independent:\n",
    paste(existing_legacy_alluvial_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 14. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - NORMATIVE-PROFILE OCCUPANCY BAR CHART\n")
cat("============================================================\n")

cat(
  "\nDesign: two independent populations; no alluvial or longitudinal ",
  "representation was used.\n",
  sep = ""
)

cat(
  "\nDefinitive upstream architecture:\n",
  "Traceability = ",
  expected_traceability,
  "\nNormalisation = ",
  expected_normalisation,
  "\nk = ",
  expected_k,
  "\nNormative profiles = ",
  expected_number_of_profiles,
  "\nControl profile sizes: Higher-exploration = ",
  expected_control_profile_count_vector[
    "Higher-exploration"
  ],
  "; Lower-exploration = ",
  expected_control_profile_count_vector[
    "Lower-exploration"
  ],
  "\n",
  sep = ""
)

cat(
  "\nFigure data:\n"
)

print(
  figure_data %>%
    dplyr::select(
      Treatment,
      Normative_Profile,
      Count,
      Percentage
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nOccupancy inference inherited from Script 016:\n",
  "Fisher's exact p = ",
  format(
    fisher_p,
    digits = 6
  ),
  "\nCramer's V = ",
  format(
    cramers_v,
    digits = 4
  ),
  "\n",
  sep = ""
)

cat(
  "\nSingle bar-chart outputs saved as:\n",
  normalizePath(
    figure_pdf_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    figure_tiff_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    figure_png_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    plot_object_output,
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

cat("\n============================================================\n")
