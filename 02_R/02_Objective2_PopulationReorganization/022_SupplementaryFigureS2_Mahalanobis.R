# ============================================================
# 022_SupplementaryFigureS2_Mahalanobis.R
#
# Supplementary Figure S2
# Covariance-adjusted distance from the normative Control centroid
#
# Definitive upstream dependency:
#   03_Results/Objects/
#   objective2_mahalanobis_normative_atlas_results_robust.rds
#
# This script does not recalculate Mahalanobis distances or inference.
# It only validates the definitive individual-level distances produced
# by Script 015 and generates the publication figure.
#
# Figure design:
#   - two independent groups;
#   - Control in coral (#F07A5A);
#   - Stress in turquoise (#63B8A6);
#   - violin distributions;
#   - coloured boxplots with black outlines;
#   - coloured individual circles with black outlines;
#   - no title, p-value, effect size, descriptive statistics, or legend.
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
# 3. Fixed settings
# ------------------------------------------------------------

required_treatments <- c(
  "Control",
  "Stress"
)

expected_control_n <- 89L
expected_stress_n <- 89L

colour_control <- "#F07A5A"
colour_stress <- "#63B8A6"

group_colours <- c(
  "Control" = colour_control,
  "Stress" = colour_stress
)

jitter_seed <- 2026L
jitter_width <- 0.085
numeric_tolerance <- 1e-10

# ------------------------------------------------------------
# 4. Input and output paths
# ------------------------------------------------------------

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

for (directory_i in c(
  objects_dir,
  figures_dir
)) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

mahalanobis_results_file <- file.path(
  objects_dir,
  "objective2_mahalanobis_normative_atlas_results_robust.rds"
)

if (!file.exists(mahalanobis_results_file)) {
  stop(
    "Required definitive Mahalanobis result object not found:\n",
    mahalanobis_results_file,
    "\nRun the corrected Script 015 before this script."
  )
}

figure_object_output <- file.path(
  objects_dir,
  "supplementary_figure_S2_mahalanobis_plot.rds"
)

figure_data_output <- file.path(
  objects_dir,
  "supplementary_figure_S2_mahalanobis_data.rds"
)

figure_pdf_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S2_Mahalanobis.pdf"
)

figure_tiff_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S2_Mahalanobis.tiff"
)

figure_png_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S2_Mahalanobis.png"
)

session_info_output <- file.path(
  objects_dir,
  "supplementary_figure_S2_mahalanobis_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load and validate the definitive Script 015 object
# ------------------------------------------------------------

mahalanobis_results <- readRDS(
  mahalanobis_results_file
)

if (!is.list(mahalanobis_results)) {
  stop(
    basename(mahalanobis_results_file),
    " does not contain the expected result list."
  )
}

required_result_fields <- c(
  "Comparison_design",
  "Distance_definition",
  "Control_distance_method",
  "Stress_distance_method",
  "Individual_distances"
)

missing_result_fields <- setdiff(
  required_result_fields,
  names(mahalanobis_results)
)

if (length(missing_result_fields) > 0L) {
  stop(
    "The definitive Mahalanobis result object is missing:\n",
    paste(missing_result_fields, collapse = ", ")
  )
}

if (
  !identical(
    as.character(mahalanobis_results[["Comparison_design"]]),
    "Independent Control and Stress populations"
  )
) {
  stop(
    "The Mahalanobis object does not describe the required ",
    "independent-group design."
  )
}

if (
  !identical(
    as.character(mahalanobis_results[["Distance_definition"]]),
    "Square root of classical Mahalanobis squared distance"
  )
) {
  stop(
    "The Mahalanobis object does not contain the definitive ",
    "non-squared distance definition."
  )
}

if (
  !identical(
    as.character(mahalanobis_results[["Control_distance_method"]]),
    "Leave-one-out control atlas"
  ) ||
  !identical(
    as.character(mahalanobis_results[["Stress_distance_method"]]),
    "Complete control atlas"
  )
) {
  stop(
    "The stored Control or Stress distance method is unexpected."
  )
}

individual_distances <- mahalanobis_results[["Individual_distances"]]

if (!is.data.frame(individual_distances)) {
  stop(
    "The Individual_distances field is not a data frame."
  )
}

required_columns <- c(
  "Treatment",
  "Mahalanobis_squared",
  "Mahalanobis_distance",
  "Distance_reference"
)

missing_columns <- setdiff(
  required_columns,
  names(individual_distances)
)

if (length(missing_columns) > 0L) {
  stop(
    "The individual-distance table is missing:\n",
    paste(missing_columns, collapse = ", ")
  )
}

if (
  !is.numeric(individual_distances[["Mahalanobis_squared"]]) ||
  !is.numeric(individual_distances[["Mahalanobis_distance"]])
) {
  stop(
    "The Mahalanobis distance columns must be numeric."
  )
}

figure_data <- individual_distances %>%
  dplyr::transmute(
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Mahalanobis_squared = as.numeric(
      Mahalanobis_squared
    ),
    Mahalanobis_distance = as.numeric(
      Mahalanobis_distance
    ),
    Distance_reference = as.character(
      Distance_reference
    )
  )

if (
  anyNA(figure_data[["Treatment"]]) ||
  any(!is.finite(figure_data[["Mahalanobis_squared"]])) ||
  any(!is.finite(figure_data[["Mahalanobis_distance"]])) ||
  any(figure_data[["Mahalanobis_squared"]] < 0) ||
  any(figure_data[["Mahalanobis_distance"]] < 0)
) {
  stop(
    "Missing, non-finite, or negative Mahalanobis distances were detected."
  )
}

if (
  any(
    abs(
      figure_data[["Mahalanobis_distance"]]^2 -
        figure_data[["Mahalanobis_squared"]]
    ) > numeric_tolerance
  )
) {
  stop(
    "The plotted Mahalanobis distance is not the square root of the ",
    "stored squared distance."
  )
}

group_count_table <- table(
  figure_data[["Treatment"]],
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
  stop(
    "Expected 89 Control and 89 Stress observations."
  )
}

expected_reference <- ifelse(
  figure_data[["Treatment"]] == "Control",
  "Leave-one-out control atlas",
  "Complete control atlas"
)

if (
  anyNA(figure_data[["Distance_reference"]]) ||
  !identical(
    figure_data[["Distance_reference"]],
    expected_reference
  )
) {
  stop(
    "At least one individual has an incorrect distance-reference label."
  )
}

# ------------------------------------------------------------
# 6. Precompute deterministic horizontal jitter
# ------------------------------------------------------------

figure_data <- figure_data %>%
  dplyr::mutate(
    Group_x = dplyr::if_else(
      Treatment == "Control",
      1,
      2
    )
  )

set.seed(
  jitter_seed
)

figure_data <- figure_data %>%
  dplyr::mutate(
    Jitter_x = Group_x +
      stats::runif(
        dplyr::n(),
        min = -jitter_width,
        max = jitter_width
      )
  )

if (
  any(!is.finite(figure_data[["Group_x"]])) ||
  any(!is.finite(figure_data[["Jitter_x"]]))
) {
  stop(
    "The horizontal point positions are invalid."
  )
}

# ------------------------------------------------------------
# 7. Build Supplementary Figure S2
# ------------------------------------------------------------

y_maximum <- max(
  figure_data[["Mahalanobis_distance"]]
)

y_upper_limit <- ceiling(
  y_maximum +
    0.5
)

if (
  !is.finite(y_upper_limit) ||
  y_upper_limit <= 0
) {
  stop(
    "The y-axis upper limit could not be calculated."
  )
}

supplementary_figure_S2 <- ggplot2::ggplot() +
  ggplot2::geom_violin(
    data = figure_data,
    mapping = ggplot2::aes(
      x = Group_x,
      y = Mahalanobis_distance,
      group = Treatment,
      fill = Treatment,
      colour = Treatment
    ),
    width = 0.78,
    trim = TRUE,
    alpha = 0.20,
    linewidth = 0.75,
    show.legend = FALSE
  ) +
  ggplot2::geom_boxplot(
    data = figure_data,
    mapping = ggplot2::aes(
      x = Group_x,
      y = Mahalanobis_distance,
      group = Treatment,
      fill = Treatment
    ),
    width = 0.22,
    outlier.shape = NA,
    alpha = 0.72,
    colour = "black",
    linewidth = 0.60,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    data = figure_data,
    mapping = ggplot2::aes(
      x = Jitter_x,
      y = Mahalanobis_distance,
      fill = Treatment
    ),
    shape = 21,
    size = 2.05,
    stroke = 0.38,
    alpha = 0.78,
    colour = "black",
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = group_colours,
    breaks = required_treatments
  ) +
  ggplot2::scale_colour_manual(
    values = group_colours,
    breaks = required_treatments
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0.55, 2.45),
    breaks = c(1, 2),
    labels = required_treatments,
    expand = ggplot2::expansion(
      mult = c(0, 0)
    )
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      from = 0,
      to = y_upper_limit,
      by = 2
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.035)
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(
      0,
      y_upper_limit
    ),
    clip = "off"
  ) +
  ggplot2::labs(
    x = NULL,
    y = paste0(
      "Mahalanobis distance from the\n",
      "normative Control centroid"
    )
  ) +
  ggplot2::theme_classic(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "none",
    axis.text = ggplot2::element_text(
      colour = "black"
    ),
    axis.text.x = ggplot2::element_text(
      size = 11.5
    ),
    axis.title.y = ggplot2::element_text(
      size = 12,
      margin = ggplot2::margin(
        r = 9
      )
    ),
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.65
    ),
    axis.ticks = ggplot2::element_line(
      colour = "black",
      linewidth = 0.55
    ),
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = ggplot2::margin(
      t = 8,
      r = 12,
      b = 8,
      l = 8
    )
  )

print(
  supplementary_figure_S2
)

# ------------------------------------------------------------
# 8. Save figure and reproducibility objects
# ------------------------------------------------------------

figure_data_object <- list(
  Source_file = mahalanobis_results_file,
  Comparison_design =
    mahalanobis_results[["Comparison_design"]],
  Distance_definition =
    mahalanobis_results[["Distance_definition"]],
  Control_distance_method =
    mahalanobis_results[["Control_distance_method"]],
  Stress_distance_method =
    mahalanobis_results[["Stress_distance_method"]],
  Palette = group_colours,
  Jitter_seed = jitter_seed,
  Figure_data = figure_data
)

saveRDS(
  supplementary_figure_S2,
  figure_object_output
)

saveRDS(
  figure_data_object,
  figure_data_output
)

ggplot2::ggsave(
  filename = figure_pdf_output,
  plot = supplementary_figure_S2,
  width = 5.4,
  height = 6.2,
  units = "in",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_tiff_output,
  plot = supplementary_figure_S2,
  width = 5.4,
  height = 6.2,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_png_output,
  plot = supplementary_figure_S2,
  width = 5.4,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "white"
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 9. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  figure_object_output,
  figure_data_output,
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
    "The following Supplementary Figure S2 outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 10. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SUPPLEMENTARY FIGURE S2 - MAHALANOBIS DISTANCE\n")
cat("============================================================\n")

cat(
  "\nDesign: two independent populations.\n",
  "Control distances: leave-one-out Control atlas.\n",
  "Stress distances: complete Control atlas.\n",
  sep = ""
)

cat(
  "\nFigure content: violin distributions, coloured boxplots, and ",
  "individual coloured circles with black outlines. No title, ",
  "legend, p-value, effect size, or descriptive annotation was ",
  "placed inside the figure.\n",
  sep = ""
)

cat(
  "\nGroup counts:\n",
  "Control = ",
  group_count_values[["Control"]],
  "\nStress = ",
  group_count_values[["Stress"]],
  "\n",
  sep = ""
)

cat(
  "\nPalette:\n",
  "Control = ",
  colour_control,
  "\nStress = ",
  colour_stress,
  "\n",
  sep = ""
)

cat("\nFiles saved as:\n")

for (output_file in expected_output_files) {
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

cat("\n============================================================\n")
