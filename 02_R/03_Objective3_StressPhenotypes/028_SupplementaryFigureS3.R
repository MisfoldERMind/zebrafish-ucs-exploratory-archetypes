# ============================================================
# 028_SupplementaryFigureS3_Objective3_v6.R
#
# Objective 3 - Supplementary Figure S3
#
# Panel A: kernel-density distributions of the four definitive
#          Control-referenced robust behavioural variables in the
#          89 UCS-exposed zebrafish.
#
# Panel B: Spearman correlation matrix of the same four robust
#          behavioural variables, inherited from corrected Script 022.
#
# Panel C: k-dependent internal-validation indices for the de novo
#          weighted Louvain analysis:
#          - Calinski-Harabasz
#          - Silhouette
#          - Davies-Bouldin
#
# Definitive Objective 3 analytical configuration:
# n = 89 Stress fish;
# traceability = Author + ID;
# variables = distance_rz, crossing_rz, top_entry_rz, top_time_rz;
# Control-referenced within-Author robust normalisation inherited
# unchanged from Objectives 1-2;
# Euclidean distance;
# similarity = 1 / (1 + distance);
# weighted undirected union-kNN graph;
# k evaluated from 5 to 80;
# selected stable two-cluster plateau = k 41-56;
# selected representative k = 48.
#
# IMPORTANT:
# - This script does NOT re-normalise the data.
# - It does NOT recompute the Spearman analysis.
# - It does NOT rerun Louvain clustering.
# - It does NOT display modularity because the manuscript reproduces
#   clustering quality using Silhouette, Calinski-Harabasz and
#   Davies-Bouldin.
# - The optimisation metrics are read directly from corrected Script 024.
#
# Graphical structure and style are adapted from:
# 010.2_SupplementaryFigureS1_Objective1.R
# ============================================================


# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

if (!dir.exists(base_dir)) {
  stop("Project directory not found:\n", base_dir)
}

setwd(base_dir)


# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "cowplot",
  "scales"
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
    "Install the following packages before running the script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(cowplot)
  library(scales)
})

# cowplot internally opens a null graphics device when ggdraw()/draw_plot()
# converts ggplot objects to grobs. On some Windows configurations, the
# default PDF null device cannot resolve Arial and produces PDF CID-font
# errors. Use the PNG null device for this internal rendering step only.
# Final PDF export remains cairo_pdf below.
cowplot::set_null_device("png")


# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(
  "01_Data",
  "Processed"
)

objects_dir <- file.path(
  "03_Results",
  "Objects"
)

figures_dir <- file.path(
  "03_Results",
  "Figures"
)

if (!dir.exists(processed_dir)) {
  stop("Processed-data directory not found:\n", processed_dir)
}

if (!dir.exists(objects_dir)) {
  stop("Objects directory not found:\n", objects_dir)
}

if (!dir.exists(figures_dir)) {
  dir.create(
    figures_dir,
    recursive = TRUE
  )
}

stress_file <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

setup_file <- file.path(
  objects_dir,
  "objective3_stress_architecture_setup.rds"
)

correlation_file <- file.path(
  objects_dir,
  "objective3_stress_correlation_matrix.rds"
)

optimisation_file <- file.path(
  objects_dir,
  "objective3_louvain_k_optimisation_results.rds"
)

selected_configuration_file <- file.path(
  objects_dir,
  "objective3_selected_louvain_configuration.rds"
)

required_input_files <- c(
  stress_file,
  setup_file,
  correlation_file,
  optimisation_file,
  selected_configuration_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive Objective 3 input files were not found:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    ),
    "\n\nRun the corrected Objective 3 scripts 020, 022 and 024 first."
  )
}


# ------------------------------------------------------------
# 4. Load definitive Objective 3 objects
# ------------------------------------------------------------

stress <- readRDS(
  stress_file
)

setup_object <- readRDS(
  setup_file
)

correlation_object <- readRDS(
  correlation_file
)

optimisation_results <- readRDS(
  optimisation_file
)

selected_configuration <- readRDS(
  selected_configuration_file
)

if (!is.data.frame(stress)) {
  stop(
    basename(stress_file),
    " does not contain a data frame."
  )
}

if (!is.list(setup_object)) {
  stop(
    basename(setup_file),
    " does not contain the expected setup list."
  )
}

if (!is.list(correlation_object)) {
  stop(
    basename(correlation_file),
    " does not contain the expected correlation-results list."
  )
}

if (!is.data.frame(optimisation_results)) {
  stop(
    basename(optimisation_file),
    " does not contain a data frame."
  )
}

if (!is.list(selected_configuration)) {
  stop(
    basename(selected_configuration_file),
    " does not contain the expected selected-configuration list."
  )
}


# ------------------------------------------------------------
# 5. Fixed definitive Objective 3 configuration
# ------------------------------------------------------------

expected_n <- 89L
expected_traceability <- "Author + ID"

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

variable_labels <- c(
  distance_rz = "Distance travelled",
  crossing_rz = "Line crossings",
  top_entry_rz = "Upper-zone entries",
  top_time_rz = "Upper-zone time"
)

expected_k_sequence <- 5:80
stable_k_min <- 41L
stable_k_max <- 56L
selected_k <- 48L
expected_number_of_clusters <- 2L

expected_selected_calinski_harabasz <- 55.0343
expected_selected_silhouette <- 0.3341
expected_selected_davies_bouldin <- 0.9712

numeric_tolerance <- 1e-10
metric_tolerance <- 5e-4

# Same definitive paper palette used in the main figures.
main_palette <- c(
  coral = "#F07A5A",
  green = "#63B8A6"
)

font_family <- "Arial"


# ------------------------------------------------------------
# 6. Audit the definitive Stress dataset
# ------------------------------------------------------------

required_stress_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_stress_columns <- setdiff(
  required_stress_columns,
  names(stress)
)

if (length(missing_stress_columns) > 0L) {
  stop(
    "The definitive Stress dataset is missing the following columns:\n",
    paste(
      missing_stress_columns,
      collapse = ", "
    )
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
    Treatment = as.character(Treatment)
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

if (nrow(stress) != expected_n) {
  stop(
    "The Objective 3 Stress dataset contains ",
    nrow(stress),
    " fish; ",
    expected_n,
    " were expected."
  )
}

if (
  anyNA(stress$Treatment) ||
  !all(stress$Treatment == "Stress")
) {
  stop(
    "The analytical dataset must contain only Stress fish."
  )
}

expected_traceability_id <- paste(
  stress$Author,
  stress$ID,
  sep = " || "
)

if (
  anyNA(stress$Author) ||
  any(stress$Author == "") ||
  anyNA(stress$ID) ||
  any(stress$ID == "") ||
  anyNA(stress$Traceability_ID) ||
  any(stress$Traceability_ID == "") ||
  anyDuplicated(stress$Traceability_ID) > 0L ||
  !identical(
    stress$Traceability_ID,
    expected_traceability_id
  )
) {
  stop(
    "Author + ID traceability failed in the definitive Stress dataset."
  )
}

if (
  dplyr::n_distinct(
    stress$Author
  ) != 4L
) {
  stop(
    "The definitive Stress population must contain four Authors/cohorts."
  )
}

non_numeric_robust_variables <- robust_variables[
  !vapply(
    stress[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_robust_variables) > 0L) {
  stop(
    "The following robust behavioural variables are not numeric:\n",
    paste(
      non_numeric_robust_variables,
      collapse = ", "
    )
  )
}

robust_matrix <- as.matrix(
  stress[
    robust_variables
  ]
)

storage.mode(
  robust_matrix
) <- "double"

if (
  !identical(
    dim(robust_matrix),
    c(
      expected_n,
      length(robust_variables)
    )
  ) ||
  any(
    !is.finite(
      robust_matrix
    )
  )
) {
  stop(
    "The definitive robust Stress matrix is invalid."
  )
}


# ------------------------------------------------------------
# 7. Audit dependence on corrected Script 020
# ------------------------------------------------------------

required_setup_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Behavioural_variables",
  "Robust_variables",
  "Reference_space",
  "Normalisation_formula"
)

missing_setup_elements <- setdiff(
  required_setup_elements,
  names(setup_object)
)

if (length(missing_setup_elements) > 0L) {
  stop(
    "The Objective 3 setup object is missing:\n",
    paste(
      missing_setup_elements,
      collapse = ", "
    )
  )
}

if (
  !identical(
    as.character(
      setup_object[["Analytical_population"]]
    ),
    "Stress only"
  ) ||
  as.integer(
    setup_object[["Stress_n"]]
  ) != expected_n
) {
  stop(
    "The setup object does not describe the definitive Stress-only population."
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
    "The setup object does not contain the definitive Objective 3 variables."
  )
}


# ------------------------------------------------------------
# 8. Audit and recover the stored Stress Spearman matrix
# ------------------------------------------------------------

required_correlation_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Correlation_method",
  "Correlation_matrix",
  "Robust_variables"
)

missing_correlation_elements <- setdiff(
  required_correlation_elements,
  names(correlation_object)
)

if (length(missing_correlation_elements) > 0L) {
  stop(
    "The corrected Script 022 correlation object is missing:\n",
    paste(
      missing_correlation_elements,
      collapse = ", "
    )
  )
}

if (
  !identical(
    as.character(
      correlation_object[["Analytical_population"]]
    ),
    "Stress only"
  ) ||
  as.integer(
    correlation_object[["Stress_n"]]
  ) != expected_n ||
  !identical(
    as.character(
      correlation_object[["Correlation_method"]]
    ),
    "Spearman"
  )
) {
  stop(
    "The loaded correlation object is not the definitive Stress Spearman analysis."
  )
}

if (
  !identical(
    as.character(
      correlation_object[["Robust_variables"]]
    ),
    robust_variables
  )
) {
  stop(
    "The correlation object uses variables different from the definitive Objective 3 robust variables."
  )
}

spearman_matrix <- as.matrix(
  correlation_object[[
    "Correlation_matrix"
  ]]
)

if (
  !identical(
    dim(spearman_matrix),
    c(4L, 4L)
  ) ||
  !identical(
    rownames(spearman_matrix),
    robust_variables
  ) ||
  !identical(
    colnames(spearman_matrix),
    robust_variables
  ) ||
  any(
    !is.finite(
      spearman_matrix
    )
  ) ||
  max(
    abs(
      spearman_matrix -
        t(spearman_matrix)
    )
  ) > numeric_tolerance ||
  max(
    abs(
      diag(spearman_matrix) -
        1
    )
  ) > numeric_tolerance
) {
  stop(
    "The stored Objective 3 Spearman matrix failed the integrity audit."
  )
}

# Audit against the definitive console results from corrected Script 022.
expected_spearman_rounded <- matrix(
  c(
    1.000, 0.638, 0.641, 0.553,
    0.638, 1.000, 0.851, 0.684,
    0.641, 0.851, 1.000, 0.864,
    0.553, 0.684, 0.864, 1.000
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    robust_variables,
    robust_variables
  )
)

if (
  !identical(
    round(
      spearman_matrix,
      3
    ),
    expected_spearman_rounded
  )
) {
  stop(
    "The stored Stress Spearman matrix does not reproduce the definitive corrected Objective 3 results."
  )
}


# ------------------------------------------------------------
# 9. Audit the definitive k-optimisation object
# ------------------------------------------------------------

required_optimisation_columns <- c(
  "k",
  "Number_of_clusters",
  "Calinski_Harabasz",
  "Silhouette",
  "Davies_Bouldin"
)

missing_optimisation_columns <- setdiff(
  required_optimisation_columns,
  names(optimisation_results)
)

if (length(missing_optimisation_columns) > 0L) {
  stop(
    "The corrected Objective 3 k-optimisation object is missing:\n",
    paste(
      missing_optimisation_columns,
      collapse = ", "
    )
  )
}

optimisation_results <- optimisation_results %>%
  dplyr::mutate(
    k = as.integer(k),
    Number_of_clusters = as.integer(
      Number_of_clusters
    )
  ) %>%
  dplyr::arrange(
    k
  )

if (
  !identical(
    optimisation_results$k,
    expected_k_sequence
  )
) {
  stop(
    "The Objective 3 optimisation object does not contain the complete ordered sequence k = 5:80."
  )
}

if (
  anyNA(
    optimisation_results$Number_of_clusters
  ) ||
  any(
    optimisation_results$Number_of_clusters < 1L
  )
) {
  stop(
    "Invalid cluster counts were detected in the Objective 3 optimisation object."
  )
}

metric_columns <- c(
  "Calinski_Harabasz",
  "Silhouette",
  "Davies_Bouldin"
)

for (metric_i in metric_columns) {
  metric_values <- optimisation_results[[metric_i]]

  if (!is.numeric(metric_values)) {
    stop(
      metric_i,
      " is not numeric in the optimisation object."
    )
  }

  invalid_metric <- !is.na(
    metric_values
  ) &
    !is.finite(
      metric_values
    )

  if (any(invalid_metric)) {
    stop(
      metric_i,
      " contains non-finite non-missing values."
    )
  }
}

selected_result <- optimisation_results %>%
  dplyr::filter(
    k == selected_k
  )

if (
  nrow(selected_result) != 1L ||
  selected_result$Number_of_clusters[[1L]] !=
    expected_number_of_clusters
) {
  stop(
    "The definitive two-cluster solution at k = 48 was not found."
  )
}

if (
  !is.finite(
    selected_result$Calinski_Harabasz[[1L]]
  ) ||
  !is.finite(
    selected_result$Silhouette[[1L]]
  ) ||
  !is.finite(
    selected_result$Davies_Bouldin[[1L]]
  ) ||
  abs(
    selected_result$Calinski_Harabasz[[1L]] -
      expected_selected_calinski_harabasz
  ) > metric_tolerance ||
  abs(
    selected_result$Silhouette[[1L]] -
      expected_selected_silhouette
  ) > metric_tolerance ||
  abs(
    selected_result$Davies_Bouldin[[1L]] -
      expected_selected_davies_bouldin
  ) > metric_tolerance
) {
  stop(
    "The k = 48 optimisation metrics do not reproduce the definitive corrected Objective 3 results."
  )
}


# ------------------------------------------------------------
# 10. Audit the definitive selected configuration
# ------------------------------------------------------------

required_configuration_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Selected_plateau_start_k",
  "Selected_plateau_end_k",
  "Selected_plateau_length",
  "Selected_plateau_number_of_communities",
  "Selected_k",
  "Selected_number_of_communities",
  "Selected_Calinski_Harabasz",
  "Selected_Silhouette",
  "Selected_Davies_Bouldin"
)

missing_configuration_elements <- setdiff(
  required_configuration_elements,
  names(selected_configuration)
)

if (length(missing_configuration_elements) > 0L) {
  stop(
    "The corrected Objective 3 selected-configuration object is missing:\n",
    paste(
      missing_configuration_elements,
      collapse = ", "
    )
  )
}

if (
  !identical(
    as.character(
      selected_configuration[["Analytical_population"]]
    ),
    "Stress only"
  ) ||
  as.integer(
    selected_configuration[["Stress_n"]]
  ) != expected_n ||
  as.integer(
    selected_configuration[["Selected_plateau_start_k"]]
  ) != stable_k_min ||
  as.integer(
    selected_configuration[["Selected_plateau_end_k"]]
  ) != stable_k_max ||
  as.integer(
    selected_configuration[["Selected_plateau_length"]]
  ) != 16L ||
  as.integer(
    selected_configuration[["Selected_plateau_number_of_communities"]]
  ) != expected_number_of_clusters ||
  as.integer(
    selected_configuration[["Selected_k"]]
  ) != selected_k ||
  as.integer(
    selected_configuration[["Selected_number_of_communities"]]
  ) != expected_number_of_clusters
) {
  stop(
    "The selected-configuration object does not reproduce the definitive k = 48, two-cluster solution within the k = 41-56 plateau."
  )
}

if (
  abs(
    as.numeric(
      selected_configuration[["Selected_Calinski_Harabasz"]]
    ) -
      expected_selected_calinski_harabasz
  ) > metric_tolerance ||
  abs(
    as.numeric(
      selected_configuration[["Selected_Silhouette"]]
    ) -
      expected_selected_silhouette
  ) > metric_tolerance ||
  abs(
    as.numeric(
      selected_configuration[["Selected_Davies_Bouldin"]]
    ) -
      expected_selected_davies_bouldin
  ) > metric_tolerance
) {
  stop(
    "The selected-configuration metrics do not reproduce the definitive Objective 3 values."
  )
}


# ------------------------------------------------------------
# 11. Final graphical parameters
# ------------------------------------------------------------

cluster_levels <- sort(
  unique(
    optimisation_results$Number_of_clusters
  )
)

optimisation_results <- optimisation_results %>%
  dplyr::mutate(
    Cluster_factor = factor(
      Number_of_clusters,
      levels = cluster_levels
    )
  )


# ------------------------------------------------------------
# 12. Shared graphical theme
# ------------------------------------------------------------

theme_s3 <- ggplot2::theme_classic(
  base_size = 11,
  base_family = font_family
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
    legend.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    legend.box.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    strip.text = ggplot2::element_text(
      face = "bold",
      colour = "black",
      size = 10
    ),
    axis.title = ggplot2::element_text(
      colour = "black",
      size = 10
    ),
    axis.text = ggplot2::element_text(
      colour = "black",
      size = 9
    ),
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.5
    ),
    axis.ticks = ggplot2::element_line(
      colour = "black",
      linewidth = 0.5
    ),
    legend.title = ggplot2::element_text(
      colour = "black",
      size = 10
    ),
    legend.text = ggplot2::element_text(
      colour = "black",
      size = 9
    ),
    plot.margin = ggplot2::margin(
      t = 6,
      r = 6,
      b = 6,
      l = 6
    )
  )


# ------------------------------------------------------------
# 13. Panel A: kernel-density distributions
# ------------------------------------------------------------

density_data <- stress %>%
  dplyr::select(
    dplyr::all_of(
      robust_variables
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable_code",
    values_to = "Robust_Z_Score"
  ) %>%
  dplyr::mutate(
    Variable = factor(
      Variable_code,
      levels = robust_variables,
      labels = unname(
        variable_labels[
          robust_variables
        ]
      ),
      ordered = TRUE
    )
  )

if (
  nrow(density_data) !=
    expected_n *
      length(robust_variables) ||
  anyNA(
    density_data$Variable
  ) ||
  any(
    !is.finite(
      density_data$Robust_Z_Score
    )
  )
) {
  stop(
    "The kernel-density plotting dataset failed the integrity audit."
  )
}

plot_A_core <- ggplot2::ggplot(
  density_data,
  ggplot2::aes(
    x = Robust_Z_Score,
    y = ggplot2::after_stat(
      density
    )
  )
) +
  ggplot2::geom_density(
    fill = main_palette[["coral"]],
    colour = main_palette[["coral"]],
    alpha = 0.42,
    linewidth = 0.9,
    adjust = 1.05
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    linetype = "dashed",
    colour = "grey65"
  ) +
  ggplot2::facet_wrap(
    ~ Variable,
    ncol = 2,
    scales = "free"
  ) +
  ggplot2::labs(
    x = "Control-referenced robust z-score",
    y = "Density"
  ) +
  theme_s3 +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank()
  )


# ------------------------------------------------------------
# 14. Panel B: stored Spearman correlation matrix
# ------------------------------------------------------------

correlation_data <- as.data.frame(
  as.table(
    spearman_matrix
  ),
  stringsAsFactors = FALSE
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(
    Variable_y_code = Var1,
    Variable_x_code = Var2,
    Spearman_rho = Freq
  ) %>%
  dplyr::mutate(
    Variable_x = factor(
      Variable_x_code,
      levels = robust_variables,
      labels = unname(
        variable_labels[
          robust_variables
        ]
      ),
      ordered = TRUE
    ),
    Variable_y = factor(
      Variable_y_code,
      levels = rev(
        robust_variables
      ),
      labels = rev(
        unname(
          variable_labels[
            robust_variables
          ]
        )
      ),
      ordered = TRUE
    ),
    Text_colour = ifelse(
      Spearman_rho >= 0.78,
      "white",
      "black"
    )
  )

if (
  nrow(correlation_data) != 16L ||
  anyNA(
    correlation_data$Variable_x
  ) ||
  anyNA(
    correlation_data$Variable_y
  ) ||
  any(
    !is.finite(
      correlation_data$Spearman_rho
    )
  )
) {
  stop(
    "The stored Spearman matrix could not be converted to the plotting table."
  )
}

plot_B_core <- ggplot2::ggplot(
  correlation_data,
  ggplot2::aes(
    x = Variable_x,
    y = Variable_y,
    fill = Spearman_rho
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.9
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf(
        "%.2f",
        Spearman_rho
      ),
      colour = Text_colour
    ),
    family = font_family,
    size = 3.5,
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_fill_gradientn(
    colours = c(
      main_palette[["coral"]],
      "white",
      "#CFE7DE",
      main_palette[["green"]]
    ),
    values = scales::rescale(
      c(
        -1,
        0,
        0.35,
        1
      )
    ),
    limits = c(
      -1,
      1
    ),
    name = expression(
      r[s]
    )
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  theme_s3 +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 35,
      hjust = 1,
      vjust = 1,
      colour = "black",
      size = 9
    ),
    axis.text.y = ggplot2::element_text(
      colour = "black",
      size = 9
    ),
    legend.position = "right",
    legend.key.height = grid::unit(
      18,
      "mm"
    )
  )


# ------------------------------------------------------------
# 15. Helper for the three metric plots in Panel C
# ------------------------------------------------------------

make_k_metric_plot <- function(
    data,
    metric_column,
    y_axis_label,
    show_x_axis = FALSE,
    show_legend = FALSE
) {

  plotting_data <- data %>%
    dplyr::filter(
      is.finite(
        .data[[metric_column]]
      )
    )

  if (nrow(plotting_data) == 0L) {
    stop(
      "No finite values are available for ",
      metric_column,
      "."
    )
  }

  plot_i <- ggplot2::ggplot(
    plotting_data,
    ggplot2::aes(
      x = k,
      y = .data[[metric_column]]
    )
  ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = stable_k_min,
      xmax = stable_k_max,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey75",
      alpha = 0.25
    ) +
    ggplot2::geom_vline(
      xintercept = selected_k,
      linetype = "dashed",
      linewidth = 0.65,
      colour = "black"
    ) +
    ggplot2::geom_line(
      colour = "black",
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = Cluster_factor
      ),
      size = 2.05,
      show.legend = show_legend
    ) +
    ggplot2::geom_point(
      data = plotting_data %>%
        dplyr::filter(
          k == selected_k
        ),
      shape = 21,
      size = 3.5,
      stroke = 0.9,
      fill = "white",
      colour = "black",
      inherit.aes = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_discrete(
      drop = FALSE,
      name = "Number of\nclusters"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        5,
        80
      ),
      breaks = seq(
        10,
        80,
        by = 10
      ),
      expand = ggplot2::expansion(
        mult = c(
          0.01,
          0.01
        )
      )
    ) +
    ggplot2::labs(
      x = if (
        show_x_axis
      ) {
        "k"
      } else {
        NULL
      },
      y = y_axis_label
    ) +
    theme_s3 +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = if (
        show_legend
      ) {
        "right"
      } else {
        "none"
      },
      axis.title.x = if (
        show_x_axis
      ) {
        ggplot2::element_text(
          colour = "black",
          size = 10
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text.x = if (
        show_x_axis
      ) {
        ggplot2::element_text(
          colour = "black",
          size = 9
        )
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (
        show_x_axis
      ) {
        ggplot2::element_line(
          colour = "black"
        )
      } else {
        ggplot2::element_blank()
      }
    )

  plot_i
}


# ------------------------------------------------------------
# 16. Panel C: three clustering-quality indices
# ------------------------------------------------------------

plot_C1 <- make_k_metric_plot(
  data = optimisation_results,
  metric_column = "Calinski_Harabasz",
  y_axis_label = "Calinski-Harabasz",
  show_x_axis = FALSE,
  show_legend = TRUE
)

plot_C2 <- make_k_metric_plot(
  data = optimisation_results,
  metric_column = "Silhouette",
  y_axis_label = "Silhouette",
  show_x_axis = FALSE,
  show_legend = FALSE
)

plot_C3 <- make_k_metric_plot(
  data = optimisation_results,
  metric_column = "Davies_Bouldin",
  y_axis_label = "Davies-Bouldin",
  show_x_axis = TRUE,
  show_legend = FALSE
)

panel_C_core <- cowplot::plot_grid(
  plot_C1,
  plot_C2,
  plot_C3,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(
    1,
    1,
    1
  )
)


# ------------------------------------------------------------
# 17. Add visible panel letters
# ------------------------------------------------------------

add_panel_label <- function(
    plot_object,
    panel_label,
    left_margin = 0.040
) {

  cowplot::ggdraw() +
    cowplot::draw_plot(
      plot_object,
      x = left_margin,
      y = 0,
      width = 1 - left_margin,
      height = 1
    ) +
    cowplot::draw_label(
      label = panel_label,
      x = 0.006,
      y = 0.995,
      hjust = 0,
      vjust = 1,
      fontfamily = font_family,
      fontface = "bold",
      size = 17,
      colour = "black"
    ) +
    ggplot2::theme(
      plot.background =
        ggplot2::element_rect(
          fill = "white",
          colour = NA
        )
    )
}

panel_A <- add_panel_label(
  plot_A_core,
  "A"
)

panel_B <- add_panel_label(
  plot_B_core,
  "B"
)

panel_C <- add_panel_label(
  panel_C_core,
  "C"
)


# ------------------------------------------------------------
# 18. Assemble the final supplementary figure
# ------------------------------------------------------------

left_column <- cowplot::plot_grid(
  panel_A,
  panel_B,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(
    1.02,
    0.98
  )
)

final_panel_core <- cowplot::plot_grid(
  left_column,
  panel_C,
  ncol = 2,
  align = "h",
  axis = "tb",
  rel_widths = c(
    1.00,
    1.28
  )
)

final_panel <- cowplot::ggdraw() +
  cowplot::draw_plot(
    final_panel_core,
    x = 0,
    y = 0,
    width = 1,
    height = 1
  ) +
  ggplot2::theme(
    plot.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      )
  )

print(
  final_panel
)


# ------------------------------------------------------------
# 19. Export the final figure
# ------------------------------------------------------------

pdf_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S3_Objective3.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S3_Objective3.tiff"
)

png_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S3_Objective3.png"
)

ggplot2::ggsave(
  filename = pdf_output,
  plot = final_panel,
  width = 14,
  height = 9,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = final_panel,
  width = 14,
  height = 9,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = png_output,
  plot = final_panel,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# 20. Verify output files
# ------------------------------------------------------------

expected_output_files <- c(
  pdf_output,
  tiff_output,
  png_output
)

missing_output_files <- expected_output_files[
  !file.exists(
    expected_output_files
  )
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following supplementary-figure files were not created:\n",
    paste(
      missing_output_files,
      collapse = "\n"
    )
  )
}

output_file_information <- file.info(
  expected_output_files
)

if (
  any(
    is.na(
      output_file_information$size
    )
  ) ||
  any(
    output_file_information$size <= 0
  )
) {
  stop(
    "At least one supplementary-figure output is empty or inaccessible."
  )
}


# ------------------------------------------------------------
# 21. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 3 - SUPPLEMENTARY FIGURE S3\n")
cat("============================================================\n")

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
  "Analytical variables: ",
  paste(
    robust_variables,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nPanel A: kernel-density distributions of the four ",
  "Control-referenced robust behavioural variables.\n",
  sep = ""
)

cat(
  "\nPanel B: stored Spearman correlation matrix from corrected Script 022:\n"
)

print(
  round(
    spearman_matrix,
    3
  )
)

cat(
  "\nPanel C k range: ",
  min(
    optimisation_results$k
  ),
  "-",
  max(
    optimisation_results$k
  ),
  "\n",
  sep = ""
)

cat(
  "Stable two-cluster plateau: k = ",
  stable_k_min,
  "-",
  stable_k_max,
  "\n",
  sep = ""
)

cat(
  "Selected k: ",
  selected_k,
  "\n",
  sep = ""
)

cat(
  "Selected number of clusters: ",
  selected_result$Number_of_clusters[[1L]],
  "\n",
  sep = ""
)

cat(
  "Calinski-Harabasz at k = 48: ",
  sprintf(
    "%.4f",
    selected_result$Calinski_Harabasz[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Silhouette at k = 48: ",
  sprintf(
    "%.4f",
    selected_result$Silhouette[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Davies-Bouldin at k = 48: ",
  sprintf(
    "%.4f",
    selected_result$Davies_Bouldin[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Modularity is intentionally omitted from this supplementary figure.\n"
)

cat(
  "\nFiles saved as:\n",
  normalizePath(
    pdf_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    tiff_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    png_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)

cat("\n============================================================\n")
