# ============================================================
# 010.2_SupplementaryFigureS1_Objective1.R
#
# Objective 1 - Supplementary Figure S1
#
# Panel A: density distributions of the four raw behavioural variables
# Panel B: Spearman correlation matrix
# Panel C: Rajput-based optimisation of k using the three
# internal-validation indices (Calinski-Harabasz, silhouette
# and Davies-Bouldin)
#
# Final analytical configuration:
# n = 89; robust normalisation = (x - median)/(1.4826 x raw MAD);
# Euclidean distance; similarity = 1/(1 + D);
# weighted undirected union kNN graph; selected k = 45;
# stable identical-partition plateau = k 44-46.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

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

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(cowplot)
library(scales)

# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

control_file <- file.path(
  "01_Data",
  "Processed",
  "control.rds"
)

optimisation_file <- file.path(
  "03_Results",
  "Objects",
  "objective1_louvain_optimisation_results_robust.rds"
)

figures_dir <- file.path(
  "03_Results",
  "Figures"
)

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

required_input_files <- c(control_file, optimisation_file)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 4. Load and audit the definitive control dataset
# ------------------------------------------------------------

control <- readRDS(control_file)

if (!is.data.frame(control)) {
  stop(basename(control_file), " does not contain a data frame.")
}

expected_n <- 89L

if (nrow(control) != expected_n) {
  stop(
    "The control dataset contains ",
    nrow(control),
    " fish; ",
    expected_n,
    " were expected."
  )
}

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

missing_behavioural_variables <- setdiff(
  behavioural_variables,
  names(control)
)

if (length(missing_behavioural_variables) > 0L) {
  stop(
    "The control dataset is missing the following variables:\n",
    paste(missing_behavioural_variables, collapse = ", ")
  )
}

non_numeric_variables <- behavioural_variables[
  !vapply(
    control[behavioural_variables],
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

behavioural_matrix <- as.matrix(control[behavioural_variables])
storage.mode(behavioural_matrix) <- "double"

if (any(!is.finite(behavioural_matrix))) {
  stop(
    "Missing or non-finite values were detected in the ",
    "behavioural variables."
  )
}

# ------------------------------------------------------------
# 5. Load and audit the definitive k-optimisation object
# ------------------------------------------------------------

optimisation_results <- readRDS(optimisation_file)

if (!is.data.frame(optimisation_results)) {
  stop(
    basename(optimisation_file),
    " does not contain a data frame."
  )
}

required_optimisation_columns <- c(
  "k",
  "Clusters",
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
    "The optimisation object is missing the following columns:\n",
    paste(missing_optimisation_columns, collapse = ", ")
  )
}

optimisation_results <- optimisation_results %>%
  dplyr::mutate(
    k = as.integer(k),
    Clusters = as.integer(Clusters)
  ) %>%
  dplyr::arrange(k)

if (!identical(optimisation_results$k, 5:80)) {
  stop(
    "The optimisation object does not contain the complete ",
    "ordered sequence k = 5:80."
  )
}

if (
  nrow(
    optimisation_results %>%
      dplyr::filter(k == 45L, Clusters == 2L)
  ) != 1L
) {
  stop(
    "The expected two-community solution at k = 45 was not found."
  )
}

optimisation_numeric <- as.matrix(
  optimisation_results[
    c(
      "k",
      "Clusters",
      "Calinski_Harabasz",
      "Silhouette",
      "Davies_Bouldin"
    )
  ]
)

storage.mode(optimisation_numeric) <- "double"

if (any(!is.finite(optimisation_numeric))) {
  stop(
    "The optimisation object contains missing or non-finite values."
  )
}

# ------------------------------------------------------------
# 6. Final graphical parameters
# ------------------------------------------------------------

stable_k_min <- 44L
stable_k_max <- 46L
selected_k <- 45L

stable_plateau_results <- optimisation_results %>%
  dplyr::filter(
    dplyr::between(k, stable_k_min, stable_k_max)
  )

if (
  nrow(stable_plateau_results) !=
    stable_k_max - stable_k_min + 1L ||
  any(stable_plateau_results$Clusters != 2L)
) {
  stop(
    "The expected two-community plateau at k = 44-46 was not found."
  )
}

main_palette <- c(
  coral = "#E9897D",
  green = "#58A987"
)

font_family <- "Arial"

cluster_levels <- sort(unique(optimisation_results$Clusters))

optimisation_results <- optimisation_results %>%
  dplyr::mutate(
    Cluster_factor = factor(
      Clusters,
      levels = cluster_levels
    )
  )

# ------------------------------------------------------------
# 7. Shared graphical theme
# ------------------------------------------------------------

theme_s1 <- ggplot2::theme_classic(
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
# 8. Panel A: density distributions of raw variables
# ------------------------------------------------------------

density_data <- control %>%
  dplyr::select(
    dplyr::all_of(behavioural_variables)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable_code",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Variable = factor(
      Variable_code,
      levels = behavioural_variables,
      labels = unname(
        variable_labels[behavioural_variables]
      ),
      ordered = TRUE
    )
  )

if (
  nrow(density_data) !=
  expected_n * length(behavioural_variables)
) {
  stop(
    "The density plotting dataset has an incorrect number of rows."
  )
}

plot_A_core <- ggplot2::ggplot(
  density_data,
  ggplot2::aes(
    x = Value,
    y = ggplot2::after_stat(density)
  )
) +
  ggplot2::geom_density(
    fill = main_palette[["coral"]],
    colour = main_palette[["coral"]],
    alpha = 0.42,
    linewidth = 0.9,
    adjust = 1.05
  ) +
  ggplot2::facet_wrap(
    ~ Variable,
    ncol = 2,
    scales = "free"
  ) +
  ggplot2::labs(
    x = "Behavioural value",
    y = "Density"
  ) +
  theme_s1 +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank()
  )

# ------------------------------------------------------------
# 9. Panel B: Spearman correlation matrix
# ------------------------------------------------------------

spearman_matrix <- stats::cor(
  behavioural_matrix,
  method = "spearman",
  use = "pairwise.complete.obs"
)

rownames(spearman_matrix) <- behavioural_variables
colnames(spearman_matrix) <- behavioural_variables

correlation_data <- as.data.frame(
  as.table(spearman_matrix),
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
      levels = behavioural_variables,
      labels = unname(
        variable_labels[behavioural_variables]
      ),
      ordered = TRUE
    ),
    Variable_y = factor(
      Variable_y_code,
      levels = rev(behavioural_variables),
      labels = rev(
        unname(
          variable_labels[behavioural_variables]
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
      label = sprintf("%.2f", Spearman_rho),
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
      c(-1, 0, 0.35, 1)
    ),
    limits = c(-1, 1),
    name = expression(r[s])
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  theme_s1 +
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
    legend.key.height = grid::unit(18, "mm")
  )

# ------------------------------------------------------------
# 10. Helper for the three Rajput metric plots in Panel C
# ------------------------------------------------------------

make_k_metric_plot <- function(
    data,
    metric_column,
    y_axis_label,
    show_x_axis = FALSE,
    show_legend = FALSE
) {

  plot_i <- ggplot2::ggplot(
    data,
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
    ggplot2::scale_colour_discrete(
      drop = FALSE,
      name = "Number of\nclusters"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(5, 80),
      breaks = seq(10, 80, by = 10),
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    ggplot2::labs(
      x = if (show_x_axis) "k" else NULL,
      y = y_axis_label
    ) +
    theme_s1 +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = if (show_legend) "right" else "none",
      axis.title.x = if (show_x_axis) {
        ggplot2::element_text(
          colour = "black",
          size = 10
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text.x = if (show_x_axis) {
        ggplot2::element_text(
          colour = "black",
          size = 9
        )
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (show_x_axis) {
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
# 11. Panel C: three Rajput internal-validation indices
# ------------------------------------------------------------

# No separate graph of the number of clusters is included.
# Cluster number is represented only by the original ggplot2
# point colours in each of the three metric plots.
# Modularity is intentionally excluded because it is not one
# of the three indices used in the Rajput optimisation method.

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
  rel_heights = c(1, 1, 1)
)

# ------------------------------------------------------------
# 12. Add visible panel letters
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
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      )
    )
}

panel_A <- add_panel_label(plot_A_core, "A")
panel_B <- add_panel_label(plot_B_core, "B")
panel_C <- add_panel_label(panel_C_core, "C")

# ------------------------------------------------------------
# 13. Assemble the final S1 figure
# ------------------------------------------------------------

left_column <- cowplot::plot_grid(
  panel_A,
  panel_B,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1.02, 0.98)
)

final_panel_core <- cowplot::plot_grid(
  left_column,
  panel_C,
  ncol = 2,
  align = "h",
  axis = "tb",
  rel_widths = c(1.00, 1.28)
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
    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    )
  )

# ------------------------------------------------------------
# 14. Export the final figure
# ------------------------------------------------------------

pdf_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S1.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S1.tiff"
)

png_output <- file.path(
  figures_dir,
  "Supplementary_Figure_S1.png"
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
# 15. Verify output files
# ------------------------------------------------------------

expected_output_files <- c(
  pdf_output,
  tiff_output,
  png_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following figure files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 16. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SUPPLEMENTARY FIGURE S1\n")
cat("============================================================\n")

cat(
  "\nNumber of control fish: ",
  nrow(control),
  "\n",
  sep = ""
)

cat(
  "Panel A variables: ",
  paste(behavioural_variables, collapse = ", "),
  "\n",
  sep = ""
)

cat("Panel B method: Spearman correlation\n")

cat(
  "Panel C k range: ",
  min(optimisation_results$k),
  "-",
  max(optimisation_results$k),
  "\n",
  sep = ""
)

cat(
  "Stable identical-partition plateau: k = ",
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