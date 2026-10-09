# ============================================================
# OBJECTIVE 4
# FIGURE 4A – POPULATION PARTITIONS ACROSS CLASSIFICATION METHODS
# CORRECTED VERSION v3
# - No title
# - Column widths calculated automatically from text
# - Export width calculated from the final table width
# ============================================================

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

library(grid)
library(gridExtra)
library(gtable)

# ------------------------------------------------------------
# 1. Output directory
# ------------------------------------------------------------

output_dir <- "03_Results/Figures"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE
  )
}

# ------------------------------------------------------------
# 2. Population partition data
# ------------------------------------------------------------

tab_partitions <- data.frame(
  Method = c(
    "Swaminathan-like threshold",
    "PCA-derived index quantile classification",
    "Gaussian mixture modelling",
    "Two-Step clustering",
    "Louvain community detection"
  ),

  Resilient = c(
    "24 (27.0%)",
    "23 (25.8%)",
    "9 (10.1%)",
    "21 (23.6%)",
    "45 (50.6%)"
  ),

  Intermediate = c(
    "\u2014",
    "43 (48.3%)",
    "52 (58.4%)",
    "\u2014",
    "\u2014"
  ),

  Susceptible = c(
    "65 (73.0%)",
    "23 (25.8%)",
    "28 (31.5%)",
    "68 (76.4%)",
    "44 (49.4%)"
  ),

  Structure = c(
    "Binary",
    "Three-class",
    "Three-class",
    "Binary",
    "Binary"
  ),

  stringsAsFactors = FALSE
)

colnames(tab_partitions) <- c(
  "Statistical method",
  "Resilient, n (%)",
  "Intermediate, n (%)",
  "Susceptible, n (%)",
  "Classification structure"
)

# ------------------------------------------------------------
# 3. Typography
# ------------------------------------------------------------

font_family <- "Arial"
font_size   <- 10.5

table_theme <- ttheme_minimal(
  base_size = font_size,
  base_family = font_family,

  core = list(
    fg_params = list(
      fontsize = font_size,
      fontfamily = font_family,
      hjust = 0,
      x = 0.03
    ),
    bg_params = list(
      fill = "white",
      col = NA
    )
  ),

  colhead = list(
    fg_params = list(
      fontsize = font_size,
      fontfamily = font_family,
      fontface = "bold",
      hjust = 0,
      x = 0.03
    ),
    bg_params = list(
      fill = "white",
      col = NA
    )
  )
)

# ------------------------------------------------------------
# 4. Build table
# ------------------------------------------------------------

tg <- tableGrob(
  tab_partitions,
  rows = NULL,
  theme = table_theme
)

# ------------------------------------------------------------
# 5. CALCULATE COLUMN WIDTHS FROM THE ACTUAL TEXT
# ------------------------------------------------------------
# This is the key correction: widths are not hard-coded.
# Each column is made wide enough for both its bold header
# and its longest body entry, plus extra horizontal padding.
# ------------------------------------------------------------

measure_text_cm <- function(text, bold = FALSE) {

  widths <- sapply(
    text,
    function(x) {
      convertWidth(
        grobWidth(
          textGrob(
            x,
            gp = gpar(
              fontfamily = font_family,
              fontsize = font_size,
              fontface = if (bold) "bold" else "plain"
            )
          )
        ),
        unitTo = "cm",
        valueOnly = TRUE
      )
    }
  )

  max(widths, na.rm = TRUE)
}

column_widths_cm <- numeric(ncol(tab_partitions))

for (j in seq_len(ncol(tab_partitions))) {

  header_width <- measure_text_cm(
    colnames(tab_partitions)[j],
    bold = TRUE
  )

  body_width <- measure_text_cm(
    tab_partitions[[j]],
    bold = FALSE
  )

  # 0.85 cm of extra padding prevents visual crowding
  column_widths_cm[j] <- max(header_width, body_width) + 0.85
}

# Apply calculated widths
tg$widths <- unit(column_widths_cm, "cm")

# Comfortable row height
tg$heights <- rep(
  unit(0.74, "cm"),
  length(tg$heights)
)

# ------------------------------------------------------------
# 6. Horizontal rules
# ------------------------------------------------------------

# Top rule
tg <- gtable_add_grob(
  tg,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y0 = unit(1, "npc"),
    y1 = unit(1, "npc"),
    gp = gpar(
      lwd = 1.0,
      col = "grey25"
    )
  ),
  t = 1,
  l = 1,
  r = ncol(tg)
)

# Rule below header
tg <- gtable_add_grob(
  tg,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y0 = unit(0, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(
      lwd = 0.9,
      col = "grey45"
    )
  ),
  t = 1,
  l = 1,
  r = ncol(tg)
)

# Light rules between rows
for (i in 2:(nrow(tab_partitions) + 1)) {

  tg <- gtable_add_grob(
    tg,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y0 = unit(0, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(
        lwd = 0.4,
        col = "grey82"
      )
    ),
    t = i,
    l = 1,
    r = ncol(tg)
  )
}

# Bottom rule
tg <- gtable_add_grob(
  tg,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y0 = unit(0, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(
      lwd = 1.0,
      col = "grey45"
    )
  ),
  t = nrow(tab_partitions) + 1,
  l = 1,
  r = ncol(tg)
)

# ------------------------------------------------------------
# 7. Calculate export dimensions from final table dimensions
# ------------------------------------------------------------

table_width_cm <- sum(column_widths_cm)

table_height_cm <- convertHeight(
  sum(tg$heights),
  unitTo = "cm",
  valueOnly = TRUE
)

# Add external margins so nothing can be clipped by the device
export_width_in  <- (table_width_cm + 1.2) / 2.54
export_height_in <- (table_height_cm + 1.2) / 2.54

cat("\nFinal column widths (cm):\n")
print(round(column_widths_cm, 2))

cat("\nExport dimensions (inches):\n")
cat(
  "Width =", round(export_width_in, 2),
  "| Height =", round(export_height_in, 2), "\n"
)

# ------------------------------------------------------------
# 8. Display
# ------------------------------------------------------------

grid.newpage()

pushViewport(
  viewport(
    width = unit(table_width_cm, "cm"),
    height = unit(table_height_cm, "cm")
  )
)

grid.draw(tg)

popViewport()

# ------------------------------------------------------------
# 9. Export PNG
# ------------------------------------------------------------

png(
  filename = file.path(
    output_dir,
    "Objective4_Method_Partitions_v3.png"
  ),
  width = export_width_in,
  height = export_height_in,
  units = "in",
  res = 600,
  bg = "white"
)

grid.newpage()
grid.draw(tg)
dev.off()

# ------------------------------------------------------------
# 10. Export TIFF
# ------------------------------------------------------------

tiff(
  filename = file.path(
    output_dir,
    "Objective4_Method_Partitions_v3.tiff"
  ),
  width = export_width_in,
  height = export_height_in,
  units = "in",
  res = 600,
  compression = "lzw",
  bg = "white"
)

grid.newpage()
grid.draw(tg)
dev.off()

# ------------------------------------------------------------
# 11. Export PDF
# ------------------------------------------------------------

cairo_pdf(
  filename = file.path(
    output_dir,
    "Objective4_Method_Partitions_v3.pdf"
  ),
  width = export_width_in,
  height = export_height_in,
  family = font_family
)

grid.newpage()
grid.draw(tg)
dev.off()

# ============================================================
# END OF SCRIPT
# ============================================================
