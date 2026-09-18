# Analysis scripts

These scripts reproduce the analytical workflow reported in the current manuscript. Obsolete exploratory versions and the separate prospective/cortisol workflow are intentionally excluded.

## Running the workflow

Run the scripts from the repository root. The scripts use the current working directory as the project root by default.

An alternative root can be supplied without editing the scripts:

```r
Sys.setenv(LAPCOM_UCS_PROJECT_ROOT = "C:/path/to/zebrafish-ucs-exploratory-archetypes")
```

The only systematic change made during repository curation was replacement of the original computer-specific Windows root with this portable project-root mechanism. Statistical operations, constants, seeds, object names and output names were preserved.

## Execution order

### 00 — Setup and data loading

1. `00_Setup/000_setup_project.R`
2. `00_Setup/001_load_lapcom_data.R`

The primary spreadsheet is read from `01_Data/Raw/Resultados revisados AP.xlsx`. Generated R objects are written to `01_Data/Processed/`.

### 01 — Control-derived reference architecture

1. `005_Figure3_PCA.R`
2. `006.0_Optimisation_k_Louvain.R`
3. `006.1_ModularityNullTest_Louvain.R`
4. `006_Figure4_Louvain.R`
5. `007_FigureS_ExploratorySegmentSummaries.R`
6. `008_TableS_ExploratorySegmentSummaries.R`
7. `009_Table3_ContinuousBehaviouralReference.R`
8. `010_ClusterStability.R`
9. `010_LouvainExploratorySubsamplingStability.R`
10. `010.1_Figure1_Objective1_MainPanel_CORRECTED.R`
11. `010.2_SupplementaryFigureS1_Objective1.R`

The exact-equivalence plateau spans `k = 44–46`. The representative `k = 45` configuration is used for null-model and resampling inference. The identical partition at `k = 46` is retained for subsequent fixed-profile projection.

### 02 — Population-level redistribution following UCS

Run scripts `011` through `018` in numerical order.

The final composite Figure 2 script is not yet included because the definitive local file `021_Figure2_Main_PCA_Mahalanobis_CORREGIDO_v4.R` has not been recovered from the available stored copies.

### 03 — De novo post-UCS organisation

Run scripts `020` through `027` in numerical order. The representative UCS graph uses `k = 48`.

The final Supplementary Figure S2 script is pending recovery of the definitive local file `028_SupplementaryFigureS3_Objective3_v6.R`; its historical filename predates the present manuscript figure numbering.

### 04 — Operational definitions and method comparison

1. Run scripts `027` through `032`.
2. Open the exported blind dataset in IBM SPSS Statistics 27.0.1.
3. Run the corrected Two-Step syntax without additional standardisation.
4. Export the individual classification as `SPSS_TwoStep/Exported_Classifications/Objective4_TwoStep_Classification_CORREGIDO.xlsx`.
5. Run scripts `033` through `035`.
6. Run scripts `037` through `039`.

The corrected SPSS syntax and definitive exported classification are pending recovery from the local project.

## Required R packages

The curated scripts use:

- cluster
- corrplot
- cowplot
- dplyr
- ggplot2
- ggrepel
- igraph
- irr
- mclust
- openxlsx
- patchwork
- permute
- readxl
- rstatix
- scales
- tibble
- tidyr
- vegan

Package versions reported in the manuscript are documented in `07_Environment/README.md`.

## Output policy

Scripts create intermediate objects, tables and figures under `01_Data/Processed/` and `03_Results/`. These generated files are excluded from version control. The primary spreadsheet and the analytical code are the authoritative repository inputs.
