# Analysis scripts

These scripts reproduce the analytical workflow reported in the current manuscript. Obsolete exploratory versions and the separate prospective/cortisol workflow are intentionally excluded.

## Running the workflow

Begin with `00_Setup/000_setup_project.R`. This script:

- identifies the repository root;
- creates any missing input, output and SPSS directories;
- verifies the primary behavioural spreadsheet;
- reports the next script to execute.

The script is idempotent and can be run repeatedly without deleting existing files. Subsequent scripts should be executed from the repository root.

An alternative root can be supplied without editing any script:

```r
Sys.setenv(
  LAPCOM_UCS_PROJECT_ROOT =
    "C:/path/to/zebrafish-ucs-exploratory-archetypes"
)
```

Computer-specific Windows paths were replaced with this portable project-root mechanism. Statistical operations, constants, seeds, object names and analytical outputs were otherwise preserved.

## Execution order

### 00 — Setup and data loading

1. `00_Setup/000_setup_project.R`
2. `00_Setup/001_load_lapcom_data.R`

Script `000` creates or verifies the directory structure and confirms that the primary spreadsheet exists at `01_Data/Raw/Resultados revisados AP.xlsx`. Script `001` then loads and audits the 178-fish dataset. Generated R objects are written to `01_Data/Processed/`.

### 01 — Control-derived reference architecture

1. `005_Figure3_PCA.R`
2. `006.0_Optimisation_k_Louvain.R`
3. `006.1_ModularityNullTest_Louvain.R`
4. `006_Figure4_Louvain.R`
5. `007_FigureS_ExploratorySegmentSummaries.R`
6. `008_TableS_ExploratorySegmentSummaries.R`
7. `009_Table3_ContinuousBehaviouralReference.R`
8. `010.00_ClusterStability.R`
9. `010.01_LouvainExploratorySubsamplingStability.R`
10. `010.02_Figure1_Objective1_MainPanel.R`
11. `010.03_SupplementaryFigureS1_Objective1.R`

The exact-equivalence plateau spans `k = 44–46`. The representative `k = 45` configuration is used for null-model and resampling inference. The identical partition at `k = 46` is retained for subsequent fixed-profile projection.

### 02 — Population-level redistribution following UCS

Run scripts `011` through `020` in numerical order, followed by `021_Figure2_Main_PCA_Mahalanobis.R` to assemble the definitive Figure 2. Script `019` reconstructs the inherited Control-derived PC1–PC2 projection, script `020` generates the descriptive 95% normal-theory data ellipses, and script `021` combines the corrected PCA-ellipse and Mahalanobis objects without recalculating inferential statistics.

### 03 — De novo post-UCS organisation

Run scripts `020` through `027` in numerical order, followed by `028_SupplementaryFigureS3.R`. The representative UCS graph uses `k = 48`.

### 04 — Operational definitions and method comparison

1. Run scripts `027` through `032`.
2. Open the blind dataset exported by script `032` in IBM SPSS Statistics 27.0.1.
3. Run `SPSS_TwoStep/Syntax/Objective4_TwoStep_Analysis_CORREGIDO.sps` without additional standardisation.
4. The final Two-Step classification is archived as `SPSS_TwoStep/Exported_Classifications/Objective4_TwoStep_Classification_CORREGIDO.xlsx` and contains 89 fish in clusters of 21 and 68.
5. Run scripts `033` through `035`.
6. Run scripts `037` through `039`.

## Required R packages

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

Scripts create intermediate objects, tables and figures under `01_Data/Processed/` and `03_Results/`. These generated files are excluded from version control. The primary spreadsheet and analytical code are the authoritative repository inputs.
