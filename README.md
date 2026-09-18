# Zebrafish UCS exploratory archetypes

Reproducible computational workflows supporting the manuscript:

> *Unpredictable chronic stress redistributes novelty-evoked exploratory space use and yields method-dependent identification of resilient-like archetypes in zebrafish*

## Study overview

This repository contains the behavioural dataset and analysis workflows used for a retrospective *in silico* study of novelty-evoked exploratory space use in adult zebrafish (*Danio rerio*) exposed to unpredictable chronic stress (UCS).

The historical dataset comprises 178 zebrafish from four independent experimental cohorts:

- 89 unstressed Control zebrafish;
- 89 UCS-exposed zebrafish.

Four variables recorded in the novel tank test define the multidimensional behavioural space:

1. total distance travelled;
2. number of crossings between virtual horizontal zones;
3. number of entries into the upper zone;
4. time spent in the upper zone.

The terms **resilient-like** and **susceptible-like behavioural archetypes** are used operationally. They describe patterns identified within this behavioural domain and do not imply genetically fixed types, independently validated biological phenotypes, or a complete organism-level definition of stress resilience.

## Analytical framework

The workflow is organised around four analytical stages:

1. **Control-derived reference architecture**  
   Reconstruction of the multidimensional reference architecture exclusively from unstressed Control zebrafish, including robust cohort-specific normalisation, PCA and weighted k-nearest-neighbour Louvain analysis.

2. **Population-level redistribution following UCS**  
   Projection of UCS-exposed zebrafish into the fixed Control-derived space, followed by centroid displacement, partial PERMANOVA, multivariate dispersion, Mahalanobis-distance and correlation-structure analyses.

3. **De novo organisation within the UCS population**  
   Independent characterisation of internal post-UCS organisation while retaining the previously established Control-referenced coordinates.

4. **Operational classification of post-UCS archetypes**  
   Comparison of five analytical frameworks: Swaminathan-like thresholding, PCA-quantile classification, Gaussian mixture modelling, Louvain community detection and Two-Step clustering.

A continuous Behavioural Index is used as a directional score in the complete four-component Control PCA space. It is used directly by the Swaminathan-like and PCA-quantile frameworks and only after model fitting to orient GMM, Louvain and Two-Step classes.

## Repository structure

```text
.
├── 01_Data/
│   ├── Raw/
│   │   └── Resultados revisados AP.xlsx
│   ├── Processed/             # Generated analytical datasets
│   ├── DATA_DICTIONARY.md
│   └── README.md
├── 02_R/
│   ├── 00_Setup/
│   ├── 01_Objective1_ControlArchitecture/
│   ├── 02_Objective2_PopulationReorganization/
│   ├── 03_Objective3_StressPhenotypes/
│   └── 04_Objective4_OperationalDefinitionsOfBehaviouralResilience/\n│       └── SPSS_TwoStep/       # Corrected syntax and classification
├── 03_Results/                # Recreated tables, figures and objects
├── 07_Environment/            # Computational environment records
├── .gitignore
└── README.md
```

The analytical workflow has been curated against the current manuscript. Obsolete exploratory versions and the separate prospective/cortisol workflow are intentionally excluded.

## Data integrity

The spreadsheet in `01_Data/Raw/` is the unchanged primary behavioural dataset used for the retrospective analyses. Derived datasets, classifications and graphical outputs should be recreated by the scripted workflow and are not treated as independent source data.

The spreadsheet uses the label `Stress` for the UCS-exposed group. Cohort provenance is retained in the `Author` column.

See [01_Data/README.md](01_Data/README.md) and [01_Data/DATA_DICTIONARY.md](01_Data/DATA_DICTIONARY.md) for details.

## Computational environment

The manuscript reports:

- R 4.6.1 as the primary analytical environment;
- IBM SPSS Statistics 27.0.1 only for Two-Step cluster analysis;
- fixed random seeds for stochastic procedures;
- individual-zebrafish analyses;
- two-sided inference unless otherwise specified;
- a significance threshold of *p* < 0.05.

The complete execution order is documented in [02_R/README.md](02_R/README.md), and the reported package versions are listed in [07_Environment/README.md](07_Environment/README.md).

## Reuse and citation

The manuscript is in preparation. Citation information and a permanent archival identifier will be added when available.

Code is distributed under the repository licence. Data reuse should preserve cohort provenance, treatment labels and the operational—not biological—interpretation of the behavioural archetypes.
