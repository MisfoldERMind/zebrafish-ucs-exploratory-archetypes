# Primary behavioural dataset

## File

`raw/Resultados_revisados_AP.xlsx`

## Contents

The workbook contains the historical novel tank test dataset used in the retrospective analyses:

- worksheet: `Página1`;
- 178 data rows;
- 89 Control zebrafish;
- 89 UCS-exposed zebrafish, labelled `Stress` in the spreadsheet;
- four independent experimental cohorts;
- no missing values in the seven analysis columns.

Cohort-by-treatment sample sizes are:

| Cohort (`Author`) | Control | Stress/UCS | Total |
|---:|---:|---:|---:|
| 1 | 24 | 24 | 48 |
| 2 | 11 | 12 | 23 |
| 3 | 18 | 17 | 35 |
| 4 | 36 | 36 | 72 |
| **Total** | **89** | **89** | **178** |

## Use in the workflow

The spreadsheet is the immutable primary input. Analysis scripts rename the four behavioural columns to concise internal names but do not overwrite the source file.

For each behavioural variable and cohort, values are placed on a common Control-referenced scale using the cohort-specific Control median and raw median absolute deviation:

```text
robust_z = (x - Control median) / (1.4826 × Control raw MAD)
```

The same cohort-specific Control parameters are applied to both Control and UCS-exposed zebrafish. No UCS-specific re-normalisation is performed.

## Integrity checks

The data-loading workflow verifies:

- expected column names;
- numeric behavioural values;
- finite and non-missing observations;
- valid treatment labels;
- four cohort levels;
- both treatments represented in every cohort;
- 178 unique fish identifiers;
- expected group sizes of 89 Control and 89 Stress/UCS.
