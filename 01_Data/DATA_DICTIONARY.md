# Data dictionary

| Spreadsheet column | Internal name | Type | Description |
|---|---|---|---|
| `Author` | `Author` | Integer/categorical | Identifier of the historical experimental cohort. Cohort provenance is retained throughout the workflow. |
| `ID` | `ID` | Integer | Individual zebrafish identifier in the assembled dataset. |
| `Treatment` | `Treatment` | Categorical | Experimental group: `Control` or `Stress`. The latter denotes exposure to unpredictable chronic stress (UCS). |
| `Distance AP` | `distance` | Numeric | Total distance travelled during the novel tank test. |
| `Crossing AP` | `crossing` | Numeric | Number of crossings between the virtual horizontal zones of the novel tank. |
| `Top_entry AP` | `top_entry` | Numeric | Number of entries into the upper zone. |
| `Top_time AP` | `top_time` | Numeric | Time spent in the upper zone. |

## Interpretation

The four behavioural variables jointly represent **novelty-evoked exploratory space use**. They are analysed as a multidimensional behavioural domain rather than as interchangeable univariate proxies.

The operational labels resilient-like and susceptible-like refer only to post-UCS configurations within this behavioural space. They must not be interpreted as genetically determined types or as independently validated endocrine, molecular or whole-organism phenotypes.
