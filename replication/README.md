# Replication Package

**Paper:** "Communication Technology and Immigrant Assimilation: Evidence from Fixed-Line Telephone Diffusion"  
**Author:** Reo Nonaka, Department of Economics, Yale University

---

## Overview

This package contains the code and data necessary to replicate all tables and figures in the paper. The analysis uses R (version 4.5.2).

---

## Data Sources

### 1. IPUMS USA Decennial Census (not included — restricted access)

The raw microdata cannot be redistributed per IPUMS terms of use. To obtain the data:

1. Register at https://usa.ipums.org/usa/
2. Submit a data extract request using the provided codebook file: `data/usa_00013.xml`
   - In the IPUMS interface, go to "My Data" → "Revise" and upload the XML file, OR
   - Manually request the following variables for Census years 1970, 1980, 1990, 2000:
     `YEAR, SERIAL, PERNUM, GQ, PERWT, STATEFIP, AGE, SEX, MARST, BPL, BPLD, CITIZEN, YRIMMIG, EDUC, EDUCD, EMPSTAT, OCC, INCTOT, POVERTY, SPEAKENG`
3. Download as `.dat` format and place in `data/` along with the `.xml` file

### 2. World Bank WDI — Fixed-Line Telephone Subscriptions (included)

File: `data/WB_WDI_IT_MLT_MAIN_P2_WIDEF.csv`  
Series: `IT.MLT.MAIN.P2` (Fixed telephone subscriptions per 100 people)  
Source: https://data.worldbank.org/indicator/IT.MLT.MAIN.P2

---

## Code

`code/analysis.R` — Single script that reproduces all results in order:

| Section | Output |
|---------|--------|
| 0–2 | Data loading and cleaning |
| 3 | WDI telephone data cleaning |
| 4 | Treatment variable construction |
| 5 | Sample merge |
| 6 | Table 1: Summary statistics |
| 7 | Table 2 (binary): Main event-study regressions |
| 7b | Table 2 (continuous): Main specification used in paper |
| 8 | Figures 3–4: Event-study plots |
| 9 | Table 3 (binary): Heterogeneity |
| 9b | Table 3 (continuous): Heterogeneity — main specification |
| 10 | Robustness: threshold sensitivity |

### Required R packages

```r
install.packages(c("haven", "dplyr", "ggplot2", "ipumsr", "fixest",
                   "readr", "tidyr", "countrycode", "broom", "stringr"))
```

### Running the code

1. Place the IPUMS data files (`usa_00013.xml` and `usa_00013.dat`) in `~/Downloads/`
2. Place `WB_WDI_IT_MLT_MAIN_P2_WIDEF.csv` in `~/Downloads/`
3. Run `code/analysis.R` from top to bottom
4. Outputs are saved to `~/Downloads/`

---

## Output Files (pre-computed)

### Tables
| File | Content |
|------|---------|
| `output/tables/table1_sumstats.csv` | Table 1: Summary statistics |
| `output/tables/table2_continuous.csv` | Table 2: Main event-study coefficients (continuous spec) |
| `output/tables/table3_heterogeneity_cont.csv` | Table 3: Heterogeneity (continuous spec) |
| `output/tables/robustness_threshold.csv` | Robustness: threshold sensitivity |

### Figures
| File | Content |
|------|---------|
| `output/figures/result1_cont.png` | Figure 3: Income event-study plot |
| `output/figures/result2_cont.png` | Figure 4: English proficiency event-study plot |
| `output/figures/Rplot07.png` | Figure 1: Map of telephone diffusion timing |

---

## Software

- R version 4.5.2
- fixest 0.12.x (for high-dimensional fixed effects)
- ipumsr (for reading IPUMS extracts)
