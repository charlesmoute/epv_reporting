# Post-Campaign Coverage Survey — Measles-Rubella · DRC Bloc 1 (2025)

> **Automated R pipeline for survey data processing, analysis, and reporting** — producing a dynamic HTML report and a Word document with fully narrative-driven outputs and embedded VCQI-aligned indicators.

---

## What this project is about

At the end of 2025, the Democratic Republic of Congo (DRC) conducted a large-scale supplemental immunization activity (SIA) targeting all children aged 6 months to 14 years for measles and rubella (MR vaccine). The campaign covered 7 provinces in Bloc 1: Bas-Uélé, Haut-Uélé, Ituri, Tanganyika, Haut-Lomami, Lualaba, and Haut-Katanga.

This repository contains the complete R pipeline used to independently evaluate the campaign's performance through a **Post-Campaign Coverage Survey (ECP)**. It was developed by the WHO DRC office as part of the technical support to the Ministry of Health.

The pipeline processes raw survey data collected in the field, runs all statistical analyses, and automatically generates a publication-ready report — without requiring any manual editing of numbers or text.

---

## Why this matters — the VCQI connection

The **WHO Vaccination Coverage Quality Indicators (VCQI)** framework defines a standard set of indicators to assess the quality and reliability of vaccination coverage surveys. These indicators go beyond a single coverage number: they evaluate data quality, sampling design, equity across groups, and the reliability of the survey process itself.

This pipeline was designed to produce VCQI-aligned indicators using R and the `survey` package, as a reproducible and transparent alternative to the Stata-based VCQI software package. The three core VCQI modules implemented here are:

| VCQI Module | Indicator | What it measures |
|---|---|---|
| **SIA-COVG-03** | Zero-dose catch-up rate | Among children with no prior MR vaccination, what share did the campaign reach? |
| **QUAL-04** | Design Effect (DEFF) and Intraclass Correlation Coefficient (ICC) | How much statistical precision is lost because of cluster sampling? |
| **ES-03** | Non-respondent analysis | Could non-response bias the coverage estimates? |

Beyond these three, the pipeline also covers:

- **SIA-COVG-01/02** — Weighted coverage nationally and by province, age, sex, and residence
- **SIA-COVG-04** — Coverage verified by physical proof (card seen by interviewer)
- **SIA-COVG-05** — Coefficient of variation across clusters
- **SIA-QUAL-01/02** — Coverage distribution heatmap and Rao-Scott Chi-square test
- **SIA-QUAL-03** — Proportion of households informed before the campaign
- **PSYCH-01 to 04** — BeSD behavioural and social drivers of vaccination
- **QUAL-01/02/03** — Backcheck concordance, Cohen's Kappa, OMS error rate thresholds

---

## Repository structure

```
ecp_rr_rdc_bloc1/
│
├── scripts_R/
│   ├── script_treatment.R      # Step 1 — Data cleaning and variable construction
│   ├── script_analyses.R       # Step 2 — All statistical analyses and figure generation
│   ├── script_reporting.R      # Step 3 — Quarto report template (HTML + Word)
│   └── init_reporting.R        # Step 4 — Pipeline launcher (sources all three steps)
│
├── data/
│   ├── raw/                    # Original .dta files from field collection (7 provinces)
│   ├── clean/                  # Cleaned datasets produced by script_treatment.R
│   └── external/               # Administrative coverage data for triangulation
│
├── outputs/
│   ├── tableaux/               # CSV and HTML tables produced by script_analyses.R
│   ├── graphiques/             # PNG figures produced by script_analyses.R
│   └── rapport/                # Final report: rapport_ecp_rdc.{html,docx,qmd}
│
├── docs/                       # Supporting documentation
└── README.md
```

---

## How the pipeline works

The entire pipeline runs from a single command:

```r
source("scripts_R/init_reporting.R")
```

This triggers four sequential steps:

**Step 1 — Data treatment** (`script_treatment.R`)
Reads the 7 raw `.dta` files from the field (one per province), harmonises variable names and value labels, applies inclusion/exclusion criteria, derives key analysis variables (vaccination status, age groups, residence type, prior vaccination history), and computes sampling weights.

**Step 2 — Statistical analyses** (`script_analyses.R`)
Runs all weighted estimates using the `survey` package (design-based inference with `svyciprop` for logit-method confidence intervals). Produces ~35 tables and ~25 figures covering:
- National and provincial coverage with 95% CI
- Coverage by age, sex, residence, and socio-demographic subgroups
- VCQI indicators (DEFF/ICC, zero-dose catch-up, non-respondent analysis)
- Reasons for non-vaccination (Pareto chart + OMS thematic taxonomy)
- Multivariable logistic regression (Firth penalised method for quasi-separation)
- Backcheck quality control (concordance rates, Cohen's Kappa, OMS error thresholds)
- Triangulation with administrative SNIS data and WHO End-Process survey

**Step 3 — Dynamic reporting** (`script_reporting.R`)
A Quarto document that reads all outputs from Step 2 and generates:
- A fully narrative HTML report with three-level structure per section: accessible prose for all readers, operational callout for decision-makers, and technical note for specialists
- A formatted Word document for official submission

All numbers in the report are computed dynamically — changing the data automatically updates every figure, table, and sentence in the report.

**Step 4 — Pipeline launcher** (`init_reporting.R`)
Coordinates the three steps in sequence and renders the final Quarto document.

---

## Key technical features

**Design-based survey inference**
All coverage estimates use `survey::svyciprop()` with the logit method, which keeps confidence intervals within [0, 1] and correctly accounts for the two-stage cluster sampling design (primary sampling units = health areas, weights = inverse probability weights adjusted for non-response).

**VCQI QUAL-04 — DEFF and ICC**
The Design Effect is approximated from the already-computed logit confidence intervals:
```
DEFF = [(IC_high - IC_low) / (2 × 1.96)]² / [p(1-p)/n]
ICC  = (DEFF - 1) / (m̄ - 1)
```
where m̄ is the average cluster size. This approach avoids a separate call to `survey::vcov()` and produces results consistent with the main estimates. Results for Bloc 1: **DEFF = 7.4**, **ICC = 0.29** — both typical of MR campaign surveys in sub-Saharan Africa.

**Firth penalised logistic regression**
When provinces with near-total coverage create quasi-separation in the logistic model, the pipeline automatically switches to Firth's penalised regression (`logistf` package), which stabilises estimates without requiring category collapsing.

**Three-level narrative structure**
Every analytical section in the report follows a consistent structure:
1. Accessible prose — findings in plain language for any reader
2. Operational callout — immediate decision for programme managers
3. Technical note (blue callout) — methodology, denominators, limits, references for specialists

**Inter-script communication**
Key scalar values computed in `script_analyses.R` (e.g., total non-vaccinates, DEFF, ICC, non-response correlation) are written to small `.txt` files in `outputs/tableaux/` and read back by `script_reporting.R`. This avoids passing R objects through `.rds` files and keeps the pipeline modular.

---

## Main results (Bloc 1 · November–December 2025)

| Indicator | Value | WHO Target | Status |
|---|---|---|---|
| National vaccination coverage | **95.5%** [93.9–96.7] | ≥ 95% | ✅ Reached |
| Household informed before campaign | **93.7%** | ≥ 90% | ✅ Reached |
| Vaccination cards distributed | **90.9%** | ≥ 85% | ✅ Reached |
| Post-vaccination symptoms declared | **9.6%** | ≤ 5% | ❌ Exceeded |
| Zero-dose catch-up rate | — | ≥ 80% | Calculated by province |
| National DEFF | **7.4** | — | Elevated |
| National ICC | **0.29** | — | Typical for SIA surveys |

---

## Requirements

```r
# Core packages
tidyverse, haven, labelled, scales, glue, cli

# Survey analysis
survey, logistf

# Reporting
quarto, officer, flextable, knitr, kableExtra

# Visualisation
ggplot2, patchwork, gt
```

R version ≥ 4.2.0 and Quarto ≥ 1.4 are required. All packages are loaded and installed automatically by `init_reporting.R` if not already present.

---

## Data availability

Raw survey data are not included in this repository as they contain household-level identifiers subject to WHO data governance policy. The pipeline structure, all analysis scripts, and the report template are fully reproducible with anonymised or synthetic data of the same structure.

---

## About this work

This pipeline was developed to support the WHO DRC office's independent evaluation of the 2025 MR campaign in Bloc 1. It demonstrates applied expertise in:

- **VCQI-aligned survey analysis in R** — producing the same indicators as the Stata-based VCQI package through fully transparent, reproducible code
- **Complex survey statistics** — design-based inference, DEFF, ICC, Rao-Scott tests
- **Automated report generation** — dynamic Quarto documents that update entirely when data changes
- **BeSD framework** — Behavioural and Social Drivers of vaccination (WHO/UNICEF 2022) integrated into the multivariable analysis
- **Data quality assessment** — backcheck concordance, Cohen's Kappa, OMS T1/T2/T3 error thresholds

The same pipeline architecture can be adapted for other countries, other antigens, or other blocks of the DRC campaign.

---

## Contact

For technical questions about the pipeline or the VCQI implementation in R, please open an issue in this repository.
