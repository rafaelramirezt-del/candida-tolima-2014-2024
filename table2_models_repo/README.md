# Table 2 – Multivariable models (IJE manuscript)

This repository folder reproduces **Table 2** (three multivariable logistic regression models):

1. **Candidemia** – Firth penalized logistic regression  
2. **Invasive candidiasis** – standard logistic regression  
3. **Non-albicans isolation (vs C. albicans)** – standard logistic regression  

## 1) Inputs

Place the following Excel files into `data/`:

- `13.regresionlogisticajustadover1firth.xlsx`
- a file containing **invasive candidiasis** model data (name can include `candidiasis`)
- a file containing **non-albicans** model data (name can include `albican`)

> The scripts try to auto-detect filenames, but you can also rename your inputs to:
> - `11.regresionlogisclasica3candidiasis.xlsx`
> - `11.regresionlogisclasica3noalbicans.xlsx`

## 2) Run

From the repo root in R:

```r
source("analysis/table2_00_run_all.R")
```

Outputs will be written to `outputs/`:
- `model_candidemia_firth.xlsx`
- `model_invasive_candidiasis_glm.xlsx`
- `model_non_albicans_glm.xlsx`
- `Table_2_combined.xlsx` (two sheets: significant predictors only, and all predictors)

## 3) Variable labels (optional)

Edit `meta/variable_labels.csv` to map model terms to publication labels.
