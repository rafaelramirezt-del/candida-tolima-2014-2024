# Figure 3 – Age distribution (Panels A + B)

This repository contains scripts to reproduce Figure 3:

- Panel A: Age vs Type of Candida infection
- Panel B: Age vs Hospital care setting

## Inputs
Place these Excel files in `data/`:

- `2.Edad_Tipo_de_infeccion.xlsx`
- `3. Edad_servicio_hospitalario.xlsx`

Each file is expected to have:
- Column 1 = age (years)
- Column 2 = group label

## Run
From the repo root in R:

```r
source("analysis/figure3_combined_AB_horizontal.R")
```

## Outputs
Written to `outputs/`:
- `Figure_Age_Distribution_Panels_AB_FINAL.tiff` (600 dpi, LZW)
- `Figure_Age_Distribution_AB_results.xlsx`
