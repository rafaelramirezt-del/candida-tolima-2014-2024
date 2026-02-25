# Código para Figura 5 (MCA) – Manuscrito IJE

Este repositorio contiene el código para reproducir la **Figura 5**:
**Multiple correspondence analysis (MCA) of Candida infection types according to data source**  
A) Base hospital/clínica, B) Base microbiológica/laboratorio.

> **Nota:** No se incluyen datos clínicos en este repositorio. Coloca los archivos Excel de entrada en la carpeta `data/` (ver `data/README_data.txt`).

## Requisitos
- R (>= 4.1 recomendado)
- Paquetes:
  - readxl
  - writexl
  - dplyr
  - FactoMineR
  - factoextra
  - ggplot2
  - scales

Instalación rápida en R:
```r
install.packages(c("readxl","writexl","dplyr","FactoMineR","factoextra","ggplot2","scales"))
```

## Entradas esperadas
Coloca en `data/`:
- `7_ACM_BD_GENERAL_RR.xlsx` (clínica)
- `8_ACM_BD_GENERAL_RR_LAB.xlsx` (laboratorio)

## Ejecutar
Desde la raíz del repositorio:

```r
setwd("ruta/a/figure5_mca_repo")  # si es necesario
source("analysis/figure5A_mca_clinical.R")
source("analysis/figure5B_mca_laboratory.R")
```

## Salidas
Se guardan en `outputs/`:
- `figure5A_mca_clinical_tables.xlsx` y `figure5A_mca_clinical_dim1_2.tiff`
- `figure5B_mca_laboratory_tables.xlsx` y `figure5B_mca_laboratory_dim1_2.tiff`

## Configuración opcional (rutas)
Si prefieres no usar la estructura `data/` y `outputs/`, copia `config.example.R` como `config.R` y ajusta rutas.
`config.R` está en `.gitignore` para que no se suba a GitHub.
