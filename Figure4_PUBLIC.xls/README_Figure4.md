# Figure 4 – Proportional distribution of *Candida* spp. isolates (n=314)

## What this figure shows
This figure was created in **Microsoft Excel** and presents the proportional distribution of *Candida* spp. isolates (100% stacked bars) by:
- **Panel A:** Year (2014–2024)
- **Panel B:** Hospital unit / service (care setting)
- **Panel C:** Clinical specimen source

Bars show **category-specific percentages** (100% stacked). The **number of isolates (n)** is displayed within each bar.

## Source workbook
- `excelFigure4_distribution.xlsx` *(rename to your actual workbook name if different)*

## Sheet/objects used
- Sheet: `Fig4_data_final`
  - Contains the **final aggregated counts** used for plotting for Panels A–C.
- Charts (Excel)
  - A: 100% stacked column chart (Year × Species)
  - B: 100% stacked column chart (Service × Species)
  - C: 100% stacked column chart (Specimen source × Species)
- Labels
  - Within-bar percentage labels (and/or n labels), as shown in the final figure.
- Legend
  - Species legend consistent across panels.

## How to reproduce the figure (Excel)
1. Open `excelFigure4_distribution.xlsx`
2. Go to **Data > Refresh All** (Datos > Actualizar todo) if pivot tables/queries are used
3. Confirm that the aggregated tables in `Fig4_data_final` are updated
4. The three charts (A–C) update automatically

## Export for journal submission (recommended)
- Export as **PDF (vector)** from Excel:
  - Select the full panel area (A+B+C + legend) > **File > Export > Create PDF/XPS**
  - Choose **Selection** (Selección) in Options
- If a raster format is required, convert the exported PDF to **TIFF at 600 dpi**.

## Data sharing note
For public sharing (e.g., GitHub/DOI), use a **PUBLIC** version of the workbook that contains only **aggregated counts** (Year/Service/Specimen × Species) and does **not** include individual-level identifiers or sensitive fields.
