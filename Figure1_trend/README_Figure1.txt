# Figure 1 – Temporal trend and cumulative number of *Candida* infections (2014–2024)

## What this figure shows
This figure was created in **Microsoft Excel** using **pivot tables** and derived summary tables to plot:

- **Panel A (clinical database):** probable *Candida* cases (clinical episodes; n=987)
- **Panel B (microbiological database / WHONET):** confirmed *Candida* spp. isolates (n=314)

Each panel shows:
1) Annual counts (bars), and  
2) Cumulative counts (line, secondary axis)

## Source workbook
- `excelFigure1_trend.xlsx`

## Sheet/objects used
- Sheet: `Fig1_data_final`
  - Contains the **final aggregated tables** used for plotting (annual counts and cumulative counts) for Panel A and Panel B.
- Pivot tables
  - Panel A pivot: rows = `Year`; values = count of episodes/cases
  - Panel B pivot: rows = `Year`; values = count of confirmed isolates

## How to reproduce the figure (Excel)
1. Open `excelFigure1_trend.xlsx`
2. Go to **Data > Refresh All** (Datos > Actualizar todo)
3. Verify that the `Fig1_data_final` table updates (annual counts and cumulative counts)
4. The charts update automatically:
   - Panel A: column chart (annual cases) + line chart (cumulative cases, secondary axis)
   - Panel B: column chart (annual isolates) + line chart (cumulative isolates, secondary axis)

## Export for journal submission (recommended)
- Export as **PDF (vector)** from Excel:
  - Select the figure area (Panels A+B) > **File > Export > Create PDF/XPS**
  - Choose **Selection** (Selección) in Options
- If a raster format is required, convert the exported PDF to **TIFF at 600 dpi**.

## Data sharing note
Share a **public version** of the workbook containing only **aggregated counts** (by year) and excluding any individual-level identifiers or sensitive fields.
