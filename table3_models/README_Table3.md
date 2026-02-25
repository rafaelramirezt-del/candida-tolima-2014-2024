# Table 3 – Antifungal susceptibility (MIC) by *Candida* species (WHONET / microbiological database)

## What this table shows
Table 3 summarizes antifungal susceptibility results for *Candida* spp. isolates with available MIC values (µg/mL). Results are reported **by species** and include denominators (number of isolates tested) for each antifungal.

## Source workbook
- `Table3_MIC_PUBLIC.xlsx` *(rename to your actual workbook name if different)*

## Sheet/objects used
- Sheet: `MIC_clean`
  - Cleaned MIC dataset used for summaries (aggregated / de-identified as appropriate).
- Sheet: `Table3_final`
  - Formatted table as included in the manuscript (n tested and S/I/R or WT/NWT, as applicable).
- (Optional) Sheet: `README`
  - Brief instructions to reproduce/export the table.

## Antifungals included
MIC data (µg/mL) were summarized for:
- Amphotericin B
- Fluconazole
- Voriconazole
- Caspofungin
- Micafungin
- Flucytosine (5‑FC)

## Interpretation standards
- Antifungals with available **clinical breakpoints** were interpreted as **S/I/R** according to the standard used in the study (e.g., CLSI).
- **Flucytosine (5‑FC)** was reported as **WT/NWT** using **EUCAST ECOFF** (epidemiological cut-offs) and is **not directly comparable** to clinical S/I/R interpretation.

## How to reproduce the table (Excel)
1. Open `Table3_MIC_PUBLIC.xlsx`
2. If pivot tables/queries are used, go to **Data > Refresh All** (Datos > Actualizar todo)
3. Verify denominators (n tested) by species and antifungal
4. The formatted output in `Table3_final` updates (or can be refreshed/recalculated)

## Export for journal submission (recommended)
- Export as **PDF** from Excel:
  - Select the Table 3 area > **File > Export > Create PDF/XPS**
  - Choose **Selection** (Selección) in Options

## Data sharing note
For public sharing (e.g., GitHub/DOI), provide a **PUBLIC** workbook containing only **aggregated summaries** (and/or de-identified MIC data where permitted). Do not include individual-level identifiers or sensitive fields.
