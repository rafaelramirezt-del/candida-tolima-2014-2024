Public de-identified datasets for the Candida Tolima 2014–2024 study

This data package contains the public de-identified datasets supporting the manuscript:

**Clinical and microbiological epidemiology of *Candida* infections in a high-complexity hospital in Tolima, Colombia (2014–2024)**  
PLOS ONE manuscript: PONE-D-26-11229

#Authors

- Rafael Augusto Ramírez-Trujillo — ORCID: 0000-0001-9355-1765
- Paula Katerine Carvajal Hernández — ORCID: 0000-0002-5024-1019
- Ángel González Marín — ORCID: 0000-0002-7052-7938

#Repository and DOI

- Zenodo conceptual DOI: https://doi.org/10.5281/zenodo.18765373
- GitHub repository: https://github.com/rafaelramirezt-del/candida-tolima-2014-2024

#Files included

| File | Description | Unit of analysis |
|---|---|---|
| `data/BD_administrativeclinical_records_public.xlsx` | Public de-identified clinical/administrative dataset used to summarize clinically recorded candidiasis episodes and to reproduce clinical descriptive analyses and exploratory models. | Clinical/administrative episode record |
| `data/BD_microbiology_laboratory_records_public.xlsx` | Public de-identified microbiology laboratory dataset used to summarize *Candida* spp. isolate records, species distribution, specimen origin, and antifungal susceptibility availability/interpretation. | Microbiology isolate record |
| `MANIFEST.csv` | File list with description, size, and SHA-256 checksum. | NA |
| `LICENSE` | MIT License for reuse of the public dataset package and associated reproducibility materials. | NA |
| `DATA_USE_NOTES.md` | Data-use and de-identification notes. | NA |

#De-identification and privacy

The public datasets do **not** include direct personal identifiers such as patient names, personal identification numbers, addresses, phone numbers, bed numbers, room numbers, or hospital floor information. Exact admission/discharge dates were removed; only year-level temporal information is retained for reproducibility of temporal summaries.

The original institutional records remain under the custody of the participating institution. These public files are intended to support reproducibility while reducing the risk of re-identification.

#Important methodological note

The clinical/administrative dataset and the microbiology laboratory dataset were generated from separate institutional information workflows and were **not linked record by record**. Therefore, isolate-level microbiology results should not be interpreted as paired patient-level data for the clinical/administrative records.

#Contents of each workbook

Each workbook includes:

- A main data sheet with de-identified records.
- A data dictionary describing the variables.
- A validation summary with key counts used to verify consistency with the manuscript.
- A README sheet with dataset-level notes.

#License

This package is released under the MIT License. See `LICENSE` for details.

#Recommended citation

Ramírez-Trujillo RA, Carvajal Hernández PK, González Marín Á. Public de-identified datasets and reproducibility materials for: Clinical and microbiological epidemiology of *Candida* infections in a high-complexity hospital in Tolima, Colombia (2014–2024). Zenodo. https://doi.org/10.5281/zenodo.18765373
