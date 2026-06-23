# Public de-identified datasets for the Candida Tolima 2014–2024 study

This folder contains the public-use de-identified datasets supporting the manuscript:

**Clinical and microbiological epidemiology of *Candida* infections in a high-complexity hospital in Tolima, Colombia (2014–2024)**
PLOS ONE manuscript: PONE-D-26-11229

## Authors

* Rafael Augusto Ramírez-Trujillo — ORCID: 0000-0001-9355-1765
* Paula Katerine Carvajal Hernández — ORCID: 0000-0002-5024-1019
* Ángel González Marín — ORCID: 0000-0002-7052-7938

## Repository and DOI

* Zenodo DOI: https://doi.org/10.5281/zenodo.18765373
* GitHub repository: https://github.com/rafaelramirezt-del/candida-tolima-2014-2024

## Files included

| File                                                  | Description                                                                                                                                                                                                           | Unit of analysis               |
| ----------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| `data/BD_administrativeclinical_records_public.xlsx`  | Public-use de-identified administrative/clinical dataset with 987 records, used to summarize clinical records and reproduce clinical descriptive analyses and exploratory models.                                     | Clinical/administrative record |
| `data/BD_microbiology_laboratory_records_public.xlsx` | Public-use de-identified microbiology laboratory dataset with 314 *Candida* spp. isolate records, used to summarize species distribution, specimen origin, and antifungal susceptibility availability/interpretation. | Microbiology isolate record    |
| `MANIFEST.csv`                                        | File list with description, size, and SHA-256 checksum.                                                                                                                                                               | NA                             |
| `DATA_USE_NOTES.md`                                   | Data-use, de-identification, and privacy notes.                                                                                                                                                                       | NA                             |
| `LICENSE`                                             | CC BY 4.0 license notice for the public-use de-identified datasets included in this folder.                                                                                                                           | NA                             |
| `README.md`                                           | Main description of the public-use dataset package.                                                                                                                                                                   | NA                             |
| `README.txt`                                          | Plain-text copy of the dataset package description, included for accessibility and archival purposes.                                                                                                                 | NA                             |

## De-identification and privacy

The public-use datasets are record-level datasets prepared for reproducibility. They do not include direct patient identifiers.

The public-use datasets do not include patient names, personal identification numbers, addresses, phone numbers, bed numbers, room numbers, floor numbers, or exact hospital admission/discharge dates.

Exact dates were removed. Only year-level temporal information is retained to support reproducibility of temporal summaries and analyses.

The original restricted institutional records remain under the custody of the participating institution. These public-use files are intended to support reproducibility while reducing the risk of patient re-identification.

## Important methodological note

The administrative/clinical dataset and the microbiology laboratory dataset were generated from separate institutional information workflows.

These two public-use datasets are independent and are not linked at the patient or record level. Therefore, isolate-level microbiology results should not be interpreted as paired patient-level data for the administrative/clinical records.

## Contents of each workbook

Each workbook includes:

* A main data sheet with the public-use de-identified records.
* A data dictionary describing the variables.
* A validation summary with key counts used to verify consistency with the manuscript.
* A README sheet with dataset-level notes.

## License

Public-use de-identified datasets included in `Public_datasets` are released under a Creative Commons Attribution 4.0 International License (CC BY 4.0), as indicated in the `LICENSE` file within this folder.

Analysis scripts and associated reproducible materials located outside `Public_datasets` are released under the MIT License, as indicated in the root repository `LICENSE` file.

Users must cite the associated manuscript and the archived Zenodo record when reusing these materials.

## Recommended citation

Ramírez-Trujillo RA, Carvajal Hernández PK, González Marín Á. Public-use de-identified datasets and reproducible materials for: Clinical and microbiological epidemiology of *Candida* infections in a high-complexity hospital in Tolima, Colombia (2014–2024). Zenodo. https://doi.org/10.5281/zenodo.18765373


