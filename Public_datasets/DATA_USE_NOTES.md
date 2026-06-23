# Data-use and de-identification notes

## Scope

This folder contains the final public de-identified datasets intended for Zenodo and GitHub release in support of the PLOS ONE resubmission package.

## Data structure

1. `BD_administrativeclinical_records_public.xlsx` contains clinical/administrative episode-level records used for clinical summaries and exploratory clinical/administrative models.
2. `BD_microbiology_laboratory_records_public.xlsx` contains microbiology isolate-level records used for species distribution, specimen source, and antifungal susceptibility summaries.

## Non-linkage statement

The two public datasets are complementary but not linked record by record. The clinical/administrative records and microbiology isolate records should not be merged or interpreted as patient-level paired observations unless the original institution performs authorized linkage under its own governance.

## De-identification choices

- Direct personal identifiers were removed/not included.
- Exact dates were removed; year-level variables were retained.
- Bed, room, floor, address, phone, personal identification numbers, and patient names are not included.
- The record identifiers included in the public files are sequential anonymized IDs for reproducibility and do not correspond to institutional identifiers.

## Recommended checks before release

- Confirm that manuscript counts match the validation summaries in each workbook.
- Confirm the final Zenodo DOI/version after uploading these files.
- Confirm that the Data Availability statement in the manuscript matches the public files uploaded.
