# ============================================================
# Figure 3A – Age vs Type of Candida infection (single panel)
# Outputs (in outputs/):
# - Figure3A_Age_vs_InfectionType.tiff (600 dpi, LZW)
# - Figure3A_Age_vs_InfectionType_results.xlsx
# ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(stringr)
  library(writexl)
})

source(file.path("R", "violin_utils.R"))

dir_data <- file.path("data")
dir_out  <- file.path("outputs")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

file_A <- file.path(dir_data, "2.Edad_Tipo_de_infeccion.xlsx")

add_sig_brackets <- TRUE
add_subtitle     <- FALSE

dfA_raw <- read_excel(file_A) |> clean_names()
col_age_A <- names(dfA_raw)[1]
col_grp_A <- names(dfA_raw)[2]

dfA <- dfA_raw |>
  rename(AGE = all_of(col_age_A),
         INFECTION = all_of(col_grp_A)) |>
  mutate(
    INFECTION = str_squish(as.character(INFECTION)),
    AGE       = suppressWarnings(as.numeric(AGE))
  ) |>
  filter(!is.na(AGE), AGE >= 0, AGE <= 110,
         !is.na(INFECTION), INFECTION != "")

dfA <- dfA |>
  mutate(INFECTION = recode(
    INFECTION,
    "Oropharyngeal candidiasis" = "Oropharyngeal mucocutaneous candidiasis"
  ))

orderA <- c(
  "Candidemia",
  "Meningitis",
  "Localized invasive candidiasis",
  "HIV-associated opportunistic candidiasis",
  "Oropharyngeal mucocutaneous candidiasis",
  "Urinary candidiasis",
  "Vulvovaginal candidiasis",
  "Cutaneous / Nail candidiasis",
  "Unspecified candidiasis"
)

palA <- c(
  "Candidemia"                                   = "#E41A1C",
  "Meningitis"                                   = "#6A3D9A",
  "Localized invasive candidiasis"               = "#006400",
  "HIV-associated opportunistic candidiasis"     = "#3B4F76",
  "Oropharyngeal mucocutaneous candidiasis"      = "#8B1C1C",
  "Urinary candidiasis"                          = "#FFD700",
  "Vulvovaginal candidiasis"                     = "#F57C00",
  "Cutaneous / Nail candidiasis"                 = "#0072B2",
  "Unspecified candidiasis"                      = "#B39DDB"
)

resA <- make_violin_panel(
  df = dfA,
  xvar = "INFECTION",
  yvar = "AGE",
  order_levels = orderA,
  pal = palA,
  x_label = bquote(bold("Type of ") * bolditalic("Candida") * bold(" infection")),
  y_label = "Age (median and IQR)",
  recode_map = NULL,
  add_sig_brackets = add_sig_brackets,
  add_subtitle = add_subtitle
)

print(resA$plot)

out_tif <- file.path(dir_out, "Figure3A_Age_vs_InfectionType.tiff")
tiff(filename = out_tif, width = 12, height = 7, units = "in",
     res = 600, compression = "lzw")
print(resA$plot)
dev.off()

summaryA <- resA$quart |>
  select(INFECTION, n, q1, med, q3)

sheets <- list(
  "SUMMARY_QUARTILES" = summaryA,
  "KRUSKAL_WALLIS"    = resA$kw,
  "DUNN_BONFERRONI"   = resA$dunn,
  "EFFECT_ETA2H"      = resA$eff,
  "SIGNIFICANT_PAIRS" = resA$sig_pairs
)

write_xlsx(sheets, path = file.path(dir_out, "Figure3A_Age_vs_InfectionType_results.xlsx"))

cat("\n✅ Saved:\n", out_tif, "\n")
