# ============================================================
# Figure 3B – Age vs Hospital care setting (single panel)
# Outputs (in outputs/):
# - Figure3B_Age_vs_CareSetting.tiff (600 dpi, LZW)
# - Figure3B_Age_vs_CareSetting_results.xlsx
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

file_B <- file.path(dir_data, "3. Edad_servicio_hospitalario.xlsx")

add_sig_brackets <- TRUE
add_subtitle     <- FALSE

dfB_raw <- read_excel(file_B) |> clean_names()
col_age_B <- names(dfB_raw)[1]
col_grp_B <- names(dfB_raw)[2]

dfB <- dfB_raw |>
  rename(AGE = all_of(col_age_B),
         CARE_SETTING = all_of(col_grp_B)) |>
  mutate(
    CARE_SETTING = str_squish(as.character(CARE_SETTING)),
    AGE          = suppressWarnings(as.numeric(AGE))
  ) |>
  filter(!is.na(AGE), AGE >= 0, AGE <= 110,
         !is.na(CARE_SETTING), CARE_SETTING != "")

orderB <- c(
  "ICU (adult)",
  "ICU (neonatal)",
  "ICU (coronary)",
  "Emergency department",
  "Hospitalization",
  "Surgery",
  "Obstetrics"
)

palB <- c(
  "ICU (adult)"            = "#E41A1C",
  "ICU (neonatal)"         = "#FFD700",
  "ICU (coronary)"         = "#006400",
  "Emergency department"   = "#B39DDB",
  "Hospitalization"        = "#8B1C1C",
  "Surgery"                = "#9ACD32",
  "Obstetrics"             = "#F57C00"
)

recodeB <- c(
  "UCI Adultos"     = "ICU (adult)",
  "UCI Neonatal"    = "ICU (neonatal)",
  "UCI Coronaria"   = "ICU (coronary)",
  "Urgencias"       = "Emergency department",
  "Hospitalización" = "Hospitalization",
  "Cirugía"         = "Surgery",
  "Obstetricia"     = "Obstetrics"
)

resB <- make_violin_panel(
  df = dfB,
  xvar = "CARE_SETTING",
  yvar = "AGE",
  order_levels = orderB,
  pal = palB,
  x_label = "Hospital care setting",
  y_label = "Age (median and IQR)",
  recode_map = recodeB,
  add_sig_brackets = add_sig_brackets,
  add_subtitle = add_subtitle
)

print(resB$plot)

out_tif <- file.path(dir_out, "Figure3B_Age_vs_CareSetting.tiff")
tiff(filename = out_tif, width = 12, height = 7, units = "in",
     res = 600, compression = "lzw")
print(resB$plot)
dev.off()

summaryB <- resB$quart |>
  select(CARE_SETTING, n, q1, med, q3)

sheets <- list(
  "SUMMARY_QUARTILES" = summaryB,
  "KRUSKAL_WALLIS"    = resB$kw,
  "DUNN_BONFERRONI"   = resB$dunn,
  "EFFECT_ETA2H"      = resB$eff,
  "SIGNIFICANT_PAIRS" = resB$sig_pairs
)

write_xlsx(sheets, path = file.path(dir_out, "Figure3B_Age_vs_CareSetting_results.xlsx"))

cat("\n✅ Saved:\n", out_tif, "\n")
