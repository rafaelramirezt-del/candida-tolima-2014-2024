# ============================================================
# Figure 3 – Age distribution (Panels A + B)
# A) Type of Candida infection
# B) Hospital care setting
#
# Outputs (in outputs/):
# - Figure_Age_Distribution_Panels_AB_FINAL.tiff (600 dpi, LZW)
# - Figure_Age_Distribution_AB_results.xlsx
# ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(stringr)
  library(writexl)
  library(patchwork)
})

source(file.path("R", "violin_utils.R"))

# ---------- Paths (relative to repo root) ----------
dir_data <- file.path("data")
dir_out  <- file.path("outputs")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

file_A <- file.path(dir_data, "2.Edad_Tipo_de_infeccion.xlsx")      # Panel A
file_B <- file.path(dir_data, "3. Edad_servicio_hospitalario.xlsx") # Panel B

# ---------- Options ----------
add_sig_brackets <- TRUE
add_subtitle     <- FALSE

# ============================================================
# PANEL A: Age vs Type of Candida infection
# ============================================================

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

# Standardize label if needed
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

# ============================================================
# PANEL B: Age vs Hospital care setting
# ============================================================

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

# Optional Spanish -> English mapping (safe even if already English)
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

# ============================================================
# Combine A + B into a single figure (A left, B right)
# ============================================================

p_combined <- (resA$plot + resB$plot) +
  patchwork::plot_annotation(
    tag_levels = "A",
    tag_prefix = "",
    tag_suffix = ")"
  ) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 18, face = "bold", family = "Arial")
  )

print(p_combined)

# ---------- Save combined TIFF (600 dpi, LZW) ----------
out_tif <- file.path(dir_out, "Figure_Age_Distribution_Panels_AB_FINAL.tiff")

tiff(filename = out_tif, width = 18, height = 7, units = "in",
     res = 600, compression = "lzw")
print(p_combined)
dev.off()

# ---------- Unified Excel export (both panels) ----------
summaryA <- resA$quart |>
  rename(GROUP = INFECTION) |>
  select(GROUP, n, q1, med, q3)

summaryB <- resB$quart |>
  rename(GROUP = CARE_SETTING) |>
  select(GROUP, n, q1, med, q3)

sheets <- list(
  "A_QUARTILES"      = summaryA,
  "A_KRUSKAL_WALLIS" = resA$kw,
  "A_DUNN"           = resA$dunn,
  "A_ETA2H"          = resA$eff,
  "A_SIG_PAIRS"      = resA$sig_pairs,

  "B_QUARTILES"      = summaryB,
  "B_KRUSKAL_WALLIS" = resB$kw,
  "B_DUNN"           = resB$dunn,
  "B_ETA2H"          = resB$eff,
  "B_SIG_PAIRS"      = resB$sig_pairs
)

write_xlsx(
  sheets,
  path = file.path(dir_out, "Figure_Age_Distribution_AB_results.xlsx")
)

cat("\n✅ Saved:\n", out_tif, "\n") 
