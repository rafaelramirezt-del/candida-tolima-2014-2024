# ============================================================
# Figure 5A – MCA (Hospital/clinical database)
# Input:  data/7_ACM_BD_GENERAL_RR.xlsx
# Output: outputs/figure5A_mca_clinical_*.tiff and *_tables.xlsx
# ============================================================

options(scipen = 999)

source(file.path("R", "mca_utils.R"))

paths <- resolve_paths()

input_file <- find_input_file(paths$data_dir, c("7_ACM_BD_GENERAL_RR.xlsx", "7. ACM BD_GENERAL_RR.xlsx"))

# ---- Lee datos ----
df_raw <- readxl::read_excel(input_file)

# (Opcional) excluir columnas tipo ID si existen:
exclude_cols <- c("ID", "ID_PACIENTE", "HISTORIA", "NUMERO_DOCUMENTO")

df_mca <- prepare_mca_data(
  df_raw,
  exclude = exclude_cols,
  max_levels = 50
)

mca <- run_mca(df_mca, ncp_max = 8)

# ---- Exporta tablas ----
out_tables <- file.path(paths$out_dir, "figure5A_mca_clinical_tables.xlsx")
export_mca_tables(mca, out_tables)

# ---- Paleta (solo si existe esa variable) ----
pal_infeccion <- c(
  "Candidemia"                  = "#E41A1C",
  "Meningitis"                  = "#6A3D9A",
  "Invasiva localizada"         = "#006400",
  "Oportunista asociada a VIH"  = "#3B4F76",
  "Mucocutánea orofaríngea"     = "#8B1C1C",
  "Urinaria"                    = "#FFD700",
  "Candidiasis vulvovaginal"    = "#F57C00",
  "Cutánea / Ungueal"           = "#0072B2",
  "Candidiasis no especificada" = "#B39DDB"
)

group_var_name <- "TIPO DE INFECCIÓN POR Candida"

if (group_var_name %in% names(df_mca)) {
  grp <- df_mca[[group_var_name]]
  grp <- factor(grp, levels = names(pal_infeccion))

  p <- plot_mca_biplot_top(
    mca = mca,
    group = grp,
    axes = c(1, 2),
    top_n = 100,
    palette = pal_infeccion,
    title = expression("MCA (clinical): Infection type by " * italic("Candida"))
  )

  out_fig <- file.path(paths$out_dir, "figure5A_mca_clinical_dim1_2.tiff")
  ggsave(out_fig, plot = p, width = 12, height = 7, units = "in", dpi = 600, compression = "lzw")
}

cat("\n✅ Figure 5A MCA listo.\n- Tablas:", out_tables, "\n")
