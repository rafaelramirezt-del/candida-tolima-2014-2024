# ============================================================
# Figure 5B – MCA (Microbiological/laboratory database)
# Input:  data/8_ACM_BD_GENERAL_RR_LAB.xlsx
# Output: outputs/figure5B_mca_laboratory_*.tiff and *_tables.xlsx
# ============================================================

options(scipen = 999)

source(file.path("R", "mca_utils.R"))

paths <- resolve_paths()

input_file <- find_input_file(paths$data_dir, c("8_ACM_BD_GENERAL_RR_LAB.xlsx", "8. ACM BD_GENERAL_RR_LAB.xlsx"))

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
out_tables <- file.path(paths$out_dir, "figure5B_mca_laboratory_tables.xlsx")
export_mca_tables(mca, out_tables)

pal_infeccion <- c(
  "Candidemia"                  = "#E41A1C",
  "Invasiva localizada"         = "#006400",
  "Respiratoria"                = "#3B4F76",
  "Mucocutánea orofaríngea"     = "#8B1C1C",
  "Digestiva / intestinal"      = "#6A3D9A",
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
    title = expression("MCA (laboratory): Infection type by " * italic("Candida"))
  )

  out_fig <- file.path(paths$out_dir, "figure5B_mca_laboratory_dim1_2.tiff")
  ggsave(out_fig, plot = p, width = 12, height = 7, units = "in", dpi = 600, compression = "lzw")
}

cat("\n✅ Figure 5B MCA listo.\n- Tablas:", out_tables, "\n")
