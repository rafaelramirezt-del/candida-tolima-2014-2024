# ============================================================
# Utilidades para Análisis de Correspondencias Múltiples (MCA)
# Figura 5 – IJE submission support
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(FactoMineR)
  library(factoextra)
  library(ggplot2)
  library(scales)
})

# ---- Resolver rutas sin hardcodear C:\... ----
resolve_paths <- function() {
  # Permite un config.R local (ignorado por git)
  if (file.exists("config.R")) source("config.R", local = TRUE)

  root <- if (exists("PROJECT_ROOT") && !is.null(PROJECT_ROOT)) {
    PROJECT_ROOT
  } else {
    # Directorio actual de ejecución
    getwd()
  }

  data_dir <- if (exists("DATA_DIR") && !is.null(DATA_DIR)) file.path(root, DATA_DIR) else file.path(root, "data")
  out_dir  <- if (exists("OUTPUTS_DIR") && !is.null(OUTPUTS_DIR)) file.path(root, OUTPUTS_DIR) else file.path(root, "outputs")

  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

  list(root = root, data_dir = data_dir, out_dir = out_dir)
}

# ---- Buscar archivo de entrada (permite nombres alternativos) ----
find_input_file <- function(data_dir, candidates) {
  for (nm in candidates) {
    p <- file.path(data_dir, nm)
    if (file.exists(p)) return(p)
  }
  stop(
    "No se encontró el archivo de entrada en '", data_dir, "'.\n",
    "Probé: ", paste(candidates, collapse = ", ")
  )
}

# ---- Preparación de datos para MCA ----
prepare_mca_data <- function(df,
                             exclude = NULL,
                             max_levels = 50,
                             drop_single_level = TRUE) {
  # Convertir a factor y limpiar nombres
  df2 <- df %>%
    mutate(across(everything(), ~ as.factor(.x))) %>%
    rename_with(~ trimws(.x))

  if (!is.null(exclude)) {
    df2 <- df2 %>% select(-any_of(exclude))
  }

  # Eliminar variables con 1 solo nivel (sin variación)
  if (drop_single_level) {
    df2 <- df2 %>% select(where(~ is.factor(.x) && nlevels(.x) > 1))
  }

  # Eliminar variables con demasiados niveles (IDs, códigos únicos, etc.)
  if (!is.null(max_levels) && is.numeric(max_levels)) {
    df2 <- df2 %>% select(where(~ nlevels(.x) <= max_levels))
  }

  df2
}

# ---- Ejecutar MCA ----
run_mca <- function(df, ncp_max = 8) {
  n_vars <- ncol(df)
  ncp <- max(2, min(ncp_max, n_vars - 1))

  FactoMineR::MCA(
    X = df,
    ncp = ncp,
    graph = FALSE
  )
}

# ---- Extraer tablas del MCA ----
extract_mca_tables <- function(mca) {
  eig_df <- as.data.frame(mca$eig)
  colnames(eig_df) <- c("Eigenvalue", "Percent", "Cumulative_percent")
  eig_df$Dimension <- seq_len(nrow(eig_df))
  eig_df <- eig_df[, c("Dimension", "Eigenvalue", "Percent", "Cumulative_percent")]

  var_coord_df <- data.frame(Category = rownames(mca$var$coord), mca$var$coord, row.names = NULL)
  var_contrib_df <- data.frame(Category = rownames(mca$var$contrib), mca$var$contrib, row.names = NULL)
  var_cos2_df <- data.frame(Category = rownames(mca$var$cos2), mca$var$cos2, row.names = NULL)

  ind_coord_df <- data.frame(Individual = rownames(mca$ind$coord), mca$ind$coord, row.names = NULL)
  ind_contrib_df <- data.frame(Individual = rownames(mca$ind$contrib), mca$ind$contrib, row.names = NULL)
  ind_cos2_df <- data.frame(Individual = rownames(mca$ind$cos2), mca$ind$cos2, row.names = NULL)

  margin_df <- data.frame(
    Category = names(mca$call$marge.col),
    Relative_frequency = as.numeric(mca$call$marge.col),
    row.names = NULL
  )

  list(
    EIGENVALUES_INERTIA = eig_df,
    VAR_COORD = var_coord_df,
    VAR_CONTRIB = var_contrib_df,
    VAR_COS2 = var_cos2_df,
    IND_COORD = ind_coord_df,
    IND_CONTRIB = ind_contrib_df,
    IND_COS2 = ind_cos2_df,
    CATEGORY_MARGINS = margin_df
  )
}

export_mca_tables <- function(mca, out_xlsx) {
  sheets <- extract_mca_tables(mca)
  writexl::write_xlsx(sheets, path = out_xlsx)
  invisible(out_xlsx)
}

# ---- Plot: biplot por grupo (elipses) + selección top categorías por contribución ----
plot_mca_biplot_top <- function(mca,
                                group,
                                axes = c(1, 2),
                                top_n = 30,
                                palette = NULL,
                                title = NULL,
                                point_size = 1.6,
                                ellipse_alpha = 0.18,
                                var_color = "grey20",
                                var_labelsize = 3.0,
                                var_pointsize = 2.6,
                                split_labels = TRUE) {

  g <- droplevels(as.factor(group))

  # Selección top categorías por contribución (suma contribuciones en axes)
  contrib <- as.data.frame(mca$var$contrib)
  contrib$varname <- rownames(contrib)
  contrib$score <- rowSums(contrib[, axes, drop = FALSE])
  top_vars <- head(contrib[order(-contrib$score), "varname"], top_n)

  # Etiquetas "variable\nnivel" sin mutar el objeto MCA
  label_map <- rownames(mca$var$coord)
  if (split_labels) {
    label_map <- sapply(strsplit(label_map, "_", fixed = TRUE), function(x) {
      if (length(x) >= 2) paste0(x[1], "\n", paste(x[-1], collapse = "_")) else x
    })
    names(label_map) <- rownames(mca$var$coord)
  }

  if (is.null(title)) title <- paste0("MCA biplot (Dim ", axes[1], "–", axes[2], ")")

  if (is.null(palette)) {
    k <- nlevels(g)
    palette <- scales::hue_pal(l = 45, c = 100)(k)
  }

  p <- factoextra::fviz_mca_biplot(
    X           = mca,
    axes        = axes,
    title       = title,
    col.ind     = g,
    palette     = palette,
    geom.ind    = "point",
    geom.var    = c("point", "text"),
    label       = "var",
    labelsize   = var_labelsize,
    col.var     = var_color,
    select.var  = list(name = top_vars),
    repel       = TRUE,
    addEllipses = TRUE
  ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.35) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      axis.title = element_text(size = 10)
    )

  # Cambiar labels en el layer de textos si split_labels
  if (split_labels) {
    # Reetiquetar en el objeto ggplot (sustitución robusta)
    p <- p + scale_x_continuous() # no-op, mantiene estructura
    # Cambiar el data de la capa de texto de variables (si existe)
    p$layers <- lapply(p$layers, function(ly) {
      if (!is.null(ly$geom) && inherits(ly$geom, "GeomText") && !is.null(ly$data$label)) {
        ly$data$label <- unname(label_map[ly$data$label])
      }
      ly
    })
  }

  # Ajustes finos de puntos y elipses
  p$layers <- lapply(p$layers, function(ly) {
    if (inherits(ly$geom, "GeomEllipse")) {
      ly$aes_params$alpha <- ellipse_alpha
      ly$aes_params$linewidth <- 0.9
    }
    if (inherits(ly$geom, "GeomPoint")) {
      if (!is.null(ly$mapping$colour)) {
        ly$aes_params$size <- point_size
        ly$aes_params$alpha <- 0.85
      } else {
        ly$aes_params$shape <- 17
        ly$aes_params$size <- var_pointsize
      }
    }
    ly
  })

  p
}
