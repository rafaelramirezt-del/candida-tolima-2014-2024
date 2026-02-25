# ============================================================
# REGRESIÓN LOGÍSTICA MULTIVARIADA – No-albicans
# Lee el archivo 11.regresionlogisclasica3.xlsx
# Columna 1  = CANDIDEMIA (No albicans / C. albicans)
# Columnas 2+ = predictores binarios (0/1)
#
# Exporta:
#   1) MODELO_LOGISTICO  -> tabla con OR, IC95, p, interpretación, efecto
#   2) RESUMEN_MODELO    -> info global del modelo (N, eventos, LR test, etc.)
# ============================================================

options(scipen = 999)  # sin notación científica

library(readxl)
library(dplyr)
library(janitor)
library(broom)
library(writexl)
library(tidyr)

# ---------- 1) Rutas y lectura ----------
dir_base  <- "C:/Users/pc/Desktop/EXTRACCION INFORMACION REXCEL"
archivo_xlsx <- "11.regresionlogisclasica3.xlsx"

df_raw <- read_excel(file.path(dir_base, archivo_xlsx)) |>
  clean_names()

# Columna dependiente = primera columna
col_dep   <- names(df_raw)[1]
col_preds <- names(df_raw)[-1]

# ---------- 2) Recodificar dependiente a 0/1 ----------
df <- df_raw |>
  mutate(
    # Candidemia = 1, todo lo demás = 0
    !!col_dep := case_when(
      !!sym(col_dep) %in% c("No albicans", "no albicans", "1") ~ 1,
      !!sym(col_dep) %in% c("C. albicans", "c. albicans", "0") ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  # Asegurar que predictores sean numéricos (0/1)
  mutate(across(all_of(col_preds), ~ as.numeric(.))) |>
  drop_na(all_of(c(col_dep, col_preds)))

# ---------- 3) Ajustar modelo logístico ----------
form_mod <- as.formula(
  paste(col_dep, "~", paste(col_preds, collapse = " + "))
)

mod <- glm(form_mod, data = df, family = binomial(link = "logit"))

# ---------- 4) Tabla de coeficientes (OR + IC95 + p) ----------
tab_coef <- tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    OR        = estimate,
    IC95_inf  = conf.low,
    IC95_sup  = conf.high,
    p_valor   = p.value
  )

# ---------- 5) Significancia, interpretación y efecto ----------
tab_modelo <- tab_coef |>
  mutate(
    # Nombre de la variable tal como quieres verlo
    Variable = paste0(term, " = Sí"),
    
    # Estrellitas
    Significancia = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01  ~ "**",
      p_valor < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    
    Interpretación = case_when(
      p_valor < 0.001 ~ "Asociación altamente significativa",
      p_valor < 0.01  ~ "Asociación significativa",
      p_valor < 0.05  ~ "Asociación débilmente significativa",
      TRUE            ~ "Sin asociación significativa"
    ),
    
    Efecto = case_when(
      OR > 1 & IC95_inf > 1 ~ "Factor de riesgo",
      OR < 1 & IC95_sup < 1 ~ "Factor protector",
      TRUE                  ~ "Sin efecto claro"
    ),
    
    # Redondeos (ajusta si quieres más/menos decimales)
    OR       = round(OR, 3),
    IC95_inf = round(IC95_inf, 3),
    IC95_sup = round(IC95_sup, 3),
    p_valor  = round(p_valor, 4)
  ) |>
  select(
    Variable,
    OR,
    `IC95% – Inferior` = IC95_inf,
    `IC95% – Superior` = IC95_sup,
    `p-valor`          = p_valor,
    Significancia,
    Interpretación,
    Efecto
  )

# ---------- 6) Resumen global del modelo ----------
N_total   <- nrow(df)
eventos_1 <- sum(df[[col_dep]] == 1, na.rm = TRUE)
eventos_0 <- sum(df[[col_dep]] == 0, na.rm = TRUE)

loglik_null   <- logLik(glm(df[[col_dep]] ~ 1, family = binomial, data = df))
loglik_modelo <- logLik(mod)

LR_chi2 <- 2 * (as.numeric(loglik_modelo) - as.numeric(loglik_null))
LR_df  <- length(col_preds)
LR_p   <- pchisq(LR_chi2, df = LR_df, lower.tail = FALSE)

resumen_modelo <- tibble(
  `Variable dependiente` = col_dep,
  `N total`       = N_total,
  `Eventos (1)`   = eventos_1,
  `No eventos (0)`= eventos_0,
  `LogLik nulo`   = round(as.numeric(loglik_null), 3),
  `LogLik modelo` = round(as.numeric(loglik_modelo), 3),
  `Chi² (LR test)`= round(LR_chi2, 3),
  `gl`            = LR_df,
  `p (LR test)`   = LR_p
)

# ---------- 7) Exportar a Excel ----------
salida_xlsx <- file.path(
  dir_base,
  "11_REGRESION_LOGISTICA_Noalbicans_3V_RESULTADOS.xlsx"
)

write_xlsx(
  list(
    MODELO_LOGISTICO = tab_modelo,
    RESUMEN_MODELO   = resumen_modelo
  ),
  path = salida_xlsx
)

cat("\n✅ Listo. Resultados guardados en:\n", salida_xlsx, "\n")

