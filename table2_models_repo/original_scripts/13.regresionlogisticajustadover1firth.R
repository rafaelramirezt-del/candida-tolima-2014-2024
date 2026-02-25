# ============================================================
# REGRESIÓN LOGÍSTICA PENALIZADA (FIRTH) PARA CANDIDEMIA
# Archivo de entrada : 13.regresionlogisticajustadover1firth.xlsx
# Carpeta base       : C:/Users/pc/Desktop/EXTRACCION INFORMACION REXCEL
# Variable dependiente: CANDIDEMIA (1 = Candidemia, 0 = No candidemia)
# Autor: Rafael Augusto Ramírez Trujillo 
# ============================================================

options(scipen = 999)  # evitar notación científica

# -------------------- 1) Librerías --------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(tibble)
library(writexl)
library(logistf)

# -------------------- 2) Rutas y archivo --------------------
dir_base   <- "C:/Users/pc/Desktop/EXTRACCION INFORMACION REXCEL/Candidemias"
archivo_in <- "13.regresionlogisticajustadover1firth.xlsx"
archivo_out <- "13_REG_LOGISTICA_CANDIDEMIA_FIRTH.xlsx"

# -------------------- 3) Funciones de formato ----------------

# p-valor con coma decimal
fmt_p_single <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0,001")
  out <- sprintf("%.3f", p)
  sub("\\.", ",", out)
}

# número con coma decimal
fmt_num <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  sub("\\.", ",", out)
}

# texto para significancia
fmt_sig <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  "ns"
}

# interpretación cualitativa del efecto
interpretar_efecto <- function(or, p) {
  if (is.na(or) || is.na(p) || p >= 0.05) {
    return("No concluyente")
  }
  if (or > 1) {
    return("Factor de riesgo")
  } else if (or < 1) {
    return("Factor protector")
  } else {
    return("No concluyente")
  }
}

# -------------------- 4) Leer y limpiar datos ----------------

df_raw <- read_excel(file.path(dir_base, archivo_in)) |>
  clean_names()   # nombres tipo snake_case

# La 1ª columna es la dependiente (candidemia)
col_dep   <- names(df_raw)[1]
col_preds <- names(df_raw)[-1]

# Copia de trabajo
df <- df_raw

# Re-codificar CANDIDEMIA a 0/1
# 1 = Candidemia, 0 = No candidemia
df[[col_dep]] <- ifelse(
  df[[col_dep]] %in% c("Candidemia", "candidemia", "1", 1, "Sí", "Si", "SI"),
  1L,
  ifelse(
    df[[col_dep]] %in% c("No candidemia", "no candidemia", "0", 0, "No", "NO"),
    0L,
    NA_integer_
  )
)

# Asegurar que los predictores sean numéricos (0/1)
df <- df |>
  mutate(across(all_of(col_preds), ~ suppressWarnings(as.numeric(.x)))) |>
  drop_na(all_of(c(col_dep, col_preds)))   # quitar filas con NA en cualquier variable

# Resumen de eventos
y        <- df[[col_dep]]
n_total  <- length(y)
n_eventos <- sum(y == 1)
n_noevent <- sum(y == 0)

if (n_eventos < 5) {
  warning("⚠ Muy pocos eventos; la estimación puede ser inestable.")
}

# -------------------- 5) Modelo Firth ajustado ----------------

form_firth <- as.formula(
  paste(col_dep, "~", paste(col_preds, collapse = " + "))
)

fit_firth <- logistf::logistf(form_firth, data = df)

# -------------------- 6) Extraer coeficientes, OR e IC95 ------
# -------------------- 6) Extraer coeficientes, OR e IC95 ------

# Coeficientes y estadísticas básicas del modelo Firth
beta      <- fit_firth$coefficients   # incluye intercepto
se_b      <- fit_firth$se
p_vec     <- fit_firth$prob
var_names <- names(beta)

# OR e intervalos de confianza (ya vienen en escala OR)
OR      <- exp(beta)              # OR "tipo Wald"
IC_low  <- fit_firth$ci.lower     # IC95% inferior (perfil penalizado)
IC_high <- fit_firth$ci.upper     # IC95% superior

# Tabla completa (incluye intercepto)
tabla_coef <- tibble(
  Variable_cruda   = var_names,
  beta             = beta,
  SE               = se_b,
  OR_bruto         = OR,
  IC95_low_bruto   = IC_low,
  IC95_high_bruto  = IC_high,
  p_val            = p_vec
)

# Quitar el intercepto para la tabla de interpretación
tabla_pred <- tabla_coef |>
  filter(Variable_cruda != "(Intercept)") |>
  mutate(
    Variable = Variable_cruda,
    OR       = OR_bruto,
    IC95     = paste0(
      sprintf("%.3f", IC95_low_bruto), " – ",
      sprintf("%.3f", IC95_high_bruto)
    ),
    `p-valor` = ifelse(
      p_val < 0.001, "<0,001",
      sub("\\.", ",", sprintf("%.3f", p_val))
    ),
    Significancia = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE         ~ "ns"
    ),
    Interpretación = case_when(
      Significancia == "***" ~ "Asociación altamente significativa",
      Significancia == "**"  ~ "Asociación significativa",
      Significancia == "*"   ~ "Asociación débilmente significativa",
      TRUE                   ~ "Sin asociación significativa"
    ),
    Efecto = case_when(
      OR > 1  ~ "Factor de riesgo",
      OR < 1  ~ "Factor protector",
      TRUE    ~ "Sin efecto claro"
    )
  ) |>
  select(Variable, OR, IC95, `p-valor`, Significancia, Interpretación, Efecto)





# -------------------- 7) Resumen global del modelo ------------

loglik_null  <- fit_firth$loglik["null"]
loglik_model <- fit_firth$loglik["full"]
chi_lr       <- 2 * (loglik_model - loglik_null)
gl_lr        <- length(col_preds)
p_lr         <- pchisq(chi_lr, df = gl_lr, lower.tail = FALSE)

resumen_model <- tibble(
  `Variable dependiente` = col_dep,
  `N total`              = n_total,
  `Eventos (1)`          = n_eventos,
  `No eventos (0)`       = n_noevent,
  `Método`               = "Regresión logística penalizada (Firth)",
  `LogLik nulo`          = fmt_num(loglik_null, 3),
  `LogLik modelo`        = fmt_num(loglik_model, 3),
  `Chi² (LR test)`       = fmt_num(chi_lr, 3),
  `gl`                   = gl_lr,
  `p (LR test)`          = fmt_p_single(p_lr)
)

# -------------------- 8) Exportar todo a Excel ----------------

hojas_out <- list(
  "MODELO_FIRTH"   = tabla_pred,
  "RESUMEN_MODELO" = resumen_model
)

write_xlsx(
  hojas_out,
  path = file.path(dir_base, archivo_out)
)

cat("\n✅ Listo. Resultados guardados en:\n",
    file.path(dir_base, archivo_out), "\n")
