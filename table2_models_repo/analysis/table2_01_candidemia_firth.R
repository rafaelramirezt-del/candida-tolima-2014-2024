# ============================================================
# TABLE 2 – Model 1: Candidemia (Firth penalized logistic regression)
# Input : data/13.regresionlogisticajustadover1firth.xlsx
# Output: outputs/model_candidemia_firth.xlsx
# ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(tibble)
  library(writexl)
  library(logistf)
})

# ---- Paths (relative to repo root) ----
repo_root <- getwd()
data_dir  <- file.path(repo_root, "data")
out_dir   <- file.path(repo_root, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Input discovery (allows small filename variations) ----
in_default <- file.path(data_dir, "13.regresionlogisticajustadover1firth.xlsx")
if (!file.exists(in_default)) {
  cand <- list.files(data_dir, pattern = "firth.*\\.xlsx$", ignore.case = TRUE, full.names = TRUE)
  if (length(cand) == 0) stop("Input file not found in /data (expected 13.regresionlogisticajustadover1firth.xlsx).")
  in_default <- cand[1]
}

out_xlsx <- file.path(out_dir, "model_candidemia_firth.xlsx")

# ---- Formatting helpers (comma decimal) ----
fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0,001")
  sub("\\.", ",", sprintf("%.3f", p))
}

fmt_num <- function(x, digits = 3) sub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))

# ---- Read and clean ----
df_raw <- read_excel(in_default) |> clean_names()

col_dep   <- names(df_raw)[1]
col_preds <- names(df_raw)[-1]

df <- df_raw

# Dependent recode: 1 = candidemia, 0 = no candidemia
df[[col_dep]] <- ifelse(
  df[[col_dep]] %in% c("Candidemia","candidemia","1",1,"Sí","Si","SI","Yes","YES"),
  1L,
  ifelse(
    df[[col_dep]] %in% c("No candidemia","no candidemia","0",0,"No","NO"),
    0L,
    NA_integer_
  )
)

df <- df |>
  mutate(across(all_of(col_preds), ~ suppressWarnings(as.numeric(.x)))) |>
  tidyr::drop_na(all_of(c(col_dep, col_preds)))

# ---- Fit Firth model ----
form_firth <- as.formula(paste(col_dep, "~", paste(col_preds, collapse = " + ")))
fit <- logistf::logistf(form_firth, data = df)

beta      <- fit$coefficients
se_b      <- fit$se
p_vec     <- fit$prob
var_names <- names(beta)

OR      <- exp(beta)
CI_low  <- fit$ci.lower
CI_high <- fit$ci.upper

model_tidy <- tibble(
  outcome = "Candidemia",
  term = var_names,
  beta = beta,
  se = se_b,
  or = OR,
  ci_low = CI_low,
  ci_high = CI_high,
  p = p_vec
) |>
  filter(term != "(Intercept)")

model_formatted <- model_tidy |>
  transmute(
    Variable = term,
    OR = fmt_num(or, 2),
    `95% CI` = paste0(fmt_num(ci_low, 2), " – ", fmt_num(ci_high, 2)),
    `p-value` = vapply(p, fmt_p, character(1)),
    Effect = case_when(
      !is.na(p) & p < 0.05 & or < 1 ~ "Inverse association",
      !is.na(p) & p < 0.05 & or > 1 ~ "Risk factor",
      TRUE ~ ""
    )
  )

# ---- Global LR test (penalized likelihood ratio approximation) ----
loglik_null  <- fit$loglik["null"]
loglik_model <- fit$loglik["full"]
chi_lr       <- 2 * (loglik_model - loglik_null)
df_lr        <- length(col_preds)
p_lr         <- pchisq(chi_lr, df = df_lr, lower.tail = FALSE)

model_info <- tibble(
  outcome = "Candidemia",
  n_total = nrow(df),
  events_1 = sum(df[[col_dep]] == 1),
  non_events_0 = sum(df[[col_dep]] == 0),
  method = "Firth penalized logistic regression",
  lr_chi2 = as.numeric(chi_lr),
  lr_df = df_lr,
  lr_p = as.numeric(p_lr),
  lr_p_fmt = fmt_p(p_lr)
)

# ---- Write Excel ----
write_xlsx(
  list(
    MODEL_TIDY = model_tidy,
    MODEL_FORMATTED = model_formatted,
    MODEL_INFO = model_info
  ),
  path = out_xlsx
)

cat("\n✅ Saved:", out_xlsx, "\n")
