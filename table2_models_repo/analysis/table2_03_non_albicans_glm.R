# ============================================================
# TABLE 2 – Model 3: Non-albicans isolation (standard logistic regression)
# Input : data/11.regresionlogisclasica3noalbicans.xlsx (or similar)
# Output: outputs/model_non_albicans_glm.xlsx
# ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(broom)
  library(tibble)
  library(writexl)
})

repo_root <- getwd()
data_dir  <- file.path(repo_root, "data")
out_dir   <- file.path(repo_root, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Input discovery ----
in_default <- file.path(data_dir, "11.regresionlogisclasica3noalbicans.xlsx")
if (!file.exists(in_default)) {
  cand <- list.files(data_dir, pattern = "albican.*\\.xlsx$", ignore.case = TRUE, full.names = TRUE)
  if (length(cand) == 0) stop("Input file not found in /data (expected *albican*.xlsx).")
  in_default <- cand[1]
}

out_xlsx <- file.path(out_dir, "model_non_albicans_glm.xlsx")

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0,001")
  sub("\\.", ",", sprintf("%.3f", p))
}
fmt_num <- function(x, digits = 3) sub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))

df_raw <- read_excel(in_default) |> clean_names()

col_dep   <- names(df_raw)[1]
col_preds <- names(df_raw)[-1]

df <- df_raw |>
  mutate(
    !!col_dep := case_when(
      !!sym(col_dep) %in% c("No albicans","no albicans","Non-albicans","non-albicans","1",1) ~ 1,
      !!sym(col_dep) %in% c("C. albicans","c. albicans","Albicans","albicans","0",0) ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  mutate(across(all_of(col_preds), ~ suppressWarnings(as.numeric(.x)))) |>
  tidyr::drop_na(all_of(c(col_dep, col_preds)))

form_mod <- as.formula(paste(col_dep, "~", paste(col_preds, collapse = " + ")))
mod <- glm(form_mod, data = df, family = binomial(link = "logit"))

tidy_or <- broom::tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  transmute(
    outcome = "Non-albicans isolation",
    term = term,
    or = estimate,
    ci_low = conf.low,
    ci_high = conf.high,
    p = p.value
  )

model_formatted <- tidy_or |>
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

# LR test vs null
loglik_null <- logLik(glm(df[[col_dep]] ~ 1, family = binomial, data = df))
loglik_mod  <- logLik(mod)
lr_chi2 <- 2 * (as.numeric(loglik_mod) - as.numeric(loglik_null))
lr_df   <- length(col_preds)
lr_p    <- pchisq(lr_chi2, df = lr_df, lower.tail = FALSE)

model_info <- tibble(
  outcome = "Non-albicans isolation",
  n_total = nrow(df),
  events_1 = sum(df[[col_dep]] == 1),
  non_events_0 = sum(df[[col_dep]] == 0),
  method = "Standard logistic regression",
  lr_chi2 = as.numeric(lr_chi2),
  lr_df = lr_df,
  lr_p = as.numeric(lr_p),
  lr_p_fmt = fmt_p(lr_p)
)

write_xlsx(
  list(
    MODEL_TIDY = tidy_or,
    MODEL_FORMATTED = model_formatted,
    MODEL_INFO = model_info
  ),
  path = out_xlsx
)

cat("\n✅ Saved:", out_xlsx, "\n")
