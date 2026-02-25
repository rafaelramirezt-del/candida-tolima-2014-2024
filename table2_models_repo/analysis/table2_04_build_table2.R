# ============================================================
# TABLE 2 – Build combined table (publication-ready)
# Requires the three model output files in outputs/
# Creates outputs/Table_2_combined.xlsx
# ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(writexl)
})

repo_root <- getwd()
out_dir   <- file.path(repo_root, "outputs")
meta_dir  <- file.path(repo_root, "meta")

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0,001")
  sub("\\.", ",", sprintf("%.3f", p))
}
fmt_num <- function(x, digits = 2) sub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))

# ---- Variable label mapping (optional but recommended) ----
map_file <- file.path(meta_dir, "variable_labels.csv")
label_map <- NULL
if (file.exists(map_file)) {
  label_map <- read.csv(map_file, stringsAsFactors = FALSE)
}
auto_label <- function(term) {
  term2 <- gsub("_", " ", term)
  term2 <- gsub("\\.", " ", term2)
  # Sentence case
  paste0(toupper(substr(term2, 1, 1)), substr(term2, 2, nchar(term2)))
}
get_label <- function(term) {
  if (!is.null(label_map) && all(c("term","label") %in% names(label_map))) {
    hit <- label_map$label[match(term, label_map$term)]
    if (!is.na(hit)) return(hit)
  }
  auto_label(term)
}

read_model <- function(path_xlsx) {
  tidy <- read_excel(path_xlsx, sheet = "MODEL_TIDY")
  info <- read_excel(path_xlsx, sheet = "MODEL_INFO")
  list(tidy = tidy, info = info)
}

files_needed <- c(
  "model_candidemia_firth.xlsx",
  "model_invasive_candidiasis_glm.xlsx",
  "model_non_albicans_glm.xlsx"
)
paths <- file.path(out_dir, files_needed)

missing <- files_needed[!file.exists(paths)]
if (length(missing) > 0) {
  stop(paste0("Missing model outputs in /outputs: ", paste(missing, collapse = ", "),
              ". Run analysis/table2_01..03 scripts first."))
}

mods <- lapply(paths, read_model)

# ---- Build rows ----
make_block <- function(tidy_df, info_df, only_sig = TRUE) {
  outcome <- unique(tidy_df$outcome)[1]
  lr_p_fmt <- info_df$lr_p_fmt[1]

  header <- tibble(
    Outcome = outcome,
    Variable = outcome,
    OR = "-",
    `95% CI` = "-",
    `p-value` = lr_p_fmt,
    Effect = "p value (LR test)"
  )

  dat <- tidy_df
  if (only_sig) dat <- dat |> filter(!is.na(p) & p < 0.05)

  body <- dat |>
    mutate(
      Variable = vapply(term, get_label, character(1)),
      OR = fmt_num(or, 2),
      `95% CI` = paste0(fmt_num(ci_low, 2), " – ", fmt_num(ci_high, 2)),
      `p-value` = vapply(p, fmt_p, character(1)),
      Effect = case_when(
        or < 1 ~ "Inverse association",
        or > 1 ~ "Risk factor",
        TRUE ~ ""
      )
    ) |>
    transmute(
      Outcome = outcome,
      Variable, OR, `95% CI`, `p-value`, Effect
    )

  bind_rows(header, body)
}

table_sig <- bind_rows(
  make_block(mods[[1]]$tidy, mods[[1]]$info, only_sig = TRUE),
  make_block(mods[[2]]$tidy, mods[[2]]$info, only_sig = TRUE),
  make_block(mods[[3]]$tidy, mods[[3]]$info, only_sig = TRUE)
)

table_all <- bind_rows(
  make_block(mods[[1]]$tidy, mods[[1]]$info, only_sig = FALSE),
  make_block(mods[[2]]$tidy, mods[[2]]$info, only_sig = FALSE),
  make_block(mods[[3]]$tidy, mods[[3]]$info, only_sig = FALSE)
)

out_combined <- file.path(out_dir, "Table_2_combined.xlsx")
write_xlsx(
  list(
    "Table_2_significant" = table_sig,
    "Table_2_all_predictors" = table_all
  ),
  path = out_combined
)

cat("\n✅ Saved combined table:", out_combined, "\n")
