# Class assignment dataset (first made on 2026-05-14).
#
# Input: data/database_assignment.rds, a fixed snapshot kept for the class.
# It matches the output of data_process.R with the three preprocessing steps
# at the end of data_process/create_database.R commented out (numeric cast,
# flx_sv recode to days, z-score of att_*). So flx_sv stays as categories 0-5
# and att_* stay on the original response scale. It has no vmt_commute column,
# which was added to the pipeline later. Do not rebuild it from the current
# data/database.rds, which is preprocessed for the ICLV model.
#
# Output: data/database_assignment.sav and data/database_assignment_def.json
# (the column notes for students are in data/database_assignment_readme.md).
#
# Run from the repository root.

source("library_loading.R")

data <- read_rds("data/database_assignment.rds") %>%
  select(
    -starts_with("visited_"),
    -starts_with("hours_"),
    -starts_with("factor_"),
    -genderkids
  ) %>%
  mutate(
    across(
      c(att_14, att_15, att_16, att_17, att_18, att_19),
      ~ case_when(
        . == 6 ~ 3,
        . == 7 ~ 4,
        . == 8 ~ 5,
        TRUE ~ .
      ) %>%
        labelled(
          labels = c(
            "Strongly disagree" = 1,
            "Disagree" = 2,
            "Slightly disagree" = 3,
            "Slightly agree" = 4,
            "Agree" = 5
          )
        )
    )
  ) %>%
  mutate(
    income = case_when(
      income == 1 ~ 1,
      income == 2 ~ 2,
      income == 3 ~ 3,
      income == 4 ~ 4,
      TRUE ~ NA_real_
    ) %>%
      labelled(
        labels = c(
          "Less than $50,000" = 1,
          "$50,000-$99,999" = 2,
          "$100,000-$149,999" = 3,
          "$150,000+" = 4
        )
      ),
    row_index = row_number(),
    cluster = cluster %>%
      labelled
      (
        labels = c(
          "Primary" = 1,
          "Temporary" = 2,
          "Home" = 3,
          "Primary-Home" = 4,
          "Home-Primary" = 5,
          "All-Mixed" = 6
        )
      )
  )

data %>%
  write_sav("data/database_assignment.sav")

# Extract metadata from SAV and write to JSON.
# Preserves manually-added fields (e.g. "background") on re-runs.
sav_def_path <- "data/database_assignment_def.json"

extract_col_meta <- function(col) {
  val_labels <- attr(col, "labels")
  list(
    type = class(col)[1],
    label = as.character(attr(col, "label") %||% ""),
    value_labels = if (!is.null(val_labels)) {
      setNames(as.list(names(val_labels)), as.character(unname(val_labels)))
    } else {
      NULL
    }
  )
}

new_cols <- setNames(
  lapply(names(data), function(nm) extract_col_meta(data[[nm]])),
  names(data)
)

existing_cols <- if (file.exists(sav_def_path)) {
  tryCatch(
    jsonlite::fromJSON(sav_def_path, simplifyVector = FALSE)$columns %||% list(),
    error = function(e) list()
  )
} else {
  list()
}

question_length_threshold <- 50

for (nm in names(new_cols)) {
  col <- new_cols[[nm]]
  ex <- existing_cols[[nm]]
  spss_lbl <- col$label %||% ""
  is_long <- nchar(spss_lbl) >= question_length_threshold

  new_cols[[nm]] <- list(
    type = col$type,
    label = if (!is.null(ex$label)) {
      ex$label
    } else if (!is_long) spss_lbl else "",
    question = if (!is.null(ex$question)) {
      ex$question
    } else if (is_long) spss_lbl else "",
    value_labels = col$value_labels,
    description = ex$description %||% ""
  )
}

write(
  jsonlite::toJSON(
    list(
      database_name = "database_assignment.sav",
      created_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      columns       = new_cols
    ),
    pretty = TRUE, auto_unbox = TRUE, null = "null"
  ),
  sav_def_path
)
