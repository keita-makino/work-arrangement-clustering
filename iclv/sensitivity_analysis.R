# Sensitivity analysis of the employer's remote work policy (Tables 7 and 8).
#
# Every worker's permission level is reduced by 0, 1 or 2 days per week
# (bounded at zero), and the estimated ICLV model predicts the probability of
# each cluster. The probabilities are averaged over all workers and over the
# workers with some permission (permission level > 0). Workdays per week are the
# predicted shares, rounded to 0.1% as in Table 7, times the cluster centroids
# of Table 2. Only predictions are made; the model is not re-estimated.

library(tidyverse)
library(apollo)

model_name <- "6_8_1729_medium_occ"

# Set up the model as in its estimation script, stopping before the estimation.
model_script <- readLines(file.path("iclv", paste0(model_name, ".R")))
estimation_line <- grep("^model <- apollo_estimate", model_script)[1]
eval(parse(text = model_script[seq_len(estimation_line - 1)]))

model <- apollo_loadModel(
  file.path("dist", "iclv", model_name)
)

scenarios <- tibble(
  scenario = c("baseline", "minus_1_day", "minus_2_days"),
  change = c(0, -1, -2)
)

cluster_names <- c(
  "Primary", "Temporary", "Home", "Primary-Home", "Home-Primary", "All-Mixed"
)

# Cluster centroids in workdays per week (Table 2).
centroids <- tibble(
  cluster = cluster_names,
  primary = c(5.15, 0.22, 0.04, 3.55, 1.72, 0.92),
  temporary = c(0.07, 5.16, 0.08, 0.19, 0.13, 2.74),
  home = c(0.05, 0.12, 5.42, 1.58, 3.43, 1.82)
)

database_observed <- database
permitted <- database_observed$flx_sv > 0

# Predicted probabilities of the six clusters for every worker.
predict_clusters <- function(change) {
  assign(
    "database",
    change_permission(database_observed, change),
    envir = .GlobalEnv
  )
  apollo_inputs <- apollo_validateInputs(silent = TRUE)
  results <- apollo_prediction(
    model,
    apollo_probabilities,
    apollo_inputs,
    prediction_settings = list(silent = TRUE)
  )
  assign("database", database_observed, envir = .GlobalEnv)

  if ("choice" %in% names(results)) {
    results <- results$choice
  }

  results %>%
    select(num_range("cluster", 1:6)) %>%
    as.matrix()
}

probabilities <- map(scenarios$change, predict_clusters)

# Shares (%) of each cluster: observed and predicted, for both groups.
get_shares <- function(rows, group) {
  observed <- table(factor(database_observed$cluster[rows], levels = 1:6))

  bind_rows(
    tibble(scenario = "observed", value = as.numeric(observed / sum(observed))),
    map2_dfr(
      scenarios$scenario,
      probabilities,
      \(scenario, p) tibble(scenario = scenario, value = colMeans(p[rows, ]))
    )
  ) %>%
    mutate(
      group = group,
      cluster = rep(cluster_names, n() / 6),
      share = 100 * value
    ) %>%
    select(group, scenario, cluster, share)
}

shares <- bind_rows(
  get_shares(rep(TRUE, nrow(database_observed)), "all"),
  get_shares(permitted, "permitted")
)

# Workdays per worker per week at each workplace.
workdays <- shares %>%
  mutate(share = round(share, 1) / 100) %>%
  left_join(centroids, by = "cluster") %>%
  group_by(group, scenario) %>%
  summarise(
    across(c(primary, temporary, home), \(x) sum(share * x)),
    .groups = "drop"
  ) %>%
  mutate(
    out_of_home = primary + temporary,
    total = primary + temporary + home,
    home_share = 100 * home / total
  ) %>%
  group_by(group) %>%
  mutate(
    across(
      c(primary, temporary, home, out_of_home),
      \(x) x - x[scenario == "baseline"],
      .names = "{.col}_change"
    )
  ) %>%
  ungroup() %>%
  arrange(group, factor(scenario, levels = c("observed", scenarios$scenario)))

shares %>%
  pivot_wider(names_from = cluster, values_from = share) %>%
  write_csv(
    file.path("dist", "iclv", paste0(model_name, "_sensitivity_shares.csv"))
  )

workdays %>%
  write_csv(
    file.path("dist", "iclv", paste0(model_name, "_sensitivity_workdays.csv"))
  )
