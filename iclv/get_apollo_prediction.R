change_permission <- function(database, degree) {
  database <- database %>%
    mutate(
      flx_sv = pmin(5.5, pmax(0, flx_sv + degree))
    )

  return(database)
}

get_apollo_prediction <- function(change, current_permission = NA) {
  database_temp <- database

  if (length(current_permission) == 2) {
    database <- database %>%
      filter(
        flx_sv >= current_permission[1] &
          flx_sv <= current_permission[2]
      )
  }

  database <- change_permission(database, change)

  assign(
    "database",
    database,
    envir = .GlobalEnv
  )

  apollo_inputs <- apollo_validateInputs()
  results <- apollo_prediction(
    model,
    apollo_probabilities,
    apollo_inputs,
    prediction_settings = list(
      silent = TRUE
    )
  )

  if ("choice" %in% names(results)) {
    results <- results$choice
  }

  results <- results %>%
    tibble() %>%
    select(
      starts_with("cluster")
    ) %>%
    colSums() %>%
    enframe() %>%
    mutate(
      cluster = paste0("cluster_", row_number()),
      ratio = value / sum(value),
      days_visited_3 = case_when(
        cluster == "cluster_1" ~ ratio * 0.05,
        cluster == "cluster_2" ~ ratio * 0.12,
        cluster == "cluster_3" ~ ratio * 5.42,
        cluster == "cluster_4" ~ ratio * 1.58,
        cluster == "cluster_5" ~ ratio * 3.43,
        cluster == "cluster_6" ~ ratio * 1.82,
      ),
      days_visited_total = case_when(
        cluster == "cluster_1" ~ ratio * 5.27,
        cluster == "cluster_2" ~ ratio * 5.5,
        cluster == "cluster_3" ~ ratio * 5.54,
        cluster == "cluster_4" ~ ratio * 5.32,
        cluster == "cluster_5" ~ ratio * 5.28,
        cluster == "cluster_6" ~ ratio * 5.48,
      )
    ) %>%
    select(
      cluster,
      ratio,
      days_visited_3,
      days_visited_total
    ) %>%
    pivot_wider(
      names_from = cluster,
      values_from = ratio
    ) %>%
    summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
    mutate(wfh_ratio = days_visited_3 / days_visited_total) %>%
    select(days_visited_3, days_visited_total, wfh_ratio, everything())

  database <- database_temp

  assign(
    "database",
    database,
    envir = .GlobalEnv
  )

  return(results)
}
