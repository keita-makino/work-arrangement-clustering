change_permission = function(database, degree) {
  database = database %>%
    mutate(
      flx_sv = pmin(5.5, pmax(0, flx_sv + degree))
    )

  return(database)
}

get_apollo_prediction = function(change, current_permission = NA) {
  database_temp = database

  if (length(current_permission) == 2) {
    database = database %>%
      filter(
        flx_sv >= current_permission[1] &
          flx_sv <= current_permission[2]
      )
  }

  database = change_permission(database, change)

  assign(
    "database",
    database,
    envir = .GlobalEnv
  )

  apollo_inputs = apollo_validateInputs()
  results = apollo_prediction(
    model,
    apollo_probabilities,
    apollo_inputs,
    prediction_settings = list(
      silent = TRUE
    )
  )

  if ("choice" %in% names(results)) {
    results = results$choice
  }

  results = results %>%
    tibble() %>%
    select(
      starts_with("cluster")
    ) %>%
    colSums() %>%
    tibble() %>%
    mutate(
      cluster = paste0("cluster_", row_number()),
      ratio = . / sum(.)
    ) %>%
    select(
      cluster,
      ratio
    ) %>%
    pivot_wider(
      names_from = cluster,
      values_from = ratio
    )

  database = database_temp

  assign(
    "database",
    database,
    envir = .GlobalEnv
  )

  return(results)
}
