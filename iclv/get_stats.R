get_stats = function(model_name) {
  model <- apollo_loadModel(
    file.path("dist", "iclv", model_name)
  )

  output_df <- apollo_modelOutput(
    model,
    modelOutput_settings = list(
      printPVal = 2
    )
  )

  return(
    output_df[, c(1, 4, 7)] %>%
      as_tibble(df,
        rownames = "name",
        .name_repair = "unique"
      )
  )
}

get_stats("6_8_1729_medium_mnl") %>%
  full_join(
    get_stats("6_8_1729_medium_nostr"),
    by = "name"
  ) %>%
  full_join(
    get_stats("6_8_1729_medium"),
    by = "name"
  ) %>%
  write_csv(
    file.path("dist", "iclv", "6_8_1729_stats.csv")
  )
