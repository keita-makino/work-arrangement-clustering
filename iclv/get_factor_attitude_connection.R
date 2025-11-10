get_factor_attitude_connection = function(n_factors, n) {
  read_csv(
    file.path(
      "dist",
      "factor",
      paste0(
        "loadings_a_",
        n_factors,
        "_n_",
        n,
        ".csv"
      )
    ),
  ) %>%
    select(
      -statement
    ) %>%
    pivot_longer(
      cols = -index,
      names_to = "factor",
      values_to = "value"
    ) %>%
    mutate(
      index = sprintf("%02d", index),
      factor = factor %>% str_replace("PA", "") %>% str_replace("RC", ""),
      value = abs(value),
    ) %>%
    group_by(index) %>%
    slice_max(
      order_by = value,
      n = 1,
      with_ties = FALSE
    ) %>%
    select(
      index,
      factor
    ) %>%
    unite(
      "string",
      everything(),
      sep = "_"
    ) %>%
    return()
}
