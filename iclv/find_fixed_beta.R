find_fixed_beta = function(variables) {
  fixed_beta = variables[
    variables %>%
      str_detect("^d")
  ] %>%
    tibble() %>%
    mutate(
      factor =
        str_extract(
          .,
          "\\d{1,2}$"
        )
    ) %>%
    group_by(factor) %>%
    slice(1) %>%
    ungroup() %>%
    select(-factor) %>%
    pull()

  return(fixed_beta)
}
