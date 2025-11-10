get_factor_variable_connection = function(
    regression_results,
    iv_list) {
  connections = c()
  for (i in 1:length(regression_results)) {
    for (j in 1:length(regression_results[[i]])) {
      var_index = iv_list %>%
        names() %>%
        str_detect(
          .,
          paste0("^", regression_results[[i]][j], "_")
        ) %>%
        which()

      connections = c(
        connections,
        paste(
          "a",
          i,
          iv_list[var_index] %>% names(),
          sep = "_"
        )
      )
    }
  }

  return(connections)
}
