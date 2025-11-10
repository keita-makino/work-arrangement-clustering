create_variable_name_vector = function(table) {
  vector = table %>%
    unite(
      "name",
      everything(),
      sep = "_"
    ) %>%
    pull()

  return(vector)
}
