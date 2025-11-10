annex_home_state = function(data) {
  home_state <- read_csv(
    file.path(
      "data",
      "home_state.csv"
    ),
    show_col_types = FALSE
  )
  print(
    paste(
      "Number of rows in the home state data:",
      nrow(home_state)
    )
  )

  data <- data %>%
    left_join(
      home_state,
      "row_index"
    ) %>%
    filter(
      Home_Google_state == "CA"
    )
  print(
    paste(
      "Number of rows in California:",
      nrow(data)
    )
  )

  write_sav(
    data,
    file.path(
      "data",
      "covid19-2023-merged-ca.sav"
    )
  )
  print(
    "Saved the California data to covid19-2023-merged-ca.sav"
  )

  return(data)
}
