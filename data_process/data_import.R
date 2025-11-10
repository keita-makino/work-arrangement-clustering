data_import = function() {
  data <- read_sav(
    file.path(
      "data",
      "covid19-2023-merged.sav"
    )
  )
  print(
    paste(
      "Number of rows in the raw data:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(
      out == 0
    )
  print(
    paste(
      "Number of rows in the filtered data:",
      nrow(data)
    )
  )

  return(data)
}
