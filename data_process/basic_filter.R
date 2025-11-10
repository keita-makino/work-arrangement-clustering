basic_filter = function(data, filter_list) {
  data <- data %>%
    filter(
      (
        C1_1_1 == 1 |
          C1_2_1 == 1 |
          C1_3_1 == 1 |
          C1_4_1 == 1
      )
    )
  print(
    paste(
      "Number of rows after filtering out non-workers:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(!is.na(C3_1))
  print(
    paste(
      "Number of rows after filtering out those who had zero regular work hours per week:",
      nrow(data)
    )
  )

  if (filter_list$supervisor_permission) {
    data <- data %>%
      filter(C5_2a != 0)
    print(
      paste(
        "Number of rows after filtering out supervisor permission:",
        nrow(data)
      )
    )
  }

  if (filter_list$job_nature_capability) {
    data <- data %>%
      filter(C5_2b != 0)
    print(
      paste(
        "Number of rows after filtering out job nature capability:",
        nrow(data)
      )
    )
  }

  return(data)
}
