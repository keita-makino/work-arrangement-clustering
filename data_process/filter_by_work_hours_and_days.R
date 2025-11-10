filter_by_work_hours_and_days = function(data) {
  data <- data %>%
    filter(
      days_total >= 5
    )
  print(
    paste(
      "Number of rows after filtering out those who worked less than 5 days of work:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(
      hours_total >= 12
    )
  print(
    paste(
      "Number of rows after filtering out those who worked less than 12 hours of work:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(
      regular_work_hour <= naive_total * 1.5 + 5
    )
  print(
    paste(
      "Number of rows after filtering out those who reported working regularly more than 1.5 x naive total + 5 hours:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(
      regular_work_hour >= naive_total / 1.5 - 5
    )
  print(
    paste(
      "Number of rows after filtering out those who reported working regularly less than naive total / 1.5 - 5 hours:",
      nrow(data)
    )
  )

  return(data)
}
