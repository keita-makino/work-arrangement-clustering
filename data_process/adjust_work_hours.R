adjust_work_hours = function(data) {
  data = data %>%
    mutate(
      ratio_total = case_when(
        hours_total == 0 ~ 0,
        T ~ (
          naive_total / hours_total
        )
      )
    )

  for (i in 1:7) {
    data = data %>%
      mutate(
        !!paste("ratio_day_", i, sep = "") :=
          case_when(
            !!sym(paste("hours_total_day_", i, sep = "")) == 0 ~ 0,
            T ~ (
              !!sym(
                paste("work_hour_last_week_day_", i, "_4", sep = "")
              ) /
                !!sym(
                  paste("hours_total_day_", i, sep = "")
                )
            )
          ),
        !!paste("diff_day_", i, sep = "") :=
          !!sym(paste("work_hour_last_week_day_", i, "_4", sep = "")) -
            !!sym(paste("hours_total_day_", i, sep = ""))
      )
  }

  data <- data %>%
    filter(ratio_total >= 0.5 & ratio_total <= 2.00)
  print(
    paste(
      "Number of rows after filtering out those who worked less than 0.5 times or more than 2 times of the naive total hours:",
      nrow(data)
    )
  )

  data <- data %>%
    filter(
      ((ratio_day_1 == 0 & abs(diff_day_1) <= 2) | (ratio_day_1 >= 0.5 & ratio_day_1 <= 2)) &
        ((ratio_day_2 == 0 & abs(diff_day_2) <= 2) | (ratio_day_2 >= 0.5 & ratio_day_2 <= 2)) &
        ((ratio_day_3 == 0 & abs(diff_day_3) <= 2) | (ratio_day_3 >= 0.5 & ratio_day_3 <= 2)) &
        ((ratio_day_4 == 0 & abs(diff_day_4) <= 2) | (ratio_day_4 >= 0.5 & ratio_day_4 <= 2)) &
        ((ratio_day_5 == 0 & abs(diff_day_5) <= 2) | (ratio_day_5 >= 0.5 & ratio_day_5 <= 2)) &
        ((ratio_day_6 == 0 & abs(diff_day_6) <= 2) | (ratio_day_6 >= 0.5 & ratio_day_6 <= 2)) &
        ((ratio_day_7 == 0 & abs(diff_day_7) <= 2) | (ratio_day_7 >= 0.5 & ratio_day_7 <= 2))
    ) %>%
    mutate(
      hours_total = hours_total * ratio_total
    )
  print(
    paste(
      "Number of rows after filtering out those who made an inconsistent work arrangement on each day:",
      nrow(data)
    )
  )

  for (i in 1:7) {
    for (j in 1:3) {
      data <- data %>%
        mutate(
          !!paste("hours_place_", j, "_day_", i, sep = "") :=
            !!sym(paste("hours_place_", j, "_day_", i, sep = "")) *
              !!sym(paste("ratio_day_", i, sep = ""))
        )
    }
    data <- data %>%
      mutate(
        !!paste("hours_total_day_", i, sep = "") :=
          !!sym(paste("hours_total_day_", i, sep = "")) *
            !!sym(paste("ratio_day_", i, sep = ""))
      )
  }

  data <- data %>%
    filter(
      !if_any(
        matches("hours_total_day_\\d"),
        ~ . > 16
      )
    )
  print(
    paste(
      "Number of rows after filtering out those who worked more than 16 hours on any day:",
      nrow(data)
    )
  )

  return(data)
}
