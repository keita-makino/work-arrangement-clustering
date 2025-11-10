compute_work_days = function(data) {
  data = data %>%
    select(
      row_index,
      matches("visited_day")
    ) %>%
    mutate(
      visited_total_1 =
        rowSums(
          select(
            .,
            starts_with("visited_day_")
          ) == 1
        ),
      visited_total_2 =
        rowSums(
          select(
            .,
            starts_with("visited_day_")
          ) == 2
        ),
      visited_total_3 =
        rowSums(
          select(
            .,
            starts_with("visited_day_")
          ) == 3
        ),
    ) %>%
    mutate(
      days_total = rowSums(select(., starts_with("visited_total")), na.rm = TRUE),
    ) %>%
    left_join(data) %>%
    select(-starts_with("visited_day_"))

  print(
    "Total days visited for each work location computed"
  )


  ratio_working_at_one_place = data %>%
    select(
      starts_with("hours_"),
      -hours_total,
    ) %>%
    pivot_longer(
      everything(),
      names_to = c(".value", "day"),
      names_pattern = "hours_(.*)_day_(\\d)"
    ) %>%
    filter(total > 0) %>%
    mutate(
      is_working_at_one_place = case_when(
        pmax(
          place_1,
          place_2,
          place_3
        )
        == total ~ 1,
        T ~ 0,
      )
    ) %>%
    select(
      is_working_at_one_place
    ) %>%
    summarise(
      ratio_working_at_one_place = mean(is_working_at_one_place, na.rm = TRUE),
    )

  print(
    paste(
      "Ratio of working at one place:",
      ratio_working_at_one_place %>% round(3)
    )
  )

  return(data)
}
