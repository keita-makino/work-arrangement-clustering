compute_work_hours = function(data) {
  data <- data %>%
    select(
      row_index,
      matches("^work_matrix_\\d{1,2}_\\d{1,2}$")
    ) %>%
    gather(
      key = "key",
      value = "value",
      matches("^work_matrix_\\d{1,2}_\\d{1,2}$"),
    ) %>%
    mutate(
      day = str_replace(
        key,
        "work_matrix_(\\d{1,2})_\\d{1,2}",
        "\\1"
      ) %>% as.numeric(),
      time = str_extract(key, "\\d{1,2}$") %>% as.numeric()
    ) %>%
    mutate(
      binary = intToBin(value),
      place_1 = (binary %>% substr(1, 1) %>% as.numeric()) /
        (binary %>% str_count("1")),
      place_2 = (binary %>% substr(2, 2) %>% as.numeric()) /
        (binary %>% str_count("1")),
      place_3 = (binary %>% substr(3, 3) %>% as.numeric()) /
        (binary %>% str_count("1")),
      place_4 = pmin(value, 1),
    ) %>%
    mutate_all(
      ~ replace_na(., 0)
    ) %>%
    mutate(
      across(
        starts_with("place"),
        ~ case_when(
          str_detect(key, "_1$") ~ . * 6,
          TRUE ~ . * 2
        )
      )
    ) %>%
    group_by(row_index, day) %>%
    reframe(
      place_1 = sum(place_1, na.rm = TRUE),
      place_2 = sum(place_2, na.rm = TRUE),
      place_3 = sum(place_3, na.rm = TRUE),
      place_4 = sum(place_4, na.rm = TRUE)
    ) %>%
    mutate(
      visited = case_when(
        place_1 > 0 ~ 1,
        place_2 > 0 ~ 2,
        place_3 > 0 ~ 3,
        TRUE ~ 0
      )
    ) %>%
    pivot_wider(
      names_from = day,
      values_from = c(starts_with("place"), visited),
      names_prefix = "day_"
    ) %>%
    rename_at(
      vars(starts_with("place")),
      ~ paste0("hours_", .)
    ) %>%
    rename_at(
      vars(starts_with("hours_place_4")),
      ~ str_replace(., "hours_place_4", "hours_total")
    ) %>%
    mutate(
      hours_total = rowSums(select(., starts_with("hours_total")), na.rm = TRUE),
    ) %>%
    left_join(data) %>%
    mutate(
      naive_total = rowSums(select(., starts_with("work_hour_last_week_day_")), na.rm = TRUE),
    ) %>%
    select(
      -starts_with("work_matrix_"),
    )

  print(
    "Work hours of each day at each location and total have been computed"
  )

  return(data)
}
