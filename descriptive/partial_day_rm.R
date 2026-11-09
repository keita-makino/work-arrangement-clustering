database %>%
  select(row_index, starts_with("hours_place_")) %>%
  pivot_longer(
    cols = starts_with("hours_place_"),
    names_to = "type",
    values_to = "hours"
  ) %>%
  filter(hours > 0) %>%
  mutate(
    place = str_extract(type, "place_[0-9]+") %>%
      str_replace("place_", "") %>%
      as.integer() %>%
      10^. /
      10,
    day = str_extract(type, "day_[0-9]+") %>%
      str_replace("day_", "") %>%
      as.integer()
  ) %>%
  select(-type, -hours) %>%
  group_by(row_index, day) %>%
  summarise(
    location = sum(place)
  ) %>%
  mutate(
    location = case_when(
      location == 100 ~ "primary",
      location == 10 ~ "temporary",
      location == 1 ~ "home",
      location == 110 ~ "primary_temporary",
      location == 101 ~ "primary_home",
      location == 11 ~ "temporary_home",
      location == 111 ~ "primary_temporary_home",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  group_by(location) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    share = (count / sum(count) * 100) %>% sprintf("%.2f%%", .)
  )
