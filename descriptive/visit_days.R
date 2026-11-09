database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

database %>%
  select(
    Primary = visited_total_1,
    Temporary = visited_total_2,
    Home = visited_total_3
  ) %>%
  mutate(
    Total = Primary + Temporary + Home
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "place",
    values_to = "days"
  ) %>%
  group_by(place, days) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = days,
      y = count,
      group = place,
      color = place
    )
  ) +
  geom_line(
    stat = "identity"
  ) +
  geom_point(
    stat = "identity"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(0, 7, by = 1)
  ) +
  labs(
    x = "Days visited to each workplace",
    y = "Count",
    color = "Workplace"
  )

ggsave(
  filename = file.path(
    "dist",
    "descriptive",
    "visit_days_by_place.png"
  ),
  width = 8,
  height = 4
)
