database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

database %>%
  select(
    pop_density,
    ret_svc_density,
    transit_freq
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "key",
    values_to = "value"
  ) %>%
  group_by(key) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  ) %>%
  write.table("clipboard", sep = ",", row.names = FALSE)

database %>%
  select(
    `Population Density` = pop_density,
    `Retail+Service Employment Density` = ret_svc_density,
    `Transit Freq. Index` = transit_freq
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "key",
    values_to = "value"
  ) %>%
  ggplot(
    aes(
      x = key,
      y = value
    )
  ) +
  geom_boxplot(
    outlier.shape = NA,
  ) +
  geom_jitter(
    aes(
      color = key
    ),
    alpha = 0.5,
    size = 0.5
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Built Environment Factors",
    y = "Value (Log transformed)",
  )

ggsave(
  filename = file.path(
    "dist",
    "descriptive",
    "built_environment_factors.png"
  ),
  width = 8,
  height = 5
)
