database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

database %>%
  select(starts_with("att")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "key",
    values_to = "value"
  ) %>%
  group_by(key, value) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  group_by(key) %>%
  mutate(
    key = factor(key,
      levels = c(
        "att_01",
        "att_02",
        "att_03",
        "att_04",
        "att_05",
        "att_06",
        "att_07",
        "att_08",
        "att_09",
        "att_10",
        "att_11",
        "att_12",
        "att_13",
        "att_14",
        "att_15",
        "att_16",
        "att_17",
        "att_18",
        "att_19",
        "att_20",
        "att_21",
        "att_22",
        "att_23",
        "att_24",
        "att_25",
        "att_26",
        "att_27",
        "att_28"
      ),
      labels = c(
        "I like trying things that are new and different. (S01)",
        "My schedule makes it hard or impossible for me to use public transportation. (S02)",
        "I like walking. (S03)",
        "I definitely want to own a car. (S04)",
        "Getting regular exercise is very important to me. (S05)",
        "We should raise the cost of driving to reduce the negative impacts of driving on the environment. (S06)",
        "I like driving a car. (S07)",
        "I always think about ways in which I can reduce my impact on the environment. (S08)",
        "Traffic congestion is a major problem in the region where I live. (S09)",
        "I am fine with not owning a car, as long as I can use/rent one any time I need it. (S10)",
        "I like riding a bike. (S11)",
        "I prefer to live in a spacious home, even if it is farther from public transportation and many places I go. (S12)",
        "To me, a car is just a way to get from place to place. (S13)",
        "I am against giving more space to pedestrians and cyclists on the road network if it implies less space for cars. (S14)",
        "I prefer to be a driver rather than a passenger. (S15)",
        "Most of the time, I have no reasonable alternative to driving. (S16)",
        "I like the idea of public transit as a means of transportation for me. (S17)",
        "I like to be among the first people to have the latest technology. (S18)",
        "I make efforts to adjust my schedule (e.g., leave earlier/later than needed) to avoid traffic congestion. (S19)",
        "I am generally satisfied with my transportation options. (S20)",
        "I like the idea of having stores, restaurants, and offices mixed among the homes in my neighborhood. (S21)",
        "Having Wi-Fi and/or good internet access on my mobile phone everywhere I go is essential to me. (S22)",
        "I am willing to pay more money to have a quicker trip. (S23)",
        "I'll stretch my budget to buy something new and exciting. (S24)",
        "We should raise the cost of driving to provide funding for better public transportation. (S25)",
        "If I felt protected from car traffic, I would ride a bicycle more often. (S26)",
        "I am less likely to drive if parking is difficult or expensive. (S27)",
        "I am generally satisfied with my life. (S28)"
      )
    ) %>% fct_rev(),
    value = factor(value,
      labels = c(
        "Strongly Disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly Agree"
      ),
    ),
    ratio = count / sum(count)
  ) %>%
  ggplot(
    aes(
      x = value,
      y = key
    )
  ) +
  geom_tile(
    aes(
      fill = ratio
    )
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.1f",
        ratio * 100
      ) %>% paste0("%")
    ),
    size = 3,
    color = "white"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(
  filename = file.path(
    "dist",
    "descriptive",
    "attitudes.png"
  ),
  width = 12,
  height = 8
)
