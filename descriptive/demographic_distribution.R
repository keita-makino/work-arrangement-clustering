database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

database %>%
  select(
    age,
    gender,
    kids,
    income,
    education,
    jobtype,
    flx_sv,
    neighborhood,
    homeownership
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "key",
    values_to = "value"
  ) %>%
  mutate(
    key = factor(key,
      levels = c(
        "age",
        "gender",
        "kids",
        "income",
        "education",
        "jobtype",
        "flx_sv",
        "neighborhood",
        "homeownership"
      ),
    )
  ) %>%
  group_by(key, value) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  group_by(key) %>%
  mutate(
    ratio = count / sum(count)
  ) %>%
  write.table("clipboard", sep = ",", row.names = FALSE)

df_ds = database %>%
  select(row_index, age, gender, income, kids, education, neighborhood, homeownership, flx_sv) %>%
  mutate(
    age = case_when(
      age == 1 ~ "-34",
      age == 2 ~ "35-54",
      T ~ "55-"
    ) %>%
      factor(
        levels = c(
          "-34",
          "35-54",
          "55-"
        )
      ),
    gender = case_when(
      gender == 1 ~ "Women",
      T ~ "Non-Women"
    ) %>%
      factor(
        levels = c(
          "Women",
          "Non-Women"
        )
      ),
    income = case_when(
      income == 1 ~ "-$49,999",
      income == 2 ~ "$50,000-$99,999",
      income == 3 ~ "$100,000-$149,999",
      T ~ "$150,000-"
    ) %>%
      factor(
        levels = c(
          "-$49,999",
          "$50,000-$99,999",
          "$100,000-$149,999",
          "$150,000-"
        )
      ),
    kids = case_when(
      kids > 0 ~ "Yes",
      T ~ "No"
    ) %>%
      factor(
        levels = c(
          "No",
          "Yes"
        )
      ),
    education = case_when(
      education == 1 ~ "High School or Less",
      education == 2 ~ "Bachelor's",
      T ~ "Graduate/Professional"
    ) %>%
      factor(
        levels = c(
          "High School or Less",
          "Bachelor's",
          "Graduate/Professional"
        )
      ),
    neighborhood = case_when(
      neighborhood == 1 ~ "Urban",
      neighborhood == 2 ~ "Suburban",
      T ~ "Rural"
    ) %>%
      factor(
        levels = c(
          "Urban",
          "Suburban",
          "Rural"
        )
      ),
    homeownership = case_when(
      homeownership == 1 ~ "Non-Own",
      T ~ "Own"
    ) %>%
      factor(
        levels = c(
          "Non-Own",
          "Own"
        )
      ),
    flx_sv = case_when(
      flx_sv == 0 ~ "None",
      flx_sv == 0.1 ~ "Less than a month",
      flx_sv == 0.5 ~ "1-3 a month",
      flx_sv == 1.5 ~ "1-2 a week",
      flx_sv == 3.5 ~ "3-4 a week",
      T ~ "5+ a week"
    ) %>%
      factor(
        levels = c(
          "None",
          "Less than a month",
          "1-3 a month",
          "1-2 a week",
          "3-4 a week",
          "5+ a week"
        )
      )
  ) %>%
  pivot_longer(
    cols = -row_index,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, value) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  mutate(
    variable = case_when(
      variable == "age" ~ "Age",
      variable == "gender" ~ "Gender",
      variable == "income" ~ "Household Income",
      variable == "kids" ~ "Kids",
      variable == "education" ~ "Education",
      variable == "neighborhood" ~ "Neighborhood",
      variable == "homeownership" ~ "Home Ownership",
      variable == "flx_sv" ~ "Max. Remote-Work Days"
    ) %>%
      factor(
        levels = c(
          "Age",
          "Gender",
          "Household Income",
          "Kids",
          "Education",
          "Neighborhood",
          "Home Ownership",
          "Max. Remote-Work Days"
        )
      )
  )

make_plot_tile = function(var) {
  p = df_ds %>%
    filter(variable == var) %>%
    ggplot(
      aes(
        x = value,
        y = percent,
        fill = value
      )
    ) +
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +
    labs(
      x = var,
      fill = "Value"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 7.5, hjust = 0.75),
      axis.title.y = element_blank(),
      legend.position = "none"
    )

  return(p)
}

p1 = make_plot_tile("Age")
p2 = make_plot_tile("Gender")
p3 = make_plot_tile("Household Income")
p4 = make_plot_tile("Kids")
p5 = make_plot_tile("Education")
p6 = make_plot_tile("Neighborhood")
p7 = make_plot_tile("Home Ownership")
p8 = make_plot_tile("Max. Remote-Work Days")
py = ggplot() +
  theme_void() +
  labs(y = "Share (%)") +
  theme(axis.title.y = element_text(angle = 90, size = 12, vjust = 0))

p = py + (
  (p1 + p2 + p3) /
    (p4 + p5 + p6) /
    (p7 + p8 + plot_layout(widths = c(1, 2)))) +
  plot_layout(
    widths = c(0.01, 1)
  ) +
  plot_annotation(
    title = "Distribution of Demographic Characteristics / Remote-Work Permission (n=1,729)",
    theme = theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
    )
  )

ggsave(
  filename = file.path(
    "dist",
    "descriptive",
    "demographics_summary.png"
  ),
  plot = p,
  width = 12,
  height = 6.75,
  units = "in",
  dpi = 300
)
