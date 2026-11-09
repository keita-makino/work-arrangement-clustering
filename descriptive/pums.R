dotenv::load_dot_env()

tidycensus::census_api_key(Sys.getenv("PUMS_KEY"), install = FALSE)

pums <- get_pums(
  variables = c(
    "ESR",
    "WKHP",
    "WKWN",
    "AGEP",
    "SEX",
    "HUPAC",
    "HINCP",
    "SCHL",
    "TEN",
    "SOCP",
    "PWGTP"
  ),
  state = "CA",
  year = 2023,
  survey = "acs1",
  rep_weights = "person"
)

ft_workers <- pums %>%
  filter(
    ESR %in% c(1, 4),
    WKHP > 20,
    AGEP >= 18 & AGEP <= 90
  )

ft_workers %>%
  transmute(
    age = case_when(
      AGEP >= 18 & AGEP <= 34 ~ "18-34",
      AGEP >= 35 & AGEP <= 54 ~ "35-54",
      AGEP >= 55 & AGEP <= 90 ~ "55+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(age) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )

ft_workers %>%
  transmute(
    gender = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(gender) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )

ft_workers %>%
  transmute(
    children = case_when(
      HUPAC %in% c(1, 2, 3) ~ "With children",
      HUPAC == 4 ~ "No children",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "No children",
          "With children"
        )
      )
  ) %>%
  group_by(children) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )

ft_workers %>%
  transmute(
    weight = PWGTP,
    income = case_when(
      HINCP < 50000 ~ "-50k",
      HINCP >= 50000 & HINCP < 100000 ~ "50-100k",
      HINCP >= 100000 & HINCP < 150000 ~ "100-150k",
      HINCP >= 150000 ~ "150k+",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "-50k",
          "50-100k",
          "100-150k",
          "150k+"
        )
      )
  ) %>%
  group_by(income) %>%
  summarise(
    n = sum(weight),
  ) %>%
  ungroup() %>%
  mutate(
    share = (n / sum(n) * 100) %>% sprintf("%.1f%%", .)
  )


ft_workers %>%
  transmute(
    education = case_when(
      SCHL <= 20 ~ "High school or associate's",
      SCHL == 21 ~ "Bachelor's",
      SCHL >= 22 ~ "Post-graduate",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "High school or associate's",
          "Bachelor's",
          "Post-graduate"
        )
      )
  ) %>%
  group_by(education) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )

ft_workers$TEN %>% table()

ft_workers %>%
  transmute(
    tenure = case_when(
      TEN == 1 | TEN == 2 ~ "Own",
      TEN == 3 | TEN == 4 ~ "Non-own",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "Own",
          "Non-own"
        )
      )
  ) %>%
  group_by(tenure) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )

dhc_vars <- load_variables(2020, "dhc", cache = TRUE)

ft_workers %>%
  transmute(
    soc_major = substr(SOCP, 1, 2),
    occ_category = case_when(
      soc_major %in% c("11", "13", "23", "41") ~ "Professional",
      soc_major %in% c("15", "17", "19") ~ "STEM",
      soc_major == "43" ~ "Administrative Support",
      soc_major %in% c(
        "21", "25", "33", "35",
        "37", "39"
      ) ~ "Services",
      soc_major == "27" ~ "Creative",
      soc_major %in% c("29", "31") ~ "Healthcare",
      soc_major %in% c(
        "45", "47", "49", "51",
        "53", "55"
      ) ~ "Skilled Trades",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(occ_category) %>%
  summarise(
    share = (n() / nrow(ft_workers) * 100) %>% sprintf("%.1f%%", .)
  )
