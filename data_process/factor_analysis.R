factor_analysis <- function(data, n_factors) {
  labels <- c(
    "I like trying things that are new and different.",
    "My schedule makes it hard or impossible for me to use public transportation.",
    "I like walking.",
    "I definitely want to own a car.",
    "Getting regular exercise is very important to me.",
    "We should raise the cost of driving to reduce the negative impacts of driving on the environment.",
    "I like driving a car.",
    "I always think about ways in which I can reduce my impact on the environment.",
    "Traffic congestion is a major problem in the region where I live.",
    "I am fine with not owning a car, as long as I can use/rent one any time I need it.",
    "I like riding a bike.",
    "I prefer to live in a spacious home, even if it is farther from public transportation and many places I go.",
    "To me, a car is just a way to get from place to place.",
    "I am against giving more space to pedestrians and cyclists on the road network if it implies less space for cars.",
    "I prefer to be a driver rather than a passenger.",
    "Most of the time, I have no reasonable alternative to driving.",
    "I like the idea of public transit as a means of transportation for me.",
    "I like to be among the first people to have the latest technology.",
    "I make efforts to adjust my schedule (e.g., leave earlier/later than needed) to avoid traffic congestion.",
    "I am generally satisfied with my transportation options.",
    "I like the idea of having stores, restaurants, and offices mixed among the homes in my neighborhood.",
    "Having Wi-Fi and/or good internet access on my mobile phone everywhere I go is essential to me.",
    "I am willing to pay more money to have a quicker trip.",
    "I'll stretch my budget to buy something new and exciting.",
    "We should raise the cost of driving to provide funding for better public transportation.",
    "If I felt protected from car traffic, I would ride a bicycle more often.",
    "I am less likely to drive if parking is difficult or expensive.",
    "I am generally satisfied with my life."
  )

  is_done = F
  rows = rep(T, 28)

  factor = NA

  while (!is_done) {
    data_temp <- data %>%
      select(
        starts_with("att_"),
      ) %>%
      .[, rows] %>%
      mutate_all(~ replace_na(., 3))

    factor <- pca(
      data_temp,
      nfactors = n_factors,
      rotate = "varimax"
    )

    factor_score <- data.frame(
      psych::factor.scores(
        data_temp, factor
      )$scores
    )

    check_significance <- factor$loadings[] %>%
      tibble() %>%
      rowwise() %>%
      summarize(
        n_factors = any(abs(.) > 0.3),
      ) %>%
      deframe()


    if (all(check_significance)) {
      is_done <- T
    }

    false_rows <- which(!check_significance)
    true_indices <- which(rows)
    to_false <- true_indices[false_rows]
    for (i in to_false) {
      rows[i] <- F
    }
  }

  print(
    "The following statements did not have a loading of more than 0.3 or less than -0.3 in the factor analysis: ",
  )

  print(
    labels[which(!rows)]
  )

  factor_result = factor$loadings[] %>%
    as_tibble() %>%
    mutate(statement = c(labels[rows])) %>%
    select(statement, all_of(sort(setdiff(names(.), "statement")))) %>%
    mutate(index = which(rows))

  factor_result %>%
    write_csv(
      file.path(
        "dist",
        "factor",
        paste0(
          "loadings_a_",
          n_factors,
          "_n_",
          nrow(data),
          ".csv"
        )
      )
    )

  print(
    paste0(
      "Factor analysis with ",
      n_factors,
      " factors completed. The results are saved in the output folder."
    )
  )

  print(
    paste0(
      "Cumulative Var: ",
      paste(
        factor$Vaccounted[3, ] %>%
          round(2),
        collapse = ", "
      )
    )
  )

  data <- data %>%
    left_join(
      factor$scores %>%
        as_tibble() %>%
        rename_at(
          vars(starts_with("RC")),
          ~ {
            paste0("factor_", .x %>% str_remove("RC"))
          }
        ) %>%
        select(all_of(sort(names(.)))) %>%
        mutate(
          row_index = data$row_index
        )
    )

  return(data)
}
