imputation = function(data) {
  data = data %>%
    mutate(
      income = case_when(
        income == 0 ~ NA,
        TRUE ~ income
      ) %>%
        factor()
    )

  imputed <- mice::mice(
    data,
    m = 1,
    maxit = 50,
    method = "polr",
    seed = 42,
    print = FALSE,
  )

  data <- data %>%
    mutate(
      income = complete(imputed, 1)$income %>% as.numeric()
    )

  print(
    "Imputation of income completed"
  )
  return(data)
}
