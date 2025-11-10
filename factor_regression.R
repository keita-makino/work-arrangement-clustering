factor_regression = function() {
  data_regression <- readRDS(
    file.path(
      "data",
      "database.rds"
    )
  ) %>%
    select(
      row_index,
      age,
      gender,
      kids,
      genderkids,
      income,
      education,
      jobtype,
      neighborhood,
      homeownership,
      starts_with("factor")
    )

  regression_results = list()

  n_factors = data_regression %>%
    select(starts_with("factor")) %>%
    ncol()

  for (i in 1:n_factors) {
    coefs <- lm(
      as.formula(
        paste0(
          "factor_", i, " ~ (age == 1) +
      (age == 3) +
      (gender == 1) +
      (kids == 1) +
      (genderkids == 1) +
      (income == 1) +
      (income == 3) +
      (income == 4) +
      (education == 2) +
      (education == 3) +
      (jobtype == 2) +
      (jobtype == 3) +
      (jobtype == 4) +
      (jobtype == 5) +
      (jobtype == 6) +
      (jobtype == 7) +
      (neighborhood == 2) +
      (neighborhood == 3) +
      (homeownership == 1)"
        )
      ),
      data = data_regression
    ) %>%
      summary() %>%
      coef() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      as_tibble()

    significant_coef_name = coefs %>%
      filter(
        `Pr(>|t|)` < 0.01
      ) %>%
      mutate(
        rowname = rowname %>%
          str_replace("TRUE", "") %>%
          str_replace_all(" == \\d", "") %>%
          str_replace_all(" & ", "&")
      ) %>%
      select(rowname) %>%
      distinct() %>%
      filter(rowname != "(Intercept)") %>%
      pull()

    print(
      paste(
        "Regression of the factor",
        i,
        "is performed. The significant coefficients are:",
        paste(significant_coef_name, collapse = ", ")
      )
    )

    regression_results[[i]] <- significant_coef_name
  }

  return(regression_results)
}

regression_results = factor_regression()
