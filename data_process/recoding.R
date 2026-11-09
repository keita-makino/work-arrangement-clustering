recoding <- function(data) {
  data <- data %>%
    mutate(
      age = 2024 - as.numeric(yearborn),
      age = case_when(
        age < 35 ~ 1,
        age < 55 ~ 2,
        TRUE ~ 3
      ) %>%
        labelled(
          label = "Age group",
          labels = c(
            "Under 35" = 1,
            "35-54" = 2,
            "55+" = 3
          )
        ),
      gender = case_when(
        gender == 1 ~ 1,
        TRUE ~ 2
      ) %>%
        labelled(
          label = "Gender",
          labels = c(
            "Woman" = 1,
            "Non-Woman" = 2
          )
        ),
      kids = case_when(
        kids > 0 ~ 1,
        TRUE ~ 0
      ) %>%
        labelled(
          label = "Kids",
          labels = c(
            "Yes" = 1,
            "No" = 0
          )
        ),
      genderkids = case_when(
        gender == 1 & kids == 1 ~ 1,
        T ~ 0
      ) %>%
        labelled(
          label = "genderkids",
          labels = c(
            "Yes" = 1,
            "No" = 0
          )
        ),
      income = case_when(
        income == 0 ~ 0,
        income < 3 ~ 1,
        income < 5 ~ 2,
        income < 6 ~ 3,
        TRUE ~ 4
      ) %>%
        labelled(
          label = "Household income level",
          labels = c(
            "No response" = 0,
            "Less than $50,000" = 1,
            "$50,000 - $99,999" = 2,
            "$100,000 - $149,999" = 3,
            "$150,000+" = 4
          )
        ),
      education = case_when(
        education < 4 ~ 1,
        education < 5 ~ 2,
        TRUE ~ 3
      ) %>%
        labelled(
          label = "Education level",
          labels = c(
            "Associate or Less" = 1,
            "Bachelor's" = 2,
            "Graduate or Professional Degree" = 3
          )
        ),
      occupation = case_when(
        T ~ occupation %>%
          labelled(
            label = "Occupation",
            labels = c(
              "Farming, Fishing, Forestry, and Extraction" = 1,
              "Construction, Installation, Maintenance, Repair, and Production" = 2,
              "Sales and Related" = 3,
              "Transportation and Material Moving" = 4,
              "Business and Financial Operations" = 5,
              "Computer and Mathematical, Architecture and Engineering, Life, Physical, and Social Science" = 6,
              "Management and Legal" = 7,
              "Office and Administrative Support" = 8,
              "Protective Service, Building and Grounds Cleaning and Maintenance" = 9,
              "Educational Instruction and Library" = 10,
              "Healthcare Practitioners and Technical, Healthcare Support" = 11,
              "Arts, Design, Entertainment, Sports, and Media" = 12,
              "Food Preparation and Serving Related" = 13,
              "Personal Care and Service" = 14,
              "Community and Social Service" = 15,
              "Military Specific" = 16
            )
          )
      ),
      neighborhood = case_when(
        neighborhood == 1 ~ 1,
        neighborhood == 2 ~ 2,
        TRUE ~ 3
      ) %>%
        labelled(
          label = "Neighborhood type",
          labels = c(
            "Urban" = 1,
            "Suburban" = 2,
            "Small Town or Rural" = 3
          )
        ),
      jobtype = case_when(
        occupation == 1 |
          occupation == 2 |
          occupation == 4 |
          occupation == 16 ~ 1,
        occupation == 3 |
          occupation == 5 |
          occupation == 7 ~ 2,
        occupation == 6 ~ 3,
        occupation == 8 ~ 4,
        occupation == 9 |
          occupation == 10 |
          occupation == 13 |
          occupation == 14 |
          occupation == 15 ~ 5,
        occupation == 11 ~ 6,
        T ~ 7
      ) %>%
        labelled(
          label = "Job Type",
          labels = c(
            "Skilled Trades" = 1,
            "Professional" = 2,
            "STEM" = 3,
            "Administrative Support" = 4,
            "Services" = 5,
            "Healthcare" = 6,
            "Creative" = 7
          )
        ),
      homeownership = case_when(
        homeownership == 2 ~ 2,
        T ~ 1,
      ) %>%
        as.numeric() %>%
        labelled(
          label = "Work time flexibility",
          labels = c(
            "Own" = 2,
            "Non-Own" = 1
          )
        ),
    ) %>%
    select(
      -yearborn,
    )

  print(
    "Recoding of demographic variables has been completed"
  )

  return(data)
}
