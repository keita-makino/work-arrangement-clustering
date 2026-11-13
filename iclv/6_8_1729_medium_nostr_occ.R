source(file.path("iclv", "create_variable_name_vector.R"))
source(file.path("iclv", "get_factor_attitude_connection.R"))
source(file.path("iclv", "find_fixed_beta.R"))
source(file.path("iclv", "get_apollo_prediction.R"))

# Companion of 6_8_1729_medium_occ.R without explanatory variables in the
# structural sub-model (the latent variables are standard normal). It is the
# earlier 6_8_1729_medium_nostr.R with the six occupation dummies added to the
# five cluster utilities, so that model is this one with those 30 parameters
# fixed at 0.

model_name <- "6_8_1729_medium_nostr_occ"

# "none" skips the Hessian (no standard errors, about 15-20 minutes). Use
# "analytic" (the Apollo default, which falls back to numerical derivatives) to
# get standard errors; the earlier main model needed about 8 hours for that step.
hessian_routine <- "analytic"

database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

apollo_initialise()

apollo_control <- list(
  modelName = model_name,
  modelDescr = "ICLV without structural covariates plus occupation in the cluster utilities",
  indivID = "row_index",
  outputDirectory = file.path("dist", "iclv"),
  nCores = 1
)

n_clusters <- 6
n_factors <- 8
n_sample <- 1729

iv_list <- list(
  "age_18_34" = 1,
  "age_55plus" = 3,
  "gender_women" = 1,
  "kids_yes" = 1,
  "genderkids_women_yes" = 1,
  "income_50minus" = 1,
  "income_100_149" = 3,
  "income_150plus" = 4,
  "education_highschool" = 1,
  "education_graduate" = 3,
  "jobtype_professional" = 2,
  "jobtype_stem" = 3,
  "jobtype_administrative" = 4,
  "jobtype_services" = 5,
  "jobtype_healthcare" = 6,
  "jobtype_creative" = 7,
  "neighborhood_suburban" = 2,
  "neighborhood_town" = 3,
  "homeownership_nonown" = 1,
  "transit_freq" = 0,
  "flx_sv" = 0
)

factor_attitude_connection <- get_factor_attitude_connection(
  n_factors,
  n_sample
) %>%
  slice(c(-9, -19, -20, -27))

variables <- c(
  paste(
    "asc",
    2:n_clusters,
    sep = "_"
  ),
  expand_grid(
    "b",
    2:n_clusters,
    iv_list %>%
      names()
  ) %>%
    create_variable_name_vector(),
  expand_grid(
    "l",
    2:n_clusters,
    (1:n_factors)
  ) %>%
    create_variable_name_vector(),
  expand_grid(
    "d",
    factor_attitude_connection
  ) %>%
    create_variable_name_vector(),
  expand_grid(
    "s",
    factor_attitude_connection
  ) %>%
    create_variable_name_vector(),
  0
) %>%
  head(-1)

apollo_beta <- setNames(
  rep(0, length(variables)), variables
)

apollo_beta[
  apollo_beta %>%
    names() %>%
    str_detect(
      .,
      "^[d|s]_"
    )
] <- 1

# Start from the estimates of the earlier no-structural model. The 30 new
# occupation parameters start at 0, so LL(start) is close to the final LL of
# that model (not equal: the earlier models were estimated in May 2025 on
# slightly different imputed income values).
model_base <- apollo_loadModel(
  file.path("dist", "iclv", "6_8_1729_medium_nostr")
)

stopifnot(
  setequal(
    names(model_base$estimate),
    names(apollo_beta) %>%
      str_subset("^b_\\d_jobtype_", negate = TRUE)
  )
)

apollo_beta[names(model_base$estimate)] <- model_base$estimate

apollo_fixed <- c()

apollo_draws <- list(
  interDrawsType = "mlhs",
  interNDraws = 200,
  interUnifDraws = c(),
  interNormDraws = paste(
    "n",
    (1:n_factors),
    sep = "_"
  ),
  intraDrawsType = "",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()

  # for (i in 1:n_factors) {
  #   term_list <- list()

  #   term_list[[1]] <- as.name(paste0("n_", i))

  #   for (j in regression_results[[i]]) {
  #     keys <- names(iv_list)[
  #       str_detect(
  #         names(iv_list),
  #         paste0("^", j, "_")
  #       )
  #     ]

  #     for (key in keys) {
  #       condition <- call("==", as.name(j), iv_list[[key]])

  #       coeff_name <- as.name(paste0("a_", i, "_", key))

  #       term_mult <- call("*", coeff_name, condition)

  #       term_list[[length(term_list) + 1]] <- term_mult
  #     }
  #   }

  #   expr_lv <- Reduce(function(x, y) call("+", x, y), term_list)

  #   randcoeff[[paste0("lv_", i)]] <- eval(expr_lv,
  #     envir = apollo_inputs
  #   )
  # }

  randcoeff[["lv_1"]] <- n_1 +
    0

  randcoeff[["lv_2"]] <- n_2 +
    0

  randcoeff[["lv_3"]] <- n_3 +
    0

  randcoeff[["lv_4"]] <- n_4 +
    0

  randcoeff[["lv_5"]] <- n_5 +
    0

  randcoeff[["lv_6"]] <- n_6 +
    0

  randcoeff[["lv_7"]] <- n_7 +
    0

  randcoeff[["lv_8"]] <- n_8 +
    0

  return(randcoeff)
}

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <-
  function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    # normalDensity_settings <- list()

    # for (i in 1:dim(factor_attitude_connection)[1]) {
    #   string = factor_attitude_connection[i, 1]
    #   attitude = str_split(string, "_")[[1]][1]
    #   factor = str_split(string, "_")[[1]][2]

    #   normalDensity_settings[[attitude]] = list(
    #     outcomeNormal = eval(parse(text = paste0("att_", attitude))),
    #     xNormal = eval(parse(text = paste0("d_", string))) * eval(parse(text = paste0("lv_", factor))),
    #     mu = 0,
    #     sigma = eval(parse(text = paste0("s_", string)))
    #   )

    #   P[[attitude]] <- apollo_normalDensity(
    #     normalDensity_settings[[attitude]],
    #     functionality
    #   )
    # }

    normalDensity_settings_01 <- list(outcomeNormal = att_01, xNormal = d_01_2 * lv_2, mu = 0, sigma = s_01_2)
    normalDensity_settings_02 <- list(outcomeNormal = att_02, xNormal = d_02_3 * lv_3, mu = 0, sigma = s_02_3)
    normalDensity_settings_03 <- list(outcomeNormal = att_03, xNormal = d_03_4 * lv_4, mu = 0, sigma = s_03_4)
    normalDensity_settings_04 <- list(outcomeNormal = att_04, xNormal = d_04_7 * lv_7, mu = 0, sigma = s_04_7)
    normalDensity_settings_05 <- list(outcomeNormal = att_05, xNormal = d_05_4 * lv_4, mu = 0, sigma = s_05_4)
    normalDensity_settings_06 <- list(outcomeNormal = att_06, xNormal = d_06_1 * lv_1, mu = 0, sigma = s_06_1)
    normalDensity_settings_07 <- list(outcomeNormal = att_07, xNormal = d_07_7 * lv_7, mu = 0, sigma = s_07_7)
    normalDensity_settings_08 <- list(outcomeNormal = att_08, xNormal = d_08_1 * lv_1, mu = 0, sigma = s_08_1)
    # normalDensity_settings_09 <- list(outcomeNormal = att_09, xNormal = d_09_5 * lv_5, mu = 0, sigma = s_09_5)
    normalDensity_settings_10 <- list(outcomeNormal = att_10, xNormal = d_10_1 * lv_1, mu = 0, sigma = s_10_1)
    normalDensity_settings_11 <- list(outcomeNormal = att_11, xNormal = d_11_6 * lv_6, mu = 0, sigma = s_11_6)
    normalDensity_settings_12 <- list(outcomeNormal = att_12, xNormal = d_12_8 * lv_8, mu = 0, sigma = s_12_8)
    normalDensity_settings_13 <- list(outcomeNormal = att_13, xNormal = d_13_7 * lv_7, mu = 0, sigma = s_13_7)
    normalDensity_settings_14 <- list(outcomeNormal = att_14, xNormal = d_14_8 * lv_8, mu = 0, sigma = s_14_8)
    normalDensity_settings_15 <- list(outcomeNormal = att_15, xNormal = d_15_7 * lv_7, mu = 0, sigma = s_15_7)
    normalDensity_settings_16 <- list(outcomeNormal = att_16, xNormal = d_16_3 * lv_3, mu = 0, sigma = s_16_3)
    normalDensity_settings_17 <- list(outcomeNormal = att_17, xNormal = d_17_1 * lv_1, mu = 0, sigma = s_17_1)
    normalDensity_settings_18 <- list(outcomeNormal = att_18, xNormal = d_18_2 * lv_2, mu = 0, sigma = s_18_2)
    # normalDensity_settings_19 <- list(outcomeNormal = att_19, xNormal = d_19_5 * lv_5, mu = 0, sigma = s_19_5)
    # normalDensity_settings_20 <- list(outcomeNormal = att_20, xNormal = d_20_4 * lv_4, mu = 0, sigma = s_20_4)
    normalDensity_settings_21 <- list(outcomeNormal = att_21, xNormal = d_21_8 * lv_8, mu = 0, sigma = s_21_8)
    normalDensity_settings_22 <- list(outcomeNormal = att_22, xNormal = d_22_2 * lv_2, mu = 0, sigma = s_22_2)
    normalDensity_settings_23 <- list(outcomeNormal = att_23, xNormal = d_23_2 * lv_2, mu = 0, sigma = s_23_2)
    normalDensity_settings_24 <- list(outcomeNormal = att_24, xNormal = d_24_2 * lv_2, mu = 0, sigma = s_24_2)
    normalDensity_settings_25 <- list(outcomeNormal = att_25, xNormal = d_25_1 * lv_1, mu = 0, sigma = s_25_1)
    normalDensity_settings_26 <- list(outcomeNormal = att_26, xNormal = d_26_6 * lv_6, mu = 0, sigma = s_26_6)
    # normalDensity_settings_27 <- list(outcomeNormal = att_27, xNormal = d_27_5 * lv_5, mu = 0, sigma = s_27_5)
    normalDensity_settings_28 <- list(outcomeNormal = att_28, xNormal = d_28_4 * lv_4, mu = 0, sigma = s_28_4)

    P[["att_01"]] <- apollo_normalDensity(normalDensity_settings_01, functionality)
    P[["att_02"]] <- apollo_normalDensity(normalDensity_settings_02, functionality)
    P[["att_03"]] <- apollo_normalDensity(normalDensity_settings_03, functionality)
    P[["att_04"]] <- apollo_normalDensity(normalDensity_settings_04, functionality)
    P[["att_05"]] <- apollo_normalDensity(normalDensity_settings_05, functionality)
    P[["att_06"]] <- apollo_normalDensity(normalDensity_settings_06, functionality)
    P[["att_07"]] <- apollo_normalDensity(normalDensity_settings_07, functionality)
    P[["att_08"]] <- apollo_normalDensity(normalDensity_settings_08, functionality)
    # P[["att_09"]] <- apollo_normalDensity(normalDensity_settings_09, functionality)
    P[["att_10"]] <- apollo_normalDensity(normalDensity_settings_10, functionality)
    P[["att_11"]] <- apollo_normalDensity(normalDensity_settings_11, functionality)
    P[["att_12"]] <- apollo_normalDensity(normalDensity_settings_12, functionality)
    P[["att_13"]] <- apollo_normalDensity(normalDensity_settings_13, functionality)
    P[["att_14"]] <- apollo_normalDensity(normalDensity_settings_14, functionality)
    P[["att_15"]] <- apollo_normalDensity(normalDensity_settings_15, functionality)
    P[["att_16"]] <- apollo_normalDensity(normalDensity_settings_16, functionality)
    P[["att_17"]] <- apollo_normalDensity(normalDensity_settings_17, functionality)
    P[["att_18"]] <- apollo_normalDensity(normalDensity_settings_18, functionality)
    # P[["att_19"]] <- apollo_normalDensity(normalDensity_settings_19, functionality)
    # P[["att_20"]] <- apollo_normalDensity(normalDensity_settings_20, functionality)
    P[["att_21"]] <- apollo_normalDensity(normalDensity_settings_21, functionality)
    P[["att_22"]] <- apollo_normalDensity(normalDensity_settings_22, functionality)
    P[["att_23"]] <- apollo_normalDensity(normalDensity_settings_23, functionality)
    P[["att_24"]] <- apollo_normalDensity(normalDensity_settings_24, functionality)
    P[["att_25"]] <- apollo_normalDensity(normalDensity_settings_25, functionality)
    P[["att_26"]] <- apollo_normalDensity(normalDensity_settings_26, functionality)
    # P[["att_27"]] <- apollo_normalDensity(normalDensity_settings_27, functionality)
    P[["att_28"]] <- apollo_normalDensity(normalDensity_settings_28, functionality)

    V <- list()
    V[["cluster1"]] <- 0

    # for (i in 2:n_clusters) {
    #   string = paste0("cluster", i)

    #   term = paste0("asc_", i)

    #   bs = variables[
    #     variables %>%
    #       str_detect(
    #         paste0("^b_", i, "_")
    #       )
    #   ]

    #   for (j in bs) {
    #     var = str_replace(j, paste0("^b_", i, "_"), "")
    #     condition = iv_list[
    #       iv_list %>%
    #         names() %>%
    #         str_detect(var)
    #     ][[1]]
    #     if (condition > 0) {
    #       term = paste0(
    #         term,
    #         " + ",
    #         j,
    #         " * (",
    #         var %>% str_split("_") %>% unlist() %>% .[1],
    #         " == ",
    #         condition,
    #         ")"
    #       )
    #     } else {
    #       term = paste0(
    #         term,
    #         " + ",
    #         j,
    #         " * ",
    #         var
    #       )
    #     }
    #   }

    #   ls = variables[
    #     variables %>%
    #       str_detect(
    #         paste0("^l_", i, "_")
    #       )
    #   ]

    #   for (j in ls) {
    #     var = str_replace(j, paste0("^l_", i, "_"), "")
    #     term = paste0(
    #       term,
    #       " + ",
    #       j,
    #       " * lv_",
    #       var
    #     )
    #   }

    #   V[[string]] <- eval(parse(text = term))
    # }

    V[["cluster2"]] <- asc_2 +
      b_2_age_18_34 * (age == 1) +
      b_2_age_55plus * (age == 3) +
      b_2_gender_women * (gender == 1) +
      b_2_kids_yes * (kids == 1) +
      b_2_genderkids_women_yes * (genderkids == 1) +
      b_2_income_50minus * (income == 1) +
      b_2_income_100_149 * (income == 3) +
      b_2_income_150plus * (income == 4) +
      b_2_education_highschool * (education == 1) +
      b_2_education_graduate * (education == 3) +
      b_2_jobtype_professional * (jobtype == 2) +
      b_2_jobtype_stem * (jobtype == 3) +
      b_2_jobtype_administrative * (jobtype == 4) +
      b_2_jobtype_services * (jobtype == 5) +
      b_2_jobtype_healthcare * (jobtype == 6) +
      b_2_jobtype_creative * (jobtype == 7) +
      b_2_neighborhood_suburban * (neighborhood == 2) +
      b_2_neighborhood_town * (neighborhood == 3) +
      b_2_homeownership_nonown * (homeownership == 1) +
      b_2_transit_freq * transit_freq +
      b_2_flx_sv * flx_sv +
      l_2_1 * lv_1 +
      l_2_2 * lv_2 +
      l_2_3 * lv_3 +
      l_2_4 * lv_4 +
      l_2_5 * lv_5 +
      l_2_6 * lv_6 +
      l_2_7 * lv_7 +
      l_2_8 * lv_8 +
      0

    V[["cluster3"]] <- asc_3 +
      b_3_age_18_34 * (age == 1) +
      b_3_age_55plus * (age == 3) +
      b_3_gender_women * (gender == 1) +
      b_3_kids_yes * (kids == 1) +
      b_3_genderkids_women_yes * (genderkids == 1) +
      b_3_income_50minus * (income == 1) +
      b_3_income_100_149 * (income == 3) +
      b_3_income_150plus * (income == 4) +
      b_3_education_highschool * (education == 1) +
      b_3_education_graduate * (education == 3) +
      b_3_jobtype_professional * (jobtype == 2) +
      b_3_jobtype_stem * (jobtype == 3) +
      b_3_jobtype_administrative * (jobtype == 4) +
      b_3_jobtype_services * (jobtype == 5) +
      b_3_jobtype_healthcare * (jobtype == 6) +
      b_3_jobtype_creative * (jobtype == 7) +
      b_3_neighborhood_suburban * (neighborhood == 2) +
      b_3_neighborhood_town * (neighborhood == 3) +
      b_3_homeownership_nonown * (homeownership == 1) +
      b_3_transit_freq * transit_freq +
      b_3_flx_sv * flx_sv +
      l_3_1 * lv_1 +
      l_3_2 * lv_2 +
      l_3_3 * lv_3 +
      l_3_4 * lv_4 +
      l_3_5 * lv_5 +
      l_3_6 * lv_6 +
      l_3_7 * lv_7 +
      l_3_8 * lv_8 +
      0

    V[["cluster4"]] <- asc_4 +
      b_4_age_18_34 * (age == 1) +
      b_4_age_55plus * (age == 3) +
      b_4_gender_women * (gender == 1) +
      b_4_kids_yes * (kids == 1) +
      b_4_genderkids_women_yes * (genderkids == 1) +
      b_4_income_50minus * (income == 1) +
      b_4_income_100_149 * (income == 3) +
      b_4_income_150plus * (income == 4) +
      b_4_education_highschool * (education == 1) +
      b_4_education_graduate * (education == 3) +
      b_4_jobtype_professional * (jobtype == 2) +
      b_4_jobtype_stem * (jobtype == 3) +
      b_4_jobtype_administrative * (jobtype == 4) +
      b_4_jobtype_services * (jobtype == 5) +
      b_4_jobtype_healthcare * (jobtype == 6) +
      b_4_jobtype_creative * (jobtype == 7) +
      b_4_neighborhood_suburban * (neighborhood == 2) +
      b_4_neighborhood_town * (neighborhood == 3) +
      b_4_homeownership_nonown * (homeownership == 1) +
      b_4_transit_freq * transit_freq +
      b_4_flx_sv * flx_sv +
      l_4_1 * lv_1 +
      l_4_2 * lv_2 +
      l_4_3 * lv_3 +
      l_4_4 * lv_4 +
      l_4_5 * lv_5 +
      l_4_6 * lv_6 +
      l_4_7 * lv_7 +
      l_4_8 * lv_8 +
      0

    V[["cluster5"]] <- asc_5 +
      b_5_age_18_34 * (age == 1) +
      b_5_age_55plus * (age == 3) +
      b_5_gender_women * (gender == 1) +
      b_5_kids_yes * (kids == 1) +
      b_5_genderkids_women_yes * (genderkids == 1) +
      b_5_income_50minus * (income == 1) +
      b_5_income_100_149 * (income == 3) +
      b_5_income_150plus * (income == 4) +
      b_5_education_highschool * (education == 1) +
      b_5_education_graduate * (education == 3) +
      b_5_jobtype_professional * (jobtype == 2) +
      b_5_jobtype_stem * (jobtype == 3) +
      b_5_jobtype_administrative * (jobtype == 4) +
      b_5_jobtype_services * (jobtype == 5) +
      b_5_jobtype_healthcare * (jobtype == 6) +
      b_5_jobtype_creative * (jobtype == 7) +
      b_5_neighborhood_suburban * (neighborhood == 2) +
      b_5_neighborhood_town * (neighborhood == 3) +
      b_5_homeownership_nonown * (homeownership == 1) +
      b_5_transit_freq * transit_freq +
      b_5_flx_sv * flx_sv +
      l_5_1 * lv_1 +
      l_5_2 * lv_2 +
      l_5_3 * lv_3 +
      l_5_4 * lv_4 +
      l_5_5 * lv_5 +
      l_5_6 * lv_6 +
      l_5_7 * lv_7 +
      l_5_8 * lv_8 +
      0

    V[["cluster6"]] <- asc_6 +
      b_6_age_18_34 * (age == 1) +
      b_6_age_55plus * (age == 3) +
      b_6_gender_women * (gender == 1) +
      b_6_kids_yes * (kids == 1) +
      b_6_genderkids_women_yes * (genderkids == 1) +
      b_6_income_50minus * (income == 1) +
      b_6_income_100_149 * (income == 3) +
      b_6_income_150plus * (income == 4) +
      b_6_education_highschool * (education == 1) +
      b_6_education_graduate * (education == 3) +
      b_6_jobtype_professional * (jobtype == 2) +
      b_6_jobtype_stem * (jobtype == 3) +
      b_6_jobtype_administrative * (jobtype == 4) +
      b_6_jobtype_services * (jobtype == 5) +
      b_6_jobtype_healthcare * (jobtype == 6) +
      b_6_jobtype_creative * (jobtype == 7) +
      b_6_neighborhood_suburban * (neighborhood == 2) +
      b_6_neighborhood_town * (neighborhood == 3) +
      b_6_homeownership_nonown * (homeownership == 1) +
      b_6_transit_freq * transit_freq +
      b_6_flx_sv * flx_sv +
      l_6_1 * lv_1 +
      l_6_2 * lv_2 +
      l_6_3 * lv_3 +
      l_6_4 * lv_4 +
      l_6_5 * lv_5 +
      l_6_6 * lv_6 +
      l_6_7 * lv_7 +
      l_6_8 * lv_8 +
      0

    mnl_settings <- list(
      alternatives = c(
        cluster1 = 1,
        cluster2 = 2,
        cluster3 = 3,
        cluster4 = 4,
        cluster5 = 5,
        cluster6 = 6
      ),
      avail = list(
        cluster1 = 1,
        cluster2 = 1,
        cluster3 = 1,
        cluster4 = 1,
        cluster5 = 1,
        cluster6 = 1
      ),
      choiceVar = cluster,
      utilities = V
    )

    # P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P[["choice"]] <- apollo_mnl(mnl_settings, functionality)

    P <- apollo_combineModels(P, apollo_inputs, functionality)
    P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)

    return(P)
  }

model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    hessianRoutine = hessian_routine
  )
)

apollo_saveOutput(model)
