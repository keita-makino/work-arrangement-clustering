source(file.path("iclv", "create_variable_name_vector.R"))
source(file.path("iclv", "find_fixed_beta.R"))
source(file.path("iclv", "get_apollo_prediction.R"))

# Companion of 6_8_1729_medium_occ.R without latent variables: a multinomial
# logit of cluster membership. It is the earlier 6_8_1729_medium_mnl.R with the
# six occupation dummies added to the five cluster utilities, so that model is
# this one with those 30 parameters fixed at 0.

model_name <- "6_8_1729_medium_mnl_occ"

# "analytic" is the Apollo default; "none" skips the Hessian (no standard errors).
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
  modelDescr = "MNL plus occupation in the cluster utilities",
  indivID = "row_index",
  outputDirectory = file.path("dist", "iclv"),
  nCores = 1
)

n_clusters <- 6
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
  0
) %>%
  head(-1)

apollo_beta <- setNames(
  rep(0, length(variables)), variables
)

# Start from the estimates of the earlier MNL. The 30 new occupation parameters
# start at 0, so LL(start) is close to the final LL of that model (not equal:
# the earlier models were estimated in May 2025 on slightly different imputed
# income values).
model_base <- apollo_loadModel(
  file.path("dist", "iclv", "6_8_1729_medium_mnl")
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

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <-
  function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()

    V <- list()
    V[["cluster1"]] <- 0

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

    P[["model"]] <- apollo_mnl(mnl_settings, functionality)

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
