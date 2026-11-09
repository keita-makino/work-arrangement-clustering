create_database <- function(data) {
  data <- data %>%
    select(
      row_index,
      visited_total_1,
      visited_total_2,
      visited_total_3,
      vmt_commute,
      starts_with("hours_place_"),
      age,
      gender,
      income,
      kids,
      genderkids,
      education,
      jobtype,
      neighborhood,
      homeownership,
      pop_density,
      ret_svc_density,
      transit_freq,
      flx_sv,
      att_01,
      att_02,
      att_03,
      att_04,
      att_05,
      att_06,
      att_07,
      att_08,
      att_09,
      att_10,
      att_11,
      att_12,
      att_13,
      att_14,
      att_15,
      att_16,
      att_17,
      att_18,
      att_19,
      att_20,
      att_21,
      att_22,
      att_23,
      att_24,
      att_25,
      att_26,
      att_27,
      att_28,
      cluster,
      starts_with("factor")
    ) %>%
    mutate_if(
      is.numeric,
      as.numeric
    ) %>%
    mutate(
      flx_sv = case_when(
        flx_sv == 0 ~ 0,
        flx_sv == 1 ~ 0.1,
        flx_sv == 2 ~ 0.5,
        flx_sv == 3 ~ 1.5,
        flx_sv == 4 ~ 3.5,
        flx_sv == 5 ~ 5.5,
      )
    ) %>%
    mutate_at(
      vars(starts_with("att_")),
      function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    )

  return(data)
}
