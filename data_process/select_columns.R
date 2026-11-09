select_columns = function(data) {
  data = data %>%
    select(
      row_index,
      ResponseId,
      att_01 = A_a,
      att_02 = A_b,
      att_03 = A_c,
      att_04 = A_d,
      att_05 = A_e,
      att_06 = A_f,
      att_07 = A_g,
      att_08 = A_h,
      att_09 = A_j,
      att_10 = A_k,
      att_11 = A_l,
      att_12 = A_m,
      att_13 = A_n,
      att_14 = A_o,
      att_15 = A_p,
      att_16 = A_q,
      att_17 = A_r,
      att_18 = A_s,
      att_19 = A_t,
      att_20 = A_u,
      att_21 = A_v,
      att_22 = A_w,
      att_23 = A_x,
      att_24 = A_y,
      att_25 = A_z,
      att_26 = A_ab,
      att_27 = A_ac,
      att_28 = A_ad,
      yearborn = B1_1,
      gender = B5,
      education = B7,
      income = B8,
      regular_work_hour = C3_1,
      occupation = C4,
      flx_sv = C5_2a,
      flx_nt = C5_2b,
      flx_pr = C5_2c,
      work_home_dist = C11_1,
      commute_pv_freq = E2_a,
      suggested_vmt_commute = E6,
      reported_vmt_commute = E6_1_TEXT,
      kids = D2_b,
      neighborhood = D3,
      starts_with("C13"),
      homeownership = D6
    ) %>%
    rename_at(
      vars(starts_with("C13a")),
      ~ str_replace(., "C13a", "work_hour_last_week_day")
    ) %>%
    rename_at(
      vars(starts_with("C13b")),
      ~ str_replace(., "C13b", "work_matrix")
    )

  return(data)
}
