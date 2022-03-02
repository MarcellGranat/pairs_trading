podivinszky_estimate_df <- crossing(t = c(50, 100, 500), rho = c(0, 1), theta = c(0, 1)) %>% 
  filter(!(rho == 1 & theta == 0)) %>% 
  repeat_rows(2000) %>% 
  splitted_mutate(
    dgp = pmap(list(rho, theta, t), .f = function(rho, theta, t) podivinszky_dgp(rho = rho, theta = theta, t = t)),
    model_xyz = map(dgp, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    xyz_r = map_dbl(model_xyz, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    # xy
    dgp_xy = map(dgp, select, x, y),
    model_xy = map(dgp_xy, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    xy_r = map_dbl(model_xy, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    # xz
    dgp_xz = map(dgp, select, x, z),
    model_xz = map(dgp_xz, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    xz_r = map_dbl(model_xz, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    # yz
    dgp_yz = map(dgp, select, y, z),
    model_yz = map(dgp_yz, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    yz_r = map_dbl(model_yz, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),

    .keep = c("xyz_r", "xy_r", "xz_r", "yz_r")
  )

save(podivinszky_estimate_df, file = "data/podivinszky_estimate.RData")