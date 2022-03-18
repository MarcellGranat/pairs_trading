podivinszky_no_wp_estimate <- crossing(t = seq(from = 50, to = 500, by = 50), rho = c(0, 1), theta = c(0, 1)) %>% 
  filter(!(rho == 1 & theta == 0)) %>% 
  repeat_rows(2000) %>% 
  splitted_mutate(
    dgp = pmap(list(rho, theta, t), .f = function(rho, theta, t) podivinszky_dgp(rho = rho, theta = theta, t = t, warm_up = 0)),
    model_xyz = map(dgp, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    xyz_r = map_dbl(model_xyz, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    # xy
    dgp_xy = map(dgp, select, x, y),
    model_xy = map(dgp_xy, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    xy_r = map_dbl(model_xy, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    .keep = c("xyz_r", "xy_r")
  )

save(podivinszky_no_wp_estimate, file = "data/podivinszky_no_wp_estimate.RData")
