podivinszky_estimate_df <- crossing(t = c(50, 100, 500), rho = c(0, .5), theta = c(0, .5)) %>% 
  filter(!(rho != 0 & theta != 0)) %>%
  repeat_rows(200) %>% 
  splitted_mutate(
    dgp = pmap(list(rho, theta, t), .f = function(rho, theta, t) podivinszky_dgp(rho = rho, theta = theta, t = t, innov_sd = 100)),
    model = map(dgp, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    estimated_r = map_dbl(model, ~ rank.test(vecm = ., type = 'trace', cval = .05)$r),
    # .keep = "estimated_r"
  )

save(podivinszky_estimate_df, file = "data/podivinszky_estimate.RData")

podivinszky_dgp(rho = 0, theta = 1) %>% 
  ts() %>% 
  autoplot()

