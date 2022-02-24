estimated_rank_df <- crossing(n = 2:3, r = 0:2, 
                              nominal_size = c(.5, .1, .05, .025), 
                              t = c(50, 100, 200, 500)) %>% 
  filter(n > r) %>%
  repeat_rows(2000) %>% 
  splitted_mutate(
    dgp = pmap(list(n, r, t), .f = function(n, r, t) simulate_dgp(n, r, t)),
    model = map(dgp, ~ VECM(data = ., lag = 0, estim = "ML", include = "none")),
    estimated_r = map2_dbl(model, nominal_size, ~ rank.test(vecm = ., type = 'trace', cval = nominal_size)$r),
    .keep = "estimated_r"
  )

save(estimated_rank_df, file = "data/estimated_rank.RData")
