library(rvest)
library(tidyverse)

yield_df <- map_df(1990:2022, ~ {
read_html(str_c("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value=", .)) %>% 
  html_table() %>% 
  first() %>% 
  select(Date, `1 Mo`:last_col()) %>% 
  mutate_all(as.character)
})

yield_df <- yield_df %>% 
  rename_at(-1, ~ str_c(str_remove_all(., "\\d|\\W"), parse_number(.))) %>% 
  janitor::clean_names() %>% 
  rename(time = date) %>% 
  mutate_at(-1, as.numeric) %>% 
  mutate(time = lubridate::mdy(time))

save(yield_df, file = "data/yield_df.RData")
