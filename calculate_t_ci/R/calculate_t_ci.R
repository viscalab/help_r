calculate_t_ci <- function(.x) {
  t.test(.x) %>% 
    tidy() %>% 
    select(estimate, conf.low, conf.high)
}
