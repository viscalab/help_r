calculate_t_ci <- function(x) {
  ci <- t.test(x) |>  
    tidy() |> 
    select(estimate, conf.low, conf.high) 
  
  se <- tibble(m = mean(x), 
               se = sd(x) / sqrt(length(x))) |> 
    mutate(se.low = m - se, se.high = m + se) |> 
    select(-m)
  
  bind_cols(ci, se)

}
