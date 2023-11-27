fit_ls <- function(.predictor, .response, .fun, .par_ini) {
  
  create_ls <- function(x, y, fun) {
    function(p) {
      y_hat <- fun(x, p)
      sum((y - y_hat)^2)
    }
  }
  
  ls <- create_ls(.predictor, .response, linear_fun)
  
  fit <- optim(.par_ini, ls)
  
  param <- tibble(p = fit$par) |> 
    rowid_to_column(var = "p_n")
  
  pred <- tibble(x = seq(min(.predictor), max(.predictor), length = 100)) |> 
    mutate(y = .fun(x, param$p))
  
  list(param = param, 
       pred = pred)
  
}