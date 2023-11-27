fit_ls <- function(.data, .response, .predictor, .fun, .par_ini) {
  x <- .data |> 
    select({{.predictor}})
  
  y <- .data |> 
    select({{.response}})
  
  create_ls <- function(x, y, fun) {
    function(p) {
      y_hat <- fun(x, p)
      sum((y - y_hat)^2)
    }
  }
  
  ls <- create_ls(x, y, linear_fun)
  
  fit <- optim(.par_ini, ls)
  
  param <- tibble(p = fit$par) |> 
    rowid_to_column(var = "p_n")
  
  min_max <- .data |>
    ungroup() |> 
    distinct({{.predictor}}) |>
    summarise(min = min({{ .predictor }}),
              max = max({{ .predictor }}))
  
  sequ <- tibble({{.predictor}} := seq(min_max$min,
                                       min_max$max,
                                       length.out = 100))
  
  pred <- sequ |> 
    mutate({{.response}} := .fun(!!ensym(.predictor), param$p))
  
  list(param = param, 
       pred = pred)
  
}
