fit_glm <- function(.data, .response, .predictor) {
  
  predictor <- ensym(.predictor)
  
  prop <- .data |>
    group_by({{ .predictor }}) |>
    summarise(k = sum({{.response}}), 
              n = n(),
              prop = mean({{ .response }}), .groups = "keep")
  
  model <- glm(inject(cbind(k, n - k) ~ !!predictor),
               family = binomial(logit),
               data = prop)
  
  param <- tidy(model) |>
    select(term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    rename(intercept = `(Intercept)`, sensitivity = {{ .predictor }}) |>
    mutate(bias = -intercept / sensitivity)
  
  min_max <- .data |>
    distinct({{.predictor}}) |>
    summarise(min = min({{ .predictor }}),
              max = max({{ .predictor }}))
  
  sequ <- tibble({{.predictor}} := seq(min_max$min,
                                       min_max$max,
                                       length.out = 100))
  
  psy <- augment(model, newdata = sequ, type.predict = "response") |>
    rename(prop = .fitted)
  
  glance_model <- glance(model) 
  
  deviance <- tibble(type = c("residual", "null"), 
                     deviance = c(glance_model$deviance, glance_model$null.deviance), 
                     df = c(glance_model$df.residual, glance_model$df.null)) |> 
    mutate(p_value = pchisq(deviance, df = df, lower.tail = FALSE)) 
  
  list(prop = prop,
       param = param,
       psy = psy, 
       deviance = deviance)
}