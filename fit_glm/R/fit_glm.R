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
  
  deviance <- glance(model) |> 
    select(null.deviance, df.null) |> 
    mutate(p_value = pchisq(null.deviance, df = df.null, lower.tail = FALSE)) |>
    rename(null_deviance = null.deviance, df_null = df.null)
  
  list(prop = prop,
       param = param,
       psy = psy, 
       deviance = deviance)
}