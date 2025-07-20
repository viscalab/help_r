analyze_cor <- function(dat, x, y) {
  cor <- dat |> 
    ungroup() |> 
    summarise(calculate_cor({{x}}, {{y}})) 
  
  p <- ggplot(dat, aes({{x}}, {{y}})) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
    labs(title = paste0("r = ", round(cor$estimate, 3), 
                        ", p = ", round(cor$p.value, 3)))
  
  list(cor = cor, 
       p = p)
}