calculate_proportions <- function(.tbl, .y) {
  .tbl %>%
    summarise(n = n(), k = sum({{.y}}), prob = mean({{.y}}), .groups = "keep") 
}