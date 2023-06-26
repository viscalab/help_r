calculate_proportions <- function(.x) {
  tibble(n = length(.x), k = sum(.x), prob = mean(.x))
}