calculate_cor <- function(x, y) {
  cor.test(x, y) |> 
    tidy()
}