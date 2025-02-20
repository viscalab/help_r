bind_dfs <- function(l, ...) {
  l_names <- list2(...)  
  var_name <- names(l_names)
  names_groups <- l_names[[1]]
  
  map2(l, names_groups, ~ mutate(.x, !!var_name := .y)) |> 
    bind_rows() |> 
    relocate(!!var_name, .before = everything())
}
