read_files <- function(path, extension = "csv", ...) {
  
  dots_t <- list2(...) |> 
    as_tibble()
  
  if (extension == "csv") {
    extension <- "*.csv"
    fun <- read_csv
  }
  
  if (extension == "txt") {
    extension <- "*.txt"
    fun <- read_table
  }
  
  tibble(name_file = list.files(path, extension)) |>
    bind_cols(dots_t) |> 
    pivot_longer(-name_file, names_to = "chain", values_to = "len") |> 
    group_by(name_file)  |> 
    mutate(final = cumsum(len),
           initial = final - len + 1,
           str = str_sub(name_file, initial, final)) |>
    select(name_file, chain, str) |> 
    pivot_wider(names_from = chain, values_from = str) |> 
    mutate(name_file_full = paste0(path, "/", name_file)) |> 
    rowwise() |>  
    mutate(file = list(fun(name_file_full, show_col_types = FALSE))) |> 
    ungroup() |>
    select(-name_file_full, -name_file) |>
    unnest(file)
  
}
