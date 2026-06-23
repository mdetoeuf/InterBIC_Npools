MR_to_tibble <- function(
    filepath,
    column_range) {
  
  file <- read_csv(
    filepath, 
    skip = 2, skip_empty_rows = TRUE,
    col_select = all_of(column_range),
    col_names = FALSE,
    show_col_types = FALSE) |> 
    setNames(names(tibble_example)) |> 
    drop_na(row) 
  
  # extract first column
  file_col1 <- file[[1]]
  
  # Replace cells with "<>" by plate name, then erase content of original cell containing that plate name
  for (cell in 2:(length(file_col1)-1)) {
    if (file_col1[cell] == "<>") {
      file_col1[cell] <- file_col1[cell-1]
      file_col1[cell-1] <- NA
    }
  }
  
  # create new version of file where only absorbance data is kept
  tibble <- file |>
    dplyr::mutate(row = file_col1) |>
    # remove useless NA rows (where plate id was stored)
    tidyr::drop_na(row)
  
  return(tibble)
}
