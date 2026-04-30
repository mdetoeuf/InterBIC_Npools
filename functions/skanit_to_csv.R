#** converts csv structure from skanit output into needed input *

#> Author      : Morgane de Toeuf
#> Version     : 2026-04-30
#> Purpose     : allow direct import from Skanit-generated csv data
#> Returns     : a list with 2 objects: 
#>               - abs_data = the clean data as needed for downstream analysis
#>               - map_data = same, but will only be correct if the user encoded 
#>               the plate map into Skanit
#>               
#** Function arguments *
#**
#**   skanit_csv    path to the csv of skanit-generated data *
#**   delim         The delimiter of values in the csv file *
#**                 default: "," for "normal" csv format. Accepts the value ";" * 


#skanit_csv <- "raw_data/Skanit_example/MR_R1_t0.csv"
#delim <- ","

skanit_to_csv <- function(
    skanit_csv,
    delim = ","
    ) {
  
  if (delim == ",") {
    file <- read_csv(
      skanit_csv, 
      comment = "Wavelength",
      skip = 6, 
      #skip_empty_rows = TRUE,
      col_names = FALSE, 
      show_col_types = FALSE
    ) |> 
      drop_na(X1)
  } else {
    file <- read_csv2(
      skanit_csv, 
      comment = "Wavelength",
      skip = 6, 
      #skip_empty_rows = TRUE,
      col_names = FALSE, 
      show_col_types = FALSE
    ) |> 
      drop_na(X1)
    
    warning(
      "Warning: your csv has values separated by a semi-colon (';') instead of a comma (','). 
      This probably means that your numerical data uses the comma instead of the dot as a digit separator. 
      This could generate issues in downstream steps, but should not generate errors in this function call.")
  }
  
  # Remove last row if contains something like "Autoloading..."
  if (str_split_i(file$X1[nrow(file)], pattern = " ", i = 1) == "Autoloading") {
    file <- file[1:(nrow(file)-1),]
  }
  
  # extract first column
  file_col1 <- file[[1]]
  #file_col1
  
  # Replace cells with "Abs" by plate name, then erase original cell containing that plate name
  for (cell in 2:(length(file_col1)-1)) {
    if (file_col1[cell] == "Abs") {
      file_col1[cell] <- file_col1[cell-1]
      file_col1[cell-1] <- NA
      }
  }
  
  # create new version of file where only absorbance data and map data is kept
  file_plate_ids <- file |> 
    mutate(X1 = file_col1) |> 
    # remove useless NA rows (where plate id was stored)
    drop_na(X1) 

    # find rownumber where cells contain "Sample" (indicates the start of plate map)
  nrow_sample <- which(file_plate_ids$X1 == "Sample")
  
  # create a vector with all indices of rows containing map data
  seq <- c()
  for (i in 1:length(nrow_sample)){
    seq <- append(seq,seq(nrow_sample[i],nrow_sample[i]+8,1))
  }
  #seq
  
  # subset of the file with all mapping elements
  anti_file <- file_plate_ids |> slice(seq)
  
  # complementary of that subset = absorbance data
  clean_file <- anti_join(
    file_plate_ids, anti_file,
    by = join_by(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13))
  
  # create a map file to export as well
  map_file <- anti_file |> 
    mutate(X1 = clean_file$X1)
  
  return(list(
    "abs_data" = clean_file,
    "map_data" = map_file
  ))
}

#> Call the function
#> 
# raw_data <- skanit_to_csv(skanit_csv)
# raw_data <- skanit_to_csv("raw_data/Skanit_example/MR_R1_t0.csv")

#> Access it's elements
#> 
# raw_data$abs_data
# raw_data$map_data



