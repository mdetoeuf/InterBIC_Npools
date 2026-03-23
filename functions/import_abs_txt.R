#** Import absorbance data from txt file *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-20
#> Purpose     : any data analysis involving data acquired from 96-well plates
#> Returns     : a list with 3 objects: 
#>               - 1 dataframe = metadata off all plates (what is in .txt file)
#>               - 1 list where 1 element = absorbance data from 1 plate
#>               - the same info in a data frame: 1 column per plate, one row per well
#>               
#** !! As is, will only extract data from files with the extension ".TXT" *


#** TO DO ! *
# If relevant: maybe pivot-longer the data frame of absorbances

#filepath <- "raw_data/Nmin/"

import_abs_txt <- function(
    filepath
    ) {
  
  library(tidyverse)
  
  # obtain list of files in the filepath 
  all_files <- list.files(
    filepath, 
    pattern = ".TXT", 
    full.names = FALSE)
  # have a look at it
  
  #all_files |> glimpse()
  
  # initiate empty something (df or list?)
  abs_data <- list()
  plate_metadata <- tibble(
    plate_id = character(),
    protocol = character(),
    date = date(),
    time = date(),
    wavelength = character()
  )
  
  # initiate an empty plate
  # create an empty table with NAs
  matrix <- matrix(NA, nrow = 8, ncol = 12)
  # give it names 1 to 12
  colnames(matrix) <- as.character(c(1:12))
  
  # turn it into a tibble and add column with letters
  plate_empty <- as_tibble(matrix) |> 
    mutate(row = LETTERS[1:8], .before = 1)
  
  # verticalize and store in a dataframe
  abs_longer <- plate_empty |> 
    pivot_longer(cols = `1`:`12`, names_to = "column", values_to = "abs") |> 
    # then remove the empty values (column "abs")
    select(!abs)
  
  #i = 1
  for (i in 1:length(all_files)) {
    # get name of file nb i
    file <- paste0(filepath, all_files[i])
    
    # store plate id in a variable
    plate_id <- str_extract(all_files[i], pattern = "(\\w*)(.)(\\TXT)", group = 1)
    
    # extract only absorbance data from file to exploit as a tibble
    plate_abs <- 
      read_tsv(file, col_names = TRUE, skip = 5, show_col_types = FALSE) |> 
      rename(row = `...1`)
    # add it as element i of the list
    abs_data[[i]] <- plate_abs
    names(abs_data)[i] <- plate_id

        
    # extract metadata from file
    meta_data <- read_tsv(file, col_names = FALSE, n_max = 3, show_col_types = FALSE)
#    problems(meta_data)
    # Store relevant info from metadat in variables
    protocol <- strsplit(meta_data$X2[1], split = " ")[[1]][1] # !! looks like a N species, but it is just the spectro protocol, with only 2 possible values: no3 (for nitrate, nitrite, TDN) or nh4 (nh4+)
    date <- strsplit(meta_data$X2[1], split = " ")[[1]][2]
    time <- strsplit(meta_data$X2[1], split = " ")[[1]][3]
    wavelength <- strsplit(meta_data$X1[3], split = ": ")[[1]][2]
    
    new_row <- c(plate_id, protocol, date, time, wavelength)
    names(new_row) <- names(plate_metadata)
    
    plate_metadata <- bind_rows(
      plate_metadata, new_row)
    
    # verticalize the absorbance data and append it to the dataframe in construction
    abs_longer <- abs_longer |> 
      mutate(
        plate_abs |> 
          pivot_longer(cols = `1`:`12`, names_to = "column", values_to = plate_id) |> 
          select(any_of(plate_id))
      )
      
  }
  
  result <- list(plate_metadata, abs_data, abs_longer)
  names(result) <- c("plate_metadata", "abs_data_list", "abs_data_df")
  
  return(result)
}

#Nmin_t1t2 <- import_abs_txt(filepath = "raw_data/Nmin/")
