#** Import absorbance data from txt file *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-20
#> Purpose     : any data analysis involving data acquired from 96-well plates
#> Returns     : a list with 2 objects: 
#>               - 1 dataframe = metadata off all plates (what is in .txt file)
#>               - 1 list where 1 element = absorbance data from 1 plate
#>               
#** !! As is, will only extract data from files with the extension ".TXT" *


#** TO DO ! *
# for now: returns a list. Add an export step? --> create rds or something? 
# or lengthen the function to go to the next step, 
# --> adding this extracted metadata to manually imported table of plate info
# --> computing verticalization to have not a list but a table with absorbance data
# --> including info from plate map (to be imported as well)

filepath <- "raw_data/Nmin/"

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
  all_files |> glimpse()
  
  # initiate empty something (df or list?)
  abs_data <- list()
  plate_metadata <- tibble(
    plate_id = character(),
    protocol = character(),
    date = date(),
    time = date(),
    wavelength = character()
  )
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
    
  }
  
  return(list(plate_metadata, abs_data))
}

#Nmin_t1t2 <- import_abs_txt(filepath = "raw_data/Nmin/")
