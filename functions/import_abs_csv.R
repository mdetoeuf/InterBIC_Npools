#** Import absorbance data from csv file *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-20
#> Purpose     : any data analysis involving data acquired from 96-well plates
#> Returns     : a list with 2 objects: 
#>               - 1 list where 1 element = absorbance data from 1 plate
#>               - the same info in a data frame: 1 column per plate, one row per well
#>               
#** !! As is, will only extract data from files with the extension ".csv" *
#** And only files with the structure where *
#**     - data is in the plate set up *
#**     - plate_id is in the upper left corner of the plate (between "A" and "1" *
#**     - data starts in the first cell of the csv ("A1" coming from Excel) *
#**.    - plates are stacked on top of each other with no empty row in between *
#**       so that a new plate starts every 9 rows and the data set is 13 columns wide *
#** All these conditions can easily be enlarged, but it would require additional *
#** data manipulation steps, and I do not see that it is needed at the moment *


#** TO DO ! *
# If relevant: maybe pivot-longer the data frame of absorbances

import_abs_csv <- function(file) {
  
  library(tidyverse)
  library(roperators)
  library(janitor) # for row_to_names
  
  # extract header rows for each plate: contain plate_id and column nb (1 to 12)
  # to get to the plate_ids
  plates <- file |> 
    # exclude rows where 1st column contains single capital letters
    filter(X1 %ni% LETTERS) |> 
    # keep only 1st column --> create a tibble (1 column) with plate_ids
    select(plate_id = X1)
  
  # initiate empty list to store absorbance data
  abs_data <- list()
  
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
  for (i in 1:nrow(plates)) {
  # store plate id
  plate_id <- plates$plate_id[i]
  
  # extract line corresponding to plate_id
   line <- which(file$X1 == plate_id)
   
   # get plate absorbances (start at line of plate_id, and get next 8 lines)
   plate_abs <- file[line:(line+8),] |> 
     row_to_names(row_number = 1) |> 
     # rename column with "A", "B", etc. Not vital, but kept here to reach same 
     # format in all functions for interoperability
     rename(row = any_of(plate_id))
   
   abs_data[[i]] <- plate_abs
   names(abs_data)[i] <- plate_id
   
   # verticalize the absorbance data and append it to the dataframe in construction
   abs_longer <- abs_longer |> 
     mutate(
       plate_abs |> 
         pivot_longer(cols = `1`:`12`, names_to = "column", values_to = plate_id) |> 
         select(any_of(plate_id))
     )
   
  } # end of for-loop
   
  result <- list(abs_data, abs_longer)
  names(result) <- c("abs_data_list", "abs_data_df")
  
  return(result)

  
} # end of import_abs_csv


#file <- read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE)
#TDN_abs <- import_abs_csv(file)
