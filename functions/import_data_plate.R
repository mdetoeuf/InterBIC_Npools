#** Import data from 96-well plate formats (absorbance data or plate maps) *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : import, verticalize and assemble raw data from files in typical 96-well plate format
#> Returns     : a list containing 
#>                  1) a vertilcalized data frame 
#>                  2) a list where each element contains the raw data of one plate
#>                  3) optional for .txt input file: plate metadata provided by the spectrophotometer
#>               
#** To Do: specify particularities *
#*
#** -  For absorbance data, 4 format options as input*
#**     1) give path to folder where files in .TXT format are found. *
#**        In that case: !! the only "." allowed in the filename *
#**        is the one before the TXT extension *
#**     2) give path to folder + filename of file with .csv format and * 
#**        structured as in example file *
#**     3) give as input a tibble, coming from previous import, for example *
#**        using read_csv(). In that case, make sure the tibble has been shaped *
#**        into the correct format *
#**     4) give a path to folder + filename of file with Skanit format *
#**        (which is a csv structured in a specific way, see example file) *
#** -  *
#*

#** Example of parameters  *

#> format_abs <- "tibble" or "txt" or "csv
# filepath <- "raw_data/Nmin_t3/" # path to folder where files are. finish it with "/" 
# 
# filename_csv <- "Nmint3_data.csv"
#> csv_file_TDN <- read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE)
#> tibble = csv_file_TDN
#> tibble = "test no tibble"
#> skanit_csv <- "raw_data/Skanit_example/MR_R1_t0.csv"


import_data_plate <- function(
    format_abs, # "csv" or "txt" or "tibble" or "skanit"
    dataset = "", # adds a column with the name of the data set, to join them later
    filepath = "", # path to folder where files are. finish it with "/" 
    filename_csv = NULL,
    tibble = NULL,
    skanit_csv = NULL,
    import_metadata = FALSE, # change to TRUE if txt and want to obtain plate metadata from txt file
    skanit_abs_or_map = "abs" # other option = "maps"
) {
  library(tidyverse)
  library(roperators)
  library(janitor) # for row_to_names
  

# 1 - Import raw absorbance data (csv, tibble) or list raw files ( --------
  
  # New case: a skanit csv is given as input
  if(format_abs == "skanit") {
    if(skanit_abs_or_map == "abs") {
      abs_data_raw <- skanit_to_csv(paste0(filepath, skanit_csv))$abs_data
    } else {
      abs_data_raw <- skanit_to_csv(paste0(filepath, skanit_csv))$map_data
    }
  } else if (format_abs == "tibble") {

  # First case: a tibble is given as input
    if (is.null(tibble)) { 
      stop("absorbance data as a tibble is missing")
    } else if (!is_tibble(tibble)) {
        stop("absorbance data expected as tibble, but is not, please reformat")
    } else { # all good: tibble format is expected, data is indeed tibble
        abs_data_raw <- tibble
    }
  # Second case: a csv is given as input
  } else if (format_abs == "csv") {
    if (is.null(filename_csv)) {
      stop("filename is missing")
    } else {
      abs_data_raw = read_csv(paste0(filepath, filename_csv), col_names = FALSE, show_col_types = FALSE) 
    }
      
  # Third case: a txt is given as input    
  } else if (format_abs == "txt") {
    # obtain list of files in the filepath 
    all_txt_files <- list.files(
      filepath, 
      pattern = ".TXT", 
      full.names = FALSE)
  # Last case: unvalid format given as input  
  } else {
    stop("invalid format. Accepted values for format_abs are `csv`, `txt`, `tibble` or 'skanit'")
  }
  

# 2 - create an empty plate format and initialize an empty absorbance list and empty metadata list ----------------------------------------

  abs_data_list <- list()
  
  # if (import_metadata == TRUE) { # keep this out of "if" or give a condition to the result section
  plate_metadata <- tibble(
    plate_id = character(),
    protocol = character(),
    date = date(),
    time = date(),
    wavelength = character()
  ) #}

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
    # remove empty column
    select(!abs)
    
  
# 3 - For csv & tibble format --> verticalize in loop per plate -----------
  
  if (format_abs %in% c("csv", "tibble", "skanit")) {
    
    # extract header rows for each plate: contain plate_id and column nb (1 to 12)
    # to get to the plate_ids
    plates <- abs_data_raw |> 
      # exclude rows where 1st column contains single capital letters
      filter(X1 %ni% LETTERS) |> 
      # keep only 1st column --> create a tibble (1 column) with plate_ids
      select(plate_id = X1)
    
    for (i in 1:nrow(plates)) {
      # store plate id
      plate_id <- plates$plate_id[i]
      
      # extract line corresponding to plate_id
      line <- which(abs_data_raw$X1 == plate_id)
      
      # get plate absorbances (start at line of plate_id, and get next 8 lines)
      plate_abs <- abs_data_raw[line:(line+8),] |> 
        row_to_names(row_number = 1) |> 
        # rename column with "A", "B", etc. Not vital, but kept here to reach same 
        # format in all functions for interoperability
        rename(row = any_of(plate_id))
      
      abs_data_list[[i]] <- plate_abs
      names(abs_data_list)[i] <- plate_id
      
      # verticalize the absorbance data and append it to the dataframe in construction
      abs_longer <- abs_longer |> 
        mutate(
          plate_abs |> 
            pivot_longer(cols = `1`:`12`, names_to = "column", values_to = plate_id) |> 
            select(any_of(plate_id))
        )
      #print(i)
      #i = i+1
    } # end of for-loop
    
    
    
    

# 4 - for txt format --> import in loop per plate and verticalize ---------

  } else if (format_abs == "txt") {
    
    
    for (i in 1:length(all_txt_files)) {
      # get name of file nb i
      file <- paste0(filepath, all_txt_files[i])
      
      # store plate id in a variable
      plate_id <- str_extract(all_txt_files[i], pattern = "(\\w*)(.)(\\TXT)", group = 1)
      
      # extract only absorbance data from file to exploit as a tibble
      plate_abs <- 
        read_tsv(file, col_names = TRUE, skip = 5, show_col_types = FALSE, name_repair = "unique_quiet") |> 
        rename(row = `...1`)
      # add it as element i of the list
      abs_data_list[[i]] <- plate_abs
      names(abs_data_list)[i] <- plate_id
    
    ?read_tsv
    
      if (import_metadata) {
        
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
        
      } # end of if import_metadata
      
      # verticalize the absorbance data and append it to the dataframe in construction
      abs_longer <- abs_longer |> 
        mutate(
          plate_abs |> 
            pivot_longer(cols = `1`:`12`, names_to = "column", values_to = plate_id) |> 
            select(any_of(plate_id))
        )
      
    } # end of per plate (file) loop
    
    
  } # end of case for txt file
  
  
  abs_result <- list(plate_metadata, abs_data_list, abs_longer)
  names(abs_result) <- c("plate_metadata", "abs_data_list", "abs_data_df")
  
  return(abs_result)
}


# test_txt <- import_data_plate(format_abs = "txt", filepath = "raw_data/Nmin/", import_metadata = TRUE)
# test_txt$plate_metadata
# test_txt$abs_data_df
# test_txt$abs_data_list$NH4_1F1

# test_csv <- import_data_plate(format_abs = "csv", filepath = "raw_data/TDN/", filename_csv = "TDN_data.csv")
# test_csv$abs_data_df
# test_csv$abs_data_list$NO3_TDN_01

# test_tibble <- import_data_plate(format_abs = "tibble", tibble = read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE))
# test_tibble$abs_data_df
# test_tibble$abs_data_list$NO3_TDN_01

# test_skanit <- import_data_plate(format_abs = "skanit", skanit_csv = "raw_data/Skanit_example/MR_R1_t0.csv")

