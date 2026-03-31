#** QC nb 1 - Check that initial absorbance measurements are all in acceptable range *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : QC - checks whether wells have absorbance outside of accepted range
#> Returns     : message or warning including table with suspicious wells
#>               
#** To Do: specify particularities *
#*
#** - The argument "empty_wells" should contain the data that is recorded in *
#**   the plate map for empty wells. The default is "empty", can be changed, *
#**   including with "NA" *
#** -  *
#*

#** Example of parameters  *

#> data <- N_all_plate
#> min_abs <- 0.03
#> max_abs <- 1.1
#> empty_wells <- "empty"
#> 
#> 

qc1_initial_range_abs <- function(
    data,
    min_abs = 0.03,
    max_abs = 1.1,
    empty_wells = "empty"
    ) {
  
  # remove empty wells, marked as "empty" --> keep only "full" wells
  # and remove absorbance with NA
  full <- data |> 
    filter(plate_map != empty_wells, !is.na(absorbance))
  
  
  # initiate data frame that will contain suspicious well ids
 # i = 1
  suspicious_rows <- c() 
  for (i in 1:nrow(full)) {
  #  if (full$absorbance[i] < min_abs || full$absorbance[i] > max_abs) {
      if (full$absorbance[i] < min_abs | full$absorbance[i] > max_abs) {
        #print(full$absorbance[i])
      suspicious_rows <- append(suspicious_rows, i)
    }
  }
  
  # Send a warning message
  if (!is.null(suspicious_rows)) {
    warning(paste0(
      "Some wells are out of range for absorbance, i.e., not in [", 
      min_abs, 
      "; ",
      max_abs, 
      "] allowed \nSee table to identify suspicious wells. "))
    
    full |> filter(row_number() %in% suspicious_rows)
  } else {
    message(paste0(
      "°^° !! YAY !! °^° All wells are in range for absorbance between ", 
      min_abs, 
      " and ", 
      max_abs))
  }
  
}

#data <- Nmin_data
#qc1_initial_range_abs(data)
