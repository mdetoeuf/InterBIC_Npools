#** Script with a series of functions to subset data from absorbance data *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : Subset only standard curve data, or extractant data, etc.
#> Returns     : tibbles, one for each subset
#>               
#** To Do: specify particularities *
#*
#** -  *
#** -  *
#*

#** Example of parameters  *

#> 
#> 
#> 

#** 1) EXTRACT STD DATA ONLY FROM DATA *
extract_std_data <- function(data) {
  std_data <- data |> 
    # take only plate-columns with standard curves
    filter(plate_map == "Std") |> 
    group_by(plate_id)
  std_data
}

#** 2) EXTRACT BLANCS ONLY FROM STD DATA, ALL and TRUSTED, AND COMPUTE AVERAGE *
extract_std_blanc <- function(data, nb_std = 2) {
  std_data <- extract_std_data(data)
  # extract ("slice") only rows with the smallest absorbance
  std_blanc_all <- std_data |> 
    slice_min(
      absorbance, # slice min according to the valus in abs
      n = nb_std,  # pick as many rows as the nb of columns with standard curve
      with_ties = FALSE # in case there are ties, it will add extra rows
    ) 
  #std_blanc_all
  
  # check that we will remove the "correct" suspicious blancs in a moment
  std_blanc_trusted <- std_blanc_all |> 
    # remove anything not first or last row of plate <=> suspicious
    filter(row %in% c("A", "H"))
  #std_blanc_trusted
  
  # compute intra-plate mean for the blanc, st-dev and coefficient of variation in %
  std_blanc_avg <-  std_blanc_trusted |> 
    summarise(
      blanc_avg = mean(absorbance), 
      blanc_sdev = sd(absorbance)) |> 
    mutate(
      blanc_coeff_var_percent = 100 * blanc_sdev / blanc_avg)
  #std_blanc_avg
  
  
  std_blanc <- list(
    "all" = std_blanc_all,
    "trusted" = std_blanc_trusted,
    "average" = std_blanc_avg
  )
  
  return(std_blanc)
}

# std_blanc <- extract_std_blanc(Nmin_data)
# std_blanc$all
# std_blanc$trusted


