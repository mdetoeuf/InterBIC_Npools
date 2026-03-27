#** QC nb 4 : Looking for outliers in blancs of Std curve *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : 
#> Returns     : 
#>               
#** To Do: specify particularities *
#*
#*#** !! THIS CANNOT BE DONE THIS WAY IN CASES WITH ONLY 1 CURVE PER PLATE !! *

#*
#** There are 2 steps to looking for outliers *
#**   1) Typical automated pipette issue: forgetting to expell before pipetting *
#**      in the first well which is often A1, where the blanc of the std curve is *
#**      So corner wells are frequent outliers. A way to spot such issues is if *
#**      the absorbance values of the std curve are not perfectly monotonous. *
#**      In this code, we decide to not trust (thus: exclude) wells that are *
#**      supposed to contain the blanc of the std curve (row A or H of the plate) *
#**      but that do not carry the lowest value of the standard curve. *
#**      --> such untrusted wells are removed. *
#*
#**   2) We compute the coefficient of variation between the 2 (remaining) *
#**      values for the blanc, and examine the plates that display a high variation *
#*

#** Example of parameters  *

#> nb_std = 2
#> data = Nmin_data
#> max_coeff = 5 # max tolerance for a coefficient of variation of 5%


qc4_std_blanc_variation <- function(
    data,
    nb_std = 2,
    max_coeff = 5,
    nb_of_plates_to_look_at = NULL # if set on NULL, will compute automatically
    
    ) {

  std_data <- extract_std_data(data)
  std_blanc <- extract_std_blanc(data, nb_std = nb_std)
  
  # # extract ("slice") only rows with the smallest absorbance
  # std_blanc_all <- std_data |> 
  #   slice_min(
  #     absorbance, # slice min according to the valus in abs
  #     n = nb_std,  # pick as many rows as the nb of columns with standard curve
  #     with_ties = FALSE # in case there are ties, it will add extra rows
  #   ) 
  # #std_blanc_all
  # 
  # 
  # # check that we will remove the "correct" suspicious blancs in a moment
  # std_blanc_trusted <- std_blanc_all |> 
  #   # remove anything not first or last row of plate <=> suspicious
  #   filter(row %in% c("A", "H"))
  # #std_blanc_trusted
  
  return_list <- list()
  
  # see that we get less rows now
  return_list[["untrusted_msg"]] <- warning(paste0(
    "There are ",
    nrow(std_blanc$all),
    " standard curves in this data set, thus in theory also ",
    nrow(std_blanc$all), 
    " wells containing the blanc for those curves. \nOf those ",
    nrow(std_blanc$all), ", ", nrow(std_blanc$all) - nrow(std_blanc$trusted),
    " are untrusted (see comments in the function definition for details on untrusted wells). Try ?qc4_std_blanc_variation().  ",
    "\nWe are thus a priori trusting ", 
    nrow(std_blanc$trusted), " wells out of ", nrow(std_blanc$all), "."
  ))
   

  # # compute intra-plate mean for the blanc, st-dev and coefficient of variation in %
  # std_blanc_avg <-  std_blanc$trusted |> 
  #   summarise(
  #     blanc_avg = mean(absorbance), 
  #     blanc_sdev = sd(absorbance)) |> 
  #   mutate(
  #     blanc_coeff_var_percent = 100 * blanc_sdev / blanc_avg)
  # #std_blanc_avg
  
  # in case of several values... (can't do this if only one curve per plate)
  if (#length(std_column) != 1 # replace by something else
    TRUE) {
    # ... and of coefficient of variation > set threshold
    if (max(std_blanc$average$blanc_coeff_var_percent,na.rm = TRUE) > max_coeff) {
      # send a warning
      return_list[["outlier_warning"]] <- warning(paste0(
        "Even after removal of untrusted wells, there are plates showing a big variation in absorbance values for the blanc of the standard curve (more than ", 
        max_coeff, 
        "%).\nPick the most likely values / remove outliers manually.\nSee table to judge on values and find suspicious wells"))
      # and show suspicious wells
      return_list[["suspicious_curve_coeff_var"]] <- 
        wrap_table(std_blanc$average |> filter(blanc_coeff_var_percent > max_coeff)) +
        labs(title = paste0("Plates with coefficient of variation above ", max_coeff, "%"))
      
      # another way to visualize it: 
      #std_blanc_avg |> slice_max(blanc_coeff_var_percent, n = 10) # coeff variation above 3% are only a few, all below 10%, and only for NO2 and NH4, acceptable
      
    } else {return_list[["outlier_warning"]] <- message(paste0(
      "After removal of untrusted wells, variation in absorbance values for the blanc of the standard curves does not exceed the maximum coefficient of variation of ",
      max_coeff, "%"))}
  } else {return_list[["outlier_warning"]] <- NA}
  
  # if not user-defined, then compute nb of plates to show based on max_coeff
  if(is.null(nb_of_plates_to_look_at)) {
    nb_of_plates_to_look_at <- std_blanc$average |> 
      filter(blanc_coeff_var_percent > max_coeff) |> nrow()
  }
 
  std_blanc_big_coeff <- std_blanc$average |> 
    slice_max(blanc_coeff_var_percent, n = nb_of_plates_to_look_at)
  
  # visualize numbers in context
  Tables <- list()
  for (i in 1:nrow(std_blanc_big_coeff)) {
    plate <- std_blanc_big_coeff$plate_id[i]
    Tables[[plate]] <- 
      wrap_table(std_data |> filter(plate_id == plate)) + 
      labs(title = paste0(
        "Plate with high coefficient of variation. ",
        i, " of ", nrow(std_blanc_big_coeff)
      ))
  }
  
  return(c(return_list, Tables))
} 

#test <- qc4_std_blanc_variation(Nmin_data,nb_of_plates_to_look_at = 3)
#lapply(test, print)
