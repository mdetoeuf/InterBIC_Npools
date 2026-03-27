#** QC nb 3 - Check consistency of number of curves per plate *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : QC : check that we have the same nb of columns with Std per plate
#> Returns     : message and plot
#>               
#** To Do: specify particularities *
#*
#** - If there are at least 2 curves, then it is an option to remove *
#**   suspicious blancs for the standard curve *
#** - This is only relevant if the curve was prepared in water and the normal *
#**   "extractant" blanc of the plate cannot be used to correct the *
#**   standard curve*
#** - This is a several step functions *
#*

#** Example of parameters  *

#> nb_std = 2 # nb of columns to check for
#> data = Nmin_data
#> 

# load the function extract_std_data
source("functions/subset_data.R")


#** 2) CHECK NB OF CURVES PER PLATE *
qc3_nb_curve_per_plate <- function(
    data,
    nb_std = 2
    ) {
  
  std_data <- extract_std_data(data)
  
  # # find suspicious blancs ()
  # std_data |> 
  #   group_by(plate_id, column) |> 
  #   slice_min(absorbance) |> 
  #   filter(row %ni% c("A", "H")) 
 
  # sum for next code returns a number that is double the nb of rows
  nb_std_per_plate <- std_data |> 
    summarise(
      min(absorbance),
      n_std = n()/8
    ) 
  # check if true
  if(nrow(nb_std_per_plate) == sum(nb_std_per_plate$n_std)/nb_std) {
    message(paste0(
      "!! YAY !! There is/are indeed on average exactly ",
      nb_std, " standard curves per plate. It is very likely that there are exactly ",
      nb_std, " curves per plate. To be sure, check the distribution of number of standard curves per plate. If there is only 1 value at ", 
      nb_std, ", then it is confirmed."))
  } # TRUE --> ok!
  
  # plot the distribution
  nb_std_per_plate |> ggplot(aes(x = n_std)) + geom_histogram(bins = 30) + labs(title = "if only 1 bin, good news :-)") # also good check
}

#qc3_nb_curve_per_plate(Nmin_data)
