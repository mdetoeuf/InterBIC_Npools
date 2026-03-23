#** Join absorbance data and plate maps in a single tidy data frame *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-23
#> Purpose     : Obtain a single tidy data frame with raw absorbance data, for downstream analysis
#> Returns     : ??
#>               
#** !! To use once subsets of data have already been imported (txt, csv) *
#** AND joined per type *
#** So as input: have a single data frame for absorbance data *
#** + a single data frame for maps *


#abs_df <- Nmin_all_abs
#maps_df <- Nmin_all_maps

join_maps_abs <- function(
    abs_df,
    maps_df
) {
  # pivot absorbance data
  Nmin_abs_longer <- abs_df |> 
    pivot_longer(
      cols = starts_with("N", ignore.case = FALSE),
      values_to = "absorbance",
      names_to = "plate_id"
    ) |> 
    mutate(column = as.numeric(column))
  
  # pivot maps data
  Nmin_maps_longer <- maps_df |> 
    pivot_longer(
      cols = starts_with("N", ignore.case = FALSE),
      values_to = "plate_map",
      names_to = "plate_id"
    ) |> 
    mutate(column = as.numeric(column))
  
  # join both
  Nmin_data <- left_join(
    Nmin_maps_longer, Nmin_abs_longer, 
    by = join_by(row, column, plate_id)
  ) |> 
    # add well_id column
    mutate(well_id = paste0(row, column), .after = 2) |> 
    # sort it per plate then per "column" (from plates)
    arrange(plate_id,column)
  
  return(Nmin_data)
  
}

#join_maps_abs(maps_df = Nmin_all_maps, abs_df = Nmin_all_abs)

