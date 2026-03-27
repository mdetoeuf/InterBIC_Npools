#** Blanc-correct Absorbance of Standard Curves *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : Correct absorbance values of standard curve by substracting blanc
#> Returns     : tibble with corrected-absorbance
#>               
#** To Do: specify particularities *
#*
#** - For now this only accounts for curves using a whole column of the plate, *
#**   with the blanc pipetted in line A or H. * 
#** - Should there be a need to accomodate for another curve potisioning, *
#**   we can upgrade this function *
#** -  *
#*

#** Example of parameters  *

#> pipetting_direction = "top_down"
#> data = Nmin_data
#> 


correct_abs_std <- function(
    data,
    pipetting_direction = "top_down"  
  ) {
  
  if (pipetting_direction == "top_down") {
    blanc_should_be_in <- "A"
  } else if (pipetting_direction == "bottom_up") {
    blanc_should_be_in <- "H"
    } else {stop("Unknown pipetting direction. Choose between `top-down` and `bottom_up`")}
  
  # # set case (no update necessary, will find the right answer if pipetting was set correctly earlier)
  # if (is.unsorted(pipetting_direction)) {
  #   lowest_well <- "H"
  # } else {lowest_well = "A"}
  
  std_data <- extract_std_data(data)
  std_blanc <- extract_std_blanc(data)
  
  
  std_corrected <- std_data |>  
    # keep only data that is not from blanc wells
    filter(
      unique_well_id %ni% std_blanc$all$unique_well_id,
      row != blanc_should_be_in
    ) |> 
    select(plate_id, well_id, absorbance) |> 
    pivot_wider(names_from = well_id, values_from = absorbance) |> 
    left_join(std_blanc$average |> select(plate_id, blanc_avg)) |> 
    relocate(blanc_avg, .before = 2) |> 
    pivot_longer(
      cols = !c(plate_id,blanc_avg), 
      names_to = "well_id",
      values_to = "absorbance",
      values_drop_na = TRUE
    ) |> 
    mutate(abs_corrected = absorbance - blanc_avg, .keep = "unused") |> 
    # this will the rows containing the blancs, but with "NA" for obs_corrected (so the right_join will not drop observations when they are missing)
    right_join(std_data) |> 
    # not vital, just for readibility: rearrange column order
    relocate(row, column, well_id, plate_id, unique_well_id, N_sp, plate_map, absorbance) |> 
    # remove rows where no corrected absorbance data (untrusted or blancs)
    filter(!is.na(abs_corrected)) |> 
    # create unique curve_id which will be needed for downstream analysis
    mutate(unique_curve_id = paste0(plate_id, "_c", column))
  
  #std_corrected |> filter(is.na(abs_corrected))
  
  return(std_corrected)
}

#correct_abs_std(Nmin_data)
