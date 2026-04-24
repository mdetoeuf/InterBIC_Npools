#** TITLE *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : 
#> Returns     : 
#>               
#** To Do: specify particularities *
#*
#** -  *
#** -  *
#*

#** Example of parameters  *

#> QC6 <- qc6_extr_trusted(QC5, cut_threshold = cut_threshold)
#> QC5 <- qc5_extr_blanc_variation(Nmin_data)
#> data <- Nmin_data
# data <- N_all_plate |> filter(plate_map == "16_t2_CFE.1x")
# data
# QC6$extr_avg_trusted |> filter(plate_id == "NO2_TDN_18")

correct_abs_samples <- function(
    data,
    QC6
  ) {
  
  raw_samples_data <- 
    data |> 
    # keep only sample data
    filter( # filter out non-sample wells
      # empty wells
      plate_map != "empty",
      # wells containing Std
      plate_map != "Std",
      # extractant wells
      str_split_i(plate_map, "_", 1) != "extr"
    ) 
  
  corrected_data <- 
    raw_samples_data |> 
    select(plate_id, well_id, absorbance) |> 
    # make it wider to have only one row per plate
    pivot_wider(names_from = well_id, values_from = absorbance) |> 
    # so that we can add average of extractant that is valid for all wells of the plate concerned
    left_join(QC6$extr_avg_trusted |> select(plate_id, extr_avg)) |> 
    # relocate extractant for easier manipulation afterwards
    relocate(extr_avg, .before = 2) |> 
    # repivot so that computation of corrected absorbance is easy (work with columns)
    pivot_longer(
      cols = !c(plate_id, extr_avg),
      names_to = "well_id",
      values_to = "absorbance",
      values_drop_na = TRUE
    ) |> 
    mutate(abs_corrected = absorbance - extr_avg) |> 
    # get the rest of the well and plate data again
    right_join(raw_samples_data) |> 
    # remove raw absorbance to prevent the probable downstream mistake 
    # of using absorbance instead of corrected absorbance
    select(!absorbance)
  
  return(corrected_data)
}

