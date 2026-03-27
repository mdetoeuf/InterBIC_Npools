#** QC nb 5 - Looking for outliers in blancs of Extractant *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : 
#> Returns     : 
#>               
#** To Do: specify particularities *
#*
#** -  If too many plates are suspicious, it can be that the final multiple plot *
#**    becomes difficult to read. Should that be the case, we can divide it into *
#**    several pages (inspiration from further in script possible...) *
#** -  *
#*

#** Example of parameters  *

#> data = Nmin_data
#> max_coeff = 5
#> 

qc5_extr_blanc_variation <- function(
    data,
    max_coeff = 5
    ) {
  
  extr_data <- data |> 
    filter(str_split_i(plate_map, "_", 1) == "extr") |> 
    group_by(plate_id, plate_map)
  # extr_data
  
  extr_avg <- extr_data |> 
    summarise(
      extr_avg = mean(absorbance),
      extr_sdev = sd(absorbance)) |> 
    mutate(extr_coeff_var_percent = 100 * extr_sdev / extr_avg)
  #extr_avg
  
  distrib_coeff <- extr_avg |> 
    ggplot(aes(x = extr_coeff_var_percent)) +
    geom_histogram(bins = 100) +
    #geom_density() +
    #geom_boxplot() +
    labs(
      title = "Distribution of coefficient of variation of absorbance of extractant (blanc)") +
    xlab("intra-plate coefficient of variation [%]")
  

  if (max(extr_avg$extr_coeff_var_percent,na.rm = TRUE) > max_coeff) {
    # store id of problematic plates
    suspicious_plate_id <- extr_avg |> 
      filter(extr_coeff_var_percent > max_coeff) |> 
      select(plate_id) |> as.vector() |> magrittr::extract2(1)
    # send a warning
    warning(paste0(
      "There is a big variation in absorbance values for the blanc  (more than ", 
      max_coeff, 
      "%).\nRemove the most unlikely values / remove outliers manually.\nSee table above to judge on values. \nSuspicious plates are stored in vector called suspicious_plate_id"))
    # and show suspicious wells
    suspicious_coeff <- extr_avg |> 
      filter(extr_coeff_var_percent > max_coeff) |> 
      arrange(desc(extr_coeff_var_percent))
    
    extr_suspicious <- extr_data |> 
      filter(plate_id %in% suspicious_plate_id)
    
    plots <- extr_suspicious |> 
      group_map(
        .f = ~ggplot(.x, aes(x = absorbance)) + 
          geom_histogram() +
          theme_minimal() +
          labs(title = .y)
      ) 
    
    multiple_plot <- wrap_plots(plots, axis_titles = "collect") +
      plot_annotation(title = "Distribution of absorbance of extractant",
           subtitle = paste0(
             "Only displays plates for which the coefficient of variation for the extractant is above ",
             max_coeff)
           )
    #multiple_plot
    
    return(list(
      "max_coeff" = max_coeff,
      "extr_data" = extr_data,
      "extr_average" = extr_avg,
      "suspicious_plate_id" = suspicious_plate_id,
      "suspicious_coeff" = suspicious_coeff,
      "distrib_coeff" = distrib_coeff,
      "extr_suspicious" = extr_suspicious,
      "multiple_plot" = multiple_plot
    ))
    
    #suspicious_coeff
  } else {
    return(list(
      "max_coeff" = max_coeff,
      "extr_data" = extr_data,
      "extr_average" = extr_avg,
      "distrib_coeff" = distrib_coeff,
    ))}
  
}

# test <- qc5_extr_blanc_variation(Nmin_data)
# test$blanc_average
# test$suspicious_coeff
# test$suspicious_plate_id
# test$distrib_coeff
# test$multiple_plot
