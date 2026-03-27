#** TITLE *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : 
#> Returns     : 
#>               
#** To Do: specify particularities *
#*
#** -  Add some warning if there are negative values for absorbance? *
#** -  Or remove them automatically ?*
#** -  *
#*

#** Example of parameters  *

#> metadata = Nmin_metadata
#> 

qc7_std_find_outlier <- function(
  std_corrected,
  metadata,
  pipetting_direction = "top_down"
  ) {
  
  # only proceed if we have the same number of plates in data and in metadata
  if(n_groups(std_corrected) == nrow(metadata)) {
    
    # check if negative absorbances
    std_corrected |> 
      arrange(abs_corrected) # no, nice :-)
    
    # extracted unsorted curves
    unsorted_curves <- std_corrected |> 
      group_by(plate_id, column) |> 
      arrange(row) |> 
      summarise(
        suspicious = is.unsorted(abs_corrected),
        .groups = "keep"
      ) |> 
      filter(suspicious) |> 
      mutate(unique_curve_id = paste0(plate_id, "_c", column))
    
    if (pipetting_direction == "top_down") {
      diluted_to_concentrated <- LETTERS[1:8]
    } else if (pipetting_direction == "bottom_up") {
      diluted_to_concentrated <- LETTERS[8:1]
    } else {stop("No valid information was provided on position of the standard curve")}
    
    # plot them all
    #i = 1
    plots <- list()
    for (i in 1:nrow(unsorted_curves)) {
      
      curve <- std_corrected |> 
        #filter(unique_curve_id == unsorted_curves$unique_curve_id[i]) |> 
        filter(plate_id == unsorted_curves$plate_id[i]) 
      
      conc <- tibble(
        conc = extract_curve(metadata, N_sp = curve$N_sp[1])[2:8],
        row = diluted_to_concentrated[2:8]
      )
      
      curve <- curve |> 
        left_join(conc)
      nudge <- (max(curve$conc) - min(curve$conc))/30
      
      plot <- 
        curve |> 
        ggplot(aes(x = abs_corrected, y = conc)) + 
        theme_minimal() +
        geom_smooth(
          method = "lm", se = FALSE, formula = y~x-1,
          color = "grey70") +
        geom_point(color = "grey30", alpha = 1) + 
        annotate(geom = "text", x = curve$abs_corrected, y = curve$conc-nudge,
                 label = curve$well_id, size = 3) +
        labs(title = curve$plate_id[1]) 
      
      plots[[i]] <- plot
      #plot
    }
    
    wrap_plots(plots,axis_titles = "collect")
    
  }
  

}

#qc7_std_find_outlier(std_corrected = std_corrected, metadata = Nmin_metadata)
