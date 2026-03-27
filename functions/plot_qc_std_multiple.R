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

#> 
#> 

# #i = 1
# metadata = Nmin_metadata
# std_data = std_tidy

plot_qc_std_multiple <- function(
    metadata,
    std_data,
    pipetting_direction = "top_down",
    nudge = NULL 
    ) {
  
  if (pipetting_direction == "top_down") {
    diluted_to_concentrated <- LETTERS[1:8]
  } else if (pipetting_direction == "bottom_up") {
    diluted_to_concentrated <- LETTERS[8:1]
  } else {stop("No valid information was provided on position of the standard curve")}
  
  plots = list()
  for (i in 1:nrow(metadata)) {
    
    plate <- metadata$plate_id[i]
    N_sp <- str_split_i(plate, "_", 1)
    
    conc <- tibble(
      conc = extract_curve(metadata, N_sp = N_sp)[2:8],
      row = diluted_to_concentrated[2:8]
    )
    #conc
    
    curve <- std_data |> 
      filter(plate_id == plate) |> 
      left_join(conc, by = join_by(row)) 
    
    # If nudge not user-defined --> give it a default value that is a third of the range of the plot
    if (is.null(nudge)) { nudge <- (max(curve$conc) - min(curve$conc))/30}
    
    plot <- curve |> 
      ggplot(aes(x = abs_corrected, y = conc, color = column)) + 
      theme_minimal() +
      geom_smooth(method = "lm", se = TRUE, formula = y~x-1, color = "grey70", alpha = 0.2) +
      geom_point(color = "grey30", alpha = 1) + 
      annotate(geom = "text", x = curve$abs_corrected, y = curve$conc-nudge,
               label = curve$well_id, size = 3) +
      labs(title = curve$plate_id[1])
    
    plots[[i]] <- plot
  }
  
  wrap_plots(plots,axis_titles = "collect")
}

#plot_qc_std_multiple(metadata = Nmin_metadata, std_data = std_tidy)
