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

#> metadata = Nmin_metadata
#> data = corrected_data
#> std_data = std_tidy
#> 
#> 
#> 

std_regression <- function(
  data,
  metadata,
  std_data,
  pipetting_direction = "top_down",
  max_nb_plots = 16,
  save_pdf = TRUE,
  filenames = "QCreg_singles_page", # will save as this + a number: 1, 2, etc.
  filepath = "output/figures/QC/" # where to save files (if save_pdf = TRUE)
  
  ) {
  
  if (pipetting_direction == "top_down") {
    diluted_to_concentrated <- LETTERS[1:8]
  } else if (pipetting_direction == "bottom_up") {
    diluted_to_concentrated <- LETTERS[8:1]
  } else {stop("No valid information was provided on position of the standard curve")}
  
  
  plots <- list()
  lm_output <- tibble(
    plate_id = character(), slope = double(), p_val_slope = double(), r_squared_mult = double()
  )
  for (i in 1:nrow(metadata)) {
    #for (i in 1:6) {
    
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
    #curve
    
    meta_line <- metadata |> filter(plate_id == plate)
    
    curve_lm <- lm(data = curve, conc ~ 0 + abs_corrected) |> summary() 
    #curve_lm
    
    lm_coeff <- curve_lm$coefficients |> as.data.frame() |> as_tibble()
    
    names(lm_coeff) <- c(
      #"rowname", # not needed when fitted to go through origin
      "Estimate", "std_error", "t_value", "p_value_slope")
    #lm_coeff
    
    slope = lm_coeff$Estimate |> signif(digits = 3)
    p_val_slope = lm_coeff$p_value_slope |> signif(digits = 3)
    r_squared_mult = curve_lm$r.squared |> signif(digits = 3)
    
    color_p_val <- case_when(
      p_val_slope > 0.05 ~ "red",
      .default = "black"
    )
    
    size_p_val <- case_when(
      p_val_slope > 0.05 ~ 4,
      .default = 2.8
    )
    unit <-  meta_line |> 
      select(std_unit) |> magrittr::extract2(1)
    extractant <- meta_line |> select(extractant_sp, extractant_unit, extractant_conc) 
    
    # Plot it
    std_curve <-  curve |> 
      ggplot(aes(x = abs_corrected, y = conc)) + 
      theme_minimal() +
      geom_smooth(method = "lm", color = "grey30", formula = y~x-1) +
      geom_jitter(alpha = 0.5) +
      labs(
        title = plate,
        subtitle = paste("slope = ", slope, "\nMultiple R-squared = ", r_squared_mult),
        caption = paste0(
          "extracted in ", 
          extractant$extractant_conc[1], extractant$extractant_unit[1],
          " ",
          extractant)) +
      ylab(paste0("Concentration of ", curve$N_sp[1], "\n[", unit, "]")) +
      xlab("Blanc-corrected absorbance") +
      annotate(
        geom = "text", 
        x = median(curve$abs_corrected),
        y = max(curve$conc),
        label = paste0("p-value of\nslope = ", p_val_slope),
        color = color_p_val,
        fontface = "bold",
        size = size_p_val)
    std_curve
    
    plots[[plate]] <- std_curve 
    #plots[[plate]]
    
    lm_output <- bind_rows(
      lm_output,
      tibble(
        plate_id = plate,
        slope = slope,
        p_val_slope = p_val_slope,
        r_squared_mult = r_squared_mult
      )
    )
    #i = i +1
  }
  
  nb_of_iterations <- length(plots) %/% max_nb_plots
  rest_to_plot <- length(plots) %% max_nb_plots
  
  #i = 1
  multi_plots = list()
  if (nb_of_iterations > 0) {
    for (i in 1:nb_of_iterations) {
      first_plot_index <- (i-1)*max_nb_plots + 1
      last_plot_index <- first_plot_index + max_nb_plots -1
      multi_plots[[i]] <- wrap_plots(plots[first_plot_index:last_plot_index], axis_titles = "collect")
      #i = i +1
    } 
    if (rest_to_plot > 0) { 
      first_plot_index <- (i-1)*max_nb_plots + 1
      last_plot_index <- first_plot_index + rest_to_plot -1
      multi_plots[[i+1]] <- wrap_plots(plots[first_plot_index:last_plot_index], axis_titles = "collect")
    }
  } else {
    first_plot_index <- 1
    last_plot_index <- rest_to_plot
    multi_plots[[1]] <- wrap_plots(plots[first_plot_index:last_plot_index], axis_titles = "collect")
  }

  # Now save the QC somewhere
  if(save_pdf) {
    #i = 1
    for (i in 1:length(multi_plots)) {
      single_file <- paste0(filepath, filenames, i, ".pdf")
      #dev.new()
      #pdf(single_file, width = 8, height = 6)
      ggsave(plot = multi_plots[[i]], single_file, width = 12, height = 8)
      #dev.off()
    }
  }
  
  # Validate p-values of all curves
  bad_p_val <- lm_output |> arrange(desc(p_val_slope)) |> filter(p_val_slope >= 0.05)
  
  if (nrow(bad_p_val) == 0) {
    message(paste0(
      "!! YAY !!\nThe linear model is significative for all plates (p-value < 0.05). You can proceed with the inference of concentrations."
    ))
  } else {
    warning(paste0(
      "!!!!! Watch out !!!! The linear model is not significative at p-value < 0.05 for ",
      nrow(bad_p_val), 
      " curve",
      if (nrow(bad_p_val > 1)) {"s"},
      ".\nCheck out the table hereabove to identify suspicious plates and decide what to do."
    ))
  }
  
  return(list(
    "lm_output" = lm_output,
    "multi_plots" = multi_plots
  ))
}
