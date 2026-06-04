
# Boxplot -----------------------------------------------------------------

#data <- 
# raw_greenhouse_t2_Nmin |>
#   filter(biol_unit_nb < 100, std_sp == "NO3") |> # exclude sand and conv soil std
#   boxplot_conc(colour = "soil") + labs(title = "NO3")
  
boxplot_conc <- function(
    data,
    x = "biol_unit_nb",
    y = "conc_mgN_L",
    colour = NULL
    ) {
  
  colour_as_aesthetics = FALSE
  
  if (is.null(colour)) {colour = "purple"} else {
    if (colour %in% names(data)) {
      colour = data[[colour]]
      colour_as_aesthetics = TRUE
      }
  }
  
  plot <- data |> 
    ggplot(aes(x = as.factor(.data[[x]]), y = .data[[y]])) + 
    theme_minimal() +
    #geom_violin() +
    geom_boxplot(outliers = FALSE) +
    {if (colour_as_aesthetics) (geom_point(alpha = 0.4, aes(colour = colour)))} +
    {if (!colour_as_aesthetics) (geom_point(alpha = 0.4, colour = colour))} +
    {if (colour_as_aesthetics) (geom_text_repel(
      aes(label = well_id, colour = colour), 
      size = 2, alpha = 1, min.segment.length = 1))} +
    {if (!colour_as_aesthetics) (geom_text_repel(
      aes(label = well_id), colour = colour, 
      size = 2, alpha = 1, min.segment.length = 1))}
  
  return(plot)
}



# Ridges ------------------------------------------------------------------

#ridges_no3 <- 
  
# raw_greenhouse_t2_Nmin |> 
#   filter(biol_unit_nb < 100, std_sp == "NO3") |>  # exclude sand and conv soil std
#   plot_ridges_conc() + facet_wrap(~soil, ncol = 4) + labs(title = "NO3")
  
plot_ridges_conc <- function(
    data,
    x = "conc_mgN_L",
    groups = "biol_unit_nb",
    colour = "cs",
    y = "biol_unit_nb"
) {
  
  plot <- data |>   
    ggplot(aes(
      x = .data[[x]], 
      groups = as.factor(.data[[groups]]), 
      color = .data[[colour]], fill = .data[[colour]])) + 
    theme_minimal() +
    geom_density_ridges(aes(
      y = .data[[y]]), alpha = 0.3) #
  
  return(plot)
}

