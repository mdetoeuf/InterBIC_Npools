#** QC nb 2 - Plot distribution of raw absorbance measures *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : QC of raw absorbance range
#> Returns     : plot
#>               
#** To Do: specify particularities *
#*
#** -  *
#** -  *
#*

#** Example of parameters  *

#> data <- Nmin_data
#> binwidth = 0.01
#> min_abs = 0.03
#> max_abs = 1.1
#> empty_wells = "empty"
#> 

qc2_plot_range_abs <- function(
  data,
  binwidth = 0.01,
  min_abs = 0.03,
  max_abs = 1.1,
  empty_wells = "empty"
  ) {
  
  # remove empty wells
  full <- data |> filter(plate_map != empty_wells)
  
  # get number of plots (facets) = nb of N species in data set
  n_facets <- full$N_sp |> unique() |> length()
  
  # plot distribution
  plot_QC_wells <- full |> 
    ggplot(aes(x = absorbance)) +
    theme_minimal() +
    #geom_boxplot(aes(x = N_sp, y = absorbance))
    geom_histogram(binwidth = binwidth) +
    geom_vline(aes(xintercept = min_abs), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = 0.1), color = "black", alpha = 0.5) +
    geom_vline(aes(xintercept = max_abs), color = "red", alpha = 0.5) +
    annotate(geom = "label", x = min_abs, y = -150, label = min_abs, color = "red", size = 2.5) +
    annotate(geom = "label", x = max_abs, y = -150, label = max_abs, color = "red", size = 2.5) +
    annotate(geom = "label", x = 0.1, y = -150, label = "0.1", color = "black", size = 2.5) +
    # geom_density() +
    facet_wrap(~N_sp, nrow = n_facets) +
    labs(title = "Distribution of absorbance per N species")
      
  plot_QC_wells
}

#qc2_plot_range_abs(data)
