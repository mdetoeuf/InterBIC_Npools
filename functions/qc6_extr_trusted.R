#** TITLE *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-27
#> Purpose     : 
#> Returns     : 
#>               
#** To Do: specify particularities *
#*
#** -  The default setting for cut_threshold ensures that the code will run anyway *
#**    but !! this means no outlier is removed by default  *
#** -  *
#*

#** Example of parameters  *

#> cut_threshold <- c(0.040, 0.041, 0.039, 0.038, 0.038, 0.041, 0.042, 0.072, 0.09, 0.09)
#> 
#> 

qc6_extr_trusted <- function(
    QC5,
    cut_threshold = rep(1.00, length(QC5$suspicious_plate_id))
  ) {
  
  # only if QC5 rendered suspicious plates, otherwise the whole outlier removal will be skipped
  if (!is.null(QC5$extr_suspicious)) {
    
    # sends a warning if there are suspicious plates but the user failed to manually set threshold values
    if (sum(cut_threshold) == length(cut_threshold)) {
      warning("You have not set a cutting threshold for outlier wells.
              \nAll average absorbance values computed will be including any potential outlier.
              \nTo change this, manually input a vector with one value for each `suspicious plate`, using `cut_threshold = c()`.")
    }
    
    #names(cut_threshold) <- QC5$suspicious_plate_id
    
    grouped_rows_suspicious <- group_rows(QC5$extr_suspicious)
    #grouped_rows_suspicious
    
    rep_threshold <- c()
    #i = 1
    for (i in 1:length(grouped_rows_suspicious)) {
      to_append <- rep(cut_threshold[i], length(grouped_rows_suspicious[[i]]))
      rep_threshold <- append(rep_threshold, to_append)
    }
    
    # create a tibble with one row per untrusted well (based on cut_threshold)
    extr_untrusted <- QC5$extr_suspicious |> 
      ungroup() |> 
      mutate(cut_threshold = rep_threshold) |> 
      filter(absorbance > cut_threshold)
    
    # filtering out untrusted values from extractant data
    extr_trusted <- QC5$extr_data |> 
      ungroup() |> 
      filter(unique_well_id %ni% extr_untrusted$unique_well_id) 
    #extr_trusted
    
    warning(paste0(
      "From ", 
      nrow(QC5$extr_data), 
      " wells in total for extractant, ", 
      nrow(QC5$extr_data) - nrow(extr_trusted),
      " have been removed because their absorbance value appeared to be an outlier from a within-plate perspective. \nThis amounts to a removal of ",
      round(100*(nrow(QC5$extr_data) - nrow(extr_trusted))/nrow(QC5$extr_data), digits = 1),
      "% of extractant wells based on an intervention tolerance threshold of ",
      QC5$max_coeff,
      "% for the intra-plate coefficient of variation"))
    
  } else {extr_trusted <- QC5$extr_data |> ungroup() } # case if no suspicious plates
  
  # re-compute extr_average per plate
  extr_avg_trusted <- extr_trusted |> 
      summarise(
        extr_avg = mean(absorbance),
        extr_sdev = sd(absorbance),
        .by = c(plate_id, plate_map)) |> 
      mutate(
        extr_coeff_var_percent = round((100 * extr_sdev / extr_avg), digits = 1),
        extr_avg = round(extr_avg, digits = 3),
        extr_sdev = round(extr_sdev, digits = 3)) |> 
      arrange(desc(extr_coeff_var_percent))
    
    plot <- extr_avg_trusted |> 
      ggplot(aes(x = extr_coeff_var_percent)) +
      geom_histogram(bins = 100) +
      #geom_density() +
      #geom_boxplot() +
      labs(title = "Distribution of variation of absorbance of extractant (blanc) after removal of outliers") +
      xlab("intra-plate coefficient of variation [%]")
    
    extr_avg_trusted_prettier <- extr_avg_trusted |> 
      mutate(
        absorbance_avg = round(extr_avg, digits = 3),
        coeff_var = extr_coeff_var_percent,
        .keep = "unused") |> 
      select(!extr_sdev)
    
   visual <-  plot + wrap_table(
      extr_avg_trusted_prettier |> filter(coeff_var > 2.5),
      panel = "full", space = "full")
    
    
   return(list(
     "extr_avg_trusted" = extr_avg_trusted,
     "extr_distrib_coeff" = visual
   ))
  }
  #extr_avg_trusted

