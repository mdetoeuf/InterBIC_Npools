#** Plot all std curves for a given N species *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-25
#> Purpose     : quality check (QC) of intra- and inter-batch variability
#> Returns     : plot
#>               
#** To Do: specify particularities *
#*
#** - Mention that sampling_time can be replaced, and how *
#** - have data and metadata already filtered for relevant N species *
#*
 
#** Example of parameters for NH4 and a choice of colorpalette *

#> metadata <- Nmin_metadata |> filter(std_sp == "NH4")
#> data <- std_rm_outlier |> filter(N_sp == "NH4")
#> color_time <- brewer.pal(n = 3, "Accent")
#> names(color_time) <- c("t1", "t2", "t3")
#> 

plot_qc_std_all <- function(
    data,
    metadata,
    pipetting_direction = top_down_pipetting,
    color_time = c("t1" = "#7FC97F", "t2" = "#BEAED4", "t3" = "#FDC086")
    ) {
  
  p <- ggplot() + theme_minimal()
  for (i in 1:nrow(metadata)) {
    plate <- metadata$plate_id[i]
    N_sp <- metadata$std_sp[i]
    
    conc <- tibble(
      conc = extract_curve(metadata, N_sp = N_sp)[2:8],
      row = pipetting_direction
    )
    
    curve <- data |> 
      filter(plate_id == plate) |> 
      left_join(conc, by = join_by(row)) 
    
    t = metadata$sampling_time[i]

    if(t == "t1"){ 
      p <- p + 
        geom_smooth(
          data = curve, 
          aes(x = abs_corrected, y = conc, 
              fill = "t1", color = "t1"),
          formula = 'y ~ x',
          method = "lm", se = TRUE, alpha = 0.1) 
    } else if (t == "t2") {
      p <- p + 
        geom_smooth(
          data = curve, 
          aes(x = abs_corrected, y = conc, 
              fill = "t2", color = "t2"),
          formula = 'y ~ x',
          method = "lm", se = TRUE, alpha = 0.1) 
    } else if (t == "t3") {
      p <- p + 
        geom_smooth(
          data = curve, 
          aes(x = abs_corrected, y = conc, 
              fill = "t3", color = "t3",),
          formula = 'y ~ x',
          method = "lm", se = TRUE, alpha = 0.1) 
    }
  }
  
  p + 
    labs(title = "Regression equations for all plates",
         subtitle = paste0("for ", N_sp)) +
    ylab(metadata$std_unit[1]) +
    xlab("blanc-corrected absorbance") +
    scale_color_manual(name = "Sampling Time", values = color_time) +
    scale_fill_manual(name = "Sampling Time", values = color_time)
}

# plot_qc_std_all(
#   data = std_rm_outlier |> filter(N_sp == "NH4"),
#   metadata = Nmin_metadata |> filter(std_sp == "NH4"))
