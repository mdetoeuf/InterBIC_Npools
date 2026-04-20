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

#> metadata <- Nmin_metadata
#> data <- corrected_data 
#> lm_output = std_reg$lm_output
#> 

abs_to_mgN_L <- function(
  data,
  metadata,
  lm_output
  ) {
  
  molar_masses <- c(
    "N" = 14.0069,
    "NO3" = 62.0051,
    "NO2" = 46.0057,
    "NH4" = 36.0775
  )
  
  data <- data |> group_by(plate_id)
  
  # create an empty table with the right structure to be iteratively completed
  data_transformed <- 
    data |> 
    filter(FALSE) |> 
    mutate(conc_mgNsp_L = double(), conc_N_L = double())
  
  for (i in 1:n_groups(data)) {
    # get plate details
    plate <- metadata$plate_id[i]
    N_sp <- str_split_i(plate, "_", 1)
    slope <- lm_output |> 
      filter(plate_id == plate) |> 
      select(slope) |> magrittr::extract2(1)
    
    # compute concentration in mg Nsp per L
    plate_data <- data |> 
      filter(plate_id == plate) |> 
      mutate(
        conc_mgNsp_L = slope * abs_corrected,
        conc_N_L = unname(conc_mgNsp_L * molar_masses["N"] / molar_masses[N_sp])
      )
    data_transformed <- bind_rows(data_transformed, plate_data)
    #i = i+1
  }
  
  # count nb of N species in data set 
  nb_Nsp <- data_transformed$N_sp |> unique() |> length()
  
  boxplot <- data_transformed |> 
    ggplot(aes(x = plate_map, y = conc_N_L)) +
    theme_minimal()+
    geom_boxplot() +
    facet_wrap(~N_sp, nrow = nb_Nsp, scales = "free_y", strip.position = "right")
  
  density <- data_transformed |> 
    ggplot(aes(x = conc_N_L)) +
    theme_minimal()+
    geom_density() +
    facet_wrap(~N_sp, nrow = nb_Nsp, scales = "free_y", strip.position = "right")
  
  return(list(
    "data" = data_transformed,
    "boxplot" = boxplot,
    "density" = density))
}

#abs_to_mgN_L(data = corrected_data,metadata = Nmin_metadata,lm_output = std_reg$lm_output)
