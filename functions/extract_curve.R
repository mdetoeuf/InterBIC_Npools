#** Extract concentrations for a standard curve *

#> Author      : Morgane de Toeuf
#> Version     : 2026-03-25
#> Purpose     : extract values of concentration of a given N species from provided metadata
#> Returns     : a vector of numerical values
#>               
#** the metadata file needs to contain the following columns: *
#**     - plate_id (starting with the N species) *
#**     - std_conc (containing the concentrations) *
#*
#** N_sp must be given in the form as in the plate_id (ex: TDN, NO3, etc), *
#** it does not matter if it corresponds to the N species that was pipetted, *
#** just that the function call will find both information on a single line of * 
#** the metadata file *
#*
#** concentrations must be encoded with "." as a digit and "-" as separators *
#** 
#** !! Don't forget to then do the necessary correction, *
#** e.g., remove the zero value etc, depending on what was encoded* 
#*

extract_curve <- function(metadata, N_sp) {
  metadata |> 
    filter(str_split_i(plate_id, "_", 1) == N_sp) |> 
    select(std_conc) |> 
    magrittr::extract2(1,1) |> 
    str_split("-") |> 
    magrittr::extract2(1) |> 
    as.double()
}
