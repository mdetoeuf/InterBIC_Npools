# Import and tidy with plate2N
Morgane de Toeuf

- [Set up](#set-up)
- [1 - Import data](#1---import-data)
- [2 - Verticalize plates](#2---verticalize-plates)
- [3 - tidy table](#3---tidy-table)
- [4 - Add plate metadata](#4---add-plate-metadata)
- [5 - Export](#5---export)

# Set up

``` r
rm(list = ls())

# once in a while, redownload
#pak::pak("mdetoeuf/plate2N”) # if doesn't work --> try pak::pak_cleanup()
library(plate2N)
library(tidyverse)
#library(janitor) # for row_to_names()
```

# 1 - Import data

All object names ending with “abs” contain absorbance data, all object
names ending with “map” contain the mapping of the equivalent abs data

Here we import all our datasets, with various functions from the package
`plate2N`. The output is displayed each time a new function is used.

``` r
# Nmin for t1 and t2
(Nmin_t1t2_abs <- txt_to_tibble("raw_data/Nmin/"))
```

    # A tibble: 891 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     0.039 0.046 0.049 0.042 0.042 0.041 0.042 0.039 0.043 0.042 0.038 0.038
     3 B     0.043 0.046 0.048 0.042 0.042 0.041 0.041 0.039 0.043 0.041 0.038 0.042
     4 C     0.047 0.045 0.049 0.042 0.041 0.040 0.041 0.039 0.044 0.041 0.038 0.046
     5 D     0.053 0.055 0.048 0.042 0.042 0.041 0.041 0.039 0.043 0.042 0.038 0.053
     6 E     0.059 0.041 0.042 0.041 0.041 0.046 0.041 0.039 0.043 0.041 0.038 0.061
     7 F     0.067 0.043 0.042 0.044 0.041 0.045 0.041 0.039 0.042 0.041 0.038 0.070
     8 G     0.096 0.042 0.042 0.041 0.041 0.045 0.041 0.039 0.042 0.040 0.038 0.103
     9 H     0.126 0.042 0.042 0.041 0.042 0.045 0.042 0.039 0.042 0.041 0.038 0.122
    10 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 881 more rows

``` r
(Nmin_t1t2_map <- csv_to_tibble("raw_data/Nmin/Nmin_maps.csv"))
```

    # A tibble: 891 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     Std   81_t… 82_t… 83_t… 84_t… 85_t… 86_t… extr  87_t… 88_t… empty Std  
     3 B     Std   81_t… 82_t… 83_t… 84_t… 85_t… 86_t… extr  87_t… 88_t… empty Std  
     4 C     Std   81_t… 82_t… 83_t… 84_t… 85_t… 86_t… extr  87_t… 88_t… empty Std  
     5 D     Std   81_t… 82_t… 83_t… 84_t… 85_t… 86_t… extr  87_t… 88_t… empty Std  
     6 E     Std   89_t… 90_t… 91_t… 92_t… 93_t… 94_t… extr  95_t… 96_t… empty Std  
     7 F     Std   89_t… 90_t… 91_t… 92_t… 93_t… 94_t… extr  95_t… 96_t… empty Std  
     8 G     Std   89_t… 90_t… 91_t… 92_t… 93_t… 94_t… extr  95_t… 96_t… empty Std  
     9 H     Std   89_t… 90_t… 91_t… 92_t… 93_t… 94_t… extr  95_t… 96_t… empty Std  
    10 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 881 more rows

``` r
# Nmin for t3
Nmin_t3_abs <- csv_to_tibble("raw_data/Nmin_t3/Nmint3_data.csv")
Nmin_t3_map <- csv_to_tibble("raw_data/Nmin_t3/Nmint3_maps.csv")

# TDN (map csv not in the right format --> back to import_csv)
TDN_abs <- csv_to_tibble("raw_data/TDN/TDN_data.csv")
(TDN_map <- 
    read_csv(
      "raw_data/TDN/TDN_maps.csv", 
      col_names = FALSE, col_select = X14:X26, show_col_types = FALSE) |> 
    na.omit() |> 
    rename_with(~c("row", paste0("X", seq(1:12)))))
```

    # A tibble: 531 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NO2_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     Std   102_… 103_… 86_t… 101_… 84_t… 86_t… extr  95_t… 83_t… 93_t… Fiel…
     3 B     Std   102_… 103_… 86_t… 101_… 84_t… 86_t… extr  95_t… 83_t… 93_t… Fiel…
     4 C     Std   102_… 103_… 86_t… 101_… 84_t… 86_t… extr  95_t… 83_t… 93_t… Fiel…
     5 D     Std   102_… 103_… 86_t… 101_… 84_t… 86_t… extr  95_t… 83_t… 93_t… Fiel…
     6 E     Std   92_t… 82_t… 94_t… 87_t… 91_t… 100_… extr  88_t… 96_t… 95_t… Fiel…
     7 F     Std   92_t… 82_t… 94_t… 87_t… 91_t… 100_… extr  88_t… 96_t… 95_t… Fiel…
     8 G     Std   92_t… 82_t… 94_t… 87_t… 91_t… 100_… extr  88_t… 96_t… 95_t… Fiel…
     9 H     Std   92_t… 82_t… 94_t… 87_t… 91_t… 100_… extr  88_t… 96_t… 95_t… Fiel…
    10 NO2_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 521 more rows

``` r
# Example for Skanit-formatted csv
(Skanit_abs <- skanit_to_tibble("raw_data/Skanit_example/MR_R1_t0.csv")$abs)
```

    # A tibble: 90 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 M12   1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     1.44… 1.48… 1.43… 1.47… 1.51… 1.49… 1.50… 1.52… 1.53… 1.52… 1.51… 1.53…
     3 B     1.45… 1.52… 1.50… 1.49… 1.53… 1.56… 1.53… 1.53… 1.53… 1.54… 1.54… 1.54…
     4 C     1.51… 1.54… 1.52… 1.53… 1.56… 1.54… 1.56… 1.57… 1.55… 1.55… 1.56… 1.55…
     5 D     1.53… 1.55… 1.57… 1.52… 1.54… 1.56… 1.55… 1.56… 1.55… 1.56… 1.55… 1.57…
     6 E     1.53… 1.55… 1.55… 1.53… 1.55… 1.55… 1.55… 1.55… 1.58… 1.56… 1.56… 1.55…
     7 F     1.52… 1.57… 1.55… 1.53… 1.52… 1.56… 1.57… 1.58… 1.56… 1.55… 1.57… 1.55…
     8 G     1.53… 1.55… 1.53… 1.53… 1.56… 1.55… 1.57… 1.55… 1.55… 1.56… 1.56… 1.57…
     9 H     1.49… 1.54… 1.52… 1.51… 1.55… 1.55… 1.56… 1.57… 1.53… 1.56… 1.57… 1.58…
    10 M16   1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 80 more rows

``` r
(Skanit_map <- skanit_to_tibble("raw_data/Skanit_example/MR_R1_t0.csv")$map)
```

    # A tibble: 90 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 M12   1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     3 B     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     4 C     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     5 D     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     6 E     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     7 F     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     8 G     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
     9 H     Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0… Std0…
    10 M16   1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 80 more rows

> [!TIP]
>
> ### For users of the Skanit file format
>
> The previous chunk (absorbance and plate map data) demonstrate the
> usage of the Skanit format version of the import function, but the
> rest of this pipeline does not require the Skanit data, so it is no
> longer used passed this import step. However, the output format
> should, in theory, be strictly identical to other format options, so
> that the object `Skanit_abs` should, in theory, be interchangeable
> with `TDN_abs` or `Nmin_abs`. Please reach out if you encounter any
> difficulty.

# 2 - Verticalize plates

We do this separately for each data set. Because those distinct data
sets will be combined eventually, we add a prefix to column names to
reflect dataset + whether it is abs or map.

``` r
# Nmint1t2
Nmin_t1t2_vert <- join_abs_map(Nmin_t1t2_abs, Nmin_t1t2_map, dataset = "Nmint1t2-")

# Nmint3
Nmin_t3_vert <- join_abs_map(Nmin_t3_abs, Nmin_t3_map, dataset = "Nmint3-")

# TDN
TDN_vert <- join_abs_map(TDN_abs, TDN_map, dataset = "TDN-")

## Check them out
Nmin_t1t2_vert; Nmin_t3_vert; TDN_vert
```

    # A tibble: 96 × 200
       row   column `Nmint1t2-abs-NH4_1F1` `Nmint1t2-abs-NH4_1F2_1`
       <chr> <chr>  <chr>                  <chr>                   
     1 A     1      0.039                  0.039                   
     2 A     2      0.046                  0.042                   
     3 A     3      0.049                  0.041                   
     4 A     4      0.042                  0.041                   
     5 A     5      0.042                  0.042                   
     6 A     6      0.041                  0.041                   
     7 A     7      0.042                  0.041                   
     8 A     8      0.039                  0.038                   
     9 A     9      0.043                  0.042                   
    10 A     10     0.042                  0.041                   
    # ℹ 86 more rows
    # ℹ 196 more variables: `Nmint1t2-abs-NH4_1F2_2` <chr>,
    #   `Nmint1t2-abs-NH4_1F3` <chr>, `Nmint1t2-abs-NH4_1F4` <chr>,
    #   `Nmint1t2-abs-NH4_1F5` <chr>, `Nmint1t2-abs-NH4_1G1` <chr>,
    #   `Nmint1t2-abs-NH4_1G2` <chr>, `Nmint1t2-abs-NH4_1G3` <chr>,
    #   `Nmint1t2-abs-NH4_1G4` <chr>, `Nmint1t2-abs-NH4_1G5` <chr>,
    #   `Nmint1t2-abs-NH4_2F1_1` <chr>, `Nmint1t2-abs-NH4_2F1_2` <chr>, …

    # A tibble: 96 × 74
       row   column `Nmint3-abs-NO3_R1R2_1` `Nmint3-abs-NO3_R2R3_1`
       <chr> <chr>  <chr>                   <chr>                  
     1 A     1      0.094                   0.134                  
     2 A     2      0.085                   0.089                  
     3 A     3      0.277                   0.151                  
     4 A     4      0.152                   0.147                  
     5 A     5      0.186                   0.182                  
     6 A     6      0.149                   0.304                  
     7 A     7      0.187                   0.205                  
     8 A     8      0.135                   0.22                   
     9 A     9      0.136                   0.227                  
    10 A     10     0.176                   0.086                  
    # ℹ 86 more rows
    # ℹ 70 more variables: `Nmint3-abs-NO3_R4R5_1` <chr>,
    #   `Nmint3-abs-NO3_R5R6_1` <chr>, `Nmint3-abs-NO3_R6R7_1` <chr>,
    #   `Nmint3-abs-NO3_R7R8_1` <chr>, `Nmint3-abs-NO2_R1R2_1` <chr>,
    #   `Nmint3-abs-NO2_R2R3_1` <chr>, `Nmint3-abs-NO2_R4R5_1` <chr>,
    #   `Nmint3-abs-NO2_R5R6_1` <chr>, `Nmint3-abs-NO2_R6R7_1` <chr>,
    #   `Nmint3-abs-NO2_R7R8_1` <chr>, `Nmint3-abs-NH4_R1R2_1` <chr>, …

    # A tibble: 96 × 120
       row   column `TDN-abs-NO3_TDN_01` `TDN-abs-NO3_TDN_02` `TDN-abs-NO3_TDN_03`
       <chr> <chr>  <chr>                <chr>                <chr>               
     1 A     1      0.095                0.097                0.113               
     2 A     2      0.537                0.552                0.53                
     3 A     3      0.528                0.559                0.528               
     4 A     4      0.562                0.555                0.521               
     5 A     5      0.51                 0.538                0.521               
     6 A     6      0.507                0.552                0.54                
     7 A     7      0.546                0.534                0.551               
     8 A     8      0.078                0.079                0.097               
     9 A     9      0.519                0.589                0.559               
    10 A     10     0.543                0.535                0.528               
    # ℹ 86 more rows
    # ℹ 115 more variables: `TDN-abs-NO3_TDN_04` <chr>, `TDN-abs-NO3_TDN_05` <chr>,
    #   `TDN-abs-NO3_TDN_06` <chr>, `TDN-abs-NO3_TDN_07` <chr>,
    #   `TDN-abs-NO3_TDN_08` <chr>, `TDN-abs-NO3_TDN_09` <chr>,
    #   `TDN-abs-NO3_TDN_10` <chr>, `TDN-abs-NO3_TDN_11` <chr>,
    #   `TDN-abs-NO3_TDN_12` <chr>, `TDN-abs-NO3_TDN_13` <chr>,
    #   `TDN-abs-NO3_TDN_14` <chr>, `TDN-abs-NO3_TDN_15` <chr>, …

Then, we use the `dplyr::left_join()` function to join all plate data in
a single data table. Note the distinct column names with their dataset
and abs/map-related prefixes

``` r
# join all 3 in a single table with all "plate data"
all_vert <- Nmin_t1t2_vert |> 
  left_join(Nmin_t3_vert, by = join_by(row, column)) |> 
  left_join(TDN_vert, by = join_by(row, column))

# check it out + look at variable names
all_vert; names(all_vert)[1:100]
```

    # A tibble: 96 × 390
       row   column `Nmint1t2-abs-NH4_1F1` `Nmint1t2-abs-NH4_1F2_1`
       <chr> <chr>  <chr>                  <chr>                   
     1 A     1      0.039                  0.039                   
     2 A     2      0.046                  0.042                   
     3 A     3      0.049                  0.041                   
     4 A     4      0.042                  0.041                   
     5 A     5      0.042                  0.042                   
     6 A     6      0.041                  0.041                   
     7 A     7      0.042                  0.041                   
     8 A     8      0.039                  0.038                   
     9 A     9      0.043                  0.042                   
    10 A     10     0.042                  0.041                   
    # ℹ 86 more rows
    # ℹ 386 more variables: `Nmint1t2-abs-NH4_1F2_2` <chr>,
    #   `Nmint1t2-abs-NH4_1F3` <chr>, `Nmint1t2-abs-NH4_1F4` <chr>,
    #   `Nmint1t2-abs-NH4_1F5` <chr>, `Nmint1t2-abs-NH4_1G1` <chr>,
    #   `Nmint1t2-abs-NH4_1G2` <chr>, `Nmint1t2-abs-NH4_1G3` <chr>,
    #   `Nmint1t2-abs-NH4_1G4` <chr>, `Nmint1t2-abs-NH4_1G5` <chr>,
    #   `Nmint1t2-abs-NH4_2F1_1` <chr>, `Nmint1t2-abs-NH4_2F1_2` <chr>, …

      [1] "row"                    "column"                 "Nmint1t2-abs-NH4_1F1"  
      [4] "Nmint1t2-abs-NH4_1F2_1" "Nmint1t2-abs-NH4_1F2_2" "Nmint1t2-abs-NH4_1F3"  
      [7] "Nmint1t2-abs-NH4_1F4"   "Nmint1t2-abs-NH4_1F5"   "Nmint1t2-abs-NH4_1G1"  
     [10] "Nmint1t2-abs-NH4_1G2"   "Nmint1t2-abs-NH4_1G3"   "Nmint1t2-abs-NH4_1G4"  
     [13] "Nmint1t2-abs-NH4_1G5"   "Nmint1t2-abs-NH4_2F1_1" "Nmint1t2-abs-NH4_2F1_2"
     [16] "Nmint1t2-abs-NH4_2F2_1" "Nmint1t2-abs-NH4_2F2_2" "Nmint1t2-abs-NH4_2F3_1"
     [19] "Nmint1t2-abs-NH4_2F3_2" "Nmint1t2-abs-NH4_2F4_1" "Nmint1t2-abs-NH4_2F4_2"
     [22] "Nmint1t2-abs-NH4_2F5_1" "Nmint1t2-abs-NH4_2F5_2" "Nmint1t2-abs-NH4_2F6_1"
     [25] "Nmint1t2-abs-NH4_2F6_2" "Nmint1t2-abs-NH4_2P1"   "Nmint1t2-abs-NH4_2P2"  
     [28] "Nmint1t2-abs-NH4_2P3"   "Nmint1t2-abs-NH4_2P4"   "Nmint1t2-abs-NH4_2P5"  
     [31] "Nmint1t2-abs-NH4_2P6_1" "Nmint1t2-abs-NH4_2P6_2" "Nmint1t2-abs-NH4_2P6_3"
     [34] "Nmint1t2-abs-NH4_2P7_1" "Nmint1t2-abs-NH4_2P7_2" "Nmint1t2-abs-NO2_1F1"  
     [37] "Nmint1t2-abs-NO2_1F2_1" "Nmint1t2-abs-NO2_1F2_2" "Nmint1t2-abs-NO2_1F3"  
     [40] "Nmint1t2-abs-NO2_1F4"   "Nmint1t2-abs-NO2_1F5"   "Nmint1t2-abs-NO2_1G1"  
     [43] "Nmint1t2-abs-NO2_1G2"   "Nmint1t2-abs-NO2_1G3"   "Nmint1t2-abs-NO2_1G4"  
     [46] "Nmint1t2-abs-NO2_1G5"   "Nmint1t2-abs-NO2_2F1_1" "Nmint1t2-abs-NO2_2F1_2"
     [49] "Nmint1t2-abs-NO2_2F2_1" "Nmint1t2-abs-NO2_2F2_2" "Nmint1t2-abs-NO2_2F3_1"
     [52] "Nmint1t2-abs-NO2_2F3_2" "Nmint1t2-abs-NO2_2F4_1" "Nmint1t2-abs-NO2_2F4_2"
     [55] "Nmint1t2-abs-NO2_2F5_1" "Nmint1t2-abs-NO2_2F5_2" "Nmint1t2-abs-NO2_2F6_1"
     [58] "Nmint1t2-abs-NO2_2F6_2" "Nmint1t2-abs-NO2_2P1"   "Nmint1t2-abs-NO2_2P2"  
     [61] "Nmint1t2-abs-NO2_2P3"   "Nmint1t2-abs-NO2_2P4"   "Nmint1t2-abs-NO2_2P5"  
     [64] "Nmint1t2-abs-NO2_2P6_1" "Nmint1t2-abs-NO2_2P6_2" "Nmint1t2-abs-NO2_2P6_3"
     [67] "Nmint1t2-abs-NO2_2P7_1" "Nmint1t2-abs-NO2_2P7_2" "Nmint1t2-abs-NO3_1F1"  
     [70] "Nmint1t2-abs-NO3_1F2_1" "Nmint1t2-abs-NO3_1F2_2" "Nmint1t2-abs-NO3_1F3"  
     [73] "Nmint1t2-abs-NO3_1F4"   "Nmint1t2-abs-NO3_1F5"   "Nmint1t2-abs-NO3_1G1"  
     [76] "Nmint1t2-abs-NO3_1G2"   "Nmint1t2-abs-NO3_1G3"   "Nmint1t2-abs-NO3_1G4"  
     [79] "Nmint1t2-abs-NO3_1G5"   "Nmint1t2-abs-NO3_2F1_1" "Nmint1t2-abs-NO3_2F1_2"
     [82] "Nmint1t2-abs-NO3_2F2_1" "Nmint1t2-abs-NO3_2F2_2" "Nmint1t2-abs-NO3_2F3_1"
     [85] "Nmint1t2-abs-NO3_2F3_2" "Nmint1t2-abs-NO3_2F4_1" "Nmint1t2-abs-NO3_2F4_2"
     [88] "Nmint1t2-abs-NO3_2F5_1" "Nmint1t2-abs-NO3_2F5_2" "Nmint1t2-abs-NO3_2F6_1"
     [91] "Nmint1t2-abs-NO3_2F6_2" "Nmint1t2-abs-NO3_2P1"   "Nmint1t2-abs-NO3_2P2"  
     [94] "Nmint1t2-abs-NO3_2P3"   "Nmint1t2-abs-NO3_2P4"   "Nmint1t2-abs-NO3_2P5"  
     [97] "Nmint1t2-abs-NO3_2P6_1" "Nmint1t2-abs-NO3_2P6_2" "Nmint1t2-abs-NO3_2P6_3"
    [100] "Nmint1t2-abs-NO3_2P7_1"

# 3 - tidy table

What we want, is to have one column for absorbance data, another one for
the mapping.

`verticalize_to_tidy()`\` brings the verticalized plates into a tidy
format, extracting from column names dataset, plate_id , mapping data
and absorbance data into their own column, thus creating a very long
tidy table where each row corresponds to a unique well (see column
unique_well_id).

``` r
(all_raw_abs_tidy <- vertical_to_tidy(all_vert))
```

    # A tibble: 18,624 × 8
       row   column well_id unique_well_id dataset  plate_id  map   abs  
       <chr> <chr>  <chr>   <chr>          <chr>    <chr>     <chr> <chr>
     1 A     1      A1      A1_NH4_1F1     Nmint1t2 NH4_1F1   Std   0.039
     2 A     1      A1      A1_NH4_1F2_1   Nmint1t2 NH4_1F2_1 Std   0.039
     3 A     1      A1      A1_NH4_1F2_2   Nmint1t2 NH4_1F2_2 Std   0.039
     4 A     1      A1      A1_NH4_1F3     Nmint1t2 NH4_1F3   Std   0.038
     5 A     1      A1      A1_NH4_1F4     Nmint1t2 NH4_1F4   Std   0.039
     6 A     1      A1      A1_NH4_1F5     Nmint1t2 NH4_1F5   Std   0.039
     7 A     1      A1      A1_NH4_1G1     Nmint1t2 NH4_1G1   Std   0.039
     8 A     1      A1      A1_NH4_1G2     Nmint1t2 NH4_1G2   Std   0.039
     9 A     1      A1      A1_NH4_1G3     Nmint1t2 NH4_1G3   Std   0.038
    10 A     1      A1      A1_NH4_1G4     Nmint1t2 NH4_1G4   Std   0.038
    # ℹ 18,614 more rows

# 4 - Add plate metadata

Now we have a tidy table that is, in theory, ready for export. In real
life, this data cannot be used on its own, and plate metadata is
necessary.

First, we import metadata for each dataset, and add a “dataset” column
that will help with the next steps.

``` r
# import csvs
(Nmin_metadata <- read_csv(
  "raw_data/Nmin/Nmin_metadata.csv", show_col_types = FALSE
  ) |>
  mutate(dataset = "Nmint1t2"))
```

    # A tibble: 99 × 17
       plate_id  date  time  sampling_time std_column std_sp std_unit    std_prep
       <chr>     <lgl> <lgl> <chr>         <chr>      <chr>  <chr>       <chr>   
     1 NH4_1F1   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     2 NH4_1F2_1 NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     3 NH4_1F2_2 NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     4 NH4_1F3   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     5 NH4_1F4   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     6 NH4_1F5   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     7 NH4_1G1   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     8 NH4_1G2   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     9 NH4_1G3   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
    10 NH4_1G4   NA    NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
    # ℹ 89 more rows
    # ℹ 9 more variables: std_conc <chr>, sample_dilution <chr>,
    #   extractant_column <lgl>, extractant_sp <chr>, extractant_unit <chr>,
    #   extractant_conc <dbl>, empty_column <lgl>, wait_min <chr>, dataset <chr>

``` r
str(Nmin_metadata)
```

    tibble [99 × 17] (S3: tbl_df/tbl/data.frame)
     $ plate_id         : chr [1:99] "NH4_1F1" "NH4_1F2_1" "NH4_1F2_2" "NH4_1F3" ...
     $ date             : logi [1:99] NA NA NA NA NA NA ...
     $ time             : logi [1:99] NA NA NA NA NA NA ...
     $ sampling_time    : chr [1:99] "t1" "t1" "t1" "t1" ...
     $ std_column       : chr [1:99] "1-12" "1-12" "1-12" "1-12" ...
     $ std_sp           : chr [1:99] "NH4" "NH4" "NH4" "NH4" ...
     $ std_unit         : chr [1:99] "mg NH4+ L-1" "mg NH4+ L-1" "mg NH4+ L-1" "mg NH4+ L-1" ...
     $ std_prep         : chr [1:99] "H2O" "H2O" "H2O" "H2O" ...
     $ std_conc         : chr [1:99] "0-0.5-1-2-3-4-8-10" "0-0.5-1-2-3-4-8-10" "0-0.5-1-2-3-4-8-10" "0-0.5-1-2-3-4-8-10" ...
     $ sample_dilution  : chr [1:99] "1x" "1x" "1x" "1x" ...
     $ extractant_column: logi [1:99] NA NA NA NA NA NA ...
     $ extractant_sp    : chr [1:99] "K2SO4" "K2SO4" "K2SO4" "K2SO4" ...
     $ extractant_unit  : chr [1:99] "M" "M" "M" "M" ...
     $ extractant_conc  : num [1:99] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
     $ empty_column     : logi [1:99] NA NA NA NA NA NA ...
     $ wait_min         : chr [1:99] "30 min" "30 min" "30 min" "30 min" ...
     $ dataset          : chr [1:99] "Nmint1t2" "Nmint1t2" "Nmint1t2" "Nmint1t2" ...

``` r
Nmin_t3_metadata <- read_csv(
  "raw_data/Nmin_t3/Nmint3_metadata.csv", show_col_types = FALSE
  ) |> 
  mutate(dataset = "Nmint3")

TDN_metadata <- read_csv(
  "raw_data/TDN/TDN_metadata.csv", show_col_types = FALSE
  ) |> 
  mutate(std_column = as.character(std_column)) |> 
  mutate(dataset = "TDN")
```

Then we join them in one big metadata file. ! this only works because
the 3 metadata files have stricly the same stucture (same column names
in the same order, containing the same data type (string, …)).

``` r
# join csv's
(all_plate_metadata <- bind_rows(Nmin_metadata, Nmin_t3_metadata, TDN_metadata))
```

    # A tibble: 194 × 18
       plate_id  date  time  sampling_time std_column std_sp std_unit    std_prep
       <chr>     <chr> <lgl> <chr>         <chr>      <chr>  <chr>       <chr>   
     1 NH4_1F1   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     2 NH4_1F2_1 <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     3 NH4_1F2_2 <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     4 NH4_1F3   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     5 NH4_1F4   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     6 NH4_1F5   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     7 NH4_1G1   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     8 NH4_1G2   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
     9 NH4_1G3   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
    10 NH4_1G4   <NA>  NA    t1            1-12       NH4    mg NH4+ L-1 H2O     
    # ℹ 184 more rows
    # ℹ 10 more variables: std_conc <chr>, sample_dilution <chr>,
    #   extractant_column <dbl>, extractant_sp <chr>, extractant_unit <chr>,
    #   extractant_conc <dbl>, empty_column <chr>, wait_min <chr>, dataset <chr>,
    #   wavelength <chr>

Keep only relevant columns

``` r
all_plate_metadata_keep <- all_plate_metadata |> 
  select(dataset, plate_id, std_sp, std_conc, std_unit, sample_dilution, date)
```

We could already here, join both metadata and tidy data, but this would
create a much larger file to save on the computer than the 2 files
separately (because rows of the metadata would be repeated 96 times, one
per well of the plate.

# 5 - Export

Some steps will be specific of TDN, so we separate the raw data
according to TDN or rest

First, prepare those subsets

``` r
all_raw_abs_TDN <- all_raw_abs_tidy |> filter(dataset == "TDN")
all_raw_abs_noTDN <- all_raw_abs_tidy |> filter_out(dataset == "TDN")

all_plate_metadata_TDN <- all_plate_metadata_keep |> 
  filter(dataset == "TDN")
all_plate_metadata_noTDN <- all_plate_metadata_keep |> 
  filter_out(dataset == "TDN")
```

Then, export them as .rds

``` r
all_raw_abs_TDN |> write_rds("output/data/1_all_raw_abs_TDN.rds")
all_raw_abs_noTDN |> write_rds("output/data/1_all_raw_abs_noTDN.rds")

all_plate_metadata_TDN |> write_rds("output/data/1_all_plate_metadata_TDN.rds")
all_plate_metadata_noTDN |> write_rds("output/data/1_all_plate_metadata_noTDN.rds")
```
