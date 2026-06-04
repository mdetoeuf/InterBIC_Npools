# Import and tidy Absorbance Data with plate2N
Morgane de Toeuf

- [Set up](#set-up)
- [1 - Import data](#1---import-data)
- [2 - Verticalize plates](#2---verticalize-plates)
- [3 - tidy table](#3---tidy-table)
- [4 - Add plate metadata](#4---add-plate-metadata)
- [5 - Export](#5---export)

To Do

- Add PNR data as well (Cloé)

# Set up

``` r
rm(list = ls())

# once in a while, redownload
#pak::pak("mdetoeuf/plate2N”) # if doesn't work --> try pak::pak_cleanup()
library(plate2N)
library(tidyverse)
```

# 1 - Import data

All object names ending with “abs” contain absorbance data, all object
names ending with “map” contain the mapping of the equivalent abs data
(sample id, or one of three options: “empty”, “Std” or “extr” for empty
wells, standard curve and extractant (K2SO4) respectively)

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

# TDN (map csv not in the right format --> back to import_csv = manual import)
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
> The previous chunk (absorbance and plate map data) demonstrates the
> usage of the Skanit format version of the import, but the rest of this
> pipeline does not require the Skanit data, so it is no longer used
> passed this import step. However, the output format should, in theory,
> be strictly identical to other format options, so that the object
> `Skanit_abs` should, in theory, be interchangeable with `TDN_abs` or
> `Nmin_abs`. Please reach out if you encounter any difficulty.

For PMN, import will be similar, but we generate the plate map from a
list of samples

``` r
sample_list <- read_csv("raw_data/PMN/PMN_sample_list.csv") |> 
  select(plate_id, short)
```

    Rows: 540 Columns: 9
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (8): plate_id, Expe, Soil, incub_time, sampling_time, rep_tech, 1st_Well...
    dbl (1): Biol_unit_Nb

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
samples <- sample_list$short 
plate_ids <- sample_list$plate_id |> unique()
n_samples_per_plate <- length(samples) / length(plate_ids)

tibble_map_pmn <- map_plates(
  samples, 
  n_samples_per_plate, 
  plate_ids,column_curves = c(1,12), column_blank = 8)

#check it out
tibble_map_pmn
```

    # A tibble: 270 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NO3_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     3 B     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     4 C     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     5 D     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     6 E     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     7 F     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     8 G     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     9 H     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
    10 NO3_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 260 more rows

``` r
tibble_map_pmn |> write_csv("../raw_data/PMN_map_fromR_to_check.csv")
```

After a manual check that all plate maps are correct (it was, but if
not, it is easy to move one or two cells in excel), we saved the file
under a different name for re-import. It seems that the headers were not
identified as such, so we filter out the first row.

``` r
PMN_map <- csv_to_tibble("raw_data/PMN/PMN_map_manually_checked.csv") |> 
  filter_out(row == "row")
PMN_abs <- txt_to_tibble("raw_data/PMN")
```

It appears that the plates are not in the same order between map and
abs, but this should not be a problem. Check it out anyway:

``` r
PMN_map ; PMN_abs
```

    # A tibble: 270 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NO3_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     3 B     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     4 C     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     5 D     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  Fiel… Fiel… Fiel… Std  
     6 E     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     7 F     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     8 G     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
     9 H     Std   Fiel… Fiel… Fiel… Fiel… Fiel… Fiel… extr  empty empty empty Std  
    10 NO3_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 260 more rows

    # A tibble: 270 × 13
       row   X1    X2    X3    X4    X5    X6    X7    X8    X9    X10   X11   X12  
       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
     1 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
     2 A     0.039 0.041 0.040 0.040 0.040 0.041 0.041 0.041 0.041 0.041 0.042 0.038
     3 B     0.043 0.040 0.040 0.040 0.039 0.041 0.040 0.042 0.041 0.042 0.042 0.043
     4 C     0.047 0.040 0.040 0.040 0.039 0.041 0.040 0.041 0.046 0.041 0.042 0.049
     5 D     0.060 0.040 0.040 0.040 0.039 0.040 0.039 0.040 0.043 0.041 0.041 0.056
     6 E     0.066 0.043 0.042 0.043 0.042 0.041 0.041 0.040 0.042 0.039 0.038 0.066
     7 F     0.075 0.042 0.042 0.043 0.042 0.041 0.041 0.041 0.042 0.039 0.041 0.075
     8 G     0.116 0.042 0.042 0.043 0.041 0.041 0.041 0.041 0.042 0.039 0.039 0.104
     9 H     0.139 0.043 0.044 0.042 0.041 0.042 0.041 0.041 0.042 0.039 0.039 0.131
    10 NH4_… 1     2     3     4     5     6     7     8     9     10    11    12   
    # ℹ 260 more rows

# 2 - Verticalize plates

Here, we transform data from plate format to “verticalized” format: 1
row per well position (well_id –\> 96 rows) and 1 column per “plate”
(actually 2 columns per physical plate: absorbance data plate and
mapping plate).

We do this separately for each data set. Because those distinct data
sets will be combined eventually, a prefix is added to column names to
reflect dataset + whether it is abs or map. Notice the “-” at the end of
the dataset prefix.

``` r
# Nmint1t2
Nmin_t1t2_vertical <- join_abs_map(
  Nmin_t1t2_abs, Nmin_t1t2_map, dataset = "Nmint1t2-")

# Nmint3
Nmin_t3_vertical <- join_abs_map(Nmin_t3_abs, Nmin_t3_map, dataset = "Nmint3-")

# TDN
TDN_vertical <- join_abs_map(TDN_abs, TDN_map, dataset = "TDN-")

# PMN
PMN_vertical <- join_abs_map(PMN_abs, PMN_map, dataset = "PMN-")

## Check them out
Nmin_t1t2_vertical; Nmin_t3_vertical; TDN_vertical; PMN_vertical
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

    # A tibble: 96 × 62
       row   column `PMN-abs-NH4_PC1` `PMN-abs-NH4_PC2` `PMN-abs-NH4_PF1`
       <chr> <chr>  <chr>             <chr>             <chr>            
     1 A     1      0.039             0.039             0.039            
     2 A     2      0.041             0.040             0.043            
     3 A     3      0.040             0.044             0.043            
     4 A     4      0.040             0.044             0.043            
     5 A     5      0.040             0.041             0.043            
     6 A     6      0.041             0.044             0.042            
     7 A     7      0.041             0.044             0.044            
     8 A     8      0.041             0.041             0.041            
     9 A     9      0.041             0.043             0.043            
    10 A     10     0.041             0.044             0.043            
    # ℹ 86 more rows
    # ℹ 57 more variables: `PMN-abs-NH4_PF2` <chr>, `PMN-abs-NH4_PF3` <chr>,
    #   `PMN-abs-NH4_PF4` <chr>, `PMN-abs-NH4_PP1` <chr>, `PMN-abs-NH4_PP2` <chr>,
    #   `PMN-abs-NH4_PP3` <chr>, `PMN-abs-NH4_PP4` <chr>, `PMN-abs-NO2_PC1` <chr>,
    #   `PMN-abs-NO2_PC2` <chr>, `PMN-abs-NO2_PF1` <chr>, `PMN-abs-NO2_PF2` <chr>,
    #   `PMN-abs-NO2_PF3` <chr>, `PMN-abs-NO2_PF4` <chr>, `PMN-abs-NO2_PP1` <chr>,
    #   `PMN-abs-NO2_PP2` <chr>, `PMN-abs-NO2_PP3` <chr>, …

Then, we use the `dplyr::left_join()` function to join all plate data in
a single data table. Note the distinct column names with their dataset
and abs/map-related prefixes

``` r
# join all 3 in a single table with all "plate data"
all_vertical <- Nmin_t1t2_vertical |> 
  left_join(Nmin_t3_vertical, by = join_by(row, column)) |> 
  left_join(TDN_vertical, by = join_by(row, column)) |> 
  left_join(PMN_vertical, by = join_by(row, column)) 

# look at the structure of variable names
sample(names(all_vertical),size = 30)
```

     [1] "Nmint1t2-abs-NO2_2F4_2" "Nmint1t2-map-NO2_2F6_2" "Nmint1t2-map-NH4_2P6_3"
     [4] "Nmint3-map-NO2_R4R5_2"  "PMN-map-NO2_PF3"        "Nmint1t2-abs-NH4_2P6_1"
     [7] "Nmint1t2-abs-NO2_2P6_3" "Nmint1t2-map-NO3_2F2_1" "Nmint1t2-abs-NO3_1G4"  
    [10] "PMN-abs-NH4_PC2"        "TDN-map-NO3_TDN_18"     "TDN-map-NO3_TDN_11"    
    [13] "Nmint1t2-map-NO3_2P6_2" "Nmint3-abs-NH4_R4R5_1"  "Nmint3-map-NH4_R5R6_1" 
    [16] "PMN-map-NH4_PP2"        "Nmint3-abs-NH4_R2R3_2"  "TDN-map-NO3_TDN_07"    
    [19] "Nmint3-abs-NH4_R6R7_1"  "Nmint1t2-map-NO2_2P1"   "PMN-abs-NH4_PP3"       
    [22] "TDN-map-NO2_TDN_01"     "Nmint1t2-abs-NO3_2F6_2" "Nmint3-map-NO2_R7R8_2" 
    [25] "Nmint1t2-map-NO2_1G1"   "TDN-abs-NO3_TDN_35"     "Nmint1t2-map-NH4_2P5"  
    [28] "Nmint1t2-abs-NH4_2P6_2" "Nmint1t2-map-NO3_2F6_2" "TDN-map-NO2_TDN_14"    

``` r
# check it out  
all_vertical
```

    # A tibble: 96 × 450
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
    # ℹ 446 more variables: `Nmint1t2-abs-NH4_1F2_2` <chr>,
    #   `Nmint1t2-abs-NH4_1F3` <chr>, `Nmint1t2-abs-NH4_1F4` <chr>,
    #   `Nmint1t2-abs-NH4_1F5` <chr>, `Nmint1t2-abs-NH4_1G1` <chr>,
    #   `Nmint1t2-abs-NH4_1G2` <chr>, `Nmint1t2-abs-NH4_1G3` <chr>,
    #   `Nmint1t2-abs-NH4_1G4` <chr>, `Nmint1t2-abs-NH4_1G5` <chr>,
    #   `Nmint1t2-abs-NH4_2F1_1` <chr>, `Nmint1t2-abs-NH4_2F1_2` <chr>, …

# 3 - tidy table

What we want is to have one column for absorbance data (all plates),
another one for the mapping (all plates), i.e., a much longer table
(number of rows = 96 \* nb of plates).

`verticalize_to_tidy()` brings the verticalized plates into a tidy
format, extracting from column names dataset, plate_id , mapping data
and absorbance data into their own column, thus creating a very long
tidy table where each row corresponds to a unique well reported in a new
column called `unique_well_id`.

``` r
(all_raw_abs_tidy <- vertical_to_tidy(all_vertical))
```

    # A tibble: 21,504 × 8
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
    # ℹ 21,494 more rows

# 4 - Add plate metadata

Now we have a tidy table that is, in theory, ready for export. In real
life, this data cannot be used on its own, and plate metadata is
necessary.

First, we import metadata for each dataset, and, if it is not already in
it, add a “dataset” column that will help with the next steps.

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
# check it out
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

PMN_metadata <- read_csv("raw_data/PMN/PMN_metadata.csv", show_col_types = FALSE) |> 
  mutate(dataset = "PMN")
```

Then we join them in one big metadata file. ! this only works because
the 3 metadata files have strictly the same structure (same column names
in the same order, containing the same data type (string, numeric, …)).

``` r
# join csv's
(all_plate_metadata <- bind_rows(Nmin_metadata, Nmin_t3_metadata, TDN_metadata, PMN_metadata))
```

    # A tibble: 224 × 18
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
    # ℹ 214 more rows
    # ℹ 10 more variables: std_conc <chr>, sample_dilution <chr>,
    #   extractant_column <dbl>, extractant_sp <chr>, extractant_unit <chr>,
    #   extractant_conc <dbl>, empty_column <chr>, wait_min <chr>, dataset <chr>,
    #   wavelength <chr>

Keep only relevant columns.

``` r
all_plate_metadata_keep <- all_plate_metadata |> 
  select(dataset, plate_id, std_sp, std_conc, std_unit, sample_dilution, date) 
```

We could already here, join both metadata and tidy data, but this would
create a much larger file to save on the computer than the 2 files
separately (because rows of the metadata would be repeated 96 times, one
per well of the plate.

# 5 - Export

Some steps will be specific of TDN (because we work with much higher
concentrations), so we separate the raw data according to TDN or not
TDN[^1].

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

[^1]: TDN stands for Total Dissolved Nitrogen, i.e., NO3- is dosed after
    an oxidation of all N compounds to NO3-
