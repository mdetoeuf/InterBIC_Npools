# Import and tidy raw data


- [To Do](#to-do)
- [Intro](#intro)
- [Set up](#set-up)
- [1 - Import Nmin data (t1, t2, t3)](#1---import-nmin-data-t1-t2-t3)
  - [1.1 - Plate metadata](#11---plate-metadata)
- [2 - Join absorbance and map data](#2---join-absorbance-and-map-data)
- [°°° — Below this, draft, to be picked up —
  °°°](#--below-this-draft-to-be-picked-up--)
- [3 - Import TDN data from csv (Sang
  style)](#3---import-tdn-data-from-csv-sang-style)
  - [2.1 - Import plate absorbance
    data](#21---import-plate-absorbance-data)
  - [2.2 - import plate map](#22---import-plate-map)
  - [2.3 - import plate metadat](#23---import-plate-metadat)
- [3 - import student csv (from
  xlsx)](#3---import-student-csv-from-xlsx)

# To Do

- For TDN: !! <u>**If we do the NO2**</u>, then need to deal with the
  fact that in plate NO2_TDN_18, there is another blanc for some samples
  (KCl 1M), but that is only in 4 wells…

- TDN:

  - Then add it to plate info of Nmin

- For raw “spectro-like” txt files:

  - Harmonize file names, containing plate nb and N sp

  - encode a table / tables with plate map infos

  - encode a metadata table: 1 row per plate, info

# Intro

Unfortunately, absorbance data has been saved in many different formats
(txt, csv, xlsx), and files have a diversity of content (how many plates
are in 1 file), of naming rules, and of structure (how many rows and
columns before the cell A1 appears), …

So I will have to either record raw data in a new form / rename files,
or code multiple ways of importing data. I will first try the latter

# Set up

Load packages

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.2.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor) # for row_to_names()
```


    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test

``` r
# load functions
source("functions/import_abs_txt.R")
source("functions/import_abs_csv.R")
source("functions/join_maps_abs.R")
```

# 1 - Import Nmin data (t1, t2, t3)

The Nmin absorbance data is in the “raw” format of a txt file as it
comes directly from the spectrophotometer

Here we use the import functions to get txt-based data and csv-based
data stored in variables. Because the data set is partly in txt (t1, t2,
absorbance data), and partly in csv format (t1, t2 maps + t3 absorbance
data and maps), we need to go through import steps separately.

``` r
#** !! Now turned off warnings --> but should check why it gives such a message and if can really be ignored... !! *

# set file path for data in .TXT format (t1, t2)
filepath <- "raw_data/Nmin/"
# import csv for data in csv format (t3)
csv_file <- read_csv("raw_data/Nmin_t3/Nmint3_data.csv", col_names = FALSE)

# import & shape data
# from .txt format
Nmin_t1t2_abs <- import_abs_txt(filepath)

# from .csv format
Nmin_t3_abs <- import_abs_csv(csv_file)

# look at structure of both data frames
Nmin_t1t2_abs$abs_data_df 
```

    # A tibble: 96 × 101
       row   column NH4_1F1 NH4_1F2_1 NH4_1F2_2 NH4_1F3 NH4_1F4 NH4_1F5 NH4_1G1
       <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
     1 A     1        0.039     0.039     0.039   0.038   0.039   0.039   0.039
     2 A     2        0.046     0.042     0.042   0.042   0.046   0.062   0.04 
     3 A     3        0.049     0.041     0.041   0.042   0.041   0.042   0.039
     4 A     4        0.042     0.041     0.041   0.042   0.042   0.04    0.039
     5 A     5        0.042     0.042     0.042   0.043   0.042   0.041   0.04 
     6 A     6        0.041     0.041     0.041   0.043   0.042   0.043   0.041
     7 A     7        0.042     0.041     0.041   0.044   0.041   0.043   0.04 
     8 A     8        0.039     0.038     0.038   0.039   0.039   0.039   0.039
     9 A     9        0.043     0.042     0.042   0.042   0.043   0.041   0.039
    10 A     10       0.042     0.041     0.041   0.042   0.042   0.046   0.04 
    # ℹ 86 more rows
    # ℹ 92 more variables: NH4_1G2 <dbl>, NH4_1G3 <dbl>, NH4_1G4 <dbl>,
    #   NH4_1G5 <dbl>, NH4_2F1_1 <dbl>, NH4_2F1_2 <dbl>, NH4_2F2_1 <dbl>,
    #   NH4_2F2_2 <dbl>, NH4_2F3_1 <dbl>, NH4_2F3_2 <dbl>, NH4_2F4_1 <dbl>,
    #   NH4_2F4_2 <dbl>, NH4_2F5_1 <dbl>, NH4_2F5_2 <dbl>, NH4_2F6_1 <dbl>,
    #   NH4_2F6_2 <dbl>, NH4_2P1 <dbl>, NH4_2P2 <dbl>, NH4_2P3 <dbl>,
    #   NH4_2P4 <dbl>, NH4_2P5 <dbl>, NH4_2P6_1 <dbl>, NH4_2P6_2 <dbl>, …

``` r
Nmin_t3_abs$abs_data_df
```

    # A tibble: 96 × 38
       row   column NO3_R1R2_1 NO3_R2R3_1 NO3_R4R5_1 NO3_R5R6_1 NO3_R6R7_1
       <chr> <chr>       <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
     1 A     1           0.094      0.134      0.09       0.088      0.088
     2 A     2           0.085      0.089      0.08       0.08       0.08 
     3 A     3           0.277      0.151      0.084      0.081      0.081
     4 A     4           0.152      0.147      0.146      0.209      0.17 
     5 A     5           0.186      0.182      0.134      0.148      0.173
     6 A     6           0.149      0.304      0.159      0.143      0.228
     7 A     7           0.187      0.205      0.163      0.201      0.353
     8 A     8           0.135      0.22       0.205      0.166      0.192
     9 A     9           0.136      0.227      0.162      0.356      0.154
    10 A     10          0.176      0.086      0.158      0.087      0.088
    # ℹ 86 more rows
    # ℹ 31 more variables: NO3_R7R8_1 <dbl>, NO2_R1R2_1 <dbl>, NO2_R2R3_1 <dbl>,
    #   NO2_R4R5_1 <dbl>, NO2_R5R6_1 <dbl>, NO2_R6R7_1 <dbl>, NO2_R7R8_1 <dbl>,
    #   NH4_R1R2_1 <dbl>, NH4_R2R3_1 <dbl>, NH4_R4R5_1 <dbl>, NH4_R5R6_1 <dbl>,
    #   NH4_R6R7_1 <dbl>, NH4_R7R8_1 <dbl>, NO3_R1R2_2 <dbl>, NO3_R2R3_2 <dbl>,
    #   NO3_R4R5_2 <dbl>, NO3_R5R6_2 <dbl>, NO3_R6R7_2 <dbl>, NO3_R7R8_2 <dbl>,
    #   NO2_R1R2_2 <dbl>, NO2_R2R3_2 <dbl>, NO2_R4R5_2 <dbl>, NO2_R5R6_2 <dbl>, …

Now that all data is in the same format, we can regroup data in two
general data frames: one for the absorbance data, one for the “maps”
data.

``` r
# join both data frames
Nmin_all_abs <- left_join(Nmin_t1t2_abs$abs_data_df, Nmin_t3_abs$abs_data_df)
```

    Joining with `by = join_by(row, column)`

``` r
Nmin_all_abs
```

    # A tibble: 96 × 137
       row   column NH4_1F1 NH4_1F2_1 NH4_1F2_2 NH4_1F3 NH4_1F4 NH4_1F5 NH4_1G1
       <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
     1 A     1        0.039     0.039     0.039   0.038   0.039   0.039   0.039
     2 A     2        0.046     0.042     0.042   0.042   0.046   0.062   0.04 
     3 A     3        0.049     0.041     0.041   0.042   0.041   0.042   0.039
     4 A     4        0.042     0.041     0.041   0.042   0.042   0.04    0.039
     5 A     5        0.042     0.042     0.042   0.043   0.042   0.041   0.04 
     6 A     6        0.041     0.041     0.041   0.043   0.042   0.043   0.041
     7 A     7        0.042     0.041     0.041   0.044   0.041   0.043   0.04 
     8 A     8        0.039     0.038     0.038   0.039   0.039   0.039   0.039
     9 A     9        0.043     0.042     0.042   0.042   0.043   0.041   0.039
    10 A     10       0.042     0.041     0.041   0.042   0.042   0.046   0.04 
    # ℹ 86 more rows
    # ℹ 128 more variables: NH4_1G2 <dbl>, NH4_1G3 <dbl>, NH4_1G4 <dbl>,
    #   NH4_1G5 <dbl>, NH4_2F1_1 <dbl>, NH4_2F1_2 <dbl>, NH4_2F2_1 <dbl>,
    #   NH4_2F2_2 <dbl>, NH4_2F3_1 <dbl>, NH4_2F3_2 <dbl>, NH4_2F4_1 <dbl>,
    #   NH4_2F4_2 <dbl>, NH4_2F5_1 <dbl>, NH4_2F5_2 <dbl>, NH4_2F6_1 <dbl>,
    #   NH4_2F6_2 <dbl>, NH4_2P1 <dbl>, NH4_2P2 <dbl>, NH4_2P3 <dbl>,
    #   NH4_2P4 <dbl>, NH4_2P5 <dbl>, NH4_2P6_1 <dbl>, NH4_2P6_2 <dbl>, …

``` r
# look at plate maps
# for Nmin t3
maps_t3_file <- read_csv("raw_data/Nmin_t3/Nmint3_maps.csv", col_names = FALSE)
```

    Rows: 324 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (13): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Nmin_t3_maps <- import_abs_csv(maps_t3_file)
Nmin_t3_maps$abs_data_df 
```

    # A tibble: 96 × 38
       row   column NO3_R1R2_1 NO3_R2R3_1 NO3_R4R5_1 NO3_R5R6_1 NO3_R6R7_1
       <chr> <chr>  <chr>      <chr>      <chr>      <chr>      <chr>     
     1 A     1      Std        Std        Std        Std        Std       
     2 A     2      extr_1     empty      extr_4     extr_5     extr_6    
     3 A     3      83_t3_z3   93_t3_z3   empty      empty      empty     
     4 A     4      87_t3_z1   103_t3_z3  86_t3_z2   94_t3_z2   96_t3_z3  
     5 A     5      88_t3_z1   104_t3_z1  91_t3_z2   95_t3_z1   99_t3_z1  
     6 A     6      90_t3_z3   std_R2_t3  92_t3_z1   95_t3_z2   101_t3_z2 
     7 A     7      91_t3_z3   empty      93_t3_z1   96_t3_z2   std_R6_t3 
     8 A     8      92_t3_z2   empty      94_t3_z3   98_t3_z3   empty     
     9 A     9      95_t3_z3   empty      98_t3_z1   std_R5_t3  empty     
    10 A     10     97_t3_z1   empty      102_t3_z2  Std        Std       
    # ℹ 86 more rows
    # ℹ 31 more variables: NO3_R7R8_1 <chr>, NO2_R1R2_1 <chr>, NO2_R2R3_1 <chr>,
    #   NO2_R4R5_1 <chr>, NO2_R5R6_1 <chr>, NO2_R6R7_1 <chr>, NO2_R7R8_1 <chr>,
    #   NH4_R1R2_1 <chr>, NH4_R2R3_1 <chr>, NH4_R4R5_1 <chr>, NH4_R5R6_1 <chr>,
    #   NH4_R6R7_1 <chr>, NH4_R7R8_1 <chr>, NO3_R1R2_2 <chr>, NO3_R2R3_2 <chr>,
    #   NO3_R4R5_2 <chr>, NO3_R5R6_2 <chr>, NO3_R6R7_2 <chr>, NO3_R7R8_2 <chr>,
    #   NO2_R1R2_2 <chr>, NO2_R2R3_2 <chr>, NO2_R4R5_2 <chr>, NO2_R5R6_2 <chr>, …

``` r
# For Nmin t1 and t2
maps_t1t1_file <- read_csv("raw_data/Nmin/Nmin_maps.csv", col_names = FALSE)
```

    Rows: 891 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (13): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Nmin_t1t1_maps <- import_abs_csv(maps_t1t1_file)
Nmin_t1t1_maps$abs_data_df
```

    # A tibble: 96 × 101
       row   column NH4_1F1  NH4_1F2_1 NH4_1F2_2 NH4_1F3  NH4_1F4  NH4_1F5   NH4_1G1
       <chr> <chr>  <chr>    <chr>     <chr>     <chr>    <chr>    <chr>     <chr>  
     1 A     1      Std      Std       Std       Std      Std      Std       Std    
     2 A     2      81_t1_z2 97_t1_z1  empty     89_t1_z3 81_t1_z1 Std_3_t1  1_t1   
     3 A     3      82_t1_z2 98_t1_z1  empty     90_t1_z3 82_t1_z3 98_t1_z3  2_t1   
     4 A     4      83_t1_z2 99_t1_z1  empty     91_t1_z1 83_t1_z3 99_t1_z3… 3_t1   
     5 A     5      84_t1_z1 100_t1_z1 empty     92_t1_z2 84_t1_z2 100_t1_z2 4_t1   
     6 A     6      85_t1_z1 101_t1_z3 empty     93_t1_z2 85_t1_z2 101_t1_z2 5_t1   
     7 A     7      86_t1_z3 102_t1_z3 empty     94_t1_z3 86_t1_z1 102_t1_z1 6_t1   
     8 A     8      extr     extr      empty     extr     extr     extr      extr   
     9 A     9      87_t1_z3 103_t1_z1 empty     95_t1_z2 87_t1_z1 103_t1_z3 empty  
    10 A     10     88_t1_z3 104_t1_z1 empty     96_t1_z3 88_t1_z1 104_t1_z3 8_t1   
    # ℹ 86 more rows
    # ℹ 92 more variables: NH4_1G2 <chr>, NH4_1G3 <chr>, NH4_1G4 <chr>,
    #   NH4_1G5 <chr>, NO2_1F1 <chr>, NO2_1F2_1 <chr>, NO2_1F2_2 <chr>,
    #   NO2_1F3 <chr>, NO2_1F4 <chr>, NO2_1F5 <chr>, NO2_1G1 <chr>, NO2_1G2 <chr>,
    #   NO2_1G3 <chr>, NO2_1G4 <chr>, NO2_1G5 <chr>, NO3_1F1 <chr>,
    #   NO3_1F2_1 <chr>, NO3_1F2_2 <chr>, NO3_1F3 <chr>, NO3_1F4 <chr>,
    #   NO3_1F5 <chr>, NO3_1G1 <chr>, NO3_1G2 <chr>, NO3_1G3 <chr>, …

``` r
# join both data frames
Nmin_all_maps <- left_join(Nmin_t1t1_maps$abs_data_df, Nmin_t3_maps$abs_data_df)
```

    Joining with `by = join_by(row, column)`

``` r
Nmin_all_maps
```

    # A tibble: 96 × 137
       row   column NH4_1F1  NH4_1F2_1 NH4_1F2_2 NH4_1F3  NH4_1F4  NH4_1F5   NH4_1G1
       <chr> <chr>  <chr>    <chr>     <chr>     <chr>    <chr>    <chr>     <chr>  
     1 A     1      Std      Std       Std       Std      Std      Std       Std    
     2 A     2      81_t1_z2 97_t1_z1  empty     89_t1_z3 81_t1_z1 Std_3_t1  1_t1   
     3 A     3      82_t1_z2 98_t1_z1  empty     90_t1_z3 82_t1_z3 98_t1_z3  2_t1   
     4 A     4      83_t1_z2 99_t1_z1  empty     91_t1_z1 83_t1_z3 99_t1_z3… 3_t1   
     5 A     5      84_t1_z1 100_t1_z1 empty     92_t1_z2 84_t1_z2 100_t1_z2 4_t1   
     6 A     6      85_t1_z1 101_t1_z3 empty     93_t1_z2 85_t1_z2 101_t1_z2 5_t1   
     7 A     7      86_t1_z3 102_t1_z3 empty     94_t1_z3 86_t1_z1 102_t1_z1 6_t1   
     8 A     8      extr     extr      empty     extr     extr     extr      extr   
     9 A     9      87_t1_z3 103_t1_z1 empty     95_t1_z2 87_t1_z1 103_t1_z3 empty  
    10 A     10     88_t1_z3 104_t1_z1 empty     96_t1_z3 88_t1_z1 104_t1_z3 8_t1   
    # ℹ 86 more rows
    # ℹ 128 more variables: NH4_1G2 <chr>, NH4_1G3 <chr>, NH4_1G4 <chr>,
    #   NH4_1G5 <chr>, NO2_1F1 <chr>, NO2_1F2_1 <chr>, NO2_1F2_2 <chr>,
    #   NO2_1F3 <chr>, NO2_1F4 <chr>, NO2_1F5 <chr>, NO2_1G1 <chr>, NO2_1G2 <chr>,
    #   NO2_1G3 <chr>, NO2_1G4 <chr>, NO2_1G5 <chr>, NO3_1F1 <chr>,
    #   NO3_1F2_1 <chr>, NO3_1F2_2 <chr>, NO3_1F3 <chr>, NO3_1F4 <chr>,
    #   NO3_1F5 <chr>, NO3_1G1 <chr>, NO3_1G2 <chr>, NO3_1G3 <chr>, …

So now we have those 2 data frames that have strictly the same
structure, with 96 rows and 137 columns, with 2 columns attributed to
the well identifier (“row” and “column”), and the remaining 135 columns
representing the 96-well plates.

## 1.1 - Plate metadata

This section need to be improved, based on files to import. (now it is
manual encoding…)

``` r
# Just as a failsafe, here is everything that we wanted to collect ...

std_column <- c("1", "12")
std_id <- c("no3", "mgN_per_L", "H2O") # n species, unit, prepared in
std_conc <- c(0,1,2,4,8,16, 24, 32) # we need numerics
extractant_column <- "7"
extractant_id <- c("K2SO4", "M") # extractant, unit
extractant_conc <- 0.5 # we need numeric
timestamp <- timestamp() # see if the format of this is important
```

    ##------ Wed Mar 25 15:05:04 2026 ------##

``` r
#wavelength <- "540 nm" # this is just to store info and will not be used for calculations, can be any format 
delay_min <- 30 # 
```

# 2 - Join absorbance and map data

First we need to use `pivot_longer()` to get absorbance data / plate map
data in a single column, before we can join the data in a single data
frame. The target structure for the data frame would be to have the
following columns: “row” (from the plate), “column” (from the plate),
“well_id” (= concatenation of row and column), plate_map (sample name or
extractant or std curve), absorbance.

The steps for this are recorded into a function `join_maps_abs()`, so we
can easily repeat it

``` r
Nmin_data <- join_maps_abs(maps_df = Nmin_all_maps, abs_df = Nmin_all_abs)
Nmin_data
```

    # A tibble: 12,960 × 8
       row   column well_id unique_well_id N_sp  plate_id plate_map absorbance
       <chr>  <dbl> <chr>   <chr>          <chr> <chr>    <chr>          <dbl>
     1 A          1 A1      NH4_1F1_A1     NH4   NH4_1F1  Std            0.039
     2 B          1 B1      NH4_1F1_B1     NH4   NH4_1F1  Std            0.043
     3 C          1 C1      NH4_1F1_C1     NH4   NH4_1F1  Std            0.047
     4 D          1 D1      NH4_1F1_D1     NH4   NH4_1F1  Std            0.053
     5 E          1 E1      NH4_1F1_E1     NH4   NH4_1F1  Std            0.059
     6 F          1 F1      NH4_1F1_F1     NH4   NH4_1F1  Std            0.067
     7 G          1 G1      NH4_1F1_G1     NH4   NH4_1F1  Std            0.096
     8 H          1 H1      NH4_1F1_H1     NH4   NH4_1F1  Std            0.126
     9 A          2 A2      NH4_1F1_A2     NH4   NH4_1F1  81_t1_z2       0.046
    10 B          2 B2      NH4_1F1_B2     NH4   NH4_1F1  81_t1_z2       0.046
    # ℹ 12,950 more rows

Now that we have our final raw data frame, we can export it to save it
as a document to use for downstream analysis

``` r
write_rds(Nmin_data, file = "output/data/Nmin_tidy.rds")
```

# °°° — Below this, draft, to be picked up — °°°

# 3 - Import TDN data from csv (Sang style)

Actually the original file is an xlsx file. But relevant sheets are
saved as csv for easier manipulation.

Everything comes from raw data csv that originates from an xlsx file :
/Users/Admin/Nextcloud/PhD/2024_trial/Fab_Mo/1_Data/TDN_data.xlsx

## 2.1 - Import plate absorbance data

Now, working for first plate

``` r
tdn_abs_raw <- read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE)
```

    Rows: 522 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (1): X1
    dbl (12): X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
i = 1
plate_id <- tdn_abs_raw$X1[i]

plate_abs <- tdn_abs_raw[i:(i+8),] |> 
  row_to_names(row_number = 1) |> 
  rename(row = 1)
```

## 2.2 - import plate map

From the sample list we can create plate maps. The next chunk does it
for one plate. —\> make it a loop or a function or something…

## 2.3 - import plate metadat

this is strictly speaking the import. Then we can set for each row i the
individual parameters

# 3 - import student csv (from xlsx)
