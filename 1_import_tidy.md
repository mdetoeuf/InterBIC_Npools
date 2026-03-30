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
  - [2.3 - import plate metadata](#23---import-plate-metadata)
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

So there are here 2 main ways to import absorbance data & 3 types of
data:

1.  Absorbance data stricto sensu = quantitative values for absorbance,
    data coming from plate reader

    - either as .TXT (see
      <a href="#fig-txt-screenshot" class="quarto-xref">Figure 1</a>)

    - or as .csv (see example in
      <a href="#fig-csv-screenshot" class="quarto-xref">Figure 2</a>)

2.  Plate map data, i.e., the map of well attribution to samples,
    standard curve and extractant (or blanc). Typically, this can have
    the same format as the .csv absorbance data (see
    <a href="#fig-csv-map-screenshot" class="quarto-xref">Figure 3</a>)

3.  Plate metadata, i.e., additional information that gives information
    that applies to the whole 96-well plate. Example: plate-id, N
    species that is dosed, wavelength, date or batch, concentration of
    the standard curve, nature and concentration of the extractant, etc.
    (see
    <a href="#fig-metadata-screenshot" class="quarto-xref">Figure 4</a>)

<div id="fig-txt-screenshot">

![](images/clipboard-120855305.png)

Figure 1: Typical structure of raw absorbance data as .TXT file as it is
given by the spectrophotometer

</div>

<div id="fig-csv-screenshot">

<img src="images/clipboard-2122399435.png" width="440" />

Figure 2: One example of structure of a .csv file containing absorbance
data. This is the basic structure that easily fits into this pipeline.
But other formats can often easily be adapted into this format as well.
Important in this set up is that plates are displayed on top of each
other (empty rows are easy to correct if needed), and that the plate_id
is in the top-left cell of each plate. It is also useful that the
plate_id start with the N species under study

</div>

<div id="fig-csv-map-screenshot">

<img src="images/clipboard-1326132073.png" width="538" />

Figure 3: One example of plate map data in the same .csv format as the
corresponding absorbance data

</div>

<div id="fig-metadata-screenshot">

![](images/clipboard-3285939582.png)

Figure 4: Example of plate metadata. There could be more or less
columns, but consistency within one import episode is important as
several data frames will be appended, which will only work if they have
the same structure. Vital columns are plate_id, std_sp, std_unit,
std_conc (with concentrations separated by a “-” and the digit separator
a “.”

</div>

# Set up

Load packages

<details class="code-fold">
<summary>Code</summary>

``` r
library(tidyverse)
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
library(janitor) # for row_to_names()
```

</details>


    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test

<details class="code-fold">
<summary>Code</summary>

``` r
# load functions
source("functions/import_abs_txt.R")
source("functions/import_abs_csv.R")
source("functions/join_maps_abs.R")
```

</details>

# 1 - Import Nmin data (t1, t2, t3)

The Nmin absorbance data is in the “raw” format of a txt file as it
comes directly from the spectrophotometer

Here we use the import functions to get txt-based data and csv-based
data stored in variables. Because the data set is partly in txt (t1, t2,
absorbance data), and partly in csv format (t1, t2 maps + t3 absorbance
data and maps), we need to go through import steps separately.

<details class="code-fold">
<summary>Code</summary>

``` r
#** !! Now turned off warnings --> but should check why it gives such a message and if can really be ignored... !! *

# set file path for data in .TXT format (t1, t2)
filepath <- "raw_data/Nmin/"

# import csv for data in csv format (t3)
csv_file <- read_csv("raw_data/Nmin_t3/Nmint3_data.csv", col_names = FALSE)
csv_file_TDN <- read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE)

# import & shape data
# from .txt format
Nmin_t1t2_abs <- import_abs_txt(filepath)

# from .csv format
Nmin_t3_abs <- import_abs_csv(csv_file)
Nmin_TDN <- import_abs_csv(csv_file_TDN)


# look at structure of data frames
Nmin_t1t2_abs$abs_data_df 
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_t3_abs$abs_data_df
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_TDN$abs_data_df
```

</details>

    # A tibble: 96 × 60
       row   column NO3_TDN_01 NO3_TDN_02 NO3_TDN_03 NO3_TDN_04 NO3_TDN_05
       <chr> <chr>       <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
     1 A     1           0.095      0.097      0.113      0.114      0.132
     2 A     2           0.537      0.552      0.53       0.535      0.564
     3 A     3           0.528      0.559      0.528      0.534      0.55 
     4 A     4           0.562      0.555      0.521      0.531      0.546
     5 A     5           0.51       0.538      0.521      0.506      0.551
     6 A     6           0.507      0.552      0.54       0.551      0.555
     7 A     7           0.546      0.534      0.551      0.528      0.542
     8 A     8           0.078      0.079      0.097      0.102      0.105
     9 A     9           0.519      0.589      0.559      0.541      0.557
    10 A     10          0.543      0.535      0.528      0.544      0.563
    # ℹ 86 more rows
    # ℹ 53 more variables: NO3_TDN_06 <dbl>, NO3_TDN_07 <dbl>, NO3_TDN_08 <dbl>,
    #   NO3_TDN_09 <dbl>, NO3_TDN_10 <dbl>, NO3_TDN_11 <dbl>, NO3_TDN_12 <dbl>,
    #   NO3_TDN_13 <dbl>, NO3_TDN_14 <dbl>, NO3_TDN_15 <dbl>, NO3_TDN_16 <dbl>,
    #   NO3_TDN_17 <dbl>, NO3_TDN_18 <dbl>, NO3_TDN_19 <dbl>, NO3_TDN_20 <dbl>,
    #   NO3_TDN_21 <dbl>, NO3_TDN_22 <dbl>, NO3_TDN_23 <dbl>, NO3_TDN_24 <dbl>,
    #   NO3_TDN_25 <dbl>, NO3_TDN_26 <dbl>, NO3_TDN_27 <dbl>, NO3_TDN_28 <dbl>, …

Now that all data is in the same format, we can regroup data in two
general data frames: one for the absorbance data, one for the “maps”
data.

<details class="code-fold">
<summary>Code</summary>

``` r
# join data frames --> this only works with 2 datavframes, unless you nested --> next bit for more elegant way
Nmin_all_abs <- left_join(
  Nmin_t1t2_abs$abs_data_df, 
  Nmin_t3_abs$abs_data_df)
```

</details>

    Joining with `by = join_by(row, column)`

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_all_abs
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
list_data <- list(Nmin_t1t2_abs$abs_data_df, 
  Nmin_t3_abs$abs_data_df, 
  Nmin_TDN$abs_data_df)

Nmin_all_abs <- plyr::join_all(list_data) |> as_tibble()
```

</details>

    Joining by: row, column
    Joining by: row, column

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_all_abs 
```

</details>

    # A tibble: 96 × 195
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
    # ℹ 186 more variables: NH4_1G2 <dbl>, NH4_1G3 <dbl>, NH4_1G4 <dbl>,
    #   NH4_1G5 <dbl>, NH4_2F1_1 <dbl>, NH4_2F1_2 <dbl>, NH4_2F2_1 <dbl>,
    #   NH4_2F2_2 <dbl>, NH4_2F3_1 <dbl>, NH4_2F3_2 <dbl>, NH4_2F4_1 <dbl>,
    #   NH4_2F4_2 <dbl>, NH4_2F5_1 <dbl>, NH4_2F5_2 <dbl>, NH4_2F6_1 <dbl>,
    #   NH4_2F6_2 <dbl>, NH4_2P1 <dbl>, NH4_2P2 <dbl>, NH4_2P3 <dbl>,
    #   NH4_2P4 <dbl>, NH4_2P5 <dbl>, NH4_2P6_1 <dbl>, NH4_2P6_2 <dbl>, …

<details class="code-fold">
<summary>Code</summary>

``` r
# look at plate maps
# for Nmin t3
maps_t3_file <- read_csv("raw_data/Nmin_t3/Nmint3_maps.csv", col_names = FALSE)
```

</details>

    Rows: 324 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (13): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_t3_maps <- import_abs_csv(maps_t3_file)
Nmin_t3_maps$abs_data_df 
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
# For Nmin t1 and t2
maps_t1t1_file <- read_csv("raw_data/Nmin/Nmin_maps.csv", col_names = FALSE) 
```

</details>

    Rows: 891 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (13): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_t1t1_maps <- import_abs_csv(maps_t1t1_file)
Nmin_t1t1_maps$abs_data_df
```

</details>

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

<details class="code-fold">
<summary>Code</summary>

``` r
# For TDN
maps_tdn_file <- 
  read_csv("raw_data/TDN/TDN_maps.csv", col_names = FALSE,col_select = X14:X26) |> 
  na.omit() |> 
  rename(X1 = X14)
```

</details>

    Rows: 1123 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (13): X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
TDN_maps <- import_abs_csv(maps_tdn_file)

# join both data frames
Nmin_all_maps <- left_join(Nmin_t1t1_maps$abs_data_df, Nmin_t3_maps$abs_data_df)
```

</details>

    Joining with `by = join_by(row, column)`

<details class="code-fold">
<summary>Code</summary>

``` r
# same for more than 2:
list_maps <- list(
  Nmin_t1t1_maps$abs_data_df,
  Nmin_t3_maps$abs_data_df,
  TDN_maps$abs_data_df
)

Nmin_all_maps <- plyr::join_all(list_maps) |> as_tibble()
```

</details>

    Joining by: row, column
    Joining by: row, column

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_all_maps
```

</details>

    # A tibble: 96 × 194
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
    # ℹ 185 more variables: NH4_1G2 <chr>, NH4_1G3 <chr>, NH4_1G4 <chr>,
    #   NH4_1G5 <chr>, NO2_1F1 <chr>, NO2_1F2_1 <chr>, NO2_1F2_2 <chr>,
    #   NO2_1F3 <chr>, NO2_1F4 <chr>, NO2_1F5 <chr>, NO2_1G1 <chr>, NO2_1G2 <chr>,
    #   NO2_1G3 <chr>, NO2_1G4 <chr>, NO2_1G5 <chr>, NO3_1F1 <chr>,
    #   NO3_1F2_1 <chr>, NO3_1F2_2 <chr>, NO3_1F3 <chr>, NO3_1F4 <chr>,
    #   NO3_1F5 <chr>, NO3_1G1 <chr>, NO3_1G2 <chr>, NO3_1G3 <chr>, …

So now we have those 2 data frames that have strictly the same
structure, with 96 rows and 194 columns, with 2 columns attributed to
the well identifier (“row” and “column”), and the remaining 192 columns
representing the 96-well plates.

## 1.1 - Plate metadata

The file containing plate metadata can contain as much information on
plates as the user wants.

Absolutely necessary columns are:

- plate_id

  - ideally one row per plate id, corrections to the code may be needed
    otherwise

- std_unit

- std_conc

  - this contains a succession of all concentrations of the standard
    curve, expressed in the unit above

  - one cell is used for all concentrations, ideally in ascending order
    (or tweak the code)

  - concentrations should be separated by “-” and digits expressed with
    “.”, not with “,”

In the proposed code, N species can be deducted from plate names. Should
that not be the case, then an additional column would be a good place to
store that information:

- N_sp

Additional information can be encoded manually.

**!! I prefered not relying on manual encoding of concentrations for the
standard curve because we tend to run big chunks of code without paying
attention that modifications are needed. In that sense, such a critical
information coming directly from imported files, so that the code bugs
when the information is not imported, feels like a good failsafe. As
much as possible, I tried to only have moments of manual encoding when
either the decision needs to be interactive (e.g., based on a plot) or
when valid default values can be relied upon without major issues. !!!**

<details class="code-fold">
<summary>Code</summary>

``` r
# import csv
Nmin_t1t2_metadata <- read_csv("raw_data/Nmin/Nmin_metadata.csv")
```

</details>

    Rows: 99 Columns: 16
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (11): plate_id, sampling_time, std_column, std_sp, std_unit, std_prep, s...
    dbl  (1): extractant_conc
    lgl  (4): date, time, extractant_column, empty_column

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_t3_metadata <- read_csv("raw_data/Nmin_t3/Nmint3_metadata.csv")
```

</details>

    Rows: 36 Columns: 16
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (10): plate_id, sampling_time, std_sp, std_unit, std_prep, std_conc, sam...
    dbl  (1): extractant_conc
    lgl  (5): date, time, std_column, extractant_column, empty_column

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
TDN_metadata <- read_csv("raw_data/TDN/TDN_metadata.csv") |> mutate(std_column = as.character(std_column))
```

</details>

    Rows: 58 Columns: 17
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (12): plate_id, date, std_sp, std_unit, std_prep, std_conc, sample_dilut...
    dbl  (3): std_column, extractant_column, extractant_conc
    lgl  (2): time, sampling_time

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
# join csv's
Nmin_all_metadata <- bind_rows(Nmin_t1t2_metadata, Nmin_t3_metadata, TDN_metadata)

# export it
Nmin_all_metadata |> write_rds("output/data/Nmin_metadata.rds")
Nmin_all_metadata
```

</details>

    # A tibble: 193 × 17
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
    # ℹ 183 more rows
    # ℹ 9 more variables: std_conc <chr>, sample_dilution <chr>,
    #   extractant_column <dbl>, extractant_sp <chr>, extractant_unit <chr>,
    #   extractant_conc <dbl>, empty_column <chr>, wait_min <chr>, wavelength <chr>

# 2 - Join absorbance and map data

First we need to use `pivot_longer()` to get absorbance data / plate map
data in a single column, before we can join the data in a single data
frame. The target structure for the data frame would be to have the
following columns: “row” (from the plate), “column” (from the plate),
“well_id” (= concatenation of row and column), plate_map (sample name or
extractant or std curve), absorbance.

The steps for this are recorded into a function `join_maps_abs()`, so we
can easily repeat it

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_data <- join_maps_abs(maps_df = Nmin_all_maps, abs_df = Nmin_all_abs)
Nmin_data |> head()
```

</details>

    # A tibble: 6 × 8
      row   column well_id unique_well_id N_sp  plate_id plate_map absorbance
      <chr>  <dbl> <chr>   <chr>          <chr> <chr>    <chr>          <dbl>
    1 A          1 A1      NH4_1F1_A1     NH4   NH4_1F1  Std            0.039
    2 B          1 B1      NH4_1F1_B1     NH4   NH4_1F1  Std            0.043
    3 C          1 C1      NH4_1F1_C1     NH4   NH4_1F1  Std            0.047
    4 D          1 D1      NH4_1F1_D1     NH4   NH4_1F1  Std            0.053
    5 E          1 E1      NH4_1F1_E1     NH4   NH4_1F1  Std            0.059
    6 F          1 F1      NH4_1F1_F1     NH4   NH4_1F1  Std            0.067

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_data |> tail()
```

</details>

    # A tibble: 6 × 8
      row   column well_id unique_well_id N_sp  plate_id   plate_map absorbance
      <chr>  <dbl> <chr>   <chr>          <chr> <chr>      <chr>          <dbl>
    1 C         12 C12     NO3_TDN_38_C12 NO3   NO3_TDN_38 empty          0.069
    2 D         12 D12     NO3_TDN_38_D12 NO3   NO3_TDN_38 empty          0.069
    3 E         12 E12     NO3_TDN_38_E12 NO3   NO3_TDN_38 empty          0.069
    4 F         12 F12     NO3_TDN_38_F12 NO3   NO3_TDN_38 empty          0.068
    5 G         12 G12     NO3_TDN_38_G12 NO3   NO3_TDN_38 empty          0.068
    6 H         12 H12     NO3_TDN_38_H12 NO3   NO3_TDN_38 empty          0.104

Now that we have our final raw data frame, we can export it to save it
as a document to use for downstream analysis

<details class="code-fold">
<summary>Code</summary>

``` r
write_rds(Nmin_data, file = "output/data/Nmin_TDN_tidy.rds")
```

</details>

# °°° — Below this, draft, to be picked up — °°°

# 3 - Import TDN data from csv (Sang style)

Actually the original file is an xlsx file. But relevant sheets are
saved as csv for easier manipulation.

Everything comes from raw data csv that originates from an xlsx file :
/Users/Admin/Nextcloud/PhD/2024_trial/Fab_Mo/1_Data/TDN_data.xlsx

## 2.1 - Import plate absorbance data

Now, working for first plate

<details class="code-fold">
<summary>Code</summary>

``` r
tdn_abs_raw <- read_csv("raw_data/TDN/TDN_data.csv", col_names = FALSE)
```

</details>

    Rows: 522 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (1): X1
    dbl (12): X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<details class="code-fold">
<summary>Code</summary>

``` r
i = 1
plate_id <- tdn_abs_raw$X1[i]

plate_abs <- tdn_abs_raw[i:(i+8),] |> 
  row_to_names(row_number = 1) |> 
  rename(row = 1)
```

</details>

## 2.2 - import plate map

From the sample list we can create plate maps. The next chunk does it
for one plate. —\> make it a loop or a function or something…

## 2.3 - import plate metadata

# 3 - import student csv (from xlsx)
