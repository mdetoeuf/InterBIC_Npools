# Import and tidy raw data


- [To Do](#to-do)
- [Intro](#intro)
- [Set up](#set-up)
- [1 - Import Nmin data (t1, t2)](#1---import-nmin-data-t1-t2)
  - [1.1 - Absorbance data](#11---absorbance-data)
  - [1.3 - Plate metadata](#13---plate-metadata)
- [1 - Import spectro-like txt](#1---import-spectro-like-txt)
  - [1.2 - import plate data (plate_map +
    infos)](#12---import-plate-data-plate_map--infos)
- [2 - Import TDN data from csv (Sang
  style)](#2---import-tdn-data-from-csv-sang-style)
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

- Nmin t1 and t2: duplicate & rename files to be divided in 2

- 

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
```

# 1 - Import Nmin data (t1, t2)

The Nmin absorbance data is in the “raw” format of a txt file as it
comes directly from the spectrophotometer

## 1.1 - Absorbance data

``` r
#** !! Now turned off warnings --> but should check why it gives such a message and if can really be ignored... !! *

filepath <- "raw_data/Nmin/"
Nmin_t1t2_abs <- import_abs_txt(filepath)
```

     chr [1:99] "NH4_1F1.TXT" "NH4_1F2_1.TXT" "NH4_1F2_2.TXT" "NH4_1F3.TXT" ...

## 1.3 - Plate metadata

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

    ##------ Fri Mar 20 21:55:28 2026 ------##

``` r
#wavelength <- "540 nm" # this is just to store info and will not be used for calculations, can be any format 
delay_min <- 30 # 
```

# 1 - Import spectro-like txt

I will start with the files that are as delivered by the specrto when we
work with a USB key (so we only get absorbance data, no plate map or
plate info).

## 1.2 - import plate data (plate_map + infos)

From same txt file (from the spectro), we can extract some metadata. The
rest will have to be supplied by extra table to be imported. Also, We
could get some info from the file name, but that is not a general code,
it will depend on each pesron and experiment

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

    ##------ Fri Mar 20 21:55:28 2026 ------##

``` r
wavelength <- "540 nm" # this is just to store info and will not be used for calculations, can be any format 
delay_min <- 30 # 
```

# 2 - Import TDN data from csv (Sang style)

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
