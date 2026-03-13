# Import and tidy raw data


- [To Do](#to-do)
- [Intro](#intro)
- [Set up](#set-up)
- [1 - Import spectro-like txt](#1---import-spectro-like-txt)
  - [1.1 - import absorbance data
    (plate_abs)](#11---import-absorbance-data-plate_abs)
  - [1.2 - import plate data (plate_map +
    infos)](#12---import-plate-data-plate_map--infos)
- [2 - Import TDN data (Sang style)](#2---import-tdn-data-sang-style)

# To Do

- retrieve all raw data, figure out formats and to do

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

# 1 - Import spectro-like txt

I will start with the files that are as delivered by the specrto when we
work with a USB key (so we only get absorbance data, no plate map or
plate info).

## 1.1 - import absorbance data (plate_abs)

In the folder “raw_data”, I copied randomly such a file.

``` r
# have a look at files in the folder
all_files <- list.files("raw_data", full.names = TRUE)
all_files
```

    [1] "raw_data/NO3_PC1.TXT"

``` r
# I'll take the first one (for now there is only one, this might change...)
plate_abs <- 
  read_tsv(all_files[1], col_names = TRUE, skip = 5) |> 
  rename(row = `...1`)
```

    New names:
    Rows: 8 Columns: 13
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "\t" chr
    (1): ...1 dbl (12): 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...1`

## 1.2 - import plate data (plate_map + infos)

From same txt file (from the spectro), we can extract some metadata. The
rest will have to be supplied by extra table to be imported. Also, We
could get some info from the file name, but that is not a general code,
it will depend on each pesron and experiment

``` r
# 
meta_data <- read_tsv(all_files[1], col_names = FALSE, n_max = 3)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 3 Columns: 2
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): X1, X2

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# !! The next line extracts something that looks like a N species, but it is just the spectro protocol, with only 2 possible values: no3 (for nitrate, nitrite, TDN) or nh4 (nh4+)
protocol <- strsplit(meta_data$X2[1], split = " ")[[1]][1]
date <- strsplit(meta_data$X2[1], split = " ")[[1]][2]
time <- strsplit(meta_data$X2[1], split = " ")[[1]][3]

wavelength <- strsplit(meta_data$X1[3], split = ": ")[[1]][2]


# Just as a failsafe, here is everything that we wanted to collect ...
plate_id <- "plate_id"
std_column <- c("1", "12")
std_id <- c("no3", "mgN_per_L", "H2O") # n species, unit, prepared in
std_conc <- c(0,1,2,4,8,16, 24, 32) # we need numerics
extractant_column <- "7"
extractant_id <- c("K2SO4", "M") # extractant, unit
extractant_conc <- 0.5 # we need numeric
timestamp <- timestamp() # see if the format of this is important
```

    ##------ Sat Mar 14 00:11:28 2026 ------##

``` r
wavelength <- "540 nm" # this is just to store info and will not be used for calculations, can be any format 
delay_min <- 30 # 
```

<u>**To be thought through:**</u>

- For the rest of the plate info, including a plate map, a file will
  have to be encoded!

# 2 - Import TDN data (Sang style)

From the sample list we can create plate maps. The next chunk does it
for one plate. —\> make it a loop or a function or something…

``` r
tdn_samples <- read_csv("../raw_data/TDN_tubes.csv")
```

    New names:
    Rows: 380 Columns: 22
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (17): Priority_campaign, batch_SSE, Expe, sampling_time, bloc, Soil, CS,... dbl
    (4): TN_sample_nb, Biol_unit_ID, batch_nb, nb_in_batch lgl (1): ...22
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...19`
    • `` -> `...20`
    • `` -> `...21`
    • `` -> `...22`

``` r
std_column <- 1
extractant_column <- 8
plate_id <- tdn_samples$Plate_id_NO3[11]

# For NO3
#tubes <- 
 # (tdn_samples |> filter(Plate_id_NO3 == plate_id))$TN_sample_nb

dilution_up <- "2Xdiluted"
dilution_down <- "nodilution"

tubes_up <- paste0(
  (tdn_samples |> filter(Plate_id_NO3 == plate_id))$TN_sample_nb, 
  "_", dilution_up)

tubes_down <- paste0(
  (tdn_samples |> filter(Plate_id_NO3 == plate_id))$TN_sample_nb, 
  "_", dilution_down)

plate_map <- rbind(tubes_up, tubes_up, tubes_up, tubes_up, 
                tubes_down, tubes_down, tubes_down, tubes_down) |>
  as.matrix() |> as_tibble() |> 
  mutate(std = rep("std"), .before = std_column) |> 
  mutate(extr = rep("extractant"), .before = extractant_column) |> 
  mutate(row = LETTERS[1:8], .before = 1) #|> 
```

    Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    `.name_repair` is omitted as of tibble 2.0.0.
    ℹ Using compatibility `.name_repair`.

    Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ℹ Please use `all_of()` or `any_of()` instead.
      # Was:
      data %>% select(std_column)

      # Now:
      data %>% select(all_of(std_column))

    See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

    Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ℹ Please use `all_of()` or `any_of()` instead.
      # Was:
      data %>% select(extractant_column)

      # Now:
      data %>% select(all_of(extractant_column))

    See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

``` r
colnames(plate_map) <- c("row", as.character(c(1:12)))

plate_map
```

    # A tibble: 8 × 13
      row   `1`   `2`    `3`   `4`   `5`   `6`   `7`   `8`   `9`   `10`  `11`  `12` 
      <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    1 A     std   11_2X… 12_2… 13_2… 14_2… 15_2… 16_2… extr… 17_2… 18_2… 19_2… 20_2…
    2 B     std   11_2X… 12_2… 13_2… 14_2… 15_2… 16_2… extr… 17_2… 18_2… 19_2… 20_2…
    3 C     std   11_2X… 12_2… 13_2… 14_2… 15_2… 16_2… extr… 17_2… 18_2… 19_2… 20_2…
    4 D     std   11_2X… 12_2… 13_2… 14_2… 15_2… 16_2… extr… 17_2… 18_2… 19_2… 20_2…
    5 E     std   11_no… 12_n… 13_n… 14_n… 15_n… 16_n… extr… 17_n… 18_n… 19_n… 20_n…
    6 F     std   11_no… 12_n… 13_n… 14_n… 15_n… 16_n… extr… 17_n… 18_n… 19_n… 20_n…
    7 G     std   11_no… 12_n… 13_n… 14_n… 15_n… 16_n… extr… 17_n… 18_n… 19_n… 20_n…
    8 H     std   11_no… 12_n… 13_n… 14_n… 15_n… 16_n… extr… 17_n… 18_n… 19_n… 20_n…
