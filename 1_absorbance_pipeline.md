# V. Pipeline for Absorbance data


- [To Do](#to-do)
- [Algorithm in natural language](#algorithm-in-natural-language)
- [Code](#code)
  - [1 - Set up](#1---set-up)
  - [2 - Vectorize plate data](#2---vectorize-plate-data)
  - [3 - QC - warning if absorbances not in
    \[0.05,1.5\]](#3---qc---warning-if-absorbances-not-in-00515)
  - [4 - Correct absorbance values](#4---correct-absorbance-values)
    - [4.1 - Correct std curves for
      blanc](#41---correct-std-curves-for-blanc)
    - [4.2 - Correct samples for blanc](#42---correct-samples-for-blanc)
    - [4.3 - Join corrected absorbances back into data
      table](#43---join-corrected-absorbances-back-into-data-table)
- [°<sup>°°°</sup> Milestone : corrected data ready for downstream
  analysis
  °<sup>°°°</sup>](#-milestone--corrected-data-ready-for-downstream-analysis-)
  - [5 - Computing regression equation btw absorbance and
    concentration](#5---computing-regression-equation-btw-absorbance-and-concentration)

# To Do

- bla

# Algorithm in natural language

- Plate info list:

  - plate number - element \<chr\>

  - column(s) with std curve(s) - vector \<chr\>

  - standard identity & unit - vector \<chr\> (element1 = name, element2
    = unit)

  - range of std concentration - vector \<num\>

  - column(s) with blanc - vector \<chr\>

  - blanc identity & unit of concentration - vector \<chr\> (element1 =
    name, element2 = unit)

  - concentration of blanc - element \<num\>

  - timestamp - date-time format (?)

  - wavelength in nm - element \<int\>

- vectorization of absorbance data, vectors are:

  - plate number

  - row

  - column

  - raw absorbance

  - legend (or ID)

- Correct absorbances for blanc

  - Return a warning message

    - when absorbances are below 0.05 or above 1.5

    - give the number of wells concerned,

    - give the max and min value of those wells

  - case when row concerns std curve: correct std absorbances for std
    blanc

    - find rows where concentration is zero (slice.min?)

    - take average of absorbance from those rows

    - substract to all absorbances of the std the average “zero” value
      and store it in a new column = “corrected_abs”

  - case when row concerns samples: Correct sample absorbances for blanc

    - find rows with blanc

    - compute variation coefficient

      - return a warning message when var.coeff \> 30%?

      - exclude wells based on this? Or human decision?

    - compute average of blancs

    - substract that value to all non-standard rows and store it in
      column “corrected_abs”

- Compute standard curve

  - Operation per plate

  - in plate df, filter rows corresponding to the std

  - 

# Code

## 1 - Set up

Loading packages

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
library(roperators) # to be able to add %ni% for "not in"
```


    Attaching package: 'roperators'

    The following object is masked from 'package:tibble':

        num

    The following object is masked from 'package:ggplot2':

        %+%

Prep template data: fake tables for the sake of building the next steps
of the code. Once that code is working, then we can figure out how to
extract real plate data instead of this fake model one.

``` r
# create an empty table with NAs
matrix <- matrix(NA, nrow = 8, ncol = 12)
# give it names 1 to 12
colnames(matrix) <- as.character(c(1:12))

# turn it into a tibble and add column with letters
plate_empty <- as_tibble(matrix) |> 
  mutate(row = LETTERS[1:8], .before = 1)

# use this template to create a plate with random numbers
plate_abs <- plate_empty
for (col in 2:13) {
  plate_abs[col] = sample(1:100, 8) / 100
}

# same but create random id's for the plate map (whole columns, who cares...)
plate_map <- plate_empty
for (col in 2:13) {
  plate_map[col] = rep(str_flatten(sample(letters, 3)), 8)
}

# create fake plate information, this info could be extracted from real data set, for ex each variable here becomes a column name, and we record one row per plate
plate_id <- "plate_id"
std_column <- c("1", "12")
std_id <- c("no3", "mgN_per_L", "H2O") # n species, unit, prepared in
std_conc <- c(1,2,3,4,5,6,7,8) # we need numerics
extractant_column <- "7"
extractant_id <- c("K2SO4", "M") # extractant, unit
extractant_conc <- 0.5 # we need numeric
timestamp <- timestamp() # see if the format of this is important
```

    ##------ Fri Mar 13 14:16:58 2026 ------##

``` r
wavelength <- "540 nm" # this is just to store info and will not be used for calculations, can be any format 
delay_min <- 30 # also just to store info, it is the incubation time of the manip (plate left under the hood before measurement). Called delay to not confuse with incubation times of PMN and PNR 
```

## 2 - Vectorize plate data

We use pivot longer

``` r
# vectorize absorbance data
abs_longer <- plate_abs |> 
  pivot_longer(cols = `1`:`12`, names_to = "column", values_to = "abs") 

# vectorize well ids
map_longer <- plate_map |> 
  pivot_longer(cols = `1`:`12`, names_to = "column", values_to = "well_content")

# join both according to info stored in "row" and in "column"
abs_data <- 
  left_join(abs_longer, map_longer, by = c("column", "row")) |> 
  # add info on plate id and well id
  mutate(
    plate_id = rep(plate_id),
    well_id = paste0(row, column),
    .before = 1) |> 
  # sort according to column
  arrange(column)
```

## 3 - QC - warning if absorbances not in \[0.05,1.5\]

The ideal range for absorbance readings (Beer-lambert in linear range of
relationship between concentration and absorbance) is between 0.1 and 1.
But these are not super strict borders. I don’t want to send out a
warning message too soon, so we take higher values.

This chunk filters out only rows where absorbance is out of range, and
returns either a warning (when there are out-of-range values) or a happy
message (when there are none). In case of a warning, it also shares the
table with suspicious wells, so that the user can take an informed
decision.

<u>**To be thought through:**</u>

- What are options then? Remove suspicious wells (replace by NAs?) –\>
  deal with it if we are confronted with it

- another option could be, instead of returning a table, to return only
  the min and max values of absorbance and/or the number of wells that
  are out of range

- We could also visualize a histogram of absorbance with threshold
  values marked on the graph

``` r
#** Make sure empty wells contain NA, otherwise, lots of warning messages? To be tested *
#*

# initiate data frame that will contain suspicious well ids
suspicious_wells <- c()
for (i in 1:nrow(abs_data)) {
  if (abs_data$abs[i] < 0.05 || abs_data$abs[i] > 1.5) {
    suspicious_wells <- append(suspicious_wells, abs_data$well_id[i])
    }
}

# Send a warning message
if (!is.null(suspicious_wells)) {
  warning("Some wells are out of range for absorbance, i.e., not in [0.05,1.5] \nSee table hereabove to identify suspicious wells")
  abs_data |> filter(well_id %in% suspicious_wells)
} else {
  message("°^° !! YAY !! °^° All wells are in range for absorbance")
}
```

    Warning: Some wells are out of range for absorbance, i.e., not in [0.05,1.5] 
    See table hereabove to identify suspicious wells

    # A tibble: 5 × 6
      plate_id well_id row   column   abs well_content
      <chr>    <chr>   <chr> <chr>  <dbl> <chr>       
    1 plate_id B3      B     3       0.01 uwp         
    2 plate_id G4      G     4       0.02 pei         
    3 plate_id E6      E     6       0.04 fph         
    4 plate_id B7      B     7       0.02 dbl         
    5 plate_id F8      F     8       0.02 rzq         

## 4 - Correct absorbance values

Now we correct absorbance values by subtracting blanc values from raw
values (absorbance of the light by the solution = absorbance by the
blank solution + absorbance by the substance to be quantified)

If the standard curves were prepared in water, then the blanc for the
standard curve is the absorbance of the well containing only water of
that curve. If it was prepared with the extractant, then the blanc is
the mean of the values of the wells where the extractant was added.

<u>**To be thought through:**</u>

- If it becomes relevant: make some sort of if condition, based on plate
  information (`blanc_id` and `std_id`)

- Make sure that the slice-min part in next chunk behaves as expected in
  the case of a tie

### 4.1 - Correct std curves for blanc

For now, this is a separate process to account for the fact that the
standard curve was prepared in H2O, not in the extractant (K2SO4 or
KCl).

First, we compute the blanc value and return a warning if blanc values
show too much variation (in the case of several plate-columns with
standard curves)

``` r
#** !! Adapt threshold parameter *

threshold <- 10 # max coeff_var that we accept [%]


# Compute blanc value (as average in case of several values)
std_data <- abs_data |> 
  # take only plate-columns with standard curves
  filter(
    column %in% std_column) 
  
# extract ("slice") only rows with the smallest absorbance
  
std_blanc_all <- std_data |> 
  slice_min(
    abs, # slice min accordint to the valus in abs
    n = length(std_column),  # pick as many rows as the nb of columns with standard curve
    with_ties = FALSE # in case there are ties, it will add extra rows
    ) 

std_blanc_avg <-  std_blanc_all |> 
  summarise(blanc_abs = mean(abs), std_dev = sd(abs)) |> 
  mutate(
    coeff_var_percent = 100 * std_dev / blanc_abs)

# Warning if values are too divergent (decide what "threshold" is for the coefficient of variation ?)

# in case of several values...
if (length(std_column) != 1) { 
  # ... and of coefficient of variation > set threshold
  if (std_blanc_avg$coeff_var_percent[1] > threshold) {
    # send a warning
    warning(paste0("There is a big variation in absorbance values for the the blanc of the standard curve (more than ", threshold, "%).\nPick the most likely values / remove outliers manually.\nSee tables above to judge on values and find suspicious wells"))
    # and show suspicious wells
    std_blanc_all
  }
}
```

    Warning: There is a big variation in absorbance values for the the blanc of the standard curve (more than 10%).
    Pick the most likely values / remove outliers manually.
    See tables above to judge on values and find suspicious wells

    # A tibble: 2 × 6
      plate_id well_id row   column   abs well_content
      <chr>    <chr>   <chr> <chr>  <dbl> <chr>       
    1 plate_id H12     H     12      0.06 lnd         
    2 plate_id E12     E     12      0.09 lnd         

``` r
std_blanc <- std_blanc_avg$blanc_abs
```

Now we can correct the absorbance values for the standard curves.

``` r
std_corrected <- std_data |> 
  filter(well_id %ni% std_blanc_all$well_id) |> 
  mutate(abs_corrected = abs - std_blanc)

# just to check if it works... Yay :-)
#left_join(abs_data, std_corrected) |> view()
```

We could add those corrected values back into the main data table, but
actually those numbers are only useful to compute the regression
equation between corrected absorbance and concentration. For thematic
clarity purpose, this will be done in a later section (to keep all work
on blancs in one place)

### 4.2 - Correct samples for blanc

Same, but here we correct the values for samples with the average blanc
value of the extractant

``` r
# Compute blanc value (as average in case of several values)
extractant_data <- abs_data |> 
  # take only relevant plate-columns
  filter(
    column %in% extractant_column) 

extractant_avg <-  extractant_data |> 
  summarise(blanc_abs = mean(abs), std_dev = sd(abs)) |> 
  mutate(
    coeff_var_percent = 100 * std_dev / blanc_abs)

# Warning if values are too divergent (decide what "threshold" is for the coefficient of variation ?)

# in case of several values...
if (length(extractant_column) != 1) { 
  # ... and of coefficient of variation > set threshold
  if (extractant_avg$coeff_var_percent[1] > threshold) {
    # send a warning
    warning(paste0("There is a big variation in absorbance values for the the blanc of the samples (extractant, coefficient of variation more than ", threshold, "%).\nPick the most likely values / remove outliers manually.\nSee tables above to judge on values and find suspicious wells"))
    # and show suspicious wells
    extractant_data
  }
}

extractant_blanc <- extractant_avg$blanc_abs
```

### 4.3 - Join corrected absorbances back into data table

Now we can correct the absorbance values for the samples and directly
store them in a new data table (additional column, less rows because we
keep neither std curves nor extractant)

! The parameter `.keep = "unused"` of the `mutate()` function is there
to get rid of the column “abs” that contained the raw data. This is just
to prevent mishaps later where the wrong data might be used. To change
this in order to keep the “abs” column, simply remove this argument or
use `.keep = "all"`.

``` r
abs_corrected <- abs_data |> 
     filter(
       well_id %ni% c(
         extractant_data$well_id,
         std_data$well_id)
       ) |> 
     mutate(
       abs_corrected = abs - extractant_blanc,
       .keep = "unused")
```

# °<sup>°°°</sup> Milestone : corrected data ready for downstream analysis °<sup>°°°</sup>

At this point, we could export the data table for downstream analysis
(Microresp, etc), although for most applications, the next step is still
needed: computing regression equation

## 5 - Computing regression equation btw absorbance and concentration
