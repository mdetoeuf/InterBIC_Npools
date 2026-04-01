# III. Data Tidying


- [Set up](#set-up)
- [1 - Greenhouse, t2](#1---greenhouse-t2)
  - [1.1 - Import data](#11---import-data)
  - [1.2 - Extract subsets: Nmin, TDN
    (incl. MBN)](#12---extract-subsets-nmin-tdn-incl-mbn)
    - [1.2.1 - Retrieve dataset information into absorbance
      dataframe](#121---retrieve-dataset-information-into-absorbance-dataframe)
    - [1.2.2 - Store subsets in new
      objects](#122---store-subsets-in-new-objects)
  - [1.3 - Export](#13---export)

In this script we tidy data sets into the shape that we need for data
transformation and following analysis and visualization

# Set up

<details class="code-fold">
<summary>Code</summary>

``` r
rm(list = ls())

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
library(janitor)
```

</details>


    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test

# 1 - Greenhouse, t2

We first focus on data from the greenhouse trial at t2. It follows a
complete randomized block design, with modalities: 4 soils x 3 crop
stands x 5 blocs

## 1.1 - Import data

<details class="code-fold">
<summary>Code</summary>

``` r
#lm_output <- read_rds("output/data/Nmin_std_curves_lm.rds")

# import absorbance data + initiate an empty column for data set
N_data <- read_rds("output/data/Nmin_conc.rds") |> 
  # add any column to be taken from metadata
  mutate(
    dataset = NA,
    sampling_time = NA) |> 
  rename(sample_short = plate_map)
N_metadata <- read_rds("output/data/N_all_metadata.rds")


# import other "wet lab" raw data
raw_data <- read_csv("../raw_data/2024_raw.csv", show_col_types = FALSE) |> clean_names() |> 
  rename(sample_short = short) 
```

</details>

    New names:
    • `` -> `...173`

## 1.2 - Extract subsets: Nmin, TDN (incl. MBN)

### 1.2.1 - Retrieve dataset information into absorbance dataframe

<details class="code-fold">
<summary>Code</summary>

``` r
for (i in 1:nrow(N_data)) {
  plate <- N_data$plate_id[i]
  line <- N_metadata |> filter(plate_id == plate)
  
  N_data$dataset[i] <- line$dataset[1]
  N_data$sampling_time[i] <- line$sampling_time[1]
    
}

#check that works:
N_data$dataset |> unique()
```

</details>

    [1] "Nmint1t2" "Nmint3"   "TDN"     

<details class="code-fold">
<summary>Code</summary>

``` r
N_data$sampling_time |> unique()
```

</details>

    [1] "t1" "t2" "t3"

### 1.2.2 - Store subsets in new objects

Now that each row has been given the corresponding dataset, we can take
a subset based on dataset

#### Nmin

For Nmin –\> we don’t take t3 for now, we’ll still have to filter based
on sampling_time

<details class="code-fold">
<summary>Code</summary>

``` r
# plate data
Nmin_plate_data <- N_data |> 
  filter(dataset == "Nmint1t2", sampling_time == "t2")

# metadata
Nmin_metadata <- N_metadata |> 
  filter(dataset == "Nmint1t2", sampling_time == "t2")

# raw data
Nmin_raw <- raw_data |> 
  # keep only t2 and pot, remove bare soil
  filter(sampling_time == "t2", expe == "Pot", cs != "B") |> 
  # tidy: 
  # For now, only actual samples (no std soil with weird number)
  filter(soil %in% c("Conv", "Ref", "Auto", "ABC")) |> 
  # sample number was made unique, not practical here
  mutate(
    sample_short = paste0(biol_unit_nb - 200, "_", sampling_time)
  ) |> 
  select(!starts_with(c("amf", "rt")))
```

</details>

Now we join those tables in one big data set

<details class="code-fold">
<summary>Code</summary>

``` r
# join
Nmin <- left_join(
  Nmin_raw, Nmin_plate_data, 
  by = join_by(sample_short, sampling_time)) |>
  left_join(
    Nmin_metadata,
    by = join_by(sampling_time, plate_id, dataset)) 
```

</details>

#### TDN

Same principle. First subset

<details class="code-fold">
<summary>Code</summary>

``` r
# For TDN and MBN
TDN_plate_data <- N_data |> 
  filter(dataset == "TDN", sampling_time == "t2")
TDN_metadata <- N_metadata |> 
  filter(dataset == "TDN", sampling_time == "t2")
TDN_raw <- Nmin_raw
```

</details>

Then join

<details class="code-fold">
<summary>Code</summary>

``` r
# join
TDN <- left_join(
  TDN_raw, TDN_plate_data, 
  by = join_by(sample_short, sampling_time)) |>
  left_join(
    TDN_metadata,
    by = join_by(sampling_time, plate_id, dataset)) 
```

</details>

## 1.3 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin |>  write_rds("output/data/Nmin_tidy.rds")
TDN |> write_rds("output/data/TDN_tidy.rds")
```

</details>
