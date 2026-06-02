# 4 Greenhouse t2 - Data wrangling


- [TO DO](#to-do)
- [Set up](#set-up)
- [1 - Subset data](#1---subset-data)
- [2 - Compute new variables](#2---compute-new-variables)
  - [2.1 - dry matter content](#21---dry-matter-content)
    - [2.1.1 - Principle and equations](#211---principle-and-equations)
    - [2.1.2 - Pivot data so that technical replicates are above each
      other, not next to each
      other](#212---pivot-data-so-that-technical-replicates-are-above-each-other-not-next-to-each-other)
  - [2.2 - Nmin concentrations in ppm](#22---nmin-concentrations-in-ppm)
  - [2.3 - Yield variables](#23---yield-variables)
  - [2.4 - Join all data](#24---join-all-data)
- [3 - Deal with standard soils](#3---deal-with-standard-soils)
- [**°°° !!! TO DO !!! °°°**](#--to-do--)
- [4 - Export](#4---export)

# TO DO

For PMN:

- water content can only be obtained from the WHC experiment (CHECK!)

Deal with standard soils.

- Problem: they have same biological unit nb. –\> a preliminary outlier
  removal needs to be ran separately, then average per biol_unit_nb,
  then it can be rejoined…

# Set up

<details class="code-fold">
<summary>Code</summary>

``` r
# clean environment
rm(list = ls())

# packages
library(tidyverse)
library(ggridges) # for geom_density_ridges()
library(ggrepel) # for geom_text_repel()
library(patchwork)

# data
lab <- read_rds("output/data/3_greenhouse_t2_raw_lab.rds")
Nmin <- read_rds("output/data/3_greenhouse_t2_conc_clean.rds") 
#lm_output <- read_rds("output/data/2_lm_output_noTDN.rds")
```

</details>

# 1 - Subset data

Yield data

<details class="code-fold">
<summary>Code</summary>

``` r
yield_data <- lab |> 
  select(biol_unit_nb:sampling_time, starts_with("yd")) 
```

</details>

Flush data (from K2SO4 extraction)

<details class="code-fold">
<summary>Code</summary>

``` r
raw_flush <- lab |> 
  select(
    !starts_with(c("yd", "whc")) &
      !cra_trial:sd_c)
```

</details>

From Nmin data (from absorbance pipeline)

<details class="code-fold">
<summary>Code</summary>

``` r
raw_Nmin <- Nmin |> 
  # create sampling_time and expe variables from plate_ids (first number and first letter after N species)
  mutate(
    sampling_time = paste0(
      "t", 
      str_extract(plate_id, "\\w_(\\d)(\\w).*", group = 1)),
    expe = str_extract(plate_id, "\\w_(\\d)(\\w).*", group = 2),
    expe = case_when(expe %in% c("P", "G") ~ "Pot", .default = expe),
    .before = plate_id) |>
  # filter based on sampling_time
  filter(sampling_time == "t2", expe == "Pot") |> 
  # separate_wider_delim(
  #   cols = map,
  #   names = c("biol_unit_nb"),
  #   delim = "_",
  #   too_many = "drop", 
  #   cols_remove = FALSE
  # ) |> 
  mutate(biol_unit_nb = as.double(biol_unit_nb))
  
# Check it out
raw_Nmin
```

</details>

    # A tibble: 253 × 17
    # Groups:   plate_id, map, biol_unit_nb [253]
       plate_id map           biol_unit_nb std_sp conc_mgN_L st_dev coef_var dataset
       <chr>    <chr>                <dbl> <chr>       <dbl>  <dbl>    <dbl> <chr>  
     1 NH4_2P1  109_t2_MR_R1…          109 NH4       0.0556  0.0285    51.3  Nmint1…
     2 NH4_2P1  109_t2_MR_R5           109 NH4       0.00617 0.0285   462.   Nmint1…
     3 NH4_2P1  109_t2_MR_R6           109 NH4       0.00617 0.0285   462.   Nmint1…
     4 NH4_2P1  10_t2                   10 NH4       0.0309  0          0    Nmint1…
     5 NH4_2P1  110_t2_MR_R1…          110 NH4       0.191   0.0247    12.9  Nmint1…
     6 NH4_2P1  110_t2_MR_R12          110 NH4       0.167   0.0247    14.8  Nmint1…
     7 NH4_2P1  110_t2_MR_R13          110 NH4       0.340   0.0247     7.27 Nmint1…
     8 NH4_2P1  110_t2_MR_R8           110 NH4       0.377   0.0403    10.7  Nmint1…
     9 NH4_2P1  17_t2                   17 NH4       0.0309  0.0403   131.   Nmint1…
    10 NH4_2P1  18_t2                   18 NH4       0.344   0.0755    22.0  Nmint1…
    # ℹ 243 more rows
    # ℹ 9 more variables: sampling_time <chr>, expe <chr>, target_sp <chr>,
    #   std_unit <chr>, slope <dbl>, adj_r_squared <dbl>, lm_p <dbl>, cs <fct>,
    #   soil <fct>

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_sample <- raw_Nmin |> filter(biol_unit_nb < 100)
Nmin_std <- raw_Nmin |> filter_out(biol_unit_nb < 100)
```

</details>

# 2 - Compute new variables

## 2.1 - dry matter content

### 2.1.1 - Principle and equations

In this data set, dry matter and water content have not yet been
computed. The raw data with 3 technical replicates is encoded, in
separate columns starting with `flush_dm...`\`. In this section, we
compute average dry matter and water content for the sample (across
technical replicates). We add additional columns with intermediary
results:

- FW<sub>neat</sub> = FW<sub>gross</sub> - tare ; DW<sub>neat</sub> =
  DW<sub>gross</sub> - tare

- average (FW, then DW) = sum(3 technical replicates) / 3

- DM = avg<sub>DW</sub> / avg<sub>FW</sub>

- WC = 1 - DM

- With

  - FW = fresh weight \[g\]

  - DW = dry weight \[g\],

  - DM = dry matter content \[g<sub>dry soil</sub> /
    g<sub>fresh soil</sub>\],

  - WC = water content \[g<sub>water</sub> / g<sub>fresh soil</sub>\]
    <u>**!! UNIT: per g fresh soil !!**</u>

- ratio = fresh weight of soil added into tubes for extraction (~10g) /
  volume of extractant added (=20ml)

Then, we can finally convert concentrations in mg/L into concentrations
in ppm (mg/kg dry soil)

- see <https://mycloud.ulb.be/index.php/s/9ZKaJg9rXJg4iB7> for equation

### 2.1.2 - Pivot data so that technical replicates are above each other, not next to each other

First, we select relevant columns and pivot to get

- one column respectively for tare, gross fresh weight (g_fw) and gross
  dry weight (g_dw)

- 1 row per technical replicate

<details class="code-fold">
<summary>Code</summary>

``` r
raw_subset_wc <- raw_flush |> 
  select(sample_short, flush_dm_tare_tr1:flush_dm_g_dw_tr3) |> 
  pivot_longer(
    cols = !sample_short,
    names_pattern = "flush_dm_(g_fw|g_dw|tare)_tr(\\d)",
    names_to = c(".value", "tech_rep"),
    values_to = "weight") 

# check it out
raw_subset_wc 
```

</details>

    # A tibble: 264 × 5
       sample_short tech_rep  tare  g_fw  g_dw
       <chr>        <chr>    <dbl> <dbl> <dbl>
     1 t2_201_F     1         4.18  12.4  11.6
     2 t2_201_F     2         4.16  12.9  12.1
     3 t2_201_F     3         4.16  12.4  11.6
     4 t2_202_W     1         4.12  12    11.4
     5 t2_202_W     2         4.13  12.1  11.5
     6 t2_202_W     3         4.14  12.8  12.2
     7 t2_203_IC    1         4.14  12.6  11.9
     8 t2_203_IC    2         4.11  12.5  11.8
     9 t2_203_IC    3         4.13  13.6  12.8
    10 t2_205_F     1         4.15  12.3  11.6
    # ℹ 254 more rows

Then, we

- compute the neat fresh weight and dry weight for each row, and the dry
  matter content and water content

- Compute the average per sample (over the technical triplicate)

<details class="code-fold">
<summary>Code</summary>

``` r
wc <- raw_subset_wc |> 
  # compute neat values
  mutate(
    n_fw = g_fw - tare,
    n_dw = g_dw - tare,
    dm = n_dw / n_fw,
    wc = 1 - dm,
    .keep = "unused"
  ) |> 
  # compute mean per sample
  summarize(
    .by = sample_short,
    dm = mean(dm),
    wc = mean(wc)
  ) 

# check it out
wc 
```

</details>

    # A tibble: 88 × 3
       sample_short    dm     wc
       <chr>        <dbl>  <dbl>
     1 t2_201_F     0.904 0.0963
     2 t2_202_W     0.926 0.0741
     3 t2_203_IC    0.923 0.0767
     4 t2_205_F     0.919 0.0807
     5 t2_206_W     0.906 0.0940
     6 t2_207_IC    0.902 0.0982
     7 t2_209_F     0.899 0.101 
     8 t2_210_W     0.923 0.0772
     9 t2_211_IC    0.922 0.0780
    10 t2_213_F     0.909 0.0910
    # ℹ 78 more rows

Now that we have that clean data, we can

- remove useless columns (raw data used above)

- rejoin it to the complete greenhouse data (with all columns)

- compute each sample’s individual “ratio” (~10g/20ml)

<details class="code-fold">
<summary>Code</summary>

``` r
# Volume of extractant used in the K2SO4 extraction
vol_extr <- 20 

flush_clean <- raw_flush |> 
  select(
    !flush_dm_tare_tr1:flush_dm_g_dw_tr3 & 
      !ends_with("comment")) |> 
  left_join(wc) |> 
  mutate(
    ratio_nf = flush_fw_nf_g / vol_extr,
    ratio_cfe = flush_fw_cfe_g / vol_extr,
    .keep = "unused") 
```

</details>

    Joining with `by = join_by(sample_short)`

<details class="code-fold">
<summary>Code</summary>

``` r
flush_clean
```

</details>

    # A tibble: 88 × 13
       biol_unit_nb expe  sample_short soil  crop_diversity cs    bloc 
              <dbl> <chr> <chr>        <fct> <fct>          <fct> <fct>
     1            1 Pot   t2_201_F     Conv  SC             F     B1   
     2            2 Pot   t2_202_W     Conv  SC             W     B1   
     3            3 Pot   t2_203_IC    Conv  IC             IC    B1   
     4            5 Pot   t2_205_F     Ref   SC             F     B1   
     5            6 Pot   t2_206_W     Ref   SC             W     B1   
     6            7 Pot   t2_207_IC    Ref   IC             IC    B1   
     7            9 Pot   t2_209_F     Auto  SC             F     B1   
     8           10 Pot   t2_210_W     Auto  SC             W     B1   
     9           11 Pot   t2_211_IC    Auto  IC             IC    B1   
    10           13 Pot   t2_213_F     ABC   SC             F     B1   
    # ℹ 78 more rows
    # ℹ 6 more variables: sampling_time <chr>, sample_name <chr>, dm <dbl>,
    #   wc <dbl>, ratio_nf <dbl>, ratio_cfe <dbl>

Now, we divide the data in 2 subsets: sample data and standard soil data
(several reps, messes up the analysis (for now))

<details class="code-fold">
<summary>Code</summary>

``` r
flush_sample <- flush_clean |> filter(biol_unit_nb < 100)
flush_std <- flush_clean |> filter_out(biol_unit_nb < 100)
```

</details>

## 2.2 - Nmin concentrations in ppm

First, we

- join absorbance data and flush data,

- Compute the N-sp N concentration in ppm (e.g. mg NO3-N / kg dry soil)

- do some data housecleaning

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_sample |> arrange(biol_unit_nb) 
```

</details>

    # A tibble: 180 × 17
    # Groups:   plate_id, map, biol_unit_nb [180]
       plate_id  map   biol_unit_nb std_sp conc_mgN_L   st_dev coef_var dataset 
       <chr>     <chr>        <dbl> <chr>       <dbl>    <dbl>    <dbl> <chr>   
     1 NH4_2P7_1 1_t2             1 NH4       0.0499  0.0250      50    Nmint1t2
     2 NO2_2P7_1 1_t2             1 NO2       0.00352 0            0    Nmint1t2
     3 NO3_2P7_1 1_t2             1 NO3       0.132   0.00898      6.79 Nmint1t2
     4 NH4_2P3   2_t2             2 NH4       0.236   0.0405      17.2  Nmint1t2
     5 NO2_2P3   2_t2             2 NO2       0.00599 0.000865    14.4  Nmint1t2
     6 NO3_2P3   2_t2             2 NO3       0.0988  0.0146      14.7  Nmint1t2
     7 NH4_2P3   3_t2             3 NH4       0.0868  0            0    Nmint1t2
     8 NO2_2P3   3_t2             3 NO2       0.00711 0.000749    10.5  Nmint1t2
     9 NO3_2P3   3_t2             3 NO3       0.103   0.0176      17.1  Nmint1t2
    10 NH4_2P3   5_t2             5 NH4       0.0744  0.0248      33.3  Nmint1t2
    # ℹ 170 more rows
    # ℹ 9 more variables: sampling_time <chr>, expe <chr>, target_sp <chr>,
    #   std_unit <chr>, slope <dbl>, adj_r_squared <dbl>, lm_p <dbl>, cs <fct>,
    #   soil <fct>

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_ppm_sample <- Nmin_sample |> 
  # join both data tables
  left_join(flush_sample) |> 
  # compute concentration in ppm
  mutate(conc_ppm = conc_mgN_L / (ratio_nf * dm)) |> 
  # remove again the bare soils
  filter_out(is.na(sample_short)) |> 
  # select relevant columns
  select(!c(
    #dataset, 
    expe, 
    target_sp:lm_p, 
    starts_with("sampl"), starts_with("ratio"),
    map, plate_id, conc_mgN_L)) |> 
  arrange(biol_unit_nb)
```

</details>

    Joining with `by = join_by(biol_unit_nb, sampling_time, expe, cs, soil)`
    Adding missing grouping variables: `plate_id`, `map`

<details class="code-fold">
<summary>Code</summary>

``` r
# Check it out
Nmin_ppm_sample
```

</details>

    # A tibble: 180 × 14
    # Groups:   plate_id, map, biol_unit_nb [180]
       plate_id  map   biol_unit_nb std_sp   st_dev coef_var dataset  cs    soil 
       <chr>     <chr>        <dbl> <chr>     <dbl>    <dbl> <chr>    <fct> <fct>
     1 NH4_2P7_1 1_t2             1 NH4    0.0250      50    Nmint1t2 F     Conv 
     2 NO2_2P7_1 1_t2             1 NO2    0            0    Nmint1t2 F     Conv 
     3 NO3_2P7_1 1_t2             1 NO3    0.00898      6.79 Nmint1t2 F     Conv 
     4 NH4_2P3   2_t2             2 NH4    0.0405      17.2  Nmint1t2 W     Conv 
     5 NO2_2P3   2_t2             2 NO2    0.000865    14.4  Nmint1t2 W     Conv 
     6 NO3_2P3   2_t2             2 NO3    0.0146      14.7  Nmint1t2 W     Conv 
     7 NH4_2P3   3_t2             3 NH4    0            0    Nmint1t2 IC    Conv 
     8 NO2_2P3   3_t2             3 NO2    0.000749    10.5  Nmint1t2 IC    Conv 
     9 NO3_2P3   3_t2             3 NO3    0.0176      17.1  Nmint1t2 IC    Conv 
    10 NH4_2P3   5_t2             5 NH4    0.0248      33.3  Nmint1t2 F     Ref  
    # ℹ 170 more rows
    # ℹ 5 more variables: crop_diversity <fct>, bloc <fct>, dm <dbl>, wc <dbl>,
    #   conc_ppm <dbl>

Now, we pivot the data so that each N-sp receives its own column

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_ppm_wider <- Nmin_ppm_sample |>
  ungroup() |>  
  select(!c(plate_id, map, st_dev, coef_var)) |> 
  pivot_wider(
    names_from = std_sp,
    values_from = conc_ppm,
    names_prefix = "ppm_"
  ) |> 
  # to make obvious that NO3 is still uncorrected
  rename(ppm_NO3_uncorrected = ppm_NO3)
```

</details>

Then we can compute final Nmin variables:

- correct NO3 value (what is measured is the sum of NO3 and NO2, bc we
  measure NO2 after reduction of NO3 to NO2)

- Compute Nmin (= NO3 + NO2 + NH4)

- Compute ratio, e.g., NO3/Nmin, NH4/Nmin, NO3/NH4

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_all_variables <- Nmin_ppm_wider |> 
  mutate(
    ppm_NO3 = ppm_NO3_uncorrected - ppm_NO2,
    ppm_Nmin = ppm_NO3_uncorrected + ppm_NH4,
    NO3_Nmin = ppm_NO3 / ppm_Nmin,
    NH4_Nmin = ppm_NH4 / ppm_Nmin,
    NO3_NH4 = ppm_NO3 / ppm_NH4
  ) |> 
  # remove uncorrected NO3
  select(!ppm_NO3_uncorrected)
```

</details>

## 2.3 - Yield variables

<details class="code-fold">
<summary>Code</summary>

``` r
yield_clean <- yield_data |> 
  # select(!c(
  #   expe:crop_diversity, 
  #   bloc:sampling_time,
  #   yd_rs_comment)) |> 
  rowwise() |> 
  relocate(starts_with(c("yd_rs_f_h", "yd_rs_f_s", "yd_rs_w_h", "yd_rs_w_ti"))) |> 
  mutate(
    # compute nb of plants of each sp
    nb_fb = case_when(
      !is.na(yd_rs_f_stem1) & !is.na(yd_rs_f_stem2) ~ 2,
      !is.na(yd_rs_f_stem1) & is.na(yd_rs_f_stem2) ~ 1
    ),
    nb_w = case_when(
      !is.na(yd_rs_w_till1) & !is.na(yd_rs_w_till2) ~ 2,
      !is.na(yd_rs_w_till1) & is.na(yd_rs_w_till2) ~ 1
    ),
    height_per_fb = mean(c_across(starts_with("yd_rs_f_h")), na.rm = TRUE),
    stem_per_fb = mean(c_across(starts_with("yd_rs_f_s")), na.rm = TRUE),
    height_per_w = mean(c_across(starts_with("yd_rs_w_h")), na.rm = TRUE),
    till_per_w = mean(c_across(starts_with("yd_rs_w_ti")), na.rm = TRUE),
    
    fw_fb_per_pot = yd_rs_f_biom_1_2plants_raw_fw_g - yd_rs_f_tare_g,
    fw_w_per_pot = yd_rs_w_biom_1_2plants_raw_fw_g - yd_rs_w_tare_g,
    dw_fb_per_pot = yd_rs_f_biom_1_2plants_raw_dw_g - yd_rs_f_tare_g,
    dw_w_per_pot = yd_rs_w_biom_1_2plants_raw_dw_g - yd_rs_w_tare_g,
    
    fw_fb_per_plant = fw_fb_per_pot / nb_fb,
    fw_w_per_plant = fw_w_per_pot / nb_w,
    dw_fb_per_plant = dw_fb_per_pot / nb_fb,
    dw_w_per_plant = dw_w_per_pot / nb_w,
    
    .keep = "unused"
    ) 

# check it out
yield_clean
```

</details>

    # A tibble: 88 × 25
    # Rowwise: 
       biol_unit_nb expe  sample_short cra_trial sd_c  soil  crop_diversity cs   
              <dbl> <chr> <chr>        <chr>     <chr> <fct> <fct>          <fct>
     1            1 Pot   t2_201_F     SyCI      Conv  Conv  SC             F    
     2            2 Pot   t2_202_W     SyCI      Conv  Conv  SC             W    
     3            3 Pot   t2_203_IC    SyCI      Conv  Conv  IC             IC   
     4            5 Pot   t2_205_F     SyCBio    SdC1  Ref   SC             F    
     5            6 Pot   t2_206_W     SyCBio    SdC1  Ref   SC             W    
     6            7 Pot   t2_207_IC    SyCBio    SdC1  Ref   IC             IC   
     7            9 Pot   t2_209_F     SyCBio    SdC2  Auto  SC             F    
     8           10 Pot   t2_210_W     SyCBio    SdC2  Auto  SC             W    
     9           11 Pot   t2_211_IC    SyCBio    SdC2  Auto  IC             IC   
    10           13 Pot   t2_213_F     SyCBio    SdC3  ABC   SC             F    
    # ℹ 78 more rows
    # ℹ 17 more variables: bloc <fct>, sampling_time <chr>, yd_rs_comment <chr>,
    #   nb_fb <dbl>, nb_w <dbl>, height_per_fb <dbl>, stem_per_fb <dbl>,
    #   height_per_w <dbl>, till_per_w <dbl>, fw_fb_per_pot <dbl>,
    #   fw_w_per_pot <dbl>, dw_fb_per_pot <dbl>, dw_w_per_pot <dbl>,
    #   fw_fb_per_plant <dbl>, fw_w_per_plant <dbl>, dw_fb_per_plant <dbl>,
    #   dw_w_per_plant <dbl>

## 2.4 - Join all data

<details class="code-fold">
<summary>Code</summary>

``` r
data_to_export <- Nmin_all_variables |> left_join(yield_clean)
```

</details>

    Joining with `by = join_by(biol_unit_nb, cs, soil, crop_diversity, bloc)`

# 3 - Deal with standard soils

# **°°° !!! TO DO !!! °°°**

# 4 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
data_to_export |> write_rds("output/data/4_t2_greenhouse_transformed.rds")
```

</details>
