# 3 Greenhouse, t2 - Import non-absorbance data


- [To Do](#to-do)
- [Set up](#set-up)
- [1 - import data](#1---import-data)
  - [1.1 - Non-absorbance data from
    AELab](#11---non-absorbance-data-from-aelab)
  - [1.2 - Data from CRA-W (Field)](#12---data-from-cra-w-field)
  - [1.3 - Microresp Data](#13---microresp-data)
  - [2 - Tidy data](#2---tidy-data)
- [3 - Export](#3---export)

# To Do

- Link to a MicroResp pipeline!

# Set up

<details class="code-fold">
<summary>Code</summary>

``` r
rm(list = ls())

library(tidyverse)
library(janitor)
```

</details>

# 1 - import data

## 1.1 - Non-absorbance data from AELab

<details class="code-fold">
<summary>Code</summary>

``` r
# import other "wet lab" raw data
raw_data_pot <- read_csv(
  "../raw_data/2024_raw.csv", show_col_types = TRUE,
  col_types = list(
    Soil = col_factor(),
    crop_diversity = col_factor(),
    CS = col_factor(),
    bloc = col_factor()
  )
  ) |> clean_names() |> 
  rename(sample_short = short) |> 
  filter(expe == "Pot")
```

</details>

    New names:
    Rows: 660 Columns: 173
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (29): Expe, short, CRA_trial, SdC, sampling_time, zone, incub_time, Res... dbl
    (138): Biol_unit_Nb, WHC_Tare_tube_g, WHC_gFW_g, WHC_Tare_dish_g, WHC_gS... lgl
    (2): Yd_grain_W_unit, Yd_Comment fct (4): Soil, crop_diversity, CS, bloc
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...173`

<details class="code-fold">
<summary>Code</summary>

``` r
raw_greenhouse_t2 <- raw_data_pot |> 
  filter(sampling_time == "t2") |> 
  # remove bare soil
  filter_out(cs == "B") |> 
  # correct biological unit
  mutate(biol_unit_nb = case_when(
    biol_unit_nb < 200 ~ biol_unit_nb,
    biol_unit_nb > 200 ~ biol_unit_nb - 200
  )) |> 
  arrange(biol_unit_nb) |> 
  # remove useless columns
  select(
    biol_unit_nb:sampling_time, 
    sample_name, #useful?
   starts_with(c("whc", "flush", "yd_rs"))
    )

# Check it out
raw_greenhouse_t2
```

</details>

    # A tibble: 88 × 44
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
    # ℹ 36 more variables: bloc <fct>, sampling_time <chr>, sample_name <chr>,
    #   whc_tare_tube_g <dbl>, whc_g_fw_g <dbl>, whc_tare_dish_g <dbl>,
    #   whc_g_sw_g <dbl>, whc_g_dw_g <dbl>, whc_comment <chr>,
    #   flush_dm_tare_tr1 <dbl>, flush_dm_g_fw_tr1 <dbl>, flush_dm_g_dw_tr1 <dbl>,
    #   flush_dm_tare_tr2 <dbl>, flush_dm_g_fw_tr2 <dbl>, flush_dm_g_dw_tr2 <dbl>,
    #   flush_dm_tare_tr3 <dbl>, flush_dm_g_fw_tr3 <dbl>, …

## 1.2 - Data from CRA-W (Field)

## 1.3 - Microresp Data

Then, MicroResp data may need to be added, probably with its own
pipeline

## 2 - Tidy data

For raw greenhouse data, it looks good for now

<details class="code-fold">
<summary>Code</summary>

``` r
raw_greenhouse_t2 |> str()
```

</details>

    tibble [88 × 44] (S3: tbl_df/tbl/data.frame)
     $ biol_unit_nb                   : num [1:88] 1 2 3 5 6 7 9 10 11 13 ...
     $ expe                           : chr [1:88] "Pot" "Pot" "Pot" "Pot" ...
     $ sample_short                   : chr [1:88] "t2_201_F" "t2_202_W" "t2_203_IC" "t2_205_F" ...
     $ cra_trial                      : chr [1:88] "SyCI" "SyCI" "SyCI" "SyCBio" ...
     $ sd_c                           : chr [1:88] "Conv" "Conv" "Conv" "SdC1" ...
     $ soil                           : Factor w/ 7 levels "Conv","Ref","Auto",..: 1 1 1 2 2 2 3 3 3 4 ...
     $ crop_diversity                 : Factor w/ 3 levels "B","SC","IC": 2 2 3 2 2 3 2 2 3 2 ...
     $ cs                             : Factor w/ 4 levels "F","W","IC","B": 1 2 3 1 2 3 1 2 3 1 ...
     $ bloc                           : Factor w/ 41 levels "pooled_rt1","pooled_rt2",..: 13 13 13 13 13 13 13 13 13 13 ...
     $ sampling_time                  : chr [1:88] "t2" "t2" "t2" "t2" ...
     $ sample_name                    : chr [1:88] "Pot_t2_B1_Conv_SC_F" "Pot_t2_B1_Conv_SC_W" "Pot_t2_B1_Conv_IC_IC" "Pot_t2_B1_Ref_SC_F" ...
     $ whc_tare_tube_g                : num [1:88] 10.61 10.7 9.08 9.6 11.53 ...
     $ whc_g_fw_g                     : num [1:88] 41.8 35.2 42 44.7 38.5 ...
     $ whc_tare_dish_g                : num [1:88] 4.17 4.18 4.17 4.15 4.2 4.23 4.14 4.18 4.16 4.17 ...
     $ whc_g_sw_g                     : num [1:88] 42.6 33.1 44.4 47.5 35.2 ...
     $ whc_g_dw_g                     : num [1:88] 33.6 26.5 35 37.3 28.7 ...
     $ whc_comment                    : chr [1:88] NA NA NA NA ...
     $ flush_dm_tare_tr1              : num [1:88] 4.18 4.12 4.14 4.15 4.15 4.16 4.16 4.15 4.15 4.19 ...
     $ flush_dm_g_fw_tr1              : num [1:88] 12.4 12 12.6 12.3 12 ...
     $ flush_dm_g_dw_tr1              : num [1:88] 11.6 11.4 11.9 11.7 11.3 ...
     $ flush_dm_tare_tr2              : num [1:88] 4.16 4.13 4.11 4.18 4.14 4.18 4.17 4.13 4.19 4.2 ...
     $ flush_dm_g_fw_tr2              : num [1:88] 12.9 12.1 12.5 12.3 12.8 ...
     $ flush_dm_g_dw_tr2              : num [1:88] 12.1 11.5 11.8 11.6 12 ...
     $ flush_dm_tare_tr3              : num [1:88] 4.16 4.14 4.13 4.18 4.2 4.16 4.19 4.16 4.18 4.18 ...
     $ flush_dm_g_fw_tr3              : num [1:88] 12.4 12.8 13.6 12.6 13.3 ...
     $ flush_dm_g_dw_tr3              : num [1:88] 11.6 12.2 12.8 11.9 12.4 ...
     $ flush_fw_nf_g                  : num [1:88] 10.1 10 10.1 10 10.1 ...
     $ flush_fw_cfe_g                 : num [1:88] 10 10.1 10 10.2 10 ...
     $ flush_dm_comment               : chr [1:88] "Check que c'est bien les bons, et cmt gérer la double mesure de MS si on ramène MR brut ici? Probablement autre"| __truncated__ "!! Data only valid for SSE. WC for MR was retaken for these samples because of failed MR batches. For use withi"| __truncated__ "!! Data only valid for SSE. WC for MR was retaken for these samples because of failed MR batches. For use withi"| __truncated__ "!! Data only valid for SSE. WC for MR was retaken for these samples because of failed MR batches. For use withi"| __truncated__ ...
     $ yd_rs_f_hgt1_cm                : num [1:88] 27.5 NA 44.5 28.5 NA 42 38.5 NA 33 37.5 ...
     $ yd_rs_f_stem1                  : num [1:88] 3 NA 3 2 NA 3 3 NA 4 3 ...
     $ yd_rs_f_hgt2_cm                : num [1:88] 38 NA NA 41 NA NA 35.5 NA NA 36.5 ...
     $ yd_rs_f_stem2                  : num [1:88] 4 NA NA 4 NA NA 3 NA NA 4 ...
     $ yd_rs_w_hgt1_cm                : num [1:88] NA 33.5 54 NA 49 52.5 NA 51 41.5 NA ...
     $ yd_rs_w_till1                  : num [1:88] NA 1 2 NA 1 1 NA 1 1 NA ...
     $ yd_rs_w_hgt2_cm                : num [1:88] NA 42 NA NA 48 ...
     $ yd_rs_w_till2                  : num [1:88] NA 1 NA NA 1 NA NA 1 NA NA ...
     $ yd_rs_f_tare_g                 : num [1:88] 6.24 NA 6.56 6.67 NA 6.32 6.25 NA 6.33 6.28 ...
     $ yd_rs_f_biom_1_2plants_raw_fw_g: num [1:88] 42.5 NA 24.3 46.1 NA ...
     $ yd_rs_f_biom_1_2plants_raw_dw_g: chr [1:88] "12.34" "Na" "9.7" "13.25" ...
     $ yd_rs_w_tare_g                 : num [1:88] NA 6.23 6.29 NA 6.19 6.4 NA 6.28 6.38 NA ...
     $ yd_rs_w_biom_1_2plants_raw_fw_g: num [1:88] NA 10.7 10.9 NA 12.5 ...
     $ yd_rs_w_biom_1_2plants_raw_dw_g: num [1:88] NA 7.63 7.67 NA 8.45 7.3 NA 8.77 7.1 NA ...
     $ yd_rs_comment                  : chr [1:88] NA NA NA NA ...

# 3 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
raw_greenhouse_t2 |> write_rds("output/data/3_raw_greenhouse_t2.rds")
```

</details>
