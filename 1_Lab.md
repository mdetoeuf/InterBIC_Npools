# 1 Wet-lab data
Morgane de Toeuf

- [To Do](#to-do)
- [Set up](#set-up)
- [1 - Import raw data](#1---import-raw-data)
- [2 - Lab data on a per plot / pot
  basis](#2---lab-data-on-a-per-plot--pot-basis)
- [3 - Lab data to get water content needed for
  PMN.](#3---lab-data-to-get-water-content-needed-for-pmn)
- [4 - Lab data to get water content and fresh weight for
  PNR](#4---lab-data-to-get-water-content-and-fresh-weight-for-pnr)
- [5 - Correspondence between biol_unit_nb, cs, soil and
  bloc](#5---correspondence-between-biol_unit_nb-cs-soil-and-bloc)
- [6 - Export](#6---export)

# To Do

# Set up

<details class="code-fold">
<summary>Code</summary>

``` r
rm(list = ls())

library(tidyverse)
library(janitor)
```

</details>

# 1 - Import raw data

<details class="code-fold">
<summary>Code</summary>

``` r
# import other "wet lab" raw data
raw_data <- read_csv(
  "../raw_data/2024_raw.csv", show_col_types = TRUE,
  col_types = list(
    Soil = col_factor(),
    crop_diversity = col_factor(),
    CS = col_factor(),
    bloc = col_factor()
  ),
  na = c("", "NA", "Na")
  ) |> clean_names() |> 
  rename(sample_short = short) 
```

</details>

    New names:
    Rows: 660 Columns: 173
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (28): Expe, short, CRA_trial, SdC, sampling_time, zone, incub_time, Res... dbl
    (139): Biol_unit_Nb, WHC_Tare_tube_g, WHC_gFW_g, WHC_Tare_dish_g, WHC_gS... lgl
    (2): Yd_grain_W_unit, Yd_Comment fct (4): Soil, crop_diversity, CS, bloc
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...173`

<details class="code-fold">
<summary>Code</summary>

``` r
raw_data_field <- raw_data |> filter(expe == "Field")
raw_data_pot <- raw_data |> filter(expe == "Pot")
```

</details>

# 2 - Lab data on a per plot / pot basis

Field: biological units = plots of t2, with 3 zones per plot

Greenhouse: biological units = pots

<details class="code-fold">
<summary>Code</summary>

``` r
# Field, t2
raw_field_t2_lab <- raw_data_field |> 
  filter(sampling_time == "t2") |> 
  arrange(biol_unit_nb, zone) |> 
  # remove useless columns
  select(
    biol_unit_nb:sample_short, 
    soil:zone,
    sample_name, #useful?
   starts_with(c("whc", "run", "flush", "yd_rs", "rt"))
    )

# Greenhouse, t2
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
```

</details>

Check it out

<details class="code-fold">
<summary>Code</summary>

``` r
raw_field_t2_lab
```

</details>

    # A tibble: 80 × 53
       biol_unit_nb expe  sample_short soil  crop_diversity cs    bloc 
              <dbl> <chr> <chr>        <fct> <fct>          <fct> <fct>
     1           81 Field t2_81_z1     ABC   IC             IC    B2   
     2           81 Field t2_81_z2     ABC   IC             IC    B2   
     3           81 Field t2_81_z3     ABC   IC             IC    B2   
     4           82 Field t2_82_z1     ABC   SC             W     B2   
     5           82 Field t2_82_z2     ABC   SC             W     B2   
     6           82 Field t2_82_z3     ABC   SC             W     B2   
     7           83 Field t2_83_z1     ABC   SC             W     B3   
     8           83 Field t2_83_z2     ABC   SC             W     B3   
     9           83 Field t2_83_z3     ABC   SC             W     B3   
    10           84 Field t2_84_z1     ABC   IC             IC    B4   
    # ℹ 70 more rows
    # ℹ 46 more variables: sampling_time <chr>, zone <chr>, sample_name <chr>,
    #   whc_tare_tube_g <dbl>, whc_g_fw_g <dbl>, whc_tare_dish_g <dbl>,
    #   whc_g_sw_g <dbl>, whc_g_dw_g <dbl>, whc_comment <chr>, run_id_mr <chr>,
    #   flush_dm_tare_tr1 <dbl>, flush_dm_g_fw_tr1 <dbl>, flush_dm_g_dw_tr1 <dbl>,
    #   flush_dm_tare_tr2 <dbl>, flush_dm_g_fw_tr2 <dbl>, flush_dm_g_dw_tr2 <dbl>,
    #   flush_dm_tare_tr3 <dbl>, flush_dm_g_fw_tr3 <dbl>, …

<details class="code-fold">
<summary>Code</summary>

``` r
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

# 3 - Lab data to get water content needed for PMN.

Unfortunately, WC was not computed separately, but can only be derived
from the WHC manip.

! Field: Although WC was computed per bloc (4 blocs per soil), PMN was
done on a pooled sample from the 4 blocs. So we compute here only one
mean value of wc per soil, that is the mean of the 4 blocs.

! Pot: there was one pot per soil for PMN

<details class="code-fold">
<summary>Code</summary>

``` r
# For field
pmn_wc_field <- raw_data_field |> 
  select(biol_unit_nb, expe, soil, bloc, sampling_time, starts_with("whc")) |> 
  filter(sampling_time == "t0", !is.na(whc_tare_tube_g)) |> 
  #dm = g dry soil / g fresh soil       
  # wc = 1 - dm
  mutate(
    dm = (whc_g_dw_g - whc_tare_dish_g) / (whc_g_fw_g - whc_tare_tube_g),
    wc = 1-dm,
    .keep = "unused",
    .after = sampling_time
  ) |> 
  group_by(soil) |> 
  summarize(
    dm = mean(dm),
    wc = mean(wc)
  )

# For greenhouse
pmn_wc_pot <- raw_data_pot |> 
  select(biol_unit_nb, expe, soil, sampling_time, starts_with("whc")) |> 
  filter(sampling_time == "t0", !is.na(whc_tare_tube_g)) |> 
  #dm = g dry soil / g fresh soil       
  # wc = 1 - dm
  mutate(
    dm = (whc_g_dw_g - whc_tare_dish_g) / (whc_g_fw_g - whc_tare_tube_g),
    wc = 1-dm,
    .keep = "unused",
    .after = sampling_time
  ) |> 
  summarize(
    .by = "soil",
    dm = mean(dm),
    wc = mean(wc)
  ) |> 
  mutate(biol_unit_nb = paste0("Pot_", soil))
```

</details>

# 4 - Lab data to get water content and fresh weight for PNR

# 5 - Correspondence between biol_unit_nb, cs, soil and bloc

<details class="code-fold">
<summary>Code</summary>

``` r
cs_map_field <- raw_field_t2_lab |> 
  select(biol_unit_nb, cs, soil, bloc) |> 
  unique() 

cs_map_pot <- raw_greenhouse_t2 |> select(biol_unit_nb, cs, soil, bloc) |> unique()

cs_map_field; cs_map_pot
```

</details>

    # A tibble: 32 × 4
       biol_unit_nb cs    soil  bloc 
              <dbl> <fct> <fct> <fct>
     1           81 IC    ABC   B2   
     2           82 W     ABC   B2   
     3           83 W     ABC   B3   
     4           84 IC    ABC   B4   
     5           85 W     ABC   B4   
     6           86 IC    ABC   B3   
     7           87 IC    ABC   B1   
     8           88 W     ABC   B1   
     9           89 IC    Ref   B1   
    10           90 W     Ref   B2   
    # ℹ 22 more rows

    # A tibble: 88 × 4
       biol_unit_nb cs    soil  bloc 
              <dbl> <fct> <fct> <fct>
     1            1 F     Conv  B1   
     2            2 W     Conv  B1   
     3            3 IC    Conv  B1   
     4            5 F     Ref   B1   
     5            6 W     Ref   B1   
     6            7 IC    Ref   B1   
     7            9 F     Auto  B1   
     8           10 W     Auto  B1   
     9           11 IC    Auto  B1   
    10           13 F     ABC   B1   
    # ℹ 78 more rows

# 6 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
cs_map_field |> write_rds("output/data/1_field_cs_map.rds")
cs_map_pot |> write_rds("output/data/1_greenhouse_cs_map.rds")
pmn_wc_field |> write_rds("output/data/1_field_pmn_wc.rds")
pmn_wc_pot |> write_rds("output/data/1_greenhouse_pmn_wc.rds")
```

</details>

Safekeep from other scripts. Consider whether this export is needed
