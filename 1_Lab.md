# 1 Wet-lab data
Morgane de Toeuf

- [To Do](#to-do)
- [Set up](#set-up)
- [1 - Import raw data](#1---import-raw-data)
- [2 - Extract data for various
  pipelines](#2---extract-data-for-various-pipelines)
  - [2.1 - Water content for PMN](#21---water-content-for-pmn)
  - [2.2 - Field data vs greenhouse
    data](#22---field-data-vs-greenhouse-data)
  - [2.3 - Data on a per plot / pot
    basis](#23---data-on-a-per-plot--pot-basis)
    - [2.1.1 - Yield data](#211---yield-data)
    - [2.1.2 - Flush data (from K2SO4 extraction) for
      Npools](#212---flush-data-from-k2so4-extraction-for-npools)
  - [2.4 - Correspondence table: biol_unit_nb, cs, soil and
    bloc](#24---correspondence-table-biol_unit_nb-cs-soil-and-bloc)
  - [2.5 - Water content and fresh weight for
    PNR](#25---water-content-and-fresh-weight-for-pnr)
- [3 - Compute new variables](#3---compute-new-variables)
  - [3.1 - Dry matter content from flush data for
    Npools](#31---dry-matter-content-from-flush-data-for-npools)
    - [3.1.1 - Principle and equations](#311---principle-and-equations)
    - [3.1.2 - Pivot data so that technical replicates are above each
      other, not next to each
      other](#312---pivot-data-so-that-technical-replicates-are-above-each-other-not-next-to-each-other)
  - [3.2 - Develop yield data
    (greenhouse)](#32---develop-yield-data-greenhouse)
    - [3.2.1 - Human error check](#321---human-error-check)
    - [3.2.2 - Compute per plant clean yield
      data](#322---compute-per-plant-clean-yield-data)
- [Export](#export)

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

# 2 - Extract data for various pipelines

## 2.1 - Water content for PMN

Unfortunately, WC was not computed separately, but can only be derived
from the WHC manip.

! Field: Although WC was computed per bloc (4 blocs per soil), PMN was
done on a pooled sample from the 4 blocs. So we compute here only one
mean value of wc per soil, that is the mean of the 4 blocs.

! Pot: there was one pot per soil for PMN

<details class="code-fold">
<summary>Code</summary>

``` r
pmn_wc <- raw_data |> 
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
    .by = c("expe", "soil"),
    dm = mean(dm),
    wc = mean(wc)
  )

# For field
pmn_wc_field <- pmn_wc |>  filter(expe == "Field") 

# For greenhouse
pmn_wc_pot <- pmn_wc |> 
  filter(expe == "Pot") |> 
  mutate(biol_unit_nb = paste0(expe, soil))
```

</details>

## 2.2 - Field data vs greenhouse data

<details class="code-fold">
<summary>Code</summary>

``` r
# raw_data_field <- raw_data |> filter(expe == "Field")
# raw_data_pot <- raw_data |> filter(expe == "Pot")
```

</details>

## 2.3 - Data on a per plot / pot basis

Field: biological units = plots of t2, with 3 zones per plot

Greenhouse: biological units = pots

<details class="code-fold">
<summary>Code</summary>

``` r
raw_t2_lab <- raw_data |> 
  filter(sampling_time == "t2") |> 
  # remove bare soil
  filter_out(cs == "B") |> 
  # remove bloc B1 from field data
  filter_out(expe == "Field" & bloc == "B1") |> 
  # correct biological unit
  mutate(biol_unit_nb = case_when(
    biol_unit_nb < 200 ~ biol_unit_nb,
    biol_unit_nb > 200 ~ biol_unit_nb - 200
  )) |> 
  arrange(biol_unit_nb, zone) |> 
  # remove useless columns
  select(
    biol_unit_nb:sample_short, 
    soil:zone,
    sample_name, #useful?
   starts_with(c("whc", "run", "flush", "yd_rs", "rt"))
    )
# 
# raw_field_t2_lab <- raw_t2_lab |> filter(expe == "Field")
# raw_greenhouse_t2_lab <- raw_t2_lab |> filter(expe == "Pot")
# 
# # Field, t2
# raw_field_t2_lab <- raw_data_field |> 
#   filter(sampling_time == "t2") |> 
#   arrange(biol_unit_nb, zone) |> 
#   # remove useless columns
#   select(
#     biol_unit_nb:sample_short, 
#     soil:zone,
#     sample_name, #useful?
#    starts_with(c("whc", "run", "flush", "yd_rs", "rt"))
#     )

# Greenhouse, t2
# raw_greenhouse_t2_lab <- raw_data_pot |> 
#   filter(sampling_time == "t2") |> 
#   # remove bare soil
#   filter_out(cs == "B") |> 
#   # correct biological unit
#   mutate(biol_unit_nb = case_when(
#     biol_unit_nb < 200 ~ biol_unit_nb,
#     biol_unit_nb > 200 ~ biol_unit_nb - 200
#   )) |> 
#   arrange(biol_unit_nb) |> 
#   # remove useless columns
#   select(
#     biol_unit_nb:sampling_time, 
#     sample_name, #useful?
#    starts_with(c("whc", "flush", "yd_rs"))
#     )
```

</details>

Check it out

<details class="code-fold">
<summary>Code</summary>

``` r
# 
# raw_field_t2_lab
# raw_greenhouse_t2_lab
```

</details>

### 2.1.1 - Yield data

<details class="code-fold">
<summary>Code</summary>

``` r
# Greenhouse
yield_data_greenhouse <- raw_t2_lab |> 
  filter(expe == "Pot") |> 
  select(biol_unit_nb:sampling_time, starts_with("yd")) 
```

</details>

### 2.1.2 - Flush data (from K2SO4 extraction) for Npools

<details class="code-fold">
<summary>Code</summary>

``` r
raw_flush <- raw_t2_lab |> 
  # remove clutter
  select(!starts_with(c("yd", "whc", "rt")) & !run_id_mr)

# raw_flush_field <- raw_flush |> filter(expe == "Field")
# raw_flush_greenhouse <- raw_flush |> filter(expe == "Pot")

# Field
# raw_flush_field <- raw_field_t2_lab |> 
#   select(!starts_with(c("yd", "whc", "rt")) & !run_id_mr) |> 
#   # remove bloc 1
#   filter_out(bloc == "B1")

# Greenhouse
# raw_flush_greenhouse <- raw_greenhouse_t2_lab |> 
#   select(
#     !starts_with(c("yd", "whc")) &
#       !cra_trial:sd_c)
```

</details>

## 2.4 - Correspondence table: biol_unit_nb, cs, soil and bloc

<details class="code-fold">
<summary>Code</summary>

``` r
cs_map <- raw_t2_lab |> 
  select(expe, biol_unit_nb, cs, soil, bloc) |> 
  unique() 

# cs_map_field <- cs_map |> filter(expe == "Field")
# cs_map_pot <- cs_map |> filter(expe == "Pot")

# Field
# cs_map_field <- raw_field_t2_lab |> 
#   select(biol_unit_nb, cs, soil, bloc) |> 
#   unique() 
# 
# # Greenhouse
# cs_map_pot <- raw_greenhouse_t2_lab |> 
#   select(biol_unit_nb, cs, soil, bloc) |> 
#   unique()
# 
# cs_map_field; cs_map_pot
```

</details>

## 2.5 - Water content and fresh weight for PNR

# 3 - Compute new variables

## 3.1 - Dry matter content from flush data for Npools

### 3.1.1 - Principle and equations

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

### 3.1.2 - Pivot data so that technical replicates are above each other, not next to each other

First, we select relevant columns and pivot to get

- one column respectively for tare, gross fresh weight (g_fw) and gross
  dry weight (g_dw)

- 1 row per technical replicate

<details class="code-fold">
<summary>Code</summary>

``` r
raw_subset_wc <- raw_flush |> 
  select(expe, sample_short, flush_dm_tare_tr1:flush_dm_g_dw_tr3) |> 
  pivot_longer(
    cols = !c(expe, sample_short),
    names_pattern = "flush_dm_(g_fw|g_dw|tare)_tr(\\d)",
    names_to = c(".value", "tech_rep"),
    values_to = "weight") 

# raw_subset_wc_field <- raw_subset_flush |> filter(expe == "Field")
# raw_subset_wc_greenhouse <- raw_subset_flush |> filter(expe == "Pot")
# 

#Field
# raw_subset_wc_field <- raw_flush_field |> 
#   select(sample_short, flush_dm_tare_tr1:flush_dm_g_dw_tr3) |> 
#   pivot_longer(
#     cols = !sample_short,
#     names_pattern = "flush_dm_(g_fw|g_dw|tare)_tr(\\d)",
#     names_to = c(".value", "tech_rep"),
#     values_to = "weight") 
# 
# # check it out
# raw_subset_wc_field 
# 
# # Greenhouse
# raw_subset_wc_greenhouse <- raw_flush_greenhouse |> 
#   select(sample_short, flush_dm_tare_tr1:flush_dm_g_dw_tr3) |> 
#   pivot_longer(
#     cols = !sample_short,
#     names_pattern = "flush_dm_(g_fw|g_dw|tare)_tr(\\d)",
#     names_to = c(".value", "tech_rep"),
#     values_to = "weight") 
# 
# # check it out
# raw_subset_wc_greenhouse
```

</details>

Then, we

- compute the neat fresh weight and dry weight for each row, and the dry
  matter content and water content

- Compute the average per sample (over the technical triplicate)

<details class="code-fold">
<summary>Code</summary>

``` r
# To simplify computation, bind both data
# raw_subset_wc <- bind_rows(
#   raw_subset_wc_field |> mutate(expe = "Field"),
#   raw_subset_wc_greenhouse |> mutate(expe = "Greenhouse")
# )

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
    .by = c(expe, sample_short),
    dm = mean(dm),
    wc = mean(wc)
  ) 

# wc_field <- wc |> filter(expe == "Field")
# 
# wc_greenhouse <- wc |> filter(expe == "Pot")
# 
# # check it out
# wc_field 
# wc_greenhouse 
```

</details>

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

    Joining with `by = join_by(expe, sample_short)`

<details class="code-fold">
<summary>Code</summary>

``` r
#flush_clean_field <- flush_clean |> filter(expe == "Field")
#flush_clean_greenhouse <- flush_clean |> filter(expe == "Pot")

# flush_clean_field <- raw_flush_field |> 
#   select(
#     !flush_dm_tare_tr1:flush_dm_g_dw_tr3 & 
#       !ends_with("comment")) |> 
#   left_join(wc_field) |> 
#   mutate(
#     ratio_nf = flush_fw_nf_g / vol_extr,
#     ratio_cfe = flush_fw_cfe_g / vol_extr,
#     .keep = "unused") 
# 
# flush_clean_greenhouse <- raw_flush_greenhouse |> 
#   select(
#     !flush_dm_tare_tr1:flush_dm_g_dw_tr3 & 
#       !ends_with("comment")) |> 
#   left_join(wc_greenhouse) |> 
#   mutate(
#     ratio_nf = flush_fw_nf_g / vol_extr,
#     ratio_cfe = flush_fw_cfe_g / vol_extr,
#     .keep = "unused") 
 
# Check it out
# flush_clean_field
# flush_clean_greenhouse
```

</details>

Now, we divide the data in 2 subsets: sample data and standard soil data
(several reps, messes up the analysis (for now))

<u>**!! TODO: decide if export sample & std grouped or separated**</u>

<details class="code-fold">
<summary>Code</summary>

``` r
flush_sample_greenhouse <- flush_clean |> filter(biol_unit_nb < 100 & expe == "Pot")
flush_std_greenhouse <- flush_clean |> filter(biol_unit_nb >= 100 & expe == "Pot")

flush_sample_field <- flush_clean |> filter(biol_unit_nb < 112 & expe == "Field")
flush_std_field <- flush_clean |> filter(biol_unit_nb >= 112 & expe == "Field")
```

</details>

## 3.2 - Develop yield data (greenhouse)

The per-plant yield data for the field was deemed not trustworthy. In
this section, there is thus only greenhouse yield data. Harvest yield
data for the field was imported and developed in the pipeline
`1_CRA_data`.

### 3.2.1 - Human error check

First, we compute the number of plants of each species in each pot

<details class="code-fold">
<summary>Code</summary>

``` r
nb_plant <- yield_data_greenhouse |> 
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
    ))
```

</details>

First, we check the comments written during experimentation

<details class="code-fold">
<summary>Code</summary>

``` r
nb_plant |> 
  select(biol_unit_nb, soil, cs, crop_diversity, nb_w, nb_fb, yd_rs_comment) |> 
  filter(!is.na(yd_rs_comment)) |> print(width = 110)
```

</details>

    # A tibble: 5 × 7
    # Rowwise: 
      biol_unit_nb soil  cs    crop_diversity  nb_w nb_fb
             <dbl> <fct> <fct> <fct>          <dbl> <dbl>
    1           14 ABC   W     SC                 2    NA
    2           19 Conv  IC    IC                 1     1
    3           27 Auto  IC    IC                 2    NA
    4           30 ABC   W     SC                 2    NA
    5           55 Ref   IC    IC                NA     2
      yd_rs_comment                                                                 
      <chr>                                                                         
    1 1 seule vraie plante. Autre toute sèche (donc feuille pas mesurée) --> check …
    2 1 féverole supplémentaire dont on n'a pas tenu compte (non prélevée)          
    3 pas de féverole mais 2 froments --> pas IC, un SC_F supplémentaire --> change…
    4 trouvé nodules dans racines --> mauvaise herbe légumineuse. Léger IC quand mê…
    5 2 féveroles, pas de froment. Colonnes à corriger                              

And we correct the data accordingly

- pot \#14: change nb of wheat plants to 1

- pot \#19: on n’en fait rien de cette info…?

- Pot \#27: Changer IC en W pour cs, et IC en SC pour crop_diversity

- Pot \#30: on ne fait rien de cette info, mais si nb est outlier, on
  sait pourquoi…

- Pot \#55: changer IC en F (cs) et en SC (crop_diversity)

<details class="code-fold">
<summary>Code</summary>

``` r
nb_plant_clean <- nb_plant |> 
  mutate(
    nb_w = case_when(biol_unit_nb == 14 ~ 1, .default = nb_w),
    cs = case_when(
      biol_unit_nb == 27 ~ "W",
      biol_unit_nb == 55 ~ "F",
      .default = cs
    ),
    crop_diversity = case_when(
      biol_unit_nb %in% c(27, 55) ~ "SC",
      .default = crop_diversity
    )
  )
```

</details>

Check the modifications

<details class="code-fold">
<summary>Code</summary>

``` r
nb_plant_clean |> 
  select(biol_unit_nb, soil, cs, crop_diversity, nb_w, nb_fb, yd_rs_comment) |> 
  filter(!is.na(yd_rs_comment)) |> print(width = 110)
```

</details>

    # A tibble: 5 × 7
    # Rowwise: 
      biol_unit_nb soil  cs    crop_diversity  nb_w nb_fb
             <dbl> <fct> <chr> <chr>          <dbl> <dbl>
    1           14 ABC   W     SC                 1    NA
    2           19 Conv  IC    IC                 1     1
    3           27 Auto  W     SC                 2    NA
    4           30 ABC   W     SC                 2    NA
    5           55 Ref   F     SC                NA     2
      yd_rs_comment                                                                 
      <chr>                                                                         
    1 1 seule vraie plante. Autre toute sèche (donc feuille pas mesurée) --> check …
    2 1 féverole supplémentaire dont on n'a pas tenu compte (non prélevée)          
    3 pas de féverole mais 2 froments --> pas IC, un SC_F supplémentaire --> change…
    4 trouvé nodules dans racines --> mauvaise herbe légumineuse. Léger IC quand mê…
    5 2 féveroles, pas de froment. Colonnes à corriger                              

All good.

### 3.2.2 - Compute per plant clean yield data

Compute per plant versions of weight, plant height and stem/till number

<details class="code-fold">
<summary>Code</summary>

``` r
yield_per_plant <- nb_plant_clean |> 
  rowwise() |> 
  mutate(
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
yield_per_plant
```

</details>

    # A tibble: 88 × 21
    # Rowwise: 
       biol_unit_nb expe  sample_short soil  crop_diversity cs    bloc 
              <dbl> <chr> <chr>        <fct> <chr>          <chr> <fct>
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
    # ℹ 14 more variables: sampling_time <chr>, yd_rs_comment <chr>,
    #   height_per_fb <dbl>, stem_per_fb <dbl>, height_per_w <dbl>,
    #   till_per_w <dbl>, fw_fb_per_pot <dbl>, fw_w_per_pot <dbl>,
    #   dw_fb_per_pot <dbl>, dw_w_per_pot <dbl>, fw_fb_per_plant <dbl>,
    #   fw_w_per_plant <dbl>, dw_fb_per_plant <dbl>, dw_w_per_plant <dbl>

Save it in clean data set to join and export

<details class="code-fold">
<summary>Code</summary>

``` r
yield_clean_greenhouse <- yield_per_plant
```

</details>

# Export

<details class="code-fold">
<summary>Code</summary>

``` r
cs_map_field <- cs_map |> filter(expe == "Field")
cs_map_pot <- cs_map |> filter(expe == "Pot")
cs_map_field |> write_rds("output/data/1_field_cs_map.rds")
cs_map_pot |> write_rds("output/data/1_greenhouse_cs_map.rds")

pmn_wc_field |> write_rds("output/data/1_field_pmn_wc.rds")
pmn_wc_pot |> write_rds("output/data/1_greenhouse_pmn_wc.rds")

flush_clean_field <- flush_clean |> filter(expe == "Field")
flush_clean_greenhouse <- flush_clean |> filter(expe == "Pot")
flush_clean_greenhouse |>  write_rds("output/data/1_greenhouse_flush_clean.rds")
flush_clean_field |>  write_rds("output/data/1_field_flush_clean.rds")
 # or flush_sample_greenhouse # and flush_std_greenhouse 

yield_clean_greenhouse |> write_rds("output/data/1_greenhouse_yield_clean.rds")
```

</details>

Safekeep from other scripts. Consider whether this export is needed
