# 1 - CRA-W data


- [To Do](#to-do)
- [Set up](#set-up)
- [1 - Import & tidy data from the
  CRA-W](#1---import--tidy-data-from-the-cra-w)
  - [1.1 - Yield data](#11---yield-data)
  - [1.2 - Soil data: TO DO](#12---soil-data-to-do)
  - [1.3 - Technical itinerary: TO DO](#13---technical-itinerary-to-do)
- [2 - Export](#2---export)

# To Do

Still missing:

- soil CRA

# Set up

<details class="code-fold">
<summary>Code</summary>

``` r
rm(list = ls())

library(plate2N) # for remove_wells
library(tidyverse)
library(janitor)
library(roperators) # for %ni%
library(ggrepel) # for geom_text_repel()
library(ggridges) # for geom_density_ridges()
library(patchwork) # for the "+" layout and plot_layout()

# functions
source("functions/plot_qc_sample_conc.R")
```

</details>

# 1 - Import & tidy data from the CRA-W

## 1.1 - Yield data

In the row data, there is a column under the headers containing units.
So we skip this in the import, re-adding column names

<details class="code-fold">
<summary>Code</summary>

``` r
colnames <- read_csv("../raw_data/SYCBIO_Froment-feverole_2024_MDeToeuf.csv",n_max = 0) |> clean_names() |> names()
```

</details>

    New names:
    Rows: 0 Columns: 21
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (21): N°, Année, SdC, Culture, Code parcelle, Code placeau, Date de réco...
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...15`
    • `` -> `...16`
    • `` -> `...17`

<details class="code-fold">
<summary>Code</summary>

``` r
yield_raw <- read_csv(
  "../raw_data/SYCBIO_Froment-feverole_2024_MDeToeuf.csv", skip = 2, col_names = colnames, show_col_types = FALSE)
```

</details>

Now we tidy this data set:

- rename –\> translate headers + add unit

- Remove empty rows (missing IC samples in Ref soil)

- Recompute (check) computed values

  - For yield: from kg / plot to t/ha :

    - /1000 : kg –\> t

    - X 10000 : m2 –\> ha (in denominator)

    - /(10 X plot_width) : nb of square meters per plot (width x 10m of
      harvesting)

- (then keep only recomputed ones, remove original raw)

<details class="code-fold">
<summary>Code</summary>

``` r
yield_full <- yield_raw |> 
  rename(
    year = annee,
    crop = culture,
    biol_unit_nb = code_placeau,
    harvest_date = date_de_recolte,
    yield_per_plot = rdt_brut_parcelle,
    harvester_width_m = largeur_de_mesure,
    water_content_1_percent = humidite_1,
    water_content_2_percent = humidite_2,
    test_weight_1_kg_per_hl = ps_1,
    test_weight_2_kg_per_hl = ps_2,
    protein_percent_dw = mpt_nx5_7
  ) |> 
  filter_out(is.na(yield_per_plot)) |> 
  rowwise() |> 
  mutate(
    soil = case_when(sd_c == 3 ~ "ABC", sd_c == 2 ~ "Auto", sd_c == 1 ~ "Ref", .default = "which_soil?" ),
    crop = case_when(crop == "Froment" ~ "Wheat", crop == "Féverole" ~ "Faba bean", .default = "which crop?"),
    test_weight_kg_per_hl = mean(c(test_weight_1_kg_per_hl, test_weight_2_kg_per_hl), na.rm = TRUE),
    water_content_percent = mean(c(water_content_1_percent, water_content_2_percent), na.rm = TRUE),
    grain_yield_t_per_ha = yield_per_plot/harvester_width_m,
    grain_yield_15p_t_per_ha = grain_yield_t_per_ha * (100 - water_content_percent) / 85) 

# Check computed values
```

</details>

Then check re-computed values: all rounding errors. Still, prefer to
compute it here

<details class="code-fold">
<summary>Code</summary>

``` r
# Water content
sum(yield_full$hum_moy != yield_full$water_content_percent)
```

</details>

    [1] 2

<details class="code-fold">
<summary>Code</summary>

``` r
yield_full |> filter(hum_moy != water_content_percent) |> select(biol_unit_nb, hum_moy, water_content_percent)
```

</details>

    # A tibble: 2 × 3
    # Rowwise: 
      biol_unit_nb hum_moy water_content_percent
             <dbl>   <dbl>                 <dbl>
    1           90    14.4                  14.4
    2           95    15.1                  15.0

<details class="code-fold">
<summary>Code</summary>

``` r
# test weight
sum(yield_full$ps_moy != yield_full$test_weight_kg_per_hl)
```

</details>

    [1] 7

<details class="code-fold">
<summary>Code</summary>

``` r
yield_full |> filter(ps_moy != test_weight_kg_per_hl) |> select(biol_unit_nb, ps_moy, test_weight_kg_per_hl)
```

</details>

    # A tibble: 7 × 3
    # Rowwise: 
      biol_unit_nb ps_moy test_weight_kg_per_hl
             <dbl>  <dbl>                 <dbl>
    1           89   65.3                  65.3
    2           90   76.9                  76.8
    3           91   75.9                  75.8
    4           92   80.3                  80.2
    5           93   77.4                  77.4
    6           94   73.2                  73.2
    7           96   79.7                  79.6

<details class="code-fold">
<summary>Code</summary>

``` r
# yield in tons per ha
sum(yield_full$rdt_brut_ha != yield_full$grain_yield_t_per_ha)
```

</details>

    [1] 24

<details class="code-fold">
<summary>Code</summary>

``` r
yield_full |> filter(rdt_brut_ha != grain_yield_t_per_ha) |> select(biol_unit_nb, rdt_brut_ha, grain_yield_t_per_ha)
```

</details>

    # A tibble: 24 × 3
    # Rowwise: 
       biol_unit_nb rdt_brut_ha grain_yield_t_per_ha
              <dbl>       <dbl>                <dbl>
     1           81        1.01                1.01 
     2           84        1.07                1.07 
     3           85        0.95                0.947
     4           86        0.97                0.967
     5           87        1.11                1.11 
     6           88        1.01                1.01 
     7           81        0.57                0.573
     8           84        0.35                0.353
     9           86        0.31                0.307
    10           87        0.56                0.56 
    # ℹ 14 more rows

<details class="code-fold">
<summary>Code</summary>

``` r
#yield 15% humidity
sum(yield_full$rdt_15_percent_ha != yield_full$grain_yield_15p_t_per_ha)
```

</details>

    [1] 32

<details class="code-fold">
<summary>Code</summary>

``` r
yield_full |> filter(rdt_15_percent_ha != grain_yield_15p_t_per_ha) |> select(biol_unit_nb, rdt_15_percent_ha, grain_yield_15p_t_per_ha)
```

</details>

    # A tibble: 32 × 3
    # Rowwise: 
       biol_unit_nb rdt_15_percent_ha grain_yield_15p_t_per_ha
              <dbl>             <dbl>                    <dbl>
     1           81              1                       1.00 
     2           82              1.07                    1.07 
     3           83              0.93                    0.927
     4           84              1.05                    1.05 
     5           85              0.93                    0.927
     6           86              0.95                    0.950
     7           87              1.11                    1.11 
     8           88              0.99                    0.992
     9           81              0.55                    0.554
    10           84              0.34                    0.343
    # ℹ 22 more rows

Now, keep only useful columns

<details class="code-fold">
<summary>Code</summary>

``` r
yield_tidy <- yield_full |> 
  select(
    year, crop, soil, biol_unit_nb, harvest_date, 
    grain_yield_15p_t_per_ha, protein_percent_dw,
    grain_yield_t_per_ha, water_content_percent, test_weight_kg_per_hl)
```

</details>

## 1.2 - Soil data: TO DO

## 1.3 - Technical itinerary: TO DO

# 2 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
yield_tidy |> write_rds("output/data/1_field_yield_clean.rds")
```

</details>
