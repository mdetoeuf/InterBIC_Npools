# IV. Data Transformation


- [Set up](#set-up)
- [1 - Nmin](#1---nmin)
  - [1.1 - Import tidy data set](#11---import-tidy-data-set)
  - [1.2 - Compute new variables](#12---compute-new-variables)
    - [1.2.1 - dry matter content](#121---dry-matter-content)
  - [1.3 - Export](#13---export)
- [2 - TDN](#2---tdn)
  - [2.1 - Import tidy data set](#21---import-tidy-data-set)

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

# 1 - Nmin

## 1.1 - Import tidy data set

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin <- read_rds("output/data/Nmin_tidy.rds") |> 
  select(
    !starts_with(c("yd", "whc", "nod", "pmn", "dna", "zone", "incub")))
#Nmin |> str()
```

</details>

## 1.2 - Compute new variables

### 1.2.1 - dry matter content

In this data set, dry matter and water content have not yet been
computed. The raw data with 3 technical replicates is still encoded, in
separate columns `sse_dm...`\`. In this section, we compute average dry
matter and water content for the sample (across technical replicates).
We add additional columns with intermediary results:

- FW<sub>neat</sub> = FW<sub>gross</sub> - tare; DW<sub>neat</sub> =
  DW<sub>gross</sub> - tare

- average (FW, then DW) = sum(3 technical replicates) / 3

- DM = avg<sub>DW</sub> / avg<sub>FW</sub>

- WC = 1 - DM

- With

  - FW = fresh weight \[g\]

  - DW = dry weight \[g\],

  - DM = dry matter content \[g dry soil / g fresh soil\],

  - WC = water content \[g water / g <u>**fresh**</u> soil\]

- ratio = fresh wait of soil added into tubes for extraction (~10g) /
  volume of extractant added (=20ml)

Then, we can finally convert concentrations in mg/L into concentrations
in ppm (mg/kg dry soil)

- see <https://mycloud.ulb.be/index.php/f/30359374> for equation

<details class="code-fold">
<summary>Code</summary>

``` r
# Volume of extractant used in the K2SO4 extraction
vol_extr <- 20

# 
Nmin_tfo <- Nmin |> 
  # Compute dm
  mutate(
    # subtract tare from gross fresh weight
    flush_dm_neat_fw_tr1 = flush_dm_g_fw_tr1 - flush_dm_tare_tr1,
    flush_dm_neat_fw_tr2 = flush_dm_g_fw_tr2 - flush_dm_tare_tr2,
    flush_dm_neat_fw_tr3 = flush_dm_g_fw_tr3 - flush_dm_tare_tr3,
    # subtract tare from gross dry weight
    flush_dm_neat_dw_tr1 = flush_dm_g_dw_tr1 - flush_dm_tare_tr1,
    flush_dm_neat_dw_tr2 = flush_dm_g_dw_tr2 - flush_dm_tare_tr2,
    flush_dm_neat_dw_tr3 = flush_dm_g_dw_tr3 - flush_dm_tare_tr3,
    # compute average fresh and dry weight
    flush_dm_avg_fw = (
      flush_dm_neat_fw_tr1 + flush_dm_neat_fw_tr2 + flush_dm_neat_fw_tr3
      ) / 3,
    flush_dm_avg_dw = (
      flush_dm_neat_dw_tr1 + flush_dm_neat_dw_tr2 + flush_dm_neat_dw_tr3) / 3,
    # compute dry matter content
    dm = flush_dm_avg_dw / flush_dm_avg_fw,
    # compute water content
    wc = 1- dm,
    # compute each sample's individual ratio (~10g moist per 20 ml)
    ratio = flush_fw_nf_g / vol_extr,
    .keep = "unused"
  ) |> 
  # Convert concentrations in mg/L to mg/kg
  mutate(
    conc_N_ppm_rt1 = unlist(conc_N_Lrt1) / (ratio*dm),
    conc_N_ppm_rt2 = unlist(conc_N_Lrt2) / (ratio*dm),
    conc_N_ppm_rt3 = unlist(conc_N_Lrt3) / (ratio*dm),
    conc_N_ppm_rt4 = unlist(conc_N_Lrt4) / (ratio*dm)
    ) |> 
  rowwise() |> 
  # compute mean between technical replicates 
  mutate(
    conc_N_ppm_avg = mean(c_across(c("conc_N_ppm_rt1", "conc_N_ppm_rt2", "conc_N_ppm_rt3", "conc_N_ppm_rt4")))
  ) |> 
 # select(!starts_with("flush")) |> 
  select(
    expe, sample_short, sd_c, soil, crop_diversity, cs, bloc, sampling_time, 
    plate_id, N_sp, dataset, std_sp, 
    wc, dm, starts_with("conc")) |> 
  select(!contains("N_L"))
```

</details>

Then, we update format of variables

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_export <- Nmin_tfo |> 
  mutate(
    soil = factor(soil, levels = c("Conv", "Ref", "Auto", "ABC")),
    crop_diversity = factor(crop_diversity, levels = c("SC", "IC")),
    cs = factor(cs, levels = c("F", "IC", "W")),
    bloc = as.factor(bloc)
  )
```

</details>

## 1.3 - Export

<details class="code-fold">
<summary>Code</summary>

``` r
Nmin_export |> write_rds("output/data/4_Nmin_tfo.rds")
```

</details>

# 2 - TDN

## 2.1 - Import tidy data set

<details class="code-fold">
<summary>Code</summary>

``` r
TDN <- read_rds("output/data/TDN_tidy.rds")
```

</details>
