unlink("C:/Users/kyle.olivencia/OneDrive - Florida Fish and Wildlife Conservation/Computer/Desktop/test_report", recursive=TRUE)

create_ltm_report <- function(dir_root = getwd(), dir_name = NA, report_name = NA) {
  
  pth <- paste(dir_root, dir_name, sep="/")
  
  if(!dir.exists(pth)) { 
    dir.create(pth)
  } else {
    cli::cli_alert_warning(c("Specified directory already exists",
                             " writing new files and folders to this",
                             " directory"))
  }
  
  if()
  # create subdirectories -----------------
  folders <- c("data", "figs", "tables", "R", "templates")
  
  for(item in folders) {
    dir.create(paste(pth, item, sep="/"))
  }
  
  # create quarto template -----
  quarto_file <- paste(pth, "/", report_name, ".qmd", sep="")
  analysis_file <- paste(pth, "/R/", report_name, ".R", sep="")
  
  file.create(quarto_file)
  
  ## add YAML -------
  cat(
    c("---",     
      "title: Annual Summary Form for LTM Systems",
      "format:",
      "  docx:",
      "    code-line-numbers: false",
      "    reference-doc: templates/custom-reference-doc.docx",
      "    echo: false",
      "  html:",
      "    code-fold: true",
      "    echo: true",
      "execute:",
      "  warning: false",
      "  cache: false",
      "  fig-dpi: 300",
      "crossref:",
      "  fig-prefix: Fig",
      "---"),
    sep = "\n",
    file = quarto_file
  )
  
  ## add setup r chunk ------
  cat(
    "\n",
    "```{r pkgs}",
    "#| cache: false",
    "suppressPackageStartupMessages(library(tidyverse))",
    "suppressPackageStartupMessages(library(flfishltm))",
    "theme_set(theme_classic()) # set figure theme",
    "str_lengthen <- function(x, nchar) {",
    "out <- stringr::str_c(x,\"                                  \")",
    "out <- stringr::str_trunc(out, nchar, \"right\")",
    "out",
    "}",
    "```",
    sep = "\n",
    append=TRUE,
    file=quarto_file
  )
  
  ## add analysis R code reference --------
  
  cat(
    "
```{r run-data-script}
#| output: false
source(\"",analysis_file,"\") # report analysis file
```", 
    sep="",
    append = TRUE,
    file = quarto_file)
  
  ## Add headings 
  cat("

| **Data Type** |
|:--------------|
| `r tg`        |
    
| **Waterbody** |
|:--------------|
| `r wb`        |
    
| **Sampling Start Date** | **Sampling End Date** |
|:------------------------|:----------------------|
| `r st`                  | `r ed`                |
",
      append=TRUE,
      file=quarto_file)
  
  
  ## add body text ---------------------------------------------------------------
  
  cat("
    
## Written Summary

Largemouth Bass *Micropterus salmoides* targeted sampling was conducted on `r lake` between `r text_start` and `r text_end`. `r n_tran` randomly selected transects were surveyed using a boat-mounted electrofishing unit according to FWC standard lentic protocols.

`r n_tot` Largemouth Bass were collected across all transects. Catch-per-unit-effort (CPUE) was `r cpue` ± `r cpue_se`, <!-- which is similar to the CPUE observed in spring 2021, and higher than all other post-drought (2016-2020) samples (@fig-cpue). The higher catch rates these past two years have been driven primarily by strong recruitment in 2020 and 2021 (@fig-cpue-size-1). CPUE of Largemouth Bass larger than 30 cm was within the normal ranges observed between 2007 and 2022, and is down from the peak observed in 2019 (@fig-cpue-size-1). CPUE of memorable and trophy sized Largemouth Bass (\\> 50 cm) is similar to that observed in 2020 and 2021, but is lower than observed during 2017-2019 (@fig-cpue-size-2).

There are noticeable peaks in the length distribution that likely correspond to age-1 (11-21 cm) and age-2 (22-36 cm) cohorts (@fig-length-freq). However, an age sample was not collected this year. --> The mean relative weight for the entire sample was `r round(mean(RelWt$relWt, na.rm = T),2)`%, <!-- which is slightly higher than the previous few years, but is within the normal ranges observed since 2007 (@fig-rw-1). Individuals less than 200 mm TL and greater than 400 mm TL tended to have higher relative weights than individuals between 300 and 400 mm TL (@fig-rw-2). --> The mean relative weight for Largemouth Bass greater than 30 cm was `r round(mean(RelWt30$relWt, na.rm = T),2)`%. Proportional stock density (PSD) was `r psd %>% round(2)*100`% and relative stock density-preferred was `r rsdp %>% round(2)*100`%, <!-- both of which are lower than in 2016-2020, but about the same as observed in 2021 (@fig-rsd).-->. 

<!--
Largemouth Bass *Micropterus salmoides* targeted sampling was conducted on `r lake` between `r text_start` and `r text_end`. `r n_tran` randomly selected transects were surveyed using a boat-mounted electrofishing unit according to FWC standard lentic protocols.

`r n_tot` Largemouth Bass were collected across all transects. Catch-per-unit-effort (CPUE) was `r cpue` ± `r cpue_se`, which is similar to the CPUE observed in spring 2021, and higher than all other post-drought (2016-2020) samples (@fig-cpue). The higher catch rates these past two years have been driven primarily by strong recruitment in 2020 and 2021 (@fig-cpue-size-1). CPUE of Largemouth Bass larger than 30 cm was within the normal ranges observed between 2007 and 2022, and is down from the peak observed in 2019 (@fig-cpue-size-1). CPUE of memorable and trophy sized Largemouth Bass (\\> 50 cm) is similar to that observed in 2020 and 2021, but is lower than observed during 2017-2019 (@fig-cpue-size-2).

There are noticeable peaks in the length distribution that likely correspond to age-1 (11-21 cm) and age-2 (22-36 cm) cohorts (@fig-length-freq). However, an age sample was not collected this year. The mean relative weight for the entire sample was `r round(mean(RelWt$relWt, na.rm = T),2)`%, which is slightly higher than the previous few years, but is within the normal ranges observed since 2007 (@fig-rw-1). Individuals less than 200 mm TL and greater than 400 mm TL tended to have higher relative weights than individuals between 300 and 400 mm TL (@fig-rw-2). The mean relative weight for Largemouth Bass greater than 30 cm was `r round(mean(RelWt30$relWt, na.rm = T),2)`%. Proportional stock density (PSD) was `r psd %>% round(2)*100`% and relative stock density-preferred was `r rsdp %>% round(2)*100`%, both of which are lower than in 2016-2020, but about the same as observed in 2021 (@fig-rsd).

Fish health codes were assigned to `r 100-fhc_nocode`% of the Largemouth Bass sampled. `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode == \"L\") %>% pull(n)` lesions and `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode == \"S\") %>% pull(n)` skeletal deformities were observed among the `r fhc_sum %>% filter(year == max(fhc_sum$year)) %>% summarize(n = sum(n)) %>% pull(n)` bass sampled.

-->
## Noteworthy Observations

<!--
-   High catch rates of age-1 Largemouth Bass (\\< 280 mm) the past two years suggest that large year-classes were produced in 2020 and 2021.

-   Very few Largemouth Bass were observed with any major health concerns. Of those that did, 3 had lesions, and 2 had skeletal deformities primarily affecting the dorsal fin.

-   1 trophy Largemouth Bass was collected and tagged during sampling (Trophy 2005: 4546g/9 lbs 15 oz).
-->

## Influential Factors

<!--

-   Submerged aquatic vegetation coverage, dominated by *Hydrilla sp.*, coverage has expanded in recent years and was above the long-term average in 2021 (@fig-sav). *Hydrilla sp.* coverage was particularly high in Macintosh Bay and in southern portions of the lake near Peegee Run. High density *Hydrilla sp.* restricted our ability to sample near-shore habitats in some areas.

-->

## Deviations

<!--

-   Thirteen sites were samples at 120 pps, due to issues reaching target amps at 60 pps.

-->    
    
    ",
      append = TRUE,
      file = quarto_file)
  
  ## add figures -----------------------------------------------------------------
  
  ### cpue-fig -------------------------------------------------------------------
  
  cat("
```{r cpue-fig}
#| label: fig-cpue
#| fig-cap: Largemouth Bass *Micropterus salmoides* catch-per-unit-effort during standardized targeted electrofishing surveys conducted during spring on Orange Lake, FL. Standard surveys were not conducted between 2012 and 2015 due to low water conditions. {{< pagebreak >}}
#| output: true

cpue_plot <- flfishltm::cpue.plot(dsum, 
                                  c(\"LMB\"), 
                                  years = c(min(dat$year):max(dat$year)), fig_scale = 5)

```
    ",
      append = TRUE,
      file = quarto_file)
  
  ### cpue-fig-size-------------------------------------------------------------
  
  cat("
```{r cpue-fig-size}
#| label: fig-cpue-size
#| fig-cap:
#|   - \"Largemouth Bass *Micropterus salmoides* catch-per-unit-effort by size class during standardized targeted electrofishing surveys conducted during spring on Orange Lake, FL. Standard surveys were not conducted between 2012 and 2015 due to low water conditions. Largemouth Bass less than 280 mm TL were assumed to be Age-1 based on previous age samples conducted at Orange Lake. {{< pagebreak >}}\"
#|   - \"Catch-per-minute of memorable and trophy sized Largemouth Bass *Micropterus salmoides* (> 500 mm TL) during standardized targeted electrofishing surveys conducted during spring on Orange Lake, FL. Standard surveys were not conducted between 2012 and 2015 due to low water conditions.{{< pagebreak >}}\" 
#| output: true

cpue_plot <- 
  flfishltm::cpue.plot(dsum, c(\"LMB\"),
                       years = c(min(dat$year): max(dat$year)),
                       species_size_strata = list(
                         LMB = list(
                           \"Age-1\" = c(0,28),
                           \'12\" +' = c(29,100)
                       )),
                       fig_scale = 5)

trophy_plot <- 
  flfishltm::cpue.plot(dsum, c(\"LMB\"),
                       years = c(min(dat$year): max(dat$year)),
                       species_size_strata = list(LMB = list(
                         \"Mem-Trophy\" = c(50,100))),
                       fig_scale = 5)

```
      ",
      append = TRUE,
      file = quarto_file)
  
  ### length-freq --------------------------------------------------------------
  cat("
```{r length-freq}
#| label: fig-length-freq
#| fig-cap: Length distribution of Largemouth Bass *Micropterus salmoides* (LMB) collected during standardized targeted electrofishing surveys conducted during spring on Orange Lake, FL. {{< pagebreak >}}
#| output: true
#| fig-height: 8
#| fig-width: 4

cpue_plot <- flfishltm::len.dist(dsum, c(\"LMB\"), years = c((max(dat$year)-3):max(dat$year)), fig_scale = 5)

```
      ",
      append = TRUE,
      file = quarto_file)
  
  ### rel-wt -------------------------------------------------------------------
  cat("
```{r rel-wt}
#| label: fig-rw
#| fig-cap: !expr 'c(\"Largemouth Bass relative weight values from annual spring samples at Orange Lake, FL. Relative weights calculated assuming a = -5.47045, b = 3.24105. Area shaded red represents 95% confidence interval. Standard surveys were not conducted between 2012 and 2015 due to low water conditions. {{< pagebreak >}}\",
#|  paste(\"Relative weight by total length of Largemouth Bass collected via standardized electrofishing on Orange Lake, FL in March\", yr, \". The blue line is a LOESS smoothing spline. Area shaded grey represents the 95% confidence interval.\"))'
#| output: true
## Relative weight values obtained from \"FishLWCalculator.xlsx\"

ggplot(data = RelWt_sum, aes(x = year, y = Wr)) +
  geom_point() +
  geom_line() +
  geom_ribbon(
    aes(ymin = Wr - (2*Wrse), ymax = Wr + (2*Wrse)),
    fill = \"red\",
    alpha = 0.2) +
  scale_x_continuous(breaks = c(min(RelWt_sum$year):max(RelWt_sum$year))) +
  coord_cartesian(ylim = c(90,110)) + 
  theme_angle_text()

ggplot(data = RelWt %>% filter(year == yr, ID != \"ltm24913\"), 
       aes(x = (TotalLength/10), y = relWt)) +
  geom_point(color = \"blue\", alpha = 0.4) +
  geom_smooth(method = \"loess\", fill = \"light gray\") + 
  labs(x = \"Total Length (cm) \",
       y = \"Relative Weight (%)\") 
```",
      append = TRUE,
      file = quarto_file)
  
  ### rsd-plot -----------------------------------------------------------------
  cat("
```{r rsd-plot}
#| label: fig-rsd
#| fig-cap: Proportional stock density and relative stock density (preferred) of Largemouth Bass *Micropterus salmoides* at Orange Lake, FL from spring electrofishing samples. Standard surveys were not conducted between 2012 and 2015 due to low water conditions. {{< pagebreak >}}
#| output: true

print(PSD_plot)

```",
      append = TRUE,
      file = quarto_file)
  
  ### sav plot -----------------------------------------------------------------
  
  cat("
![Submersed vegetation areal coverage (%) that was visually estimated during Spring LTM Largemouth Bass *Micropterus salmoides* electrofishing on Orange Lake from 2006 to 2021. No samples during 2012-2015 due to low water conditions.](figs/sav.png){#fig-sav}",
      append = TRUE,
      file = quarto_file)
  
  
  # create R template --------
  file.create(analysis_file)
  cat(create_ltm_analysis_template(), file = analysis_file)
}


# TEST -------------------------------------------------------------------------

create_ltm_report("C:/Users/kyle.olivencia/OneDrive - Florida Fish and Wildlife Conservation/Computer/Desktop", "test_report", "kyles_report")




