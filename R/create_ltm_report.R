
#' Create a new LTM report template
#'
#' @param dir_name *string* specifying the name of the report directory to create
#' @param report_name *string* specifying the report name, used in file names so avoid spaces
#'   defaults to name specified in directory 
#' @param dir_root *string* specifying the path to the root directory in which to create the report project. 
#'   Defaults to the current working directory.
#'
#' @return boolean - returns `TRUE` if the function executes without error
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' create_ltm_report(dir_name = "test_report", 
#'                   report_name ="LakeDoe_LMB_2020")
#' }
#' 
create_ltm_report <- function(dir_name, report_name = dir_name, dir_root = getwd()) {
  
  pth <- paste(dir_root, dir_name, sep="/")
  rel <- 
  if(!dir.exists(pth)) { 
    dir.create(pth)
  } else {
    cli::cli_alert_warning(c("Specified directory already exists",
                             " writing new files and folders to this",
                             " directory"))
  }
  
  if(length(list.files(pth, pattern = "*.Rproj")) == 0) {
    rstudioapi::initializeProject(pth)
  }
  
  # create subdirectories -----------------
  folders <- c("data", "figs", "tables", "R", "templates")
  
  for(item in folders) {
    dir.create(paste(pth, item, sep="/"))
  }
  
  # create quarto template -----
  quarto_file_name <- paste(report_name, ".qmd", sep="")
  quarto_file <- paste(pth, "/", quarto_file_name, sep="")
  
  analysis_file_name <- paste(report_name, ".R", sep="")
  analysis_file <- paste("R/", analysis_file_name, sep="")
  
  file.create(quarto_file)
  
  ## add YAML -------
  cat(
    c("---",     
      "title: Annual Summary Form for LTM Systems",
      "format:",
      "  html:",
      "    code-fold: true",
      "    echo: true",
      "    embed-resources: true",
      "  docx:",
      "    code-line-numbers: false",
      "    reference-doc: templates/custom-reference-doc.docx",
      "    echo: false",
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
    "#| echo: false",
    "suppressPackageStartupMessages(library(tidyverse))",
    "suppressPackageStartupMessages(library(flfishltm))",
    "suppressPackageStartupMessages(library(viridis))",
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
#| echo: false
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

<!-- Make sure to replace the bold XXXX's and bold-italic statements with the correct values and adjust text as needed -->

Largemouth Bass *Micropterus salmoides* targeted sampling was conducted on `r lake` between `r text_start` and `r text_end`. 
A total of `r n_tran` randomly selected transects were surveyed using a boat-mounted electrofishing unit according to FWC standard lentic protocols.

A total of `r n_tot` Largemouth Bass were collected across all transects. 
Catch-per-unit-effort (CPUE) was `r cpue` ± `r cpue_se` LMB/minute which **XXXX** compared to the CPUE observed in spring `r pv_yr` (`r pv_cpue` ± `r pv_cpue_se` LMB/min; @fig-cpue).
***Talk about the CPUE of age 1+ vs. 12\"+*** (@fig-cpue-size).
CPUE of memorable and trophy sized Largemouth Bass (\\> 50 cm) is ***EXAMPLE: similar to that observed in 2020 and 2021, but is lower than observed during 2017-2019*** (@fig-cpue-trophy).

***Talk about the length distribution*** (@fig-length-freq). 
The mean relative weight for the entire sample was `r round(mean(RelWt$relWt, na.rm = T),2)`%, which is **XXXX** than the ***EXAMPLE: previous few years*** (@fig-rw).
***Talk about any differences or trends in relative weight by cm groups*** (@fig-rw-LOESS). 
The mean relative weight for Largemouth Bass greater than 30 cm was `r round(mean(RelWt30$relWt, na.rm = T),2)`%. 
Proportional stock density (PSD) was `r psd %>% round(2)*100`% and relative stock density-preferred was `r rsdp %>% round(2)*100`%, which are ***EXAMPLE: lower than in 2016-2020, but about the same as observed in 2021*** (@fig-rsd).

Fish health codes were assigned to `r 100-fhc_nocode`% of the Largemouth Bass sampled during `r yr`. Among the `r n_tot` bass sampled, `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode1 == \"L\") %>% pull(n)` had lesions and `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode1 == \"S\") %>% pull(n)` had skeletal deformities.



<!--
BELOW IS AN EXAMPLE WRITE UP OF THE REPORT FOR LMB ORANGE 2022.

Largemouth Bass *Micropterus salmoides* targeted sampling was conducted on `r lake` between `r text_start` and `r text_end`. `r n_tran` randomly selected transects were surveyed using a boat-mounted electrofishing unit according to FWC standard lentic protocols.

`r n_tot` Largemouth Bass were collected across all transects. Catch-per-unit-effort (CPUE) was `r cpue` ± `r cpue_se`, which is similar to the CPUE observed in spring 2021, and higher than all other post-drought (2016-2020) samples (@fig-cpue). The higher catch rates these past two years have been driven primarily by strong recruitment in 2020 and 2021 (@fig-cpue-size-1). CPUE of Largemouth Bass larger than 30 cm was within the normal ranges observed between 2007 and 2022, and is down from the peak observed in 2019 (@fig-cpue-size-1). CPUE of memorable and trophy sized Largemouth Bass (\\> 50 cm) is similar to that observed in 2020 and 2021, but is lower than observed during 2017-2019 (@fig-cpue-size-2).

There are noticeable peaks in the length distribution that likely correspond to age-1 (11-21 cm) and age-2 (22-36 cm) cohorts (@fig-length-freq). However, an age sample was not collected this year. The mean relative weight for the entire sample was `r round(mean(RelWt$relWt, na.rm = T),2)`%, which is slightly higher than the previous few years, but is within the normal ranges observed since 2007 (@fig-rw-1). Individuals less than 200 mm TL and greater than 400 mm TL tended to have higher relative weights than individuals between 300 and 400 mm TL (@fig-rw-2). The mean relative weight for Largemouth Bass greater than 30 cm was `r round(mean(RelWt30$relWt, na.rm = T),2)`%. Proportional stock density (PSD) was `r psd %>% round(2)*100`% and relative stock density-preferred was `r rsdp %>% round(2)*100`%, both of which are lower than in 2016-2020, but about the same as observed in 2021 (@fig-rsd).
Fish health codes were assigned to `r 100-fhc_nocode`% of the Largemouth Bass sampled during `r yr`. Among the `r fhc_sum %>% filter(year == max(fhc_sum$year)) %>% summarize(n = sum(n)) %>% pull(n)` bass sampled, `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode1 == \"L\") %>% pull(n)` had lesions and `r fhc_sum %>% filter(year == max(fhc_sum$year), FishHealthCode1 == \"S\") %>% pull(n)` had skeletal deformities.-->


## Noteworthy Observations


-   ***EXAMPLE: High catch rates of age-1 Largemouth Bass (\\< 280 mm) the past two years suggest that large year-classes were produced in 2020 and 2021.***

-   ***EXAMPLE: Very few Largemouth Bass were observed with any major health concerns. Of those that did, 3 had lesions, and 2 had skeletal deformities primarily affecting the dorsal fin.***

-   ***EXAMPLE: 1 trophy Largemouth Bass was collected and tagged during sampling (Trophy 2005: 4546g/9 lbs 15 oz).***


## Influential Factors


-   ***EXAMPLE: Submerged aquatic vegetation coverage, dominated by *Hydrilla sp.*, coverage has expanded in recent years and was above the long-term average in 2021 (@fig-sav). *Hydrilla sp.* coverage was particularly high in Macintosh Bay and in southern portions of the lake near Peegee Run. High density *Hydrilla sp.* restricted our ability to sample near-shore habitats in some areas.***


## Deviations


-   ***EXAMPLE: Thirteen sites were samples at 120 pps, due to issues reaching target amps at 60 pps.***

    
    ",
      append = TRUE,
      file = quarto_file)
  
  ## add figures -----------------------------------------------------------------
  
  ### cpue-fig -------------------------------------------------------------------
  
  cat("
```{r cpue-fig}
#| label: fig-cpue
#| fig-cap: !expr 'paste(\"Largemouth Bass *Micropterus salmoides* catch-per-unit-effort during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true


cpue_plot <- flfishltm::cpue.plot(dsum, 
                                  c(\"LMB\"), 
                                  years = c(min(catch$year):max(catch$year)), fig_scale = 5)

```
    ",
      append = TRUE,
      file = quarto_file)
  
  ### cpue-fig-size-------------------------------------------------------------
  
  cat("
```{r cpue-fig-size}
#| label: fig-cpue-size
#| fig-cap: !expr 'paste(\"Largemouth Bass *Micropterus salmoides* catch-per-unit-effort by size class during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. Largemouth Bass less than 280 mm TL were assumed to be Age-1 based on previous age samples conducted at \", stringr::str_trim(wb),\". {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true

cpue_plot <- 
  flfishltm::cpue.plot(dsum, c(\"LMB\"),
                       years = c(min(catch$year): max(catch$year)),
                       species_size_strata = list(
                         LMB = list(
                           \"Age-1\" = c(0,28),
                           \'12\" +' = c(29,100)
                       )),
                       fig_scale = 5)

```
      ",
      append = TRUE,
      file = quarto_file)
  
  ### cpue-fig-trophy-------------------------------------------------------------
  
  cat("
```{r cpue-fig-trophy}
#| label: fig-cpue-trophy
#| fig-cap: !expr 'paste(\"Catch-per-unit-effort of memorable and trophy sized Largemouth Bass *Micropterus salmoides* (> 500 mm TL) during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true

trophy_plot <- 
  flfishltm::cpue.plot(dsum, c(\"LMB\"),
                       years = c(min(catch$year): max(catch$year)),
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
#| fig-cap: !expr 'paste(\"Length distribution of Largemouth Bass *Micropterus salmoides* collected during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true
#| fig-height: 8
#| fig-width: 4

cpue_plot <- flfishltm::len.dist(dsum, c(\"LMB\"), years = c((max(catch$year)-3):max(catch$year)), fig_scale = 5)

```
      ",
      append = TRUE,
      file = quarto_file)
  
  ### rel-wt -------------------------------------------------------------------
  
  cat("
```{r rel-wt}
#| label: fig-rw
#| fig-cap: !expr 'paste(\"Relative weight values of Largemouth Bass *Micropterus salmoides* collected during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. Relative weights calculated assuming a = -5.47045, b = 3.24105. Area shaded red represents 95% confidence interval. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true
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
```
      ",
      append = TRUE,
      file = quarto_file)
  
  
  ### rel-wt-LOESS -------------------------------------------------------------------
 
  cat("
```{r rel-wt-LOESS}
#| label: fig-rw-LOESS
#| fig-cap: !expr 'paste(\"Relative weight values by total length of Largemouth Bass *Micropterus salmoides* collected during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. The blue line is a LOESS smoothing spline. Area shaded grey represents the 95% confidence interval. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true
## Relative weight values obtained from \"FishLWCalculator.xlsx\"

ggplot(data = RelWt %>% filter(year == yr, ID != \"ltm24913\"), 
       aes(x = (TotalLength/10), y = relWt)) +
  geom_point(color = \"blue\", alpha = 0.4) +
  geom_smooth(method = \"loess\", fill = \"light gray\") + 
  labs(x = \"Total Length (cm) \",
       y = \"Relative Weight (%)\") 
```
      ",
      append = TRUE,
      file = quarto_file)
  
### rsd-plot -----------------------------------------------------------------
  
  cat("
```{r rsd-plot}
#| label: fig-rsd
#| fig-cap: !expr 'paste(\"Proportional stock density and relative stock density (preferred) of Largemouth Bass *Micropterus salmoides* collected during standardized targeted electrofishing surveys conducted between \",text_start,\" to \",text_end,\" on \", stringr::str_trim(wb),\", FL. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true

print(PSD_plot)

```
      ",
      append = TRUE,
      file = quarto_file)
  
### sav plot -----------------------------------------------------------------

  cat("
```{r sav-plot}
#| label: fig-sav
#| fig-cap: !expr 'paste(\"Submersed vegetation areal coverage (%) that was visually estimated during standardized targeted Largemouth Bass *Micropterus salmoides* electrofishing surveys conducted on \", stringr::str_trim(wb),\", FL. {{< pagebreak >}}\", sep = \"\")'
#| output: true
#| include: true

legend_title <- \"Year\"

sav_plot <- ggplot(data=habitat, aes(Year, ID_Sub, group = Year, fill = as.factor(Year))) +
  geom_boxplot()

sav_plot + scale_x_continuous() +
  theme_bw() +
  ylab(\"Submersed Vegetation Areal Coverage (%)\") +
  scale_fill_viridis(legend_title, discrete = TRUE)

```
      ",
      append = TRUE,
      file = quarto_file)
  
 # create R template --------
  file.create(analysis_file)
  cat(create_ltm_analysis_template(catch = report_name), 
      file = paste(pth, analysis_file, sep="/"))

 return(TRUE)
}


