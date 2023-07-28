unlink("C:/Users/jason.oconnor/Desktop/test_report", recursive=TRUE)

create_ltm_report <- function(dir_root = getwd(), dir_name = NA, report_name = NA) {
  
  pth <- paste(dir_root, dir_name, sep="/")
 
  if(!dir.exists(pth)) { 
    dir.create(pth)
  } else {
    cli::cli_alert_warning(c("Specified directory already exists",
                             " writing new files and folders to this",
                             " directory"))
  }
  
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
  
   
   
   
  # create R template --------
  file.create(analysis_file)
  
}


# TEST ------

create_ltm_report("C:/Users/jason.oconnor/desktop", "test_report", "kyles_report")




