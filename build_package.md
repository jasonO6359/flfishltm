# Build `flfishltm`
# 

First download a local copy of the `flfishltm` repository. 

Then open the flffishltm.Rproj file to open the project in RStudio.

Make sure that you have the `devtools` package installed. 

```
install.packages('devtools')
```
Then to build the flfishltm package run:

```
src <- devtools::build()
bin <- devtools::build(src, binary = TRUE, vignettes = TRUE, manual = TRUE)
```

The package can now be installed using

```
install.packages(bin, repos = NULL, type = "binary")
```

