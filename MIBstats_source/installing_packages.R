# _Script for installing R packages required by this app (MIBstats)

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("preprocessCore")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("vsn")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("RankProd")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("LMGene")

install.packages(c(
  "shiny",
  "cowplot",
  "rlist",
  "reshape",
  "shinyBS",
  "ggfortify",
  "plotly",
  "Rmpfr",
  "shinyjs",
  "tools",
  "ggplot2",
  "gplots",
  "dplyr",
  "data.table",
  "plyr",
  "DT"
))
