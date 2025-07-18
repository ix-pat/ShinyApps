library(knitr)
library(kableExtra)
library(pBrackets)
library(bookdown)
#library(bookdownplus)
#library(plotrix)
library(colorspace)
library(haven)
#library(rgl)
library(mvtnorm)
#library(pat.book)

prt_ <- function(x){
  paste(capture.output(x),collapse = " \n ")
}

div_ <- function(x){
  gsub("\\n  \\n \\n", "\\\n \\\\[ \\\\]\\\n ", prt_(x))
}

f <- dir("R/")
lapply(paste0("R/",f), source)
