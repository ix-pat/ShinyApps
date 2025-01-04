library(knitr)
library(stringr)


# Funzione per sostituire le espressioni R con i risultati calcolati
replace_expressions <- function(file_path,punti) {
  item2 <- function(new=FALSE,start=FALSE,num = FALSE){
    sp <- "."
    sp2 <- " "
    if (!exists("i1")) assign("i1",1, envir = parent.frame())
    if (!exists("i2")) assign("i2",1, envir = parent.frame())
    if (start) assign("i1",0, envir = parent.frame())
    if (new)   assign("i1",i1 + 1, envir = parent.frame())
    if (new)   assign("i2",1, envir = parent.frame()) else assign("i2",i2 + 1, envir = parent.frame())
    it <- (paste(i1,sp,ifelse(num,i2,letters[i2]),sp2,sep = ""))
    return(it)}
  item_start <- function(num = TRUE) item2(new = TRUE,start = TRUE,num = num)
  item_      <- function(num = TRUE) item2(new = FALSE,start = FALSE,num = num)
  item_next  <- function(num = TRUE) item2(new = TRUE,start = FALSE,num = num)
  
  score <- 0
  for (i in 1:length(punti)){
    score <- c(score,unlist(punti[[i]]))
  }
  score <- score[-1]
  cont_ <- readLines(file_path, warn = FALSE)
  #content <- paste(content, collapse = "\n")  # Unisci le righe in un'unica stringa
  
  # Trova tutte le espressioni `r punti_p(arg_opzionale)`
  pattern <- "`r punti_p\\(([^)]*)\\)`"
  #matches <- str_match_all(content, pattern)[[1]]
  ii <- 0
  for (i in 1:length(cont_)) {
    content <- cont_[i]
    matches <- str_match_all(content, pattern)[[1]]
    expression <- matches[1]
    argument <- matches[2]
    if (!is.na(expression)){
      it <- ifelse(argument =="start=T",item2(new = TRUE,start = TRUE),item2())
      it <- ifelse(argument =="nex = T"|argument == "nex=T",item2(new = TRUE),it)
      ii <- ii + 1
      ptt <- unlist(punti)[ii]
      result <- paste(it,"**(Punti ",ptt,")**",sep="")
      # Sostituisci l'espressione con il risultato
      cont_[i] <- gsub(expression, as.character(result), content, fixed = TRUE)
    }
  }
  
  # Scrivi il nuovo contenuto in un nuovo file
  new_file_path <- sub(".Rmd", "_processed.Rmd", file_path)
  writeLines(cont_, file_path)
  return(new_file_path)
}

source("punti.R")

replace_expressions("2021.Rmd",punti_2021)
replace_expressions("2022.Rmd",punti_2022)
replace_expressions("2023.Rmd",punti_2023)
replace_expressions("2024.Rmd",punti_2024)

# Esempio di utilizzo
# file_paths <- c("path/to/your_file1.Rmd", "path/to/your_file2.Rmd") # Sostituisci con i percorsi dei tuoi file Rmd
# 
# sapply(file_paths, replace_expressions)
