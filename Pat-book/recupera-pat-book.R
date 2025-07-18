library(httr)
library(jsonlite)
library(colorspace)
# Crea la cartella R se non esiste
if (!dir.exists("R")) {
  dir.create("R")
}

# URL dell'API GitHub per elencare i file nella directory R
api_url <- "https://api.github.com/repos/ix-pat/stat/contents/R"

# Richiedi l'elenco dei file nella directory R
response <- GET(api_url)

# Controlla se la richiesta ha avuto successo
if (response$status_code == 200) {
  # Parse la risposta JSON
  files <- fromJSON(content(response, "text"))
  
  # Base URL del repository raw
  base_url <- "https://raw.githubusercontent.com/ix-pat/stat/main/R/"
  
  # Funzione per scaricare e sorgere ogni file
  source_github_file <- function(file_name) {
    url <- paste0(base_url, file_name)
    response <- GET(url)
    file_path <- file.path("R", file_name)
    writeBin(content(response, "raw"), file_path)
    source(file_path)
  }
  
  # Sorgi tutti i file R
  sapply(files$name, source_github_file)
} else {
  stop("Errore nel recupero della lista dei file dalla directory R")
}
