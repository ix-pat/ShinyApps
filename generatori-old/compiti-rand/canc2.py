import re
from pathlib import Path

# Definisci i percorsi completi dei file specifici
files_to_modify = [
    Path('2021.Rmd'),
    Path('2022.Rmd'),
    Path('2023.Rmd')
]

# Definisci la stringa da cercare e il blocco di codice da inserire
search_string = r"`r i2 <- 0; i1 <- i1\+1`"
replacement_text = '''```{r}
rm(list = ls())
source("src/main-functions.R")
source(src_("intro.R"))
```

`r i2 <- 0; i1 <- i1+1`'''

# Itera sui file specificati
for file_path in files_to_modify:
    # Assicurati che il file esista
    if file_path.is_file():
        # Leggi il contenuto del file
        with open(file_path, 'r', encoding='utf-8') as file:
            file_content = file.read()
        
        # Sostituisci la stringa target con il blocco di codice
        updated_content = re.sub(search_string, replacement_text, file_content, flags=re.MULTILINE)
        
        # Scrivi il contenuto aggiornato nello stesso file
        with open(file_path, 'w', encoding='utf-8') as file:
            file.write(updated_content)
        
        print(f"File aggiornato: {file_path}")
    else:
        print(f"Il file {file_path} non esiste.")
