import re
from pathlib import Path

# Percorso della directory che contiene i file Rmd
directory_path = Path("")

# Blocchi di codice R da aggiungere
code_block = """
```{r}
rm(list = ls())
source("src/main-functions.R")
source(src_("intro.R"))
```
"""

# Pattern per trovare i titoli di livello tre
pattern = r"(### .*\n)"

# Elabora ogni file Rmd nella directory
for file_path in directory_path.glob("*.Rmd"):
    # Leggi il contenuto del file
    content = file_path.read_text()
    
    # Sostituisci ogni occorrenza del titolo di livello tre con se stesso seguito dal blocco di codice R
    modified_content = re.sub(pattern, r"\1" + code_block, content)
    
    # Sovrascrivi il file con il contenuto modificato
    file_path.write_text(modified_content)

print("Modifica completata.")
