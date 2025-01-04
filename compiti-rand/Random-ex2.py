import re
import random
from pathlib import Path
import time
# Genera un numero casuale come esempio
random_number = random.randint(1, 100)
print(random_number)

# Imposta un seed casuale basato sul tempo corrente
random.seed(time.time())

# Definisce il percorso base dove si trovano i file Rmd
base_path = Path('')

# Inizializza una lista per contenere i paragrafi selezionati
selected_paragraphs = []

# Itera attraverso i file da Esercizi_1.Rmd a Esercizi_6.Rmd
for j in range(1, 7):
    file_path = base_path / f'Esercizi_{j}.Rmd'
    
    # Leggi il contenuto del file
    content = file_path.read_text()
    
    # Trova tutti i paragrafi che iniziano con '### Esercizio'
    paragraphs = re.split(r'\n(?=### Esercizio)', content)
    
    # Seleziona casualmente un paragrafo se ce ne sono
    if paragraphs:
        selected_paragraph = random.choice(paragraphs)
        selected_paragraphs.append(selected_paragraph)

# Combina i paragrafi selezionati in un unico documento
combined_content = '\n\n'.join(selected_paragraphs)



# Definisci il preambolo
# Assicurati che ogni backslash in una f-string sia doppiato se fa parte di una stringa letterale e non di un'espressione
preamble0_com = """---
title: Compito di Statistica
"""
preamble0_sol = """---
title: Compito (con soluzioni)
"""

preamble1 = """
html_document: null
editor_options:
  chunk_output_type: console
date: ""
output:
  html_document:
"""
preamble2_sol = "    css: 'sol.css' \n    toc: true"
preamble2_com = "    css: 'comp.css'\n    toc: false"
preamble3 = """
    toc_depth: 6
    toc_float:
      collapsed: false
      smooth_scroll: false
      df_print: paged
header-includes:
- \\usepackage{{amsmath}}
- \\usepackage{{amssymb}}
- \\usepackage{{xfrac}}
- \\usepackage{{stackrel}}
- \\usepackage{{cancel}}
- \\usepackage{{xcolor}}
- \\DeclareMathOperator*{{\\das}}{{\\sim}}
- \\definecolor{{mygray}}{{gray}}{{0.6}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,fig.height = 7*5/7, fig.width = 11*5/7,warning = F, message = F,results='asis')
options(digits=4,nsmall=4,knitr.kable.NA = '',scipen = 1,dev.args = list(bg = 'transparent'))
library(knitr)
library(xtable)
library(kableExtra)
library(plotrix)
library(xaringan)

source("intro.R")

par(bg="transparent")
```


:::{.tit}
Scrivere in stampatello
```{r}
bx <- "$\\\\LARGE\\\\square$"
#bx <- "□"
k <- 20
tab <- matrix(bx,nrow = 2,ncol = k)
tab <- cbind(c("COGNOME","NOME"),tab)
tab2<- matrix(bx,nrow = 2,ncol = k)
tab2 <- cbind(c("CDL","Matricola"),tab2)
tab2[1,7:(k+1)] <- NA
tab <- rbind(tab,tab2[2:1,])
kable((tab))%>%
  column_spec(2:(k+1),width = "1mm",color = "lightgrey")%>%
  kable_styling(full_width = T)
```
:::
"""

# Unisci il preambolo con il contenuto combinato

final_content1 = preamble0_sol + preamble1 + preamble2_sol +  preamble3 + '\n\n' + combined_content
final_content2 = preamble0_com + preamble1 + preamble2_com +  preamble3 + '\n\n' + combined_content

# Salva il nuovo documento combinato come un file Rmd
new_file_path = base_path / 'compito_sol.Rmd'
new_file_path.write_text(final_content1)
new_file_path = base_path / 'compito_com.Rmd'
new_file_path.write_text(final_content2)

