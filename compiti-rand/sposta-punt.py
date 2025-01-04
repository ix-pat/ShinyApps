import re


def rimv_chunk_ttl(file_path):
    with open(file_path, 'r') as file:
        cont = file.read()

    # Definisce la regex per trovare e rimuovere i chunk di codice specificati
    patt_chunk = r'```{r [^}]*}\ncomp <- comp \+ 1\npunt <- punti_list\[\[comp\]\]\n```\n'
    cont_mod = re.sub(patt_chunk, '', cont)

    # Definisce la regex per trovare e rimuovere i titoli di primo e secondo livello
    patt_ttl = r'^(# .*\n)|(^## .*\n)'
    cont_mod = re.sub(patt_ttl, '', cont_mod, flags=re.MULTILINE)

    # Scrive il contenuto modificato in un nuovo file
    new_file_path = file_path.replace('.Rmd', '.Rmd')
    with open(new_file_path, 'w') as new_file:
        new_file.write(cont_mod)

    print(f'File mod salvato come {new_file_path}')

# Sostituisci 'tuo_file.Rmd' con il percorso del tuo file
rimv_chunk_ttl('2021.Rmd')
rimv_chunk_ttl('2022.Rmd')
rimv_chunk_ttl('2023.Rmd')
rimv_chunk_ttl('2024.Rmd')


def rimuovere_chunk(file_path):
    with open(file_path, 'r') as file:
        contenuto = file.read()

    # Definisce la regex per trovare e rimuovere i chunk di codice specificati
    pattern = r'```{r [^}]*}\ncompito <- compito \+ 1\npunti <- punti_list\[\[compito\]\]\n```\n'

    # Applica la sostituzione per rimuovere i chunk
    nuovo_contenuto = re.sub(pattern, '', contenuto)

    # Scrive il contenuto modificato in un nuovo file
    nuovo_file_path = file_path.replace('.Rmd', '.Rmd')
    with open(nuovo_file_path, 'w') as nuovo_file:
        nuovo_file.write(nuovo_contenuto)

    print(f'File modificato salvato come {nuovo_file_path}')

# Sostituisci 'tuo_file.Rmd' con il percorso del tuo file
rimuovere_chunk('2021.Rmd')
rimuovere_chunk('2022.Rmd')
rimuovere_chunk('2023.Rmd')
rimuovere_chunk('2024.Rmd')
