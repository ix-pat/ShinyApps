import re

# Lista dei file specifici
#filenames = ['2021.Rmd', '2022.Rmd', '2023.Rmd', '2024.Rmd']
filenames = ['2023.Rmd', '2024.Rmd']

# Inizializza una struttura dati per raccogliere gli esercizi per numero
esercizi_per_numero = {i: [] for i in range(1, 7)}  # Per esercizi da 1 a 6

# Legge i file e raccoglie gli esercizi
for filename in filenames:
    with open(filename, 'r') as file:
        content = file.read()
        for num_esercizio in esercizi_per_numero.keys():
            if num_esercizio != 6:
                # Per esercizi da 1 a 5
                pattern = rf'### Esercizio {num_esercizio}(.*?)(?=### Esercizio \d+|### $|\Z)'
            else:
                # Per l'esercizio 6, escludi eventuali titoli di livello due successivi
                pattern = rf'### Esercizio {num_esercizio}(.*?)(?=##|\Z)'
            matches = re.findall(pattern, content, re.DOTALL)
            esercizi_per_numero[num_esercizio].extend([match.strip() for match in matches])

# Crea un file per ogni gruppo di esercizi
for num_esercizio, esercizi in esercizi_per_numero.items():
    nome_file = f'Esercizi_{num_esercizio}.Rmd'
    with open(nome_file, 'w') as file:
        for esercizio in esercizi:
            # Aggiunge un titolo di terzo livello per ogni esercizio
            file.write(f'### Esercizio {num_esercizio}\n{esercizio}\n\n')
