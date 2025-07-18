import re
import random

# Lista dei file specifici
filenames = ['2021.Rmd', '2022.Rmd', '2023.Rmd']

# Prepara una struttura dati per raccogliere gli esercizi per numero
esercizi = {i: [] for i in range(1, 7)}  # Assumendo che ci siano 6 esercizi

# Legge i file specificati e raccoglie gli esercizi
for filename in filenames:
    with open(filename, 'r') as file:
        content = file.read()
        # Trova tutti gli esercizi utilizzando un'espressione regolare
        for match in re.finditer(r'### Esercizio (\d+)(.*?)###', content, re.DOTALL):
            num_esercizio = int(match.group(1))
            testo_esercizio = match.group(0)
            # Assicurati che l'ultimo esercizio venga catturato aggiungendo il controllo sulla fine del file
            if match.end() == len(content) or content[match.end():match.end()+3] == '###':
                esercizi[num_esercizio].append(testo_esercizio)
            else:
                # Includi tutto il testo fino al prossimo esercizio
                testo_esercizio_completo = content[match.start():content.find('###', match.end())]
                esercizi[num_esercizio].append(testo_esercizio_completo)

# Seleziona casualmente un esercizio per ogni numero e crea il nuovo documento
nuovo_documento = ""
for i in range(1, 7):
    if esercizi[i]:  # Controlla che ci sia almeno un esercizio per questo numero
        esercizio_scelto = random.choice(esercizi[i])
        nuovo_documento += esercizio_scelto + "\n\n"

# Salva il nuovo documento
with open('Esercizi_Casuali.Rmd', 'w') as file:
    file.write(nuovo_documento)

