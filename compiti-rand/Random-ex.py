import random

# Lista dei nomi dei file da cui pescare
file_names = ["Esercizi_1.Rmd", "Esercizi_2.Rmd", "Esercizi_3.Rmd", "Esercizi_4.Rmd", "Esercizi_5.Rmd", "Esercizi_6.Rmd"]

# Inizializza una struttura dati per raccogliere gli esercizi
esercizi_selezionati = []

# Legge gli esercizi da ciascun file
for file_name in file_names:
    with open(file_name, "r") as file:
        content = file.read()
        # Dividi il contenuto in esercizi separati
        esercizi = content.split("### Esercizio")
        # Rimuovi il primo elemento vuoto
        esercizi = esercizi[1:]
        # Seleziona casualmente un esercizio da questo file
        esercizio_scelto = random.choice(esercizi).strip()
        # Aggiungi l'esercizio selezionato alla lista con il titolo corretto
        numero_esercizio = file_names.index(file_name) + 1
        esercizio_completo = f"### Esercizio {numero_esercizio}\n{esercizio_scelto}"
        esercizi_selezionati.append(esercizio_completo)

# Crea un nuovo file ".Rmd" con i sei esercizi selezionati
nuovo_documento = "\n\n".join(esercizi_selezionati)
with open("Esercizi_Selezionati.Rmd", "w") as file:
    file.write(nuovo_documento)
