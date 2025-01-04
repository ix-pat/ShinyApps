
import re
from pathlib import Path

def process_file(file_path):
    # Assicurati che il file esista
    if file_path.is_file():
        # Leggi il contenuto del file
        with open(file_path, 'r', encoding='utf-8') as file:
            final_content = file.read()

        # Calcola total_points
        pattern = r"Punti (\d{1,2})"
        matches = re.findall(pattern, final_content)
        total_points = sum(int(match) for match in matches)

        # Se total_points è maggiore di 110, elimina "### Esercizio 6" e il relativo contenuto
        if total_points > 120:
            exercise_6_pattern = r'### Esercizio 6.*?(?=^###|\Z)'
            updated_content = re.sub(exercise_6_pattern, '', final_content, flags=re.DOTALL | re.MULTILINE)
        else:
            updated_content = final_content

        # Scrivi il contenuto aggiornato nel file originale
        with open(file_path, 'w', encoding='utf-8') as file:
            file.write(updated_content)

        print("Sostituzione completata.")
    else:
        print("Il file non esiste.")

# Esegui la funzione
file_path = Path("compito_com.Rmd")
process_file(file_path)
file_path = Path("compito_sol.Rmd")
process_file(file_path)

# Percorso al file che vuoi modificare
file_path = Path("compito_sol.Rmd")

# Assicurati che il file esista
if file_path.is_file():
    # Leggi il contenuto del file
    with open(file_path, 'r', encoding='utf-8') as file:
        final_content = file.read()

    # Calcola total_points come prima
    pattern = r"Punti (\d{1,2})"
    matches = re.findall(pattern, final_content)
    total_points = sum(int(match) for match in matches)

    # Funzione per la sostituzione
    def replace_function(match):
        points = int(match.group(1))
        # Calcola la proporzione dei punti rispetto a 30
        points_out_of_30 = round((points / total_points) * 30, 2)
        return f"Punti {points} su {total_points} ({points_out_of_30} su 30)"

    # Sostituisce nel contenuto
    updated_content = re.sub(pattern, replace_function, final_content)

    # Scrivi il contenuto aggiornato nel file (o in uno nuovo se preferisci)
    with open(file_path, 'w', encoding='utf-8') as file:
        file.write(updated_content)

    print("Sostituzione completata.")
else:
    print("Il file non esiste.")


