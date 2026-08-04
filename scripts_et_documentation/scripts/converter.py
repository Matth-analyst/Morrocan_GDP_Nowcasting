import pandas as pd
import os
from pathlib import Path

def excel_to_csv(repertoire):
    for fichier in os.listdir(repertoire):
        if fichier.endswith(('.xlsx', '.xls')):
            chemin_complet = os.path.join(repertoire, fichier)
            
            try:
                # Pour les fichiers .xls, utiliser engine='xlrd' ou 'openpyxl'
                if fichier.endswith('.xls'):
                    df = pd.read_excel(chemin_complet, engine='xlrd')  # Nécessite xlrd
                else:
                    df = pd.read_excel(chemin_complet, engine='openpyxl')
                
                nom_csv = Path(fichier).stem + '.csv'
                chemin_csv = os.path.join(repertoire, nom_csv)
                df.to_csv(chemin_csv, index=False, encoding='utf-8-sig')
                print(f"✅ Converti : {fichier} → {nom_csv}")
                
            except Exception as e:
                print(f"❌ Erreur avec {fichier} : {e}")

excel_to_csv('.')
