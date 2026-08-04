import os

def supprimer_xlsx(repertoire):
    """
    Supprime tous les fichiers .xlsx dans le répertoire
    """
    compteur = 0
    
    for fichier in os.listdir(repertoire):
        if fichier.endswith('.xlsx'):
            chemin_complet = os.path.join(repertoire, fichier)
            try:
                os.remove(chemin_complet)
                print(f"🗑️ Supprimé : {fichier}")
                compteur += 1
            except Exception as e:
                print(f"❌ Erreur avec {fichier} : {e}")
    
    print(f"\n✅ {compteur} fichiers .xlsx supprimés")

# Utilisation
supprimer_xlsx('.')  # Dossier courant
