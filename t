git commit -m "feat: workflow complet de modélisation des branches PIB - sélection, BVAR, facteurs, équations de pont

1. Sélection des variables (refaite) :
   - Révision complète de la sélection des indicateurs pour chaque branche
   - Tous les détails de la sélection et des tests documentés dans :
     Models/Method_1_Higgins/

2. Reconstruction des BVAR :
   - Modèles BVAR estimés pour chaque branche sur la base des sélections actualisées

3. Détermination des facteurs et approche Jagged Edge :
   - Extraction des facteurs communs
   - Application de la méthode Jagged Edge sur les 12 branches disposant d'indicateurs
   - Gestion des panels déséquilibrés (données manquantes, fréquences variables)

4. Déflation des séries nominales :
   - Identification et déflation de 10 séries monétaires et nominales (M3, crédit bancaire,
     secteur privé, sociétés non financières, recettes touristiques, crédit BTP,
     exportations/importations, céphalopodes, etc.)
   - Passage des valeurs nominales aux valeurs réelles via déflateur approprié
   - Certaines séries renommées "(déflaté, réel)", d'autres déflatées sans renommage

5. Construction des équations de passerelle (bridge equations) :
   - Industrie de transformation : passage de 15 à 6 meilleurs indicateurs,
     modèle combiné réussi avec R² = 0,947 (période 2016-2021, n=20)
   - 3 branches avec équation individuelle adoptée :
       • Immobilier : crédits à l'habitat (R² = 0,13)
       • Commerce : évolution des prix (R² = 0,23)
       • Industrie d'extraction : IPM (p=0,051)
   - Vérification valeur par valeur que les séries utilisées sont bien les déflatées

Résultat final : 9 branches sur 12 exploitables
  - 6 modèles combinés solides
  - 3 équations individuelles
  - 3 branches en AR(4) : Agriculture, Transports, Information-communication

Fichiers ajoutés/mis à jour :
  Models/Method_1_Higgins