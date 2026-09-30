# ============================================================================
# run_pipeline.R -- Execution du pipeline GDPNow-Maroc
# ============================================================================
# Pipeline complet : les 26 phases de PLAN_CORRECTIONS_NOWCASTING.md sont
# appliquees. Les scripts de la version anterieure au plan sont conserves dans
# R/_archive_v1/ a titre de trace ; ils ne sont plus executes.
#
# Source des donnees : GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx
# (jamais modifie) + VA_reelle_par_branche.xlsx.
# Echanges entre etapes : fichiers CSV dans data/, organises par branche.
# Rapports d'etape : report/
# ============================================================================

scripts <- c(
  # --- donnees -------------------------------------------------------------
  "R/01_import_donnees.R",        # phase 1 : import, datation fin de periode, CSV
  "R/02_analyse_exploratoire.R",  # phase 2 : transformations locales + exploration
  "R/02b_rapport_phase2.R",       # rapport de la phase 2
  # --- BVAR ----------------------------------------------------------------
  "R/03_bvar_trimestriel.R",      # phase 3 : BVAR recursif, prior Minnesota  (~40 min)
  "R/03c_selection_conjointe.R",  # phase 3 bis : selection JOINTE            (~30 min)
  "R/03d_bvar_etendu.R",          # phase 3 ter : BVAR rejoue depuis 2008     (< 1 min)
  "R/06a_poids.R",               # poids en prix courants : 03e en a besoin
  "R/03e_branches_instables.R",   # phase 3 quater : branches a rupture de variance
  "R/03b_rapport_phase3.R",       # rapport de la phase 3
  # --- passerelles ---------------------------------------------------------
  "R/04_bridge_equations.R",      # phase 4 : agregation, selection, bridge, delta
  "R/04b_variantes_passerelle.R", # phase 4 bis : six pistes d amelioration   (~10 min)
  "R/04b2_ridge_corrige.R",       # ridge rejouee apres correction de la GCV
  "R/04c0_validation_kalman.R",   # trous artificiels : le Kalman vaut-il son cout
  "R/04c_kalman_trous.R",         # phase 4 ter : comblement par Kalman       (~25 min)
  "R/04d_combinaison_etat.R",     # phase 4 quater : poids dependant de l etat
  "R/09_test_affinement_intra_trimestre.R", # etape 8 : M0/M1/M2/M3          (~2 h 30)
  "R/09c_seuil_trimestriel.R",    # seuil renforce sur les indicateurs trimestriels
  "R/09d_selection_conditionnelle.R", # selection conditionnelle contre marginale
  "R/09e_test_m3_mensuel.R",      # l'anomalie M2 > M3, experience decisive
  "R/05_ar4_non_couvertes.R",     # etape 5 : branches sans indicateur
  # --- agregation, puis ce qui en depend ------------------------------------
  "R/06_agregation_fisher.R",     # etape 6 : poids prix courants, agregat
  "R/09b_backtest_etendu.R",      # etape 8 bis : backtest etendu a 2008      (~1 h)
  "R/10_incertitude.R",           # Diebold-Mariano, test groupe, bootstrap
  "R/11_sensibilite_delais.R",    # sensibilite aux delais de publication
  "R/12_incertitude_parametrique.R",  # loi predictive du BVAR et calibration
  "R/12b_recalibration.R",        # recalibration conforme des intervalles
  "R/08a_nowcast_courant.R",      # le chiffre publie : 12c en a besoin
  "R/12c_intervalle_combinaison.R",   # intervalle autour du chiffre publie
  "R/07_validation_pseudo_temps_reel.R", # etape 7 : fonction centrale et controles
  # --- rapports finaux, une fois TOUS les resultats disponibles --------------
  "R/13_benchmarks.R",            # phases 16-18 : etalons externes           (~15 min)
  "R/14_robustesse.R",            # phases 20-24 : delta, fenetre, selection  (~40 min)
  "R/14b_courbe_delta.R",         # courbe de perte selon delta, et sa lecture
  "R/15_diagnostics.R",           # phase 25 : contributions, residus, episodes
  "R/16_direct_indirect.R",       # voie directe contre voie indirecte        (~20 min)
  "R/04e_rapport_phases4_13.R",   # rapport des phases 4 et 13
  "R/08_rapport_synthese.R",      # etape 9 : synthese et nowcast courant
  "R/rapport_global.R"            # le rapport unique, de la donnee au chiffre publie
)

# L'ordre suit les DEPENDANCES, pas la numerotation :
#   03d avant 09b      -- le backtest etendu a besoin du BVAR remontant a 2008 ;
#   06a avant 03e      -- 03e evalue son effet SUR L'AGREGAT, donc lui faut les
#                         poids ; et les poids ne dependent d'aucune prevision ;
#   03e avant 06       -- l'agregation consomme les previsions CORRIGEES ;
#   09 avant 06        -- l'agregation consomme les previsions intra-trimestrielles ;
#   06 avant 07 et 08  -- la fonction centrale et le nowcast ont besoin des poids ;
#   09b et 10 avant 04e -- le rapport compose les sections 17 et 18 a partir d eux ;
#   06 et 12 avant 12c  -- l intervalle de la combinaison a besoin des deux ;
#   08a avant 12c      -- 12c encadre le CHIFFRE PUBLIE, donc il lui faut ;
#                         et 08 ne peut composer sa section 8.1 qu apres 12c.
# Duree totale : plus de six heures. Les phases longues ecrivent des points de
# reprise par tache ; une relance relit ce qui existe au lieu de le refaire.

for (s in scripts) {
  cat("\n", strrep("=", 74), "\n", "EXECUTION : ", s, "\n", strrep("=", 74), "\n", sep = "")
  source(s)
}

cat("\n", strrep("=", 74), "\n", "Pipeline termine.\n", sep = "")
