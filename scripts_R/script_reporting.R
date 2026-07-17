# =============================================================================
# SCRIPT DE REPORTING
# Enquête de Couverture Post-Vaccinale (ECP) Rougeole-Rubéole en RDC
# OMS RDC - 2025/2026
# =============================================================================
# Auteur  : Charles Mouté (révisé)
# Date    : Mai 2026
# Objet   :
#   Générer un document Word (via officer + flextable) et un document Quarto
#   (.qmd) qui intègrent automatiquement les graphiques et tableaux produits
#   par `script_analyses.R`.
# =============================================================================


# --- 0. PACKAGES -------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org", quiet = TRUE)
}
pacman::p_load(officer, flextable, tidyverse, cli, glue)

PATH_OUTPUTS    <- "outputs"
PATH_GRAPHIQUES <- file.path(PATH_OUTPUTS, "graphiques")
PATH_TABLEAUX   <- file.path(PATH_OUTPUTS, "tableaux")
PATH_CARTES     <- file.path(PATH_OUTPUTS, "cartes")
PATH_RAPPORT    <- file.path(PATH_OUTPUTS, "rapport")
walk(c(PATH_OUTPUTS, PATH_RAPPORT),
     ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))

if (!dir.exists(PATH_GRAPHIQUES) && !dir.exists(PATH_TABLEAUX)) {
  cli_alert_warning("Aucun graphique/tableau trouvé - lancez d'abord script_analyses.R")
}

cli_h1("Génération des documents de reporting")


# --- 1. STYLES & HELPERS WORD ------------------------------------------------

# Styles texte
sty_titre        <- fp_text(font.size = 22, bold = TRUE,
                            color = "#003366", font.family = "Calibri")
sty_sous_titre   <- fp_text(font.size = 13, italic = TRUE,
                            color = "#666666", font.family = "Calibri")
sty_section      <- fp_text(font.size = 14, bold = TRUE,
                            color = "#003366", font.family = "Calibri")

# Helper : ajoute un tableau CSV (s'il existe) au document Word
add_csv_table <- function(doc, csv_path, header_color = "#003366") {
  if (!file.exists(csv_path)) return(doc)
  d <- read_csv(csv_path, show_col_types = FALSE)
  ft <- flextable(d) %>%
    theme_vanilla() %>%
    bg(part = "header", bg = header_color) %>%
    color(part = "header", color = "white") %>%
    bold(part = "header") %>%
    fontsize(part = "all", size = 9) %>%
    align(part = "header", align = "center") %>%
    align(part = "body", align = "left", j = 1) %>%
    autofit()
  doc %>% body_add_flextable(ft) %>% body_add_par("")
}

# Helper : ajoute une figure (s'il existe) avec légende
add_figure <- function(doc, image_path, caption, width = 6.2, height = 4) {
  if (!file.exists(image_path)) return(doc)
  doc %>%
    body_add_img(src = image_path, width = width, height = height) %>%
    body_add_par(caption, style = "Image Caption") %>%
    body_add_par("")
}


# --- 2. CONSTRUCTION DU DOCUMENT WORD ----------------------------------------

doc <- read_docx()

# Page de titre
doc <- doc %>%
  body_add_fpar(fpar(ftext("Enquête de Couverture Post-Vaccinale",
                           sty_titre)), style = "heading 1") %>%
  body_add_fpar(fpar(ftext("Rougeole-Rubéole en RDC - 2025/2026",
                           sty_sous_titre))) %>%
  body_add_par("") %>%
  body_add_par("OMS RDC", style = "Normal") %>%
  body_add_par(paste("Date de génération :",
                     format(Sys.Date(), "%d %B %Y")), style = "Normal") %>%
  body_add_break(pos = "after")

# Table des matières
doc <- doc %>%
  body_add_par("Table des matières", style = "heading 1") %>%
  body_add_toc(level = 3) %>%
  body_add_break(pos = "after")

# 1. Introduction
doc <- doc %>%
  body_add_par("1. Introduction", style = "heading 1") %>%
  body_add_par(
    paste(
      "Ce rapport présente les résultats de l'enquête de couverture",
      "post-vaccinale (ECP) Rougeole-Rubéole conduite en République",
      "Démocratique du Congo en 2025-2026. Les analyses sont réalisées",
      "conformément au plan d'analyse établi et utilisent des méthodes",
      "statistiques tenant compte du plan de sondage complexe."
    ),
    style = "Normal") %>%
  body_add_par("") %>%
  body_add_par(
    paste(
      "Toutes les estimations de couverture sont pondérées pour assurer",
      "leur représentativité et sont présentées avec leurs intervalles",
      "de confiance à 95%."
    ),
    style = "Normal")

# 2. Conduite de l'enquête
doc <- doc %>%
  body_add_par("2. Conduite de l'enquête", style = "heading 1") %>%
  body_add_par("2.1. Dénombrement", style = "heading 2") %>%
  body_add_par(
    paste(
      "La phase de dénombrement a permis d'identifier et de recenser les",
      "ménages éligibles dans les zones de dénombrement sélectionnées."
    ),
    style = "Normal")

doc <- doc %>%
  add_csv_table(file.path(PATH_TABLEAUX, "denombrement_par_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "01_denombrement_par_province.png"),
             "Figure 1. Ménages dénombrés par province") %>%
  body_add_par("2.2. Déroulement de la collecte", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "flowchart_collecte.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "00_flowchart_collecte.png"),
             "Figure 1 bis. Diagramme de flux de la collecte", width = 5, height = 6)

# 3. Résultats
doc <- doc %>%
  body_add_par("3. Résultats", style = "heading 1") %>%
  body_add_par("3.1. Couverture vaccinale nationale", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "cv_global.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "02_cv_globale.png"),
             "Figure 2. Couverture vaccinale nationale", width = 5, height = 4) %>%
  body_add_par("3.2. Couverture par province", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "cv_par_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "03_cv_par_province.png"),
             "Figure 3. Couverture par province (estimations pondérées, IC 95%)") %>%
  body_add_par("3.3. Couverture par tranche d'âge", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "cv_par_age.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "04_cv_par_age.png"),
             "Figure 4. Couverture par tranche d'âge") %>%
  body_add_par("3.4. Couverture par sexe", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "cv_par_sexe.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "05_cv_par_sexe.png"),
             "Figure 5. Couverture par sexe", width = 5, height = 4) %>%
  body_add_par("3.4 bis. Couverture par milieu de résidence",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "cv_par_milieu.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "05b_cv_par_milieu.png"),
             "Figure 5 bis. Couverture par milieu de résidence (urbain/rural)",
             width = 5, height = 4) %>%
  body_add_par("3.5. Comparaison avec CV administrative et End-Process OMS",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "comparaison_cv_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "23_comparaison_cv_province.png"),
             "Figure 5 ter. Comparaison CV par province (ECP, administrative, End-Process OMS)") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "comparaison_cv_zone_sante.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "24_comparaison_cv_zone_sante.png"),
             "Figure 5 quater. Concordance ECP vs sources de référence par zone de santé") %>%
  body_add_par("3.6. Statut vaccinal antérieur (6-59 mois)",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "statut_vaccinal_anterieur.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "06_statut_vaccinal_anterieur.png"),
             "Figure 6. Statut vaccinal antérieur") %>%
  body_add_par("3.7. Motifs de non-vaccination", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "raisons_non_vaccination.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "07_raisons_non_vaccination.png"),
             "Figure 7. Motifs de non-vaccination") %>%
  body_add_par("3.8. Confirmation par carte", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "confirmation_carte.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "08_confirmation_carte.png"),
             "Figure 8. Confirmation par carte de vaccination") %>%
  body_add_par("3.8. Carte de la couverture par province",
               style = "heading 2") %>%
  add_figure(file.path(PATH_CARTES, "carte_cv_par_province.png"),
             "Figure 9. Carte choroplèthe de la couverture par province",
             width = 5.5, height = 5.5)

# 3bis. Couverture par sous-groupes
doc <- doc %>%
  body_add_par("3.9. Couverture par sous-groupes socio-démographiques",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "couverture_sousgroupes.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "12_couverture_sousgroupes.png"),
             "Figure 12. Couverture vaccinale par sous-groupes (IC 95% Wilson)")

# 4. Raisons de non-vaccination
doc <- doc %>%
  body_add_par("4. Raisons de non-vaccination", style = "heading 1") %>%
  body_add_par("4.1. Diagramme de Pareto", style = "heading 2") %>%
  add_figure(file.path(PATH_GRAPHIQUES, "13_pareto_raisons.png"),
             "Figure 13. Diagramme de Pareto des raisons de non-vaccination") %>%
  body_add_par("4.2. Taxonomie OMS des raisons par province", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "raisons_par_groupe.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "14_groupes_raisons_province.png"),
             "Figure 14. Groupes de raisons (taxonomie OMS) par province")

# 5. Disparités géographiques
doc <- doc %>%
  body_add_par("5. Disparités géographiques", style = "heading 1") %>%
  body_add_par("5.1. Test de disparité (Chi-2)", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "test_chi2_geographique.csv")) %>%
  body_add_par("5.2. Couverture par zone de santé", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "couverture_par_zone.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "15_heatmap_zone_sante.png"),
             "Figure 15. Heatmap de la couverture par zone de santé")

# 6. Déterminants de la vaccination
.meth_path_w <- file.path(PATH_TABLEAUX, ".methode_modele.txt")
.meth_w <- if (file.exists(.meth_path_w)) readLines(.meth_path_w, warn = FALSE) else c("non spécifiée", "—")
note_pond <- if (grepl("pond", .meth_w[1], ignore.case = TRUE)) {
  "Les poids d'échantillonnage ont été appliqués au modèle (weights = poids_final), garantissant la représentativité des Odds Ratios au niveau national/provincial."
} else if (grepl("non pond", .meth_w[1], ignore.case = TRUE)) {
  "Les poids d'échantillonnage n'ont pas pu être appliqués (logistf ne supporte pas les poids) ; les OR sont des associations conditionnelles à l'échantillon."
} else {
  "Estimation par svyglm sur design pondéré ; OR représentatifs au niveau national/provincial."
}
doc <- doc %>%
  body_add_par("6. Déterminants de la vaccination", style = "heading 1") %>%
  body_add_par(
    paste(
      "Analyse par régression logistique (pondérée si plan de sondage",
      "complexe via survey::svyglm). Les Odds Ratios ajustés (ORa)",
      "mesurent l'association entre chaque facteur et la probabilité",
      "d'être vacciné, en contrôlant mutuellement les autres facteurs."
    ), style = "Normal") %>%
  body_add_par(
    paste0("Note méthodologique — Méthode d'estimation effectivement employée : ",
           .meth_w[1], " (", .meth_w[2], " observations). ",
           "La pénalisation de Firth (1993) est appliquée en cas de quasi-séparation ",
           "pour stabiliser les estimations. ", note_pond,
           " Limite : Firth pondéré (brglm2) ne tient pas compte du clustering par ",
           "aire de santé pour les écarts-types ; les IC 95 % peuvent être légèrement ",
           "anticonservateurs. Pour les indicateurs principaux (couverture), nous ",
           "utilisons svyciprop qui prend en compte le plan complet (poids + clusters)."),
    style = "Normal") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "determinants_or_ajustes.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "16_forest_plot_determinants.png"),
             "Figure 16. Forest plot des Odds Ratios ajustés")

# 7. Sources d'information
doc <- doc %>%
  body_add_par("7. Sources d'information", style = "heading 1") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "canaux_information.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "17_canaux_information.png"),
             "Figure 17. Canaux d'information sur la campagne") %>%
  add_figure(file.path(PATH_GRAPHIQUES, "18_couverture_par_canal.png"),
             "Figure 18. Couverture vaccinale par canal d'information")

# 8. Profil des enfants vaccinés
doc <- doc %>%
  body_add_par("8. Profil des enfants vaccinés", style = "heading 1") %>%
  body_add_par("8.1. Lieu de vaccination", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "lieu_vaccination.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "20_lieu_vaccination.png"),
             "Figure 19. Lieu de vaccination") %>%
  body_add_par("8.2. Dépenses de vaccination", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "depenses_par_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "19_depenses_par_province.png"),
             "Figure 20. Dépenses déclarées par province") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "depenses_motifs.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "19b_depenses_motifs.png"),
             "Figure 20 bis. Motifs des dépenses déclarées") %>%
  body_add_par("8.3. Symptômes post-vaccinaux", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "symptomes_post_vaccinaux.csv")) %>%
  add_csv_table(file.path(PATH_TABLEAUX, "symptomes_detail.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "21_symptomes_detail.png"),
             "Figure 21. Symptômes post-vaccinaux déclarés par type")

# 9. Synthèse - KPI
doc <- doc %>%
  body_add_par("9. Synthèse : indicateurs clés", style = "heading 1") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "kpi_dashboard.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "21_kpi_dashboard.png"),
             "Figure 21. Tableau de bord des indicateurs clés")

# 10. Contrôle qualité
doc <- doc %>%
  body_add_par("10. Contrôle qualité", style = "heading 1") %>%
  body_add_par("10.1. Concordance backcheck (enfants)", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "concordance_enfant.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "10_concordance_enfant.png"),
             "Figure 22. Taux d'accord par variable") %>%
  add_figure(file.path(PATH_GRAPHIQUES, "11_kappa_enfant.png"),
             "Figure 23. Coefficient Kappa de Cohen par variable") %>%
  body_add_par("10.2. Synthèse par type de variable", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "qualite_par_type.csv"))

# 11. Annexe méthodologique
doc <- doc %>%
  body_add_par("11. Annexe méthodologique", style = "heading 1") %>%
  body_add_par("11.1. Pondération et calcul du poids de sondage",
               style = "heading 2") %>%
  body_add_par(
    paste(
      "Tous les enfants n'ayant pas la même probabilité d'être sélectionnés,",
      "un poids de sondage est attribué à chacun : il correspond au nombre",
      "d'enfants de la population réelle qu'il représente. Le poids reflète",
      "la probabilité de sélection à chaque étape du tirage : tirage des zones",
      "de dénombrement avec probabilité proportionnelle à la taille (PPS),",
      "tirage aléatoire simple des îlots, puis des ménages. Le poids initial",
      "est l'inverse du produit de ces probabilités : plus un enfant avait peu",
      "de chances d'être tiré, plus son poids est élevé."
    ), style = "Normal") %>%
  body_add_par(
    paste(
      "Ce poids initial est ensuite ajusté par un facteur de non-réponse",
      "calculé au niveau de l'aire de santé, afin que les ménages répondants",
      "compensent les ménages absents ou refusants comparables. Le poids final",
      "= (inverse de la probabilité de sélection) x (facteur de correction pour",
      "non-réponse). Les estimations sont produites avec le package R 'survey'",
      "(Lumley, 2010)."
    ), style = "Normal") %>%
  body_add_par("11.2. Intervalles de confiance", style = "heading 2") %>%
  body_add_par(
    paste(
      "Les intervalles de confiance des proportions sont calculés par",
      "transformation logit (survey::svyciprop, method = 'logit'), qui",
      "contraint mathématiquement les bornes dans l'intervalle [0 % ; 100 %]",
      "et corrige les anomalies de l'approximation de Wald (bornes > 100 %",
      "lorsque la couverture est très élevée)."
    ), style = "Normal") %>%
  body_add_par("11.3. Test de disparité géographique", style = "heading 2") %>%
  body_add_par(
    paste(
      "La comparaison de la couverture entre provinces utilise le test de",
      "Rao-Scott (survey::svychisq), adaptation du Chi-2 de Pearson aux plans",
      "de sondage complexes (Rao & Scott, 1984), qui tient compte de l'effet",
      "de grappe et de la pondération."
    ), style = "Normal") %>%
  body_add_par("11.4. Régression logistique et pénalisation de Firth",
               style = "heading 2") %>%
  body_add_par(
    paste(
      "Les déterminants de la vaccination sont estimés par régression",
      "logistique pondérée (survey::svyglm, quasibinomial). En cas de",
      "quasi-séparation (faibles effectifs dans certaines modalités, rendant",
      "les estimations instables), le modèle bascule sur une régression",
      "pénalisée de Firth (Firth, 1993 ; packages brglm2 ou logistf), qui",
      "corrige le biais et resserre les intervalles de confiance. Les variables",
      "candidates couvrent la province, le niveau d'instruction, l'accès",
      "à l'information, l'importance perçue des vaccins, la connaissance",
      "du lieu, la facilité de paiement, le sexe et l'âge de l'enfant."
    ), style = "Normal") %>%
  body_add_par("11.5. Mesures de concordance (Kappa de Cohen)",
               style = "heading 2") %>%
  body_add_par(
    paste(
      "Le pourcentage d'accord simple et le coefficient Kappa de Cohen",
      "(package 'irr') sont calculés pour chaque variable commune entre",
      "l'enquête principale et le backcheck. L'interprétation suit",
      "l'échelle de Landis & Koch (1977) : < 0,00 pauvre ; 0,00-0,20 léger ;",
      "0,21-0,40 passable ; 0,41-0,60 modéré ; 0,61-0,80 substantiel ;",
      "0,81-1,00 presque parfait. Les variables sont classifiées",
      "en T1 (très stables, seuil d'erreur 5%), T2 (connaissances/attitudes,",
      "seuil 15%) et T3 (sensibles, seuil 25%)."
    ), style = "Normal") %>%
  body_add_par("11.6. Dérivation du milieu de résidence", style = "heading 2") %>%
  body_add_par(
    paste(
      "Le milieu (urbain/rural) n'étant pas collecté dans le questionnaire,",
      "il est dérivé de la base de sondage : pour chaque zone de santé, on",
      "détermine le milieu prédominant selon la population recensée, puis on",
      "l'affecte aux enfants de cette zone."
    ), style = "Normal") %>%
  body_add_par("11.7. Références bibliographiques", style = "heading 2") %>%
  body_add_par(
    paste(
      "Lumley, T. (2010). Complex Surveys: A Guide to Analysis Using R. Wiley. |",
      "Landis, J. R., & Koch, G. G. (1977). Biometrics, 33(1), 159-174. |",
      "Rao, J. N. K., & Scott, A. J. (1984). Annals of Statistics, 12(1), 46-60. |",
      "Firth, D. (1993). Biometrika, 80(1), 27-38. |",
      "Institut National de la Statistique (INS) - RDC. https://www.ins.cd/ |",
      "OMS. Vaccination coverage cluster surveys: reference manual. Genève."
    ), style = "Normal")

# 12. Caractéristiques du ménage (résultats uniquement)
doc <- doc %>%
  body_add_par("12. Caractéristiques du ménage", style = "heading 1") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "caracteristiques_menage.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "22_caracteristiques_menage.png"),
             "Figure 22. Caractéristiques des ménages enquêtés", width = 5.5, height = 5)

# 13. Résultats complémentaires par province
doc <- doc %>%
  body_add_par("13. Résultats complémentaires par province", style = "heading 1") %>%
  body_add_par("13.1. Canaux d'information par province", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "canaux_information_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "17b_canaux_information_province.png"),
             "Figure 23. Canaux d'information par province") %>%
  body_add_par("13.2. Confirmation par carte par province", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "confirmation_carte_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "08c_confirmation_carte_province.png"),
             "Figure 24. Confirmation par carte par province") %>%
  body_add_par("13.3. Lieu de vaccination par province", style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "lieu_vaccination_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "20b_lieu_vaccination_province.png"),
             "Figure 25. Lieu de vaccination par province")

# 14. Annexes statistiques complémentaires
doc <- doc %>%
  body_add_par("14. Annexes statistiques complémentaires", style = "heading 1") %>%
  body_add_par("14.1. Importance perçue de la vaccination, par province",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_importance_vaccins_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "A01_importance_vaccins_province.png"),
             "Figure 26. Importance perçue de la vaccination par province") %>%
  body_add_par("14.2. Soutien des parents et amis proches, par province",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_parents_amis_vaccin_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "A02_parents_amis_province.png"),
             "Figure 27. Soutien des parents et amis par province") %>%
  body_add_par("14.3. Vaccins souhaités par le tuteur, par province",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_vaccins_souhaites_province.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "A03_vaccins_souhaites_province.png"),
             "Figure 28. Vaccins souhaités par province") %>%
  body_add_par("14.4. Couverture selon les caractéristiques du chef de ménage",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_couverture_chef_menage.csv")) %>%
  body_add_par("14.5. Couverture selon les caractéristiques du tuteur",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_couverture_tuteur.csv")) %>%
  body_add_par("14.6. Couverture selon les caractéristiques de l'enfant, par province",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "annexe_couverture_enfant_province.csv")) %>%
  body_add_par("14.7. Statut vaccinal antérieur détaillé (6-59 mois)",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "statut_vaccinal_anterieur_detail.csv")) %>%
  body_add_par("14.8. Qualité des données : concordance backcheck (enfants)",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "concordance_enfant.csv")) %>%
  add_figure(file.path(PATH_GRAPHIQUES, "10_concordance_enfant.png"),
             "Figure 29. Taux d'accord par variable (enquête vs backcheck)") %>%
  add_figure(file.path(PATH_GRAPHIQUES, "11_kappa_enfant.png"),
             "Figure 30. Coefficient Kappa de Cohen par variable") %>%
  body_add_par("14.9. Matrice complète province × groupe de raisons",
               style = "heading 2") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "matrice_province_raison.csv")) %>%
  body_add_par("14.10. Tableaux d'accompagnement des graphiques", style = "heading 2") %>%
  body_add_par("Distribution par tranche d'âge", style = "heading 3") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "distribution_age.csv")) %>%
  body_add_par("Couverture vaccinale par canal d'information", style = "heading 3") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "couverture_par_canal.csv")) %>%
  body_add_par("Couverture vaccinale par zone de santé", style = "heading 3") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "couverture_par_zone.csv")) %>%
  body_add_par("14.11. Registre des effectifs par indicateur", style = "heading 2") %>%
  body_add_par(
    paste("Ce registre précise, pour chaque indicateur, la population concernée,",
          "le numérateur, le dénominateur et le nombre d'enfants exclus faute",
          "d'information sur la variable. Il documente la base de calcul de chaque résultat."),
    style = "Normal") %>%
  add_csv_table(file.path(PATH_TABLEAUX, "registre_effectifs.csv"))

# Sauvegarde Word
word_path <- file.path(PATH_RAPPORT, "rapport_ecp_rdc.docx")
print(doc, target = word_path)
cli_alert_success("Document Word : {.path {word_path}}")

# --- Forcer la mise à jour automatique de la table des matières -------------
# officer insère la TOC sous forme de "champ" Word qui s'affiche vide tant qu'il
# n'est pas rafraîchi. On injecte <w:updateFields w:val="true"/> dans
# word/settings.xml pour que Word recalcule la TOC à l'ouverture du document.
.activer_maj_toc <- function(docx_path) {
  tryCatch({
    tmp <- file.path(tempdir(), paste0("docx_", as.integer(Sys.time())))
    if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
    dir.create(tmp)
    utils::unzip(docx_path, exdir = tmp)
    settings_path <- file.path(tmp, "word", "settings.xml")
    if (!file.exists(settings_path)) {
      # Créer un settings.xml minimal s'il est absent
      dir.create(dirname(settings_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<w:settings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
        '<w:updateFields w:val="true"/></w:settings>'), settings_path)
    } else {
      x <- readLines(settings_path, warn = FALSE, encoding = "UTF-8")
      x <- paste(x, collapse = "\n")
      if (!grepl("updateFields", x)) {
        # Insérer juste après la balise ouvrante <w:settings ...>
        x <- sub("(<w:settings[^>]*>)",
                 '\\1<w:updateFields w:val="true"/>', x)
        writeLines(x, settings_path, useBytes = TRUE)
      }
    }
    # Recompresser le .docx (zip à plat depuis le dossier temporaire)
    wd <- getwd(); on.exit(setwd(wd), add = TRUE)
    setwd(tmp)
    files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    if (file.exists(docx_path)) file.remove(docx_path)
    abs_out <- file.path(wd, docx_path)
    utils::zip(zipfile = abs_out, files = files, flags = "-r9Xq")
    setwd(wd)
    cli_alert_success("Table des matières : mise à jour automatique activée.")
  }, error = function(e)
    cli_alert_warning("Activation MAJ TOC impossible : {e$message}"))
}
.activer_maj_toc(word_path)


# --- 3. GENERATION DU FICHIER QUARTO -----------------------------------------

# On écrit le .qmd dans outputs/rapport/ pour que les chemins relatifs vers
# graphiques/ tableaux/ cartes/ soient corrects (../graphiques/...).
qmd_path <- file.path(PATH_RAPPORT, "rapport_ecp_rdc.qmd")

qmd_content <- r"--[---
title: "Enquête de Couverture Post-Vaccinale — Rougeole-Rubéole en République Démocratique du Congo"
subtitle: "Campagne Novembre–Décembre 2025 | Rapport de restitution"
author: "OMS RDC — Bureau Pays"
date: today
date-format: "D MMMM YYYY"
lang: fr

format:
  html:
    toc: true
    toc-depth: 3
    toc-location: left
    toc-title: "Table des matières"
    number-sections: true
    theme: cosmo
    code-fold: true
    embed-resources: true
    fig-width: 10
    fig-height: 6
  docx:
    toc: true
    toc-depth: 3
    number-sections: true
    fig-width: 6.5
    fig-height: 4
  pdf:
    toc: true
    toc-depth: 3
    number-sections: true
    fig-width: 6.5
    fig-height: 4
    include-in-header:
      text: |
        \usepackage{booktabs}
        \usepackage{float}
        \usepackage{fancyhdr}
        \pagestyle{fancy}
        \fancyhead[L]{\small ECP Rougeole-Rubéole 2025 — RDC}
        \fancyhead[R]{\small\thepage}
        \fancyfoot[C]{}

execute:
  echo: false
  warning: false
  message: false
---

```{r setup}
#| include: false
knitr::opts_chunk$set(
  echo      = FALSE,
  warning   = FALSE,
  message   = FALSE,
  fig.align = "center",
  dpi       = 180,
  out.width = "100%"
)

is_pdf  <- knitr::is_latex_output()
is_html <- knitr::is_html_output()

# ── Packages ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(scales)
  library(glue)
})

# ── Chemins (le .qmd est dans outputs/rapport/) ───────────────────────────────
PATH_TAB <- "../tableaux"
PATH_FIG <- "../graphiques"
PATH_CAR <- "../cartes"

# ─────────────────────────────────────────────────────────────────────────────
# CHARGEMENT DE TOUS LES TABLEAUX — source unique de vérité
# ─────────────────────────────────────────────────────────────────────────────
read_t <- function(name) {
  p <- file.path(PATH_TAB, paste0(name, ".csv"))
  if (!file.exists(p)) return(tibble())
  read_csv(p, show_col_types = FALSE)
}

cv_global      <- read_t("cv_global")
cv_province    <- read_t("cv_par_province")
cv_age         <- read_t("cv_par_age")
cv_sexe        <- read_t("cv_par_sexe")
cv_milieu      <- read_t("cv_par_milieu")
cv_sousgroupes <- read_t("couverture_sousgroupes")
cv_zone        <- read_t("couverture_par_zone")
denom          <- read_t("denombrement_par_province")
raisons_det    <- read_t("raisons_non_vaccination")
raisons_gr     <- read_t("raisons_par_groupe")
statut_ant     <- read_t("statut_vaccinal_anterieur")
confirmation   <- read_t("confirmation_carte")
determinants   <- read_t("determinants_or_ajustes")
carte_vs_decl       <- read_t("couverture_carte_vs_declaratif")
carte_vs_decl_prov  <- read_t("couverture_carte_vs_declaratif_province")
reponse_province    <- read_t("reponse_par_province")
canaux         <- read_t("canaux_information")
lieu_vacc      <- read_t("lieu_vaccination")
depenses       <- read_t("depenses_par_province")
symptomes      <- read_t("symptomes_post_vaccinaux")
symptomes_detail_t <- read_t("symptomes_detail")
matrice_complete_t <- read_t("matrice_province_raison")
distribution_age_t <- read_t("distribution_age")
profil_province_t <- read_t("profil_echantillon_province")
profil_enfant_t   <- read_t("profil_echantillon_enfant")
profil_tuteur_t   <- read_t("profil_echantillon_tuteur")
carac_menage_t    <- read_t("caracteristiques_menage")
depenses_motifs_t <- read_t("depenses_motifs")
registre_effectifs_t <- read_t("registre_effectifs")
couverture_canal_t <- read_t("couverture_par_canal")
couverture_zone_t  <- read_t("couverture_par_zone")
comparaison_cv_prov_t <- read_t("comparaison_cv_province")
comparaison_cv_zs_t   <- read_t("comparaison_cv_zone_sante")
chi2           <- read_t("test_chi2_geographique")
kpis           <- read_t("kpi_dashboard")
concordance    <- read_t("concordance_enfant")
qualite        <- read_t("qualite_par_type")
modele_adj     <- read_t("modele_ajustement")
# Nouvelles sorties (annexes et ajustements par province)
flowchart_t        <- read_t("flowchart_collecte")
dist_importance_t  <- read_t("annexe_importance_vaccins_province")
dist_pression_t    <- read_t("annexe_parents_amis_vaccin_province")
dist_souhaits_t    <- read_t("annexe_vaccins_souhaites_province")
cv_chef_t          <- read_t("annexe_couverture_chef_menage")
cv_tuteur_t        <- read_t("annexe_couverture_tuteur")
cv_enfant_prov_t   <- read_t("annexe_couverture_enfant_province")
carac_menage_t     <- read_t("caracteristiques_menage")
canaux_prov_t      <- read_t("canaux_information_province")
carte_prov_t       <- read_t("confirmation_carte_province")
lieu_prov_t        <- read_t("lieu_vaccination_province")
statut_detail_t    <- read_t("statut_vaccinal_anterieur_detail")

# ─────────────────────────────────────────────────────────────────────────────
# VARIABLES DYNAMIQUES GLOBALES — calculées une seule fois, réutilisées partout
# ─────────────────────────────────────────────────────────────────────────────

# Couverture nationale (estimation pondérée — source unique)
cv_nat_pct  <- cv_global$`Estimation (%)`[1]
cv_nat_ic   <- cv_global$`IC 95% (%)`[1]
cv_nat_n    <- cv_global$`Enfants enquêtés`[1]
cible_oms   <- 95
cv_atteinte <- cv_nat_pct >= cible_oms

# Dénombrement
tot_menages_denom <- denom$`Ménages dénombrés`[denom$Province == "TOTAL"]
tot_menages_elig  <- denom$`Ménages éligibles`[denom$Province == "TOTAL"]
tx_eligibilite    <- denom$`Taux d'éligibilité (%)`[denom$Province == "TOTAL"]
n_provinces       <- nrow(denom %>% filter(Province != "TOTAL"))

# Couvertures provinciales
cv_prov_min   <- cv_province %>% slice_min(`Couverture (%)`, n = 1)
cv_prov_max   <- cv_province %>% slice_max(`Couverture (%)`, n = 1)
n_prov_cible  <- sum(cv_province$Catégorie == "≥ 95% (cible OMS atteinte)")
n_prov_total  <- nrow(cv_province)
ecart_prov    <- round(cv_prov_max$`Couverture (%)` - cv_prov_min$`Couverture (%)`, 1)

# Chi-2
chi2_stat <- round(chi2$statistique[1], 1)
chi2_p    <- chi2$p_value[1]
chi2_sig  <- chi2_p < 0.05

# Zone à risque (< 80 %)
zones_critiques <- cv_zone %>% filter(pct < 80) %>%
  arrange(pct) %>%
  mutate(etiq = glue("{zone_sante_clean} ({province_clean}) : {round(pct,1)}%"))
n_zones_crit <- nrow(zones_critiques)

# Tranches d'âge — calculs DYNAMIQUES (la tranche la plus basse n'est pas fixe :
# elle varie selon les données ; on identifie celle avec la couverture minimale).
cv_age_sorted <- cv_age %>% arrange(`Couverture (%)`)
cv_age_min        <- cv_age_sorted$`Tranche d'âge`[1]
cv_age_min_pct    <- cv_age_sorted$`Couverture (%)`[1]
cv_age_min_sous_cible <- cv_age_min_pct < cible_oms
# Aliases conservés pour compatibilité avec d'anciens narratifs
cv_6_59   <- cv_age_sorted %>% slice(1)
cv_6_59_n <- cv_age_min_pct
cv_6_59_sous_cible <- cv_age_min_sous_cible
# Tranches sous la cible (peut être 0, 1 ou plusieurs)
tranches_sous_cible <- cv_age %>% filter(`Couverture (%)` < cible_oms) %>%
  arrange(`Couverture (%)`)
n_tranches_sous_cible <- nrow(tranches_sous_cible)

# Milieu de résidence (peut être absent si base de sondage indisponible)
milieu_dispo <- nrow(cv_milieu) > 0
if (milieu_dispo) {
  cv_rural  <- cv_milieu %>% filter(Milieu == "Rural")
  cv_urbain <- cv_milieu %>% filter(Milieu == "Urbain")
  cv_rural_n  <- if (nrow(cv_rural))  cv_rural$`Couverture (%)`[1]  else NA_real_
  cv_urbain_n <- if (nrow(cv_urbain)) cv_urbain$`Couverture (%)`[1] else NA_real_
  ecart_milieu <- if (!is.na(cv_rural_n) && !is.na(cv_urbain_n))
    round(abs(cv_rural_n - cv_urbain_n), 1) else NA_real_
  milieu_plus_haut <- if (!is.na(cv_rural_n) && !is.na(cv_urbain_n))
    (if (cv_rural_n >= cv_urbain_n) "rural" else "urbain") else NA_character_
  milieu_plus_bas  <- if (!is.na(milieu_plus_haut))
    (if (milieu_plus_haut == "rural") "urbain" else "rural") else NA_character_
}

# Non-vaccinés
n_non_vax       <- sum(raisons_det$n)

# Scalaires VCQI (zéro dose, DEFF, ICC, non-répondants)
.vcqi <- local({
  p <- file.path("..", "tableaux", ".vcqi_scalaires.txt")
  if (file.exists(p)) {
    v <- suppressWarnings(as.numeric(readLines(p, warn = FALSE)))
    list(zero_dose_pct = v[1], n_zero = as.integer(v[2]),
         deff = v[3], icc = v[4], cor_nr = v[5])
  } else list(zero_dose_pct = NA_real_, n_zero = NA_integer_,
              deff = NA_real_, icc = NA_real_, cor_nr = NA_real_)
})
zero_dose_pct_campagne <- .vcqi$zero_dose_pct
n_zero                 <- .vcqi$n_zero
deff_global            <- .vcqi$deff
icc_global             <- .vcqi$icc
cor_nr_cv              <- .vcqi$cor_nr   # 313 : non-vaccinés ayant déclaré une raison (depuis le tableau)
# Vrais effectifs lus depuis le fichier produit par les analyses :
#   ligne 1 = total non-vaccinés (374) ; ligne 2 = ceux ayant déclaré une raison (313)
.nv_path <- file.path("..", "tableaux", ".effectifs_nonvax.txt")
.nv <- if (file.exists(.nv_path)) suppressWarnings(as.integer(readLines(.nv_path, warn = FALSE))) else c(NA_integer_, NA_integer_)
n_non_vax_total <- .nv[1]                                  # 374
if (!is.na(.nv[2]) && .nv[2] > 0) n_non_vax <- .nv[2]      # 313 (source fiable si dispo)
n_sans_raison   <- if (!is.na(n_non_vax_total)) n_non_vax_total - n_non_vax else NA_integer_
raison_top1     <- raisons_det %>% slice_max(n, n = 1)
raison_top2     <- raisons_det %>% slice_max(n, n = 2) %>% slice_tail(n = 1)
pct_top2_cumul  <- round(
  (raisons_det %>% slice_max(n, n = 2) %>% pull(n) %>% sum()) /
  n_non_vax * 100, 1)

# Groupes de raisons
rg <- raisons_gr %>%
  group_by(raison_groupe) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(pct))
gr_top1 <- rg$raison_groupe[1]
gr_top1_pct <- rg$pct[1]
gr_top2 <- rg$raison_groupe[2]
gr_top2_pct <- rg$pct[2]

# Confirmation par carte
conf_vue <- confirmation %>% filter(confirmation_carte == "Carte vue")
conf_vue_pct <- conf_vue$pct[1]
conf_pas_carte <- confirmation %>% filter(confirmation_carte == "Pas de carte reçue")
conf_pas_pct <- conf_pas_carte$pct[1]

# Statut antérieur
sat_zero <- statut_ant %>% filter(statut_vaccinal_ant == "Zéro dose")
sat_zero_pct <- sat_zero$pct[1]

# Déterminants — OR ajustés (lookups génériques, non liés à une province précise)
or_info  <- determinants %>% filter(str_detect(Variable, "Informé"))
or_imp   <- determinants %>% filter(str_detect(Variable, "Importance"))

# Informés
kpi_info  <- kpis %>% filter(str_detect(indicateur, "information"))
pct_info  <- round(kpi_info$valeur[1], 1)

# Canaux top 2
can_top1  <- canaux %>% slice_max(n, n = 1)
can_top2  <- canaux %>% slice_max(n, n = 2) %>% slice_tail(n = 1)
pct_can12 <- round(
  (canaux %>% slice_max(n, n = 2) %>% pull(pct) %>% sum()) * 100, 1)

# Lieu vaccination
lieu_top1 <- lieu_vacc %>% slice_max(pct, n = 1) %>%
  mutate(pct_lbl = paste0(round(100 * pct, 1), "%"))

# Dépenses anormales
dep_top2 <- depenses %>% arrange(desc(pct_dep)) %>% head(2)

# Symptômes
symp_oui  <- symptomes %>% filter(measles_symptom_f == "Oui")
symp_pct  <- symp_oui$pct[1]
kpi_symp_cible <- kpis %>% filter(str_detect(indicateur, "ymptôme")) %>% pull(cible)

# Qualité données
qt_t1 <- qualite %>% filter(Type == "T1")
qt_t2 <- qualite %>% filter(Type == "T2")
qt_t3 <- qualite %>% filter(Type == "T3")

# Cartes
kpi_cartes <- kpis %>% filter(str_detect(indicateur, "Cartes"))
pct_cartes <- round(kpi_cartes$valeur[1], 1)

# ── Helpers d'affichage ───────────────────────────────────────────────────────

# Formate un pourcentage en fraction lisible (ex : 94,5 % → "19 sur 20")
fraction_lisible <- function(pct) {
  if (pct >= 99) return("pratiquement la totalité")
  if (pct >= 97.5) return("plus de 39 sur 40")
  if (pct >= 95) return("plus de 19 sur 20")
  if (pct >= 90) return("plus de 9 sur 10")
  if (pct >= 80) return("plus de 4 sur 5")
  if (pct >= 75) return("3 enfants sur 4")
  if (pct >= 50) return("plus d'1 sur 2")
  return(paste0("moins d'1 sur 2"))
}

# Affiche un pourcentage avec la virgule française
pct_fr <- function(x) format(round(x, 1), nsmall = 1, big.mark = " ",
                              decimal.mark = ",")

# Affiche un nombre avec espace comme séparateur de milliers
n_fr <- function(x) format(as.integer(x), big.mark = " ", scientific = FALSE)

# Helpers de formatage français (alias robustes, gèrent NA)
fmt_pct1 <- function(x) ifelse(is.na(x), "—",
  format(round(x, 1), nsmall = 1, decimal.mark = ",", big.mark = " ", trim = TRUE))
fmt_n <- function(x) ifelse(is.na(x), "—",
  format(as.integer(round(x)), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_ic <- function(bas, haut) ifelse(is.na(bas) | is.na(haut), "—",
  sprintf("[%s - %s]", format(round(bas, 1), nsmall = 1, decimal.mark = ","),
          format(round(haut, 1), nsmall = 1, decimal.mark = ",")))

# Tableau kableExtra adaptatif (HTML interactif, Word/PDF natif)
afficher_tableau <- function(df, caption = NULL, ...) {
  if (is_html) {
    df %>%
      kbl(caption = caption, booktabs = TRUE, ...) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width        = FALSE
      )
  } else {
    # Word / PDF : tableau natif sans styles HTML (évite l'erreur de rendu)
    df %>%
      kbl(caption = caption, booktabs = TRUE, ...) %>%
      kable_styling(
        latex_options = c("striped", "hold_position"),
        full_width    = FALSE
      )
  }
}

# Mini-convertisseur markdown -> HTML (gère **gras**, *italique*, sauts <br>)
# Nécessaire car le HTML injecté via asis_output n'est PAS retraité par Pandoc :
# la syntaxe markdown **gras** resterait littérale sinon.
.md_to_html <- function(x) {
  x <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
  x <- gsub("(?<![\\*])\\*(?!\\*)(.+?)(?<!\\*)\\*(?!\\*)", "<em>\\1</em>",
            x, perl = TRUE)
  x
}

# Encadré callout
callout <- function(texte, type = "info") {
  # Les encadrés interprétatifs sont du texte narratif : HTML uniquement.
  if (!is_html) return(invisible(NULL))
  cfg <- list(
    info    = list(bg = "#E3F2FD", bd = "#1565C0", ico = "ℹ️"),
    success = list(bg = "#E8F5E9", bd = "#2E7D32", ico = "✅"),
    warning = list(bg = "#FFF8E1", bd = "#F57F17", ico = "⚠️"),
    danger  = list(bg = "#FFEBEE", bd = "#C62828", ico = "🔴")
  )[[type]]
  knitr::asis_output(sprintf(
    '<div style="background:%s;border-left:5px solid %s;padding:12px 16px;
     border-radius:4px;margin:16px 0;font-size:0.95em;line-height:1.6;
     color:#1a1a1a;">%s %s</div>',
    cfg$bg, cfg$bd, cfg$ico, .md_to_html(texte)))
}

# Saut de page
newpage <- function() {
  if (is_pdf) knitr::asis_output("\\newpage")
  else knitr::asis_output('<hr style="border:2px solid #1565C0;margin:36px 0;">')
}

# Inclure une figure si elle existe
show_fig <- function(fname, cap = "") {
  # Si le chemin contient déjà un séparateur (ex. "../cartes/x.png"), on l'utilise
  # tel quel ; sinon on le préfixe par le dossier des graphiques.
  p <- if (grepl("/", fname)) fname else file.path(PATH_FIG, fname)
  # Chemin absolu normalisé : indispensable pour que le rendu auto-contenu
  # (self_contained / embed-resources) embarque correctement l'image, même
  # lorsqu'elle se trouve hors du répertoire de travail (chemin en ../).
  if (file.exists(p)) {
    include_graphics(normalizePath(p, winslash = "/", mustWork = FALSE))
  } else {
    cat(glue("*(Figure non disponible : {fname})*"))
  }
}

# Texte narratif dynamique : affiché UNIQUEMENT en HTML.
# En Word/PDF, on ne produit que les résultats (tableaux, graphiques).
narratif <- function(texte) {
  if (is_html) knitr::asis_output(paste0("\n", texte, "\n"))
  else invisible(NULL)
}

# note_pop() : note dynamique précisant population / numérateur / dénominateur
# d'un indicateur, lue depuis le registre des effectifs produit par l'analyse.
# `cle` est un motif (regex, insensible casse) cherché dans la colonne indicateur.
# Rendu en callout "info" visible dans tous les formats (résultats + HTML).
note_pop <- function(cle) {
  if (!exists("registre_effectifs_t") || is.null(registre_effectifs_t) ||
      nrow(registre_effectifs_t) == 0) return(invisible(NULL))
  ligne <- registre_effectifs_t[grepl(cle, registre_effectifs_t$indicateur,
                                       ignore.case = TRUE), , drop = FALSE]
  if (nrow(ligne) == 0) return(invisible(NULL))
  ligne <- ligne[1, ]
  txt <- paste0(
    "**Population et base de calcul.** ",
    "*Population concernée :* ", ligne$population, ". ",
    "*Numérateur :* ", ligne$numerateur, ". ",
    "*Dénominateur :* ", ligne$denominateur, " — soit **", n_fr(ligne$n_denom),
    " enfants** (", n_fr(ligne$n_exclus),
    " exclus de l'échantillon analytique pour information manquante sur cette variable)."
  )
  callout(txt, "info")
}
```

# Résumé exécutif {-}

```{r resume-kpis, results='asis'}
# Encadré KPIs en haut du résumé exécutif
kpi_txt <- glue(
  "**Couverture nationale : {pct_fr(cv_nat_pct)} %** (IC 95 % : {cv_nat_ic}) — ",
  "Cible OMS ≥ {cible_oms} % : ",
  if (cv_atteinte) "**atteinte** ✅" else "**non atteinte** ⚠️ (–{pct_fr(cible_oms - cv_nat_pct)} point)"
)
callout(kpi_txt, if (cv_atteinte) "success" else "warning")
```

::: {.content-visible when-format="html"}
La campagne de vaccination contre la rougeole et la rubéole conduite en RDC de novembre à décembre 2025 a mobilisé les équipes de santé dans **`r n_provinces` provinces** pour atteindre les enfants âgés de 6 mois à 14 ans. Cette enquête de couverture post-campagne (ECP) a été menée auprès de **`r n_fr(cv_nat_n)` enfants** afin d'évaluer de façon indépendante les résultats obtenus.
:::

::: {.content-visible when-format="html"}
Le résultat le plus important est que **`r fraction_lisible(cv_nat_pct)` des enfants éligibles ont été vaccinés** lors de cette campagne (`r pct_fr(cv_nat_pct)` %). Ce résultat place la campagne **`r if (cv_atteinte) "légèrement au-dessus" else "légèrement en dessous"` de la cible internationale de `r cible_oms` %** fixée par l'OMS. Ce chiffre national cache cependant des disparités importantes entre provinces : les résultats varient de **`r pct_fr(cv_prov_min$"Couverture (%)")`  %** à `r cv_prov_min$Province[1]` jusqu'à **`r pct_fr(cv_prov_max$"Couverture (%)")` %** à `r cv_prov_max$Province[1]`, soit un écart de **`r ecart_prov` points de pourcentage**. Sur les `r n_prov_total` provinces enquêtées, **`r n_prov_cible`** ont atteint la cible OMS de `r cible_oms` %.
:::

::: {.content-visible when-format="html"}
Du côté des enfants qui n'ont pas été vaccinés (`r if (!is.na(n_non_vax_total)) n_fr(n_non_vax_total) else n_fr(n_non_vax)` enfants documentés, dont `r n_fr(n_non_vax)` ont déclaré une raison), deux raisons dominent : **`r tolower(raison_top1$raison_pas_lbl[1])`** (`r pct_fr(raison_top1$pct[1])` %) et **`r tolower(raison_top2$raison_pas_lbl[1])`** (`r pct_fr(raison_top2$pct[1])` %). Ces deux causes représentent à elles seules **`r pct_top2_cumul` %** de tous les cas de non-vaccination documentés, ce qui signifie qu'elles constituent les leviers d'action prioritaires pour les prochaines campagnes.
:::

::: {.content-visible when-format="html"}
L'analyse des facteurs favorisant la vaccination confirme le rôle central de l'information : **un enfant dont les parents ont été informés avant le début de la campagne a `r round(or_info$"OR ajusté"[1])` fois plus de chances d'être vacciné** qu'un enfant dont la famille n'a pas reçu cette information. Ce résultat plaide fortement pour un renforcement de la mobilisation sociale préalable à toute campagne.
:::

```{r messages-cles, results='asis'}
# Provinces sous la cible OMS (peut être 0, 1 ou plusieurs)
prov_sous_cible <- cv_province %>%
  filter(`Couverture (%)` < cible_oms) %>%
  arrange(`Couverture (%)`)
n_prov_sous <- nrow(prov_sous_cible)

# Message 2 : provinces nécessitant action corrective (dynamique)
if (n_prov_sous == 0) {
  msg2 <- paste0(
    "② **Toutes les provinces atteignent la cible OMS** — un acquis à consolider ",
    "pour prévenir tout relâchement lors des prochaines campagnes.<br>")
} else if (n_prov_sous == 1) {
  msg2 <- paste0(
    "② **", prov_sous_cible$Province[1], "** (",
    pct_fr(prov_sous_cible$`Couverture (%)`[1]), " %) est la seule province sous la cible OMS — ",
    "elle nécessite des actions correctives prioritaires.<br>")
} else {
  msg2 <- paste0(
    "② **", prov_sous_cible$Province[1], "** (",
    pct_fr(prov_sous_cible$`Couverture (%)`[1]), " %) et **",
    prov_sous_cible$Province[2], "** (",
    pct_fr(prov_sous_cible$`Couverture (%)`[2]), " %) sont les provinces ",
    "les plus en retard — actions correctives prioritaires.<br>")
}

# Message 3 : tranche d'âge la plus basse (dynamique, jamais "6-59 mois" en dur)
if (cv_age_min_sous_cible) {
  msg3 <- paste0(
    "③ Les enfants de **", cv_age_min, "** (couverture : ",
    pct_fr(cv_age_min_pct), " %) constituent la tranche d'âge la plus vulnérable, ",
    "sous la cible OMS — un effort ciblé est nécessaire pour ce groupe.<br>")
} else {
  msg3 <- paste0(
    "③ Toutes les tranches d'âge atteignent la cible OMS, la plus basse étant ",
    "les enfants de **", cv_age_min, "** (", pct_fr(cv_age_min_pct), " %).<br>")
}

# Message 5 : dépenses dynamiques (selon disponibilité)
if (exists("dep_top2") && nrow(dep_top2) >= 2) {
  # Récupérer le nom de la colonne province (peut être "province_clean" ou "Province")
  prov_col <- intersect(c("province_clean", "Province"), names(dep_top2))[1]
  p1 <- if (!is.na(prov_col)) dep_top2[[prov_col]][1] else "Province 1"
  p2 <- if (!is.na(prov_col)) dep_top2[[prov_col]][2] else "Province 2"
  msg5 <- paste0(
    "⑤ Des **dépenses anormales** signalées à ", p1, " (",
    pct_fr(dep_top2$pct_dep[1]), " % des ménages) et ", p2, " (",
    pct_fr(dep_top2$pct_dep[2]), " %) compromettent la gratuité effective de la campagne.")
} else {
  msg5 <- paste0(
    "⑤ La **gratuité effective de la campagne** est globalement préservée, ",
    "seules quelques dépenses résiduelles étant signalées (transport, jeton).")
}

callout(paste0(
  "**5 messages clés :**<br>",
  "① La couverture nationale de **", pct_fr(cv_nat_pct), " %** est ",
  if (cv_atteinte) "au niveau de" else "juste en dessous de",
  " la cible OMS (", cible_oms, " %) — un résultat encourageant mais fragile.<br>",
  msg2,
  msg3,
  "④ **L'information préalable** est le déterminant le plus puissant de la vaccination ",
  "(ORa = ", or_info$"OR ajusté"[1], ") — renforcer la mobilisation sociale est la priorité n° 1.<br>",
  msg5
), "info")
```

`r newpage()`

# Introduction et contexte

## Contexte de la campagne

::: {.content-visible when-format="html"}
La rougeole et la rubéole figurent parmi les maladies infectieuses les plus contagieuses. En l'absence d'une couverture vaccinale suffisante, elles peuvent provoquer des épidémies aux conséquences graves — encéphalites, surdités, malformations congénitales. En République Démocratique du Congo, pays de plus de 100 millions d'habitants, maintenir une immunité collective élevée exige des efforts constants tant en vaccination de routine qu'en campagnes supplémentaires.
:::

::: {.content-visible when-format="html"}
La campagne de vaccination contre la **Rougeole-Rubéole (RR)** s'est déroulée de **novembre à décembre 2025** dans **`r n_provinces` provinces** du pays. Elle ciblait les enfants âgés de **6 mois à 14 ans** résidant dans les zones de santé couvertes, quel que soit leur statut vaccinal antérieur.
:::

## Objectif de l'enquête

::: {.content-visible when-format="html"}
Cette enquête de couverture post-campagne (ECP) a été commanditée par l'**OMS Bureau RDC** afin de mesurer de façon indépendante et rigoureuse les résultats obtenus. Elle répond à quatre questions concrètes :
:::

1. **Quelle proportion d'enfants a effectivement été vaccinée** lors de la campagne ?
2. **Les résultats varient-ils** selon la province, la zone de santé, l'âge ou le sexe de l'enfant ?
3. **Pourquoi certains enfants n'ont-ils pas été vaccinés** ?
4. **Quels facteurs favorisent ou freinent** la vaccination, pour orienter les prochaines campagnes ?

## Provinces enquêtées et dénombrement

::: {.content-visible when-format="html"}
La phase préliminaire de dénombrement a permis d'identifier les ménages éligibles dans `r n_provinces` provinces. Au total, **`r n_fr(tot_menages_denom)` ménages ont été dénombrés**, dont **`r n_fr(tot_menages_elig)` (soit `r pct_fr(tx_eligibilite)` %) ont été déclarés éligibles** car hébergeant au moins un enfant dans la tranche d'âge cible.
:::

```{r tab-denom}
denom %>%
  filter(Province != "TOTAL") %>%
  bind_rows(denom %>% filter(Province == "TOTAL")) %>%
  rename(
    `Ménages dénombrés` = `Ménages dénombrés`,
    `Ménages éligibles` = `Ménages éligibles`,
    `Taux éligibilité (%)` = `Taux d'éligibilité (%)`
  ) %>%
  mutate(
    `Ménages dénombrés` = n_fr(`Ménages dénombrés`),
    `Ménages éligibles` = n_fr(`Ménages éligibles`)
  ) %>%
  afficher_tableau(
    caption = "Dénombrement des ménages éligibles par province"
  ) %>%
  row_spec(nrow(denom), bold = TRUE, background = "#E3F2FD")
```

```{r fig-denom, fig.cap="Ménages dénombrés par province — phase de dénombrement ECP", fig.height=5}
show_fig("01_denombrement_par_province.png")
```

::: {.content-visible when-format="html"}
`r local({ d <- denom %>% filter(Province != "TOTAL"); idx <- which.min(d[["Taux d'éligibilité (%)"]]); paste0("Le taux d'éligibilité le plus faible est enregistré à **", d$Province[idx], "** (", pct_fr(d[["Taux d'éligibilité (%)"]][idx]), " %), ce qui peut refléter une proportion plus élevée de ménages sans enfants dans la tranche d'âge cible.") })`
:::

`r newpage()`

# Méthode de l'enquête

## Un sondage en grappes représentatif

::: {.content-visible when-format="html"}
L'ECP a utilisé un **sondage en grappes à deux degrés** : dans un premier temps, des aires de santé ont été tirées au sort avec une probabilité proportionnelle à leur taille (méthode PPS — *Probability Proportional to Size*), garantissant ainsi une meilleure représentation des zones les plus peuplées. Dans un second temps, des ménages ont été sélectionnés de façon aléatoire au sein de chaque aire tirée.
:::

::: {.content-visible when-format="html"}
En langage pratique : l'enquête a été conçue pour que ses résultats soient **représentatifs de l'ensemble de la population cible** dans chaque province, à condition de tenir compte des poids attribués à chaque ménage.
:::

## Pondération et intervalles de confiance

::: {.content-visible when-format="html"}
Tous les résultats présentés dans ce rapport utilisent des **estimations pondérées**, c'est-à-dire que les chiffres tiennent compte de la probabilité qu'avait chaque ménage d'être sélectionné. Chaque estimation est accompagnée d'un **intervalle de confiance à 95 %** (IC 95 %), qui indique la plage dans laquelle se situerait le vrai résultat si l'on répétait l'enquête de nombreuses fois dans les mêmes conditions.
:::

> **Comment lire un intervalle de confiance ?** Lorsqu'on lit « `r pct_fr(cv_nat_pct)` % [IC 95 % : `r cv_nat_ic`] », cela signifie : « nous estimons que la couverture est de `r pct_fr(cv_nat_pct)` %, et nous sommes confiants à 95 % qu'elle se situe entre les deux bornes indiquées. »

## Taille de l'échantillon

**`r n_fr(cv_nat_n)` enfants** ont finalement été inclus dans l'analyse, après application des critères d'éligibilité (présence durant la campagne, ménage consentant, données complètes). Le questionnaire a été administré au répondant principal de chaque ménage — généralement un parent ou tuteur de l'enfant.

## Déroulement de la collecte

```{r tab-flowchart}
if (nrow(flowchart_t) > 0) {
  flowchart_t %>%
    select(Étape, Effectif) %>%
    afficher_tableau(caption = "Effectifs aux étapes clés de la collecte")
}
```

```{r fig-flowchart, fig.cap="Diagramme de flux de la collecte des données — effectifs aux étapes clés", fig.height=8}
show_fig("00_flowchart_collecte.png")
```

```{r interp-flowchart, results='asis'}
if (nrow(flowchart_t) > 0) {
  get_eff <- function(motif) {
    r <- flowchart_t[grepl(motif, flowchart_t$Étape, ignore.case = TRUE), ]
    if (nrow(r)) as.character(r$Effectif[1]) else "—"
  }
  zd  <- get_eff("ZD")
  men <- get_eff("approchés")
  enf <- get_eff("analysés")
  narratif(glue(
    "Le diagramme ci-dessus retrace le parcours de la collecte : de **",
    "{zd} zones de dénombrement** visitées, les équipes ont approché **",
    "{men} ménages**, pour aboutir à **{enf} enfants** retenus pour ",
    "l'analyse après application des critères d'éligibilité. Le très faible nombre ",
    "de refus témoigne d'une bonne acceptabilité de l'enquête par les communautés."
  ))
  callout(glue(
    "**Note technique — pour les spécialistes.**\n\n",
    "**Critères d'éligibilité :** enfant âgé de 6 mois à 14 ans révolu au moment de la ",
    "campagne, présent dans la zone de résidence pendant la période de vaccination, ",
    "consentement du tuteur obtenu.\n\n",
    "**Taux de couverture de l'enquête :** {get_eff('éligibles')} ménages éligibles sur ",
    "{men} approchés. Les non-éligibles correspondent à des ménages sans enfants dans ",
    "la tranche d'âge cible.\n\n",
    "**Dénominateurs successifs :** l'échantillon analytique ({enf} enfants) sert de ",
    "référence pour le diagramme. Le dénominateur effectif varie ensuite selon ",
    "l'indicateur calculé : voir la note sur les effectifs ci-dessous."
  ), "info")
}
```

```{r note-effectifs, results='asis'}
# Note explicative sur les écarts d'effectifs entre flowchart et tableaux de CV
if (exists("enfant_anal")) {
  n_anal   <- nrow(enfant_anal)                            # Échantillon analytique
  n_cv     <- if ("vaccine_bin" %in% names(enfant_anal))   # Dénominateur CV nationale
                sum(!is.na(enfant_anal$vaccine_bin)) else NA
  n_age    <- if (all(c("vaccine_bin","tranche_age") %in% names(enfant_anal)))
                sum(!is.na(enfant_anal$vaccine_bin) &
                    !is.na(enfant_anal$tranche_age)) else NA
  exclus_nsp <- n_anal - n_cv
  exclus_age <- n_cv   - n_age
  callout(glue(
    "**Note sur les effectifs utilisés dans les analyses.** ",
    "Le diagramme de flux retient **{n_fr(n_anal)} enfants** comme échantillon ",
    "analytique (interviewés et éligibles). Selon l'indicateur calculé, le ",
    "dénominateur effectif peut être légèrement inférieur :<br>",
    "• **Couverture vaccinale nationale et provinciale : {n_fr(n_cv)} enfants** ",
    "— les **{n_fr(exclus_nsp)} enfants** dont le statut vaccinal est \"Ne sait pas\" ",
    "sont exclus du numérateur ET du dénominateur (codés `NA`), conformément aux ",
    "recommandations de l'OMS pour les enquêtes de couverture.<br>",
    "• **Couverture par tranche d'âge : {n_fr(n_age)} enfants** — on retire en plus ",
    "les **{n_fr(exclus_age)} enfants** sans tranche d'âge renseignée (information ",
    "incomplète sur l'âge déclaré). La somme des effectifs provinciaux dans ce tableau ",
    "correspond donc à ce sous-total et non à l'échantillon analytique complet.<br>",
    "Ces écarts sont **attendus** et reflètent la qualité de l'information disponible ",
    "pour chaque variable ; ils ne traduisent ni perte de données ni biais. Les ",
    "estimations restent toutes pondérées par le plan de sondage (voir section suivante)."
  ), "info")
}
```

## Comment le poids de sondage est-il calculé ?

::: {.content-visible when-format="html"}
Cette sous-section explique, en termes simples, ce qu'est un **poids de sondage** et pourquoi il est indispensable. Elle s'adresse aux lecteurs non familiers des méthodes statistiques.
:::

**Le principe de base.** Il serait impossible d'interroger *tous* les enfants des sept provinces. On en sélectionne donc un échantillon, par tirage au sort. Mais tous les enfants n'ont pas exactement la même probabilité d'être sélectionnés : un enfant vivant dans une petite zone peu peuplée peut avoir plus (ou moins) de chances d'être tiré qu'un enfant d'une grande ville. Si l'on comptait chaque enfant de la même façon, on donnerait trop d'importance à certains et pas assez à d'autres. **Le poids corrige ce déséquilibre.**

**Une image simple.** Imaginez que dans l'échantillon, un enfant a été tiré parmi 100 enfants similaires de sa zone. Cet enfant « représente » donc environ 100 enfants de la population réelle : son poids est d'environ 100. Un autre enfant tiré parmi 500 en représente 500 : son poids est plus élevé. Lorsqu'on calcule la couverture, on additionne ces poids plutôt que de simplement compter les têtes — ainsi chaque enfant compte à hauteur du nombre d'enfants réels qu'il représente.

**Les trois étapes du tirage.** Le poids reflète la probabilité de sélection à chacune des étapes du sondage :

1. **Tirage des zones de dénombrement (ZD)** avec une probabilité proportionnelle à leur taille (méthode dite *PPS*) : les zones les plus peuplées ont plus de chances d'être retenues, ce qui est logique puisqu'elles abritent plus d'enfants.
2. **Tirage des îlots** (sous-ensembles d'une ZD) de façon aléatoire simple.
3. **Tirage des ménages** au sein de chaque îlot, également de façon aléatoire.

::: {.content-visible when-format="html"}
À chaque étape, on connaît la probabilité de sélection. Le **poids initial** d'un enfant est l'inverse du produit de ces trois probabilités : plus un enfant avait *peu* de chances d'être tiré, plus son poids est *élevé*, car il représente davantage d'enfants non tirés.
:::

**L'ajustement pour la non-réponse.** Certains ménages sélectionnés sont absents ou refusent de participer. Pour éviter que ces absences ne biaisent les résultats, on **augmente légèrement le poids** des ménages répondants d'une même aire de santé, de sorte qu'ils « compensent » les non-répondants comparables. C'est le **facteur d'ajustement pour non-réponse**.

> **En résumé :** le poids final d'un enfant = (inverse de sa probabilité de sélection aux trois étapes) × (facteur de correction pour non-réponse). Les estimations de couverture, ainsi que tous les pourcentages et intervalles de confiance de ce rapport, sont calculés à l'aide de ces poids, via le package R `survey` (Lumley, 2010). C'est ce qui garantit que les chiffres reflètent la **population réelle** des enfants éligibles, et non simplement l'échantillon interrogé.

`r newpage()`

# Résultats de la couverture vaccinale

## Couverture nationale

```{r kpi-cv-national, results='asis'}
msg <- glue(
  "Couverture vaccinale nationale : **{pct_fr(cv_nat_pct)} %** ",
  "(IC 95 % : {cv_nat_ic} | n = {n_fr(cv_nat_n)} enfants) — ",
  "Cible OMS ≥ {cible_oms} % : ",
  if (cv_atteinte) "**ATTEINTE** ✅"
  else glue("**NON ATTEINTE** ⚠️ ({pct_fr(cible_oms - cv_nat_pct)} point en dessous)")
)
callout(msg, if (cv_atteinte) "success" else "warning")
```

::: {.content-visible when-format="html"}
Sur l'ensemble des **`r n_fr(cv_nat_n)` enfants** éligibles enquêtés, **`r fraction_lisible(cv_nat_pct)`** ont été vaccinés lors de la campagne de novembre–décembre 2025, soit un taux de **`r pct_fr(cv_nat_pct)` %**. Ce résultat `r if (cv_atteinte) "atteint" else "s'approche de"` la cible internationale fixée par l'OMS (`r cible_oms` %) `r if (!cv_atteinte) glue("mais reste {pct_fr(cible_oms - cv_nat_pct)} point en dessous de l'objectif")`.
:::

```{r fig-cv-global, fig.cap=glue("Couverture vaccinale nationale — {pct_fr(cv_nat_pct)} % (IC 95 % : {cv_nat_ic})"), fig.height=5.5}
show_fig("02_cv_globale.png")
```

::: {.content-visible when-format="html"}
Il est important de souligner que ce chiffre national **résulte d'une estimation pondérée** tenant compte du plan de sondage complexe. C'est cette valeur qui doit être retenue comme référence officielle.
:::

```{r note-pop-cv-global, results='asis'}
note_pop("Couverture vaccinale \\(globale")
```

## Couverture par province {#couverture-province}

::: {.content-visible when-format="html"}
Les résultats varient de façon importante selon les provinces, confirmant que la performance nationale masque des **disparités géographiques significatives** (test du Chi-2 : khi² = `r chi2_stat`, p < 0,001 — la différence entre provinces ne peut être attribuée au hasard).
:::

```{r tab-cv-province}
cv_province %>%
  mutate(
    `Couverture (%)` = paste0(pct_fr(`Couverture (%)`), " %"),
    `n (enfants)` = n_fr(`n (enfants)`)
  ) %>%
  rename(
    `IC 95 %` = `IC 95%`,
    `Catégorie` = Catégorie
  ) %>%
  afficher_tableau(
    caption = glue("Couverture vaccinale pondérée par province (cible OMS : ≥ {cible_oms} %)")
  ) %>%
  row_spec(
    which(cv_province$Catégorie == "≥ 95% (cible OMS atteinte)"),
    background = "#E8F5E9"
  ) %>%
  row_spec(
    which(cv_province$Catégorie != "≥ 95% (cible OMS atteinte)"),
    background = "#FFF8E1"
  )
```

```{r fig-cv-province, fig.cap="Couverture vaccinale pondérée par province avec intervalles de confiance à 95 %", fig.height=5.5}
show_fig("03_cv_par_province.png")
```

**`r n_prov_cible` province`r if(n_prov_cible > 1) "s" else ""`** sur `r n_prov_total` ont atteint la cible OMS : `r paste(cv_province$Province[cv_province$Catégorie == "≥ 95% (cible OMS atteinte)"], collapse = ", ")`. À l'opposé, **`r cv_prov_min$Province[1]`** (`r pct_fr(cv_prov_min$"Couverture (%)")` %) `r if (nrow(cv_province %>% filter(Catégorie != "≥ 95% (cible OMS atteinte)")) > 1) paste0("et ", paste(cv_province$Province[cv_province$Catégorie != "≥ 95% (cible OMS atteinte)" & cv_province$Province != cv_prov_min$Province[1]], collapse = ", "), " ont des couvertures") else "a une couverture"` sous la cible, représentant un défi important pour les prochaines interventions.

::: {.content-visible when-format="html"}
L'écart entre la province la plus performante (`r cv_prov_max$Province[1]` : `r pct_fr(cv_prov_max$"Couverture (%)")` %) et la moins performante (`r cv_prov_min$Province[1]` : `r pct_fr(cv_prov_min$"Couverture (%)")` %) est de **`r ecart_prov` points**. Cet écart appelle des stratégies différenciées par province.
:::

::: {.content-visible when-format="html"}
La carte ci-dessous visualise géographiquement ces disparités : les provinces en vert foncé atteignent la cible, celles en orange/rouge restent en deçà.
:::

```{r fig-carte-prov, fig.cap="Carte choroplèthe de la couverture vaccinale par province — la couleur reflète le niveau de couverture atteint", fig.height=6}
show_fig("../cartes/carte_cv_par_province.png")
```

## Disparités intra-provinciales : analyse par zone de santé

::: {.content-visible when-format="html"}
`r local({ p <- file.path("..","tableaux",".cv_inter_zone.txt"); cvz <- if (file.exists(p)) readLines(p, warn=FALSE)[1] else NA; if (!is.na(cvz)) paste0("Au-delà des résultats provinciaux, l'analyse par zone de santé révèle des **variations encore plus importantes à l'intérieur de chaque province**. Le coefficient de variation inter-zones est de **", cvz, " %**, traduisant une hétérogénéité marquée des performances.") else "Au-delà des résultats provinciaux, l'analyse par zone de santé révèle des **variations encore plus importantes à l'intérieur de chaque province**." })`
:::

```{r fig-heatmap, fig.cap="Heatmap de la couverture vaccinale par zone de santé — les zones en rouge/jaune sont sous la cible", fig.height=10}
show_fig("15_heatmap_zone_sante.png")
```

```{r alerte-zones-critiques, results='asis'}
if (n_zones_crit > 0) {
  txt <- paste0(
    "**", n_zones_crit, " zone", if (n_zones_crit > 1) "s" else "",
    " de santé** présentent une couverture inférieure à 80 % — ",
    "seuil critique nécessitant une intervention correctrice immédiate :<br>",
    paste0("• ", zones_critiques$etiq, collapse = "<br>")
  )
  callout(txt, "danger")
}
```

```{r tab-zones-critiques}
if (n_zones_crit > 0) {
  cv_zone %>%
    filter(pct < 90) %>%
    arrange(pct) %>%
    mutate(
      Couverture = paste0(pct_fr(pct), " %"),
      N = n_fr(N),
      Vaccinés = n_fr(Nvax)
    ) %>%
    select(Province = province_clean, `Zone de santé` = zone_sante_clean,
           N, Vaccinés, Couverture) %>%
    afficher_tableau(
      caption = "Zones de santé avec couverture < 90 % — priorités d'action"
    ) %>%
    row_spec(which(as.numeric(str_remove(
      (cv_zone %>% filter(pct < 90) %>% arrange(pct))$pct |> round(1),
      "%")) < 80), bold = TRUE, background = "#FFEBEE")
}
```

## Couverture par tranche d'âge

```{r tab-cv-age}
cv_age %>%
  mutate(`Couverture (%)` = paste0(pct_fr(`Couverture (%)`), " %")) %>%
  afficher_tableau(
    caption = "Couverture vaccinale pondérée par tranche d'âge"
  ) %>%
  row_spec(which(cv_age$`Couverture (%)` < cible_oms),
           background = "#FFF8E1", bold = FALSE)
```

```{r fig-cv-age, fig.cap="Couverture vaccinale par tranche d'âge avec intervalles de confiance à 95 %", fig.height=5}
show_fig("04_cv_par_age.png")
```

```{r note-pop-cv-age, results='asis'}
note_pop("Couverture par tranche d.âge")
```

::: {.content-visible when-format="html"}
Les effectifs par tranche d'âge présentés ici correspondent **exactement** à ceux du tableau de distribution figurant à l'annexe K.1 : les deux tableaux reposent désormais sur la même population (enfants au statut vaccinal et à la tranche d'âge connus).
:::

```{r interp-age, results='asis'}
# Utiliser la tranche EFFECTIVEMENT la plus basse (cv_age_min) — cohérent avec
# le texte qui suit (qui utilise which.min)
if (cv_age_min_sous_cible) {
  if (n_tranches_sous_cible == 1) {
    msg <- glue(
      "Les enfants de **{cv_age_min}** sont la seule tranche dont la couverture ",
      "({pct_fr(cv_age_min_pct)} %) passe sous la cible OMS de {cible_oms} %. ",
      "Ce groupe nécessite une attention particulière lors des prochaines campagnes."
    )
  } else {
    autres <- paste(tranches_sous_cible$`Tranche d'âge`[-1], collapse = ", ")
    msg <- glue(
      "Les enfants de **{cv_age_min}** présentent la couverture la plus basse ",
      "({pct_fr(cv_age_min_pct)} %), sous la cible OMS de {cible_oms} %. ",
      "{n_tranches_sous_cible} tranches d'âge sont concernées (également : {autres})."
    )
  }
  callout(msg, "warning")
} else {
  callout(glue(
    "Toutes les tranches d'âge atteignent la cible OMS de {cible_oms} %, ",
    "la plus basse étant les **{cv_age_min}** à {pct_fr(cv_age_min_pct)} %."
  ), "success")
}
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Dénominateur :** {n_fr(sum(cv_age$n, na.rm=TRUE))} enfants ayant un statut vaccinal ET une tranche ",
  "d'âge renseignés (exclusion des « Ne sait pas » et des âges manquants).\n\n",
  "**Méthode :** estimations pondérées par svyciprop (méthode logit, package survey). ",
  "La tranche d'âge minimale est déterminée dynamiquement par which.min() sur ",
  "les proportions estimées — pas par un groupe d'âge prédéfini.\n\n",
  "**Tranches définies :** 6–11 mois, 1–4 ans, 5–9 ans, 10–14 ans. ",
  "La classification suit le standard OMS pour les ECP campagnes RR.\n\n",
  "**Test de disparité :** aucun test formel de comparaison inter-tranches n'est ",
  "présenté ici (les IC 95 % permettent une évaluation visuelle du chevauchement). ",
  "Pour un test formel, utiliser svychisq sur le design de sondage complet."
), "info")
```

::: {.content-visible when-format="html"}
La tranche des **`r cv_age_min`** présente la couverture la plus basse (`r pct_fr(cv_age_min_pct)` %). Cette situation peut s'expliquer par plusieurs facteurs : craintes des effets secondaires chez les très jeunes enfants, difficultés de mobilisation des nourrissons aux sites fixes, ou encore incertitude des parents sur l'âge d'éligibilité.
:::

## Couverture par sexe

```{r tab-cv-sexe}
cv_sexe %>%
  mutate(`Couverture (%)` = paste0(pct_fr(`Couverture (%)`), " %")) %>%
  afficher_tableau(caption = "Couverture vaccinale pondérée par sexe")
```

```{r fig-cv-sexe, fig.cap="Couverture vaccinale par sexe avec intervalles de confiance à 95 %", fig.height=4.5}
show_fig("05_cv_par_sexe.png")
```

```{r note-pop-cv-sexe, results='asis'}
note_pop("Couverture par sexe")
```

::: {.content-visible when-format="html"}
La différence de couverture entre les garçons (`r pct_fr(cv_sexe$"Couverture (%)"[cv_sexe$Sexe == "Masculin"])` %) et les filles (`r pct_fr(cv_sexe$"Couverture (%)"[cv_sexe$Sexe == "Féminin"])` %) est de seulement **`r pct_fr(abs(diff(cv_sexe$"Couverture (%)")))` point**. Cette quasi-absence de disparité selon le sexe est une **bonne nouvelle** : elle indique que la campagne a atteint les enfants des deux sexes de manière équitable.
:::

## Couverture par milieu de résidence

```{r tab-cv-milieu, eval=milieu_dispo}
cv_milieu %>%
  mutate(`Couverture (%)` = paste0(pct_fr(`Couverture (%)`), " %")) %>%
  afficher_tableau(caption = "Couverture vaccinale pondérée par milieu de résidence")
```

```{r fig-cv-milieu, eval=milieu_dispo, fig.cap="Couverture vaccinale par milieu de résidence (urbain/rural) avec intervalles de confiance à 95 %", fig.height=5}
show_fig("05b_cv_par_milieu.png")
```

```{r interp-milieu, eval=milieu_dispo, results='asis'}
if (milieu_dispo && !is.na(ecart_milieu)) {
  msg <- glue(
    "La couverture en milieu **{milieu_plus_haut}** ",
    "({pct_fr(if (milieu_plus_haut=='rural') cv_rural_n else cv_urbain_n)} %) ",
    "dépasse celle du milieu **{milieu_plus_bas}** ",
    "({pct_fr(if (milieu_plus_bas=='rural') cv_rural_n else cv_urbain_n)} %) ",
    "de **{pct_fr(ecart_milieu)} points**. "
  )
  if (milieu_plus_haut == "rural") {
    msg <- paste0(msg, "Ce résultat, où le rural fait mieux que l'urbain, ",
      "peut s'expliquer par une mobilisation communautaire plus dense en zone rurale ",
      "(crieurs, relais) et par les difficultés propres aux grands centres urbains ",
      "(mobilité des familles, habitat dispersé, méfiance). Les zones urbaines méritent ",
      "donc une attention renforcée lors des prochaines campagnes.")
  } else {
    msg <- paste0(msg, "Les zones rurales, souvent plus difficiles d'accès, ",
      "nécessitent un renforcement logistique (stratégie avancée, équipes mobiles).")
  }
  callout(msg, if (ecart_milieu >= 5) "warning" else "info")
  callout(glue(
    "**Note technique — pour les spécialistes.**\n\n",
    "**Dérivation du milieu :** la variable « milieu » (urbain/rural) n'est pas collectée ",
    "directement dans le questionnaire. Elle est dérivée de la base de sondage par jointure ",
    "numérique : les 5 premiers chiffres du zd_id de la base correspondent au zone_sante_id ",
    "de l'enquête (jointure vérifiée : 42/42 zones appariées, 0 valeur manquante). ",
    "Le milieu retenu est le milieu **prédominant** de la zone de santé (pondéré par la ",
    "population recensée preEA_Popn). Cette approximation est robuste au niveau de la zone ",
    "mais ne reflète pas les variations intra-zone.\n\n",
    "**Dénominateur :** {n_fr(sum(cv_milieu$n, na.rm=TRUE))} enfants au statut vaccinal et au milieu dérivé connus.\n\n",
    "**Méthode :** estimations pondérées (svyciprop, méthode logit). ",
    "L'écart de {pct_fr(ecart_milieu)} points entre milieux doit être interprété avec prudence : ",
    "milieu et province sont corrélés (Lualaba et Haut Katanga sont les plus urbanisées). ",
    "L'analyse des déterminants multivariés (section suivante) isole l'effet propre du milieu ",
    "en contrôlant la province."
  ), "info")
}
```

`r if (milieu_dispo) "Le milieu de résidence n'est pas collecté directement dans le questionnaire : il a été dérivé de la base de sondage par jointure sur l'identifiant numérique de zone de santé (les cinq premiers chiffres du zd_id de la base correspondent au zone_sante_id de l'enquête). Cette jointure par identifiant, insensible aux écarts d'orthographe des noms de zones, attribue à chaque enfant le milieu prédominant de sa zone de santé (pondéré par la population recensée)." else "Le milieu de résidence n'a pas pu être dérivé pour cette enquête (base de sondage indisponible)."`

```{r note-pop-cv-milieu, results='asis'}
note_pop("Couverture par milieu")
```

## Couverture par sous-groupes socio-démographiques

```{r fig-sousgroupes, fig.cap="Couverture vaccinale par sous-groupes — chaque point représente une modalité, les barres horizontales sont les intervalles de confiance à 95 %", fig.height=7}
show_fig("12_couverture_sousgroupes.png")
```

```{r interp-sousgroupes, results='asis'}
sg_info_oui <- cv_sousgroupes %>%
  filter(str_detect(groupe, "nform"), str_detect(label, "Oui"))
sg_info_non <- cv_sousgroupes %>%
  filter(str_detect(groupe, "nform"), str_detect(label, "Non"))

if (nrow(sg_info_oui) > 0 && nrow(sg_info_non) > 0) {
  ecart_info <- round(100 * (sg_info_oui$pct[1] - sg_info_non$pct[1]), 1)
  callout(glue(
    "L'écart de couverture entre les ménages **informés avant la campagne** ",
    "({pct_fr(100 * sg_info_oui$pct[1])} %) et ceux **non informés** ",
    "({pct_fr(100 * sg_info_non$pct[1])} %) est de **{pct_fr(ecart_info)} points**. ",
    "Informer les familles à l'avance est le levier le plus puissant."
  ), "info")
}
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Variables analysées :** instruction du tuteur, information pré-campagne, ",
  "importance perçue des vaccins, statut du tuteur, taille du ménage, sexe de l'enfant. ",
  "Ces sous-groupes sont des analyses univariées — chaque variable est analysée ",
  "indépendamment, sans contrôle des cofacteurs.\n\n",
  "**Méthode :** estimations pondérées par svyciprop (méthode logit) pour chaque ",
  "modalité. Les intervalles de confiance à 95 % permettent d'évaluer visuellement ",
  "la significativité des écarts. Pour des tests formels ajustés, se référer à ",
  "l'analyse multivariée (section Déterminants).\n\n",
  "**Interprétation :** les écarts observés ici peuvent refléter des effets de confusion ",
  "(par exemple, les non-informés sont aussi plus ruraux ou plus éloignés). ",
  "La régression logistique (section suivante) isole l'effet propre de chaque variable."
), "info")
```

::: {.content-visible when-format="html"}
L'analyse par sous-groupe confirme le rôle déterminant de **l'accès à l'information** : les enfants dont les parents ont été informés avant le lancement de la campagne ont une couverture bien supérieure à ceux dont les parents n'avaient pas reçu cette information. Elle confirme également qu'une perception positive de la vaccination est fortement associée à une meilleure couverture.
:::

`r newpage()`

# Comparaison avec les couvertures administratives et le monitorage End-Process OMS

::: {.content-visible when-format="html"}
Cette section confronte les estimations de couverture vaccinale issues de la présente enquête (ECP) à deux sources de référence pour le bloc 1 : la **couverture administrative** consolidée par le SNIS (rapport des prestataires sur la base de la cible attendue) et le **monitorage End-Process OMS** mené en décembre 2025 (rapport indépendant, n = 124 729 enfants enquêtés au sein des ménages).

Cette triangulation permet d'apprécier la concordance entre les trois sources, d'identifier les écarts éventuels et d'éclairer leur interprétation. La CV administrative peut dépasser 100 % en cas de surestimation des vaccinés ou de sous-estimation de la cible (dénominateur), tandis que la CV End-Process et la CV ECP sont fondées sur des enquêtes ménages indépendantes mais diffèrent par la méthode d'échantillonnage (exhaustivité opérationnelle vs sondage en grappes pondéré).
:::

## Comparaison par province

```{r tab-comp-cv-province}
if (nrow(comparaison_cv_prov_t) > 0) {
  comparaison_cv_prov_t %>%
    transmute(Province,
              `n (ECP)` = fmt_n(n_ecp),
              `CV ECP (%)` = paste0(fmt_pct1(cv_ecp_pct), " %"),
              `IC 95% ECP` = fmt_ic(ic_bas, ic_haut),
              `CV admin. (%)` = paste0(fmt_pct1(cv_admin_pct), " %"),
              `CV End-Process OMS (%)` = paste0(fmt_pct1(cv_endprocess_pct), " %"),
              `Écart admin. − ECP (pp)` = fmt_pct1(ecart_admin_ecp),
              `Écart End-Process − ECP (pp)` = fmt_pct1(ecart_ep_ecp)) %>%
    afficher_tableau(caption = "Comparaison des couvertures vaccinales RR par province (ECP, administrative, End-Process OMS)")
}
```

```{r fig-comp-cv-province, fig.cap="Comparaison des couvertures RR par province selon les trois sources (ECP, administrative, End-Process OMS)", fig.height=6}
show_fig("23_comparaison_cv_province.png")
```

```{r interp-comp-prov, results='asis'}
if (nrow(comparaison_cv_prov_t) > 0) {
  # Identifier les provinces avec le plus grand écart admin vs ECP
  ecarts <- comparaison_cv_prov_t %>%
    mutate(abs_ecart_admin = abs(ecart_admin_ecp),
           abs_ecart_ep = abs(ecart_ep_ecp))
  prov_max_admin <- ecarts %>% slice_max(abs_ecart_admin, n = 1)
  prov_max_ep <- ecarts %>% slice_max(abs_ecart_ep, n = 1)
  narratif(glue(
    "**Lecture :** au niveau provincial, les trois sources convergent largement, avec ",
    "toutefois quelques écarts notables. La plus grande divergence entre la CV administrative ",
    "et la CV ECP concerne **{prov_max_admin$Province}** ",
    "({fmt_pct1(prov_max_admin$ecart_admin_ecp)} points). ",
    "Entre l'End-Process OMS et l'ECP, l'écart maximal est observé pour ",
    "**{prov_max_ep$Province}** ({fmt_pct1(prov_max_ep$ecart_ep_ecp)} points). ",
    "Ces écarts peuvent refléter : (i) des incertitudes sur le dénominateur ",
    "administratif (cible estimée), (ii) des différences de méthode d'échantillonnage ",
    "(exhaustivité opérationnelle de l'End-Process vs sondage en grappes pondéré de l'ECP), ",
    "ou (iii) des fluctuations attendues liées à la précision statistique de chaque enquête."
  ))
  # Provinces dont la CV administrative dépasse 100% (anomalie de dénominateur)
  prov_admin_sup100 <- comparaison_cv_prov_t %>%
    filter(!is.na(cv_admin_pct), cv_admin_pct > 100) %>%
    arrange(desc(cv_admin_pct))
  if (nrow(prov_admin_sup100) > 0) {
    liste_prov <- if (nrow(prov_admin_sup100) == 1) {
      prov_admin_sup100$Province[1]
    } else {
      paste(paste(prov_admin_sup100$Province[-nrow(prov_admin_sup100)], collapse = ", "),
            "et", prov_admin_sup100$Province[nrow(prov_admin_sup100)])
    }
    callout(paste0(
      "Les **CV administratives** dépassent parfois 100 %, ce qui signale habituellement ",
      "une **sous-estimation du dénominateur** (cible administrative inférieure à la ",
      "population réelle) ou un cumul d'unités vaccinées dépassant la cible enregistrée. ",
      "Cette anomalie est particulièrement marquée pour **", liste_prov, "**."),
      "warning")
  }
  # Calcul n_cv local depuis cv_province (n_cv global non disponible dans ce chunk)
  n_cv_local <- sum(cv_province[["n (enfants)"]], na.rm = TRUE)
  callout(glue(
    "**Note technique — pour les spécialistes.**\n\n",
    "**Sources comparées :** (1) ECP : sondage en grappes pondéré, n = {n_fr(n_cv_local)} enfants, ",
    "estimations svyciprop (méthode logit). (2) CV administrative SNIS : ratio ",
    "vaccinés déclarés / cible administrative, non pondéré, source prestataires. ",
    "(3) End-Process OMS : enquête ménages exhaustive, décembre 2025, n = 124 729 enfants.\n\n",
    "**Biais connus :** la CV administrative est sujette à un biais de dénominateur ",
    "(sous-estimation de la population cible → surestimation de la couverture, ",
    "pouvant dépasser 100 %). L'End-Process OMS utilise une approche d'exhaustivité ",
    "opérationnelle (toutes les ZD) vs sondage probabiliste pour l'ECP — les deux ",
    "méthodes peuvent diverger en cas d'hétérogénéité inter-ZD.\n\n",
    "**Interprétation des écarts :** un écart ECP vs End-Process > IC 95 % combinés ",
    "suggère une différence réelle de performance plutôt qu'une variabilité d'échantillonnage. ",
    "Un écart ECP vs Admin > 20 points signale généralement un problème de dénominateur."
  ), "info")
}
```

```{r fig-comp-cv-zs, fig.cap="Concordance par zone de santé : ECP vs CV administrative (gauche) et ECP vs End-Process OMS (droite). Ligne pointillée : accord parfait (y = x)", fig.height=6}
show_fig("24_comparaison_cv_zone_sante.png")
```

::: {.content-visible when-format="html"}
Le tableau ci-dessous présente la comparaison détaillée pour chaque zone de santé enquêtée par l'ECP, avec les couvertures correspondantes dans le SNIS et dans l'End-Process OMS lorsqu'elles sont disponibles. Les zones sont triées par province puis par nom.
:::

```{r tab-comp-cv-zs}
if (nrow(comparaison_cv_zs_t) > 0) {
  comparaison_cv_zs_t %>%
    transmute(Province, `Zone de santé` = ZS,
              `n (ECP)` = fmt_n(n_ecp),
              `CV ECP (%)` = paste0(fmt_pct1(cv_ecp_pct), " %"),
              `CV admin. (%)` = paste0(fmt_pct1(cv_admin_pct), " %"),
              `CV End-Process (%)` = paste0(fmt_pct1(cv_endprocess_pct), " %"),
              `Écart admin. − ECP (pp)` = fmt_pct1(ecart_admin),
              `Écart End-Process − ECP (pp)` = fmt_pct1(ecart_ep)) %>%
    afficher_tableau(caption = "Comparaison des couvertures RR par zone de santé (ECP, administrative, End-Process OMS)")
}
```

```{r interp-comp-zs, results='asis'}
if (nrow(comparaison_cv_zs_t) > 0) {
  n_total <- nrow(comparaison_cv_zs_t)
  n_concord_ep <- comparaison_cv_zs_t %>%
    filter(!is.na(ecart_ep), abs(ecart_ep) <= 5) %>% nrow()
  n_concord_admin <- comparaison_cv_zs_t %>%
    filter(!is.na(ecart_admin), abs(ecart_admin) <= 5) %>% nrow()
  narratif(glue(
    "Sur **{n_total} zones de santé** enquêtées dans l'ECP et appariées aux sources de référence, ",
    "**{n_concord_ep}** présentent un écart de moins de 5 points entre l'ECP et l'End-Process OMS, ",
    "tandis que **{n_concord_admin}** affichent un écart inférieur à 5 points avec la CV administrative. ",
    "Les divergences plus importantes méritent une attention particulière, ",
    "notamment dans les zones où les effectifs ECP sont faibles (incertitude statistique accrue) ",
    "ou dans les zones où la CV administrative dépasse nettement 100 %."
  ))
}
```

`r newpage()`

# Statut vaccinal antérieur

::: {.content-visible when-format="html"}
Cette section porte uniquement sur les **enfants de 6 à 59 mois**, pour lesquels la question de l'historique vaccinal est la plus pertinente d'un point de vue épidémiologique.
:::

```{r tab-statut-ant}
statut_ant %>%
  rename(
    `Statut vaccinal antérieur` = statut_vaccinal_ant,
    `Effectif` = n,
    `Pourcentage (%)` = pct
  ) %>%
  mutate(`Pourcentage (%)` = paste0(pct_fr(`Pourcentage (%)`), " %")) %>%
  afficher_tableau(
    caption = "Statut vaccinal antérieur des enfants de 6–59 mois (avant la campagne)"
  ) %>%
  row_spec(which(statut_ant$statut_vaccinal_ant == "Zéro dose"),
           background = "#FFEBEE", bold = TRUE)
```

```{r fig-statut-ant, fig.cap="Répartition du statut vaccinal antérieur — enfants 6–59 mois", fig.height=4.5}
show_fig("06_statut_vaccinal_anterieur.png")
```

```{r note-pop-statut, results='asis'}
note_pop("Statut vaccinal antérieur")
```

```{r interp-statut-ant, results='asis'}
callout(glue(
  "**{pct_fr(sat_zero_pct)} % des enfants de moins de 5 ans** n'avaient reçu ",
  "**aucune dose de vaccin contre la rougeole** avant cette campagne. ",
  "C'est environ 1 enfant sur {round(100/sat_zero_pct)} dans cette tranche d'âge. ",
  "Ces enfants à « zéro dose » sont les plus vulnérables aux épidémies ",
  "et doivent être une priorité absolue pour la vaccination de routine."
), "danger")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Population :** enfants de 6 à 59 mois dont le statut vaccinal antérieur est connu. ",
  "Note : {pct_fr(statut_ant$pct[statut_ant$statut_vaccinal_ant == 'Ne sait pas'])} % ",
  "des répondants n'ont pas pu indiquer ce statut (« Ne sait pas »), ce qui réduit la ",
  "valeur de cet indicateur et peut introduire un biais si les non-répondants ont un ",
  "profil vaccinal différent.\n\n",
  "**Variable :** statut_vaccinal_ant_fus — fusion des modalités « une dose » et ",
  "« une dose ou plus (nombre non précisé) » en une seule catégorie. Les modalités ",
  "détaillées sont disponibles en Annexe I.\n\n",
  "**Méthode :** proportions non pondérées (les poids de sondage s'appliquent à ",
  "la couverture vaccinale de la campagne, pas au statut antérieur qui est une ",
  "caractéristique déclarative de l'enfant).\n\n",
  "**Référence OMS :** la proportion d'enfants zéro dose dans les 6–59 mois est ",
  "l'un des indicateurs de routine du PEV. Une valeur > 10 % signale une faiblesse ",
  "structurelle du programme de vaccination de routine nécessitant une investigation."
), "info")
```

::: {.content-visible when-format="html"}
La proportion de **`r pct_fr(sat_zero_pct)` % d'enfants à zéro dose** parmi les moins de 5 ans est préoccupante : elle révèle une **faiblesse de la vaccination de routine** qui ne peut être comblée que par les campagnes supplémentaires. Par ailleurs, **`r pct_fr(statut_ant$pct[statut_ant$statut_vaccinal_ant == "Ne sait pas"])` % des répondants** n'ont pas pu indiquer le statut vaccinal antérieur de leur enfant — une limite importante qui réduit la valeur de cet indicateur.
:::

`r newpage()`

# Couverture de rattrapage des enfants zéro dose (SIA-COVG-03)

::: {.content-visible when-format="html"}
Cet indicateur VCQI (SIA-COVG-03) répond à une question essentielle : **parmi les enfants qui n'avaient reçu aucun vaccin contre la rougeole avant la campagne, combien ont été atteints par la campagne ?** C'est l'indicateur le plus direct de l'efficacité de rattrapage de la campagne pour les enfants les plus vulnérables.
:::

```{r tab-zero-dose}
zero_dose_t <- read_t("zero_dose_couverture_campagne")
if (nrow(zero_dose_t) > 0) {
  zero_dose_t %>%
    transmute(Province,
              `N (zéro dose)`         = fmt_n(n_zero_dose),
              `Vaccinés campagne`      = fmt_n(n_vaccines),
              `Taux de rattrapage (%)` = paste0(fmt_pct1(cv_pct), " %")) %>%
    afficher_tableau(caption = "Couverture de la campagne parmi les enfants zéro dose (6-59 mois) — VCQI SIA-COVG-03")
}
```

```{r fig-zero-dose, fig.cap="Taux de rattrapage des enfants zéro dose par province (VCQI SIA-COVG-03)", fig.height=5}
show_fig("10b_zero_dose_rattrapage.png")
```

```{r tab-zero-dose-croise}
zero_croise_t <- read_t("zero_dose_croise_campagne")
if (nrow(zero_croise_t) > 0) {
  zero_croise_t %>%
    transmute(`Statut vaccinal antérieur` = statut_ant_grp,
              `Statut campagne`            = vax_camp,
              Effectif                     = fmt_n(n),
              `Pourcentage (%)`            = paste0(fmt_pct1(pct), " %")) %>%
    afficher_tableau(caption = "Croisement statut antérieur × participation à la campagne (6-59 mois)")
}
```

```{r interp-zero-dose, results='asis'}
zero_dose_t <- read_t("zero_dose_couverture_campagne")
if (nrow(zero_dose_t) > 0) {
  n_zd      <- if (!is.na(n_zero)) n_fr(n_zero) else n_fr(sum(zero_dose_t$n_zero_dose))
  n_zd_num  <- if (!is.na(n_zero)) n_zero else sum(zero_dose_t$n_zero_dose)
  pct_zd    <- if (!is.na(zero_dose_pct_campagne)) pct_fr(zero_dose_pct_campagne) else "N/D"
  pct_zd_num <- if (!is.na(zero_dose_pct_campagne)) zero_dose_pct_campagne else NA_real_

  prov_min_zd   <- zero_dose_t %>% slice_min(cv_pct, n = 1)
  prov_max_zd   <- zero_dose_t %>% slice_max(cv_pct, n = 1)
  prov_sous_80  <- zero_dose_t %>% filter(cv_pct < 80) %>% arrange(cv_pct)
  n_prov_sous_80 <- nrow(prov_sous_80)
  n_non_atteints <- if (!is.na(pct_zd_num)) round((1 - pct_zd_num/100) * n_zd_num) else NA_integer_

  qualif <- dplyr::case_when(
    is.na(pct_zd_num)  ~ "indéterminé",
    pct_zd_num >= 90   ~ "très satisfaisant",
    pct_zd_num >= 80   ~ "acceptable mais perfectible",
    pct_zd_num >= 60   ~ "insuffisant",
    TRUE               ~ "critique"
  )

  # ── Narratif accessible (tous lecteurs) ──────────────────────────────────
  narratif(glue(
    "**Qu'est-ce qu'un enfant « zéro dose » ?** C'est un enfant qui n'a reçu ",
    "aucun vaccin contre la rougeole avant la campagne — pas de vaccination de routine, ",
    "pas de campagne antérieure. Ces enfants sont entièrement susceptibles à la maladie ",
    "et constituent le principal réservoir de transmission en cas de flambée. ",
    "Leur rattrapage est l'objectif le plus critique d'une campagne supplémentaire.\n\n",
    "**Ce que les données montrent.** Sur les **{n_zd} enfants de 6 à 59 mois identifiés ",
    "comme zéro dose**, **{pct_zd} %** ont été vaccinés lors de la campagne — ",
    "un taux de rattrapage qualifié de **{qualif}**. ",
    if (!is.na(n_non_atteints))
      glue("Environ **{n_fr(n_non_atteints)} enfants zéro dose** n'ont pas été rejoints ",
           "et demeurent sans protection contre la rougeole.")
    else "", "\n\n",
    "**Les disparités entre provinces sont marquées.** ",
    "{prov_max_zd$Province[1]} réalise la meilleure performance avec un taux de rattrapage ",
    "de {fmt_pct1(prov_max_zd$cv_pct[1])} %, tandis que {prov_min_zd$Province[1]} affiche ",
    "le taux le plus bas ({fmt_pct1(prov_min_zd$cv_pct[1])} %). ",
    if (n_prov_sous_80 > 0)
      glue("**{n_prov_sous_80} province{if(n_prov_sous_80>1) 's' else ''} ",
           "({paste(prov_sous_80$Province, collapse=', ')}) ",
           "{if(n_prov_sous_80>1) 'n\\'atteignent pas' else 'n\\'atteint pas'} ",
           "le seuil indicatif de 80 % de rattrapage** et nécessitent une attention prioritaire.")
    else "Toutes les provinces atteignent ou dépassent le seuil indicatif de 80 %."
  ))

  # ── Note technique — spécialistes ────────────────────────────────────────
  callout(glue(
    "**Note technique (VCQI SIA-COVG-03) — pour les spécialistes.**\n\n",
    "**Indicateur :** VCQI SIA-COVG-03 — couverture de la campagne parmi les enfants ",
    "sans antécédent vaccinal contre la rougeole (zéro dose).\n\n",
    "**Population :** enfants de 6 à 59 mois dont le statut vaccinal antérieur est connu ",
    "et identifié comme « zéro dose » (n = {n_zd}).\n\n",
    "**Numérateur :** enfants zéro dose déclarés vaccinés lors de la campagne ",
    "(vaccine_bin == 1).\n\n",
    "**Dénominateur :** ensemble des enfants zéro dose au statut vaccinal de campagne connu ",
    "(exclusion des « Ne sait pas »).\n\n",
    "**Estimation :** proportion non pondérée (pondération non appliquée à ce sous-groupe ",
    "en raison des faibles effectifs par province). À interpréter avec prudence pour les ",
    "provinces avec n < 30 zéro dose.\n\n",
    "**Seuil de référence :** pas de seuil OMS officiel pour cet indicateur. Le seuil ",
    "indicatif de 80 % est issu de la pratique ; il est plus exigeant que la couverture ",
    "globale car ce sous-groupe est structurellement plus difficile à atteindre.\n\n",
    "**Interprétation croisée :** combiner avec le statut vaccinal antérieur (section 3.3) ",
    "et les raisons de non-vaccination (section suivante) pour identifier si les zéro dose ",
    "non atteints appartiennent à un profil géographique ou socio-démographique particulier."
  ), "info")

  # ── Callout décisionnel ───────────────────────────────────────────────────
  if (!is.na(pct_zd_num)) {
    if (pct_zd_num < 80) {
      callout(glue(
        "**Alerte : taux de rattrapage des zéro dose insuffisant ({pct_fr(pct_zd_num)} %).** ",
        if (!is.na(n_non_atteints))
          glue("Environ {n_fr(n_non_atteints)} enfants zéro dose demeurent sans protection. ")
        else "",
        "Ces enfants ne fréquentent pas encore les écoles et ne se rendent pas ",
        "spontanément dans les formations sanitaires — ils ne seront atteints que par ",
        "des stratégies actives de porte-à-porte et d'équipes mobiles. ",
        "**Action requise :** planifier des séances de rattrapage ciblées, avant la ",
        "prochaine campagne ou au plus tard pendant celle-ci."
      ), "danger")
    } else {
      callout(glue(
        "**Taux de rattrapage satisfaisant ({pct_fr(pct_zd_num)} %).** ",
        "La campagne a bien réussi à rejoindre la majorité des enfants sans antécédent ",
        "vaccinal. Maintenir la vigilance pour les provinces les moins performantes et ",
        "pour les nouvelles cohortes d'enfants zéro dose générées entre les campagnes."
      ), "success")
    }
  }
} else {
  narratif("Données zéro dose insuffisantes pour calculer cet indicateur (variable statut antérieur non disponible ou effectifs trop faibles).")
}
```

`r newpage()`

# Raisons de non-vaccination

## Vue d'ensemble

```{r alerte-non-vax, results='asis'}
# Nombre total de non-vaccinés (374) vs ayant déclaré une raison (313)
  total_txt <- if (!is.na(n_non_vax_total)) {
    glue("{n_fr(n_non_vax_total)} enfants non vaccinés documentés, ",
         "dont {n_fr(n_non_vax)} ont déclaré une raison ",
         "({n_fr(n_sans_raison)} sans raison renseignée)")
  } else {
    glue("{n_fr(n_non_vax)} enfants non vaccinés ayant déclaré une raison")
  }
callout(glue(
  "**{total_txt}.** ",
  "Deux raisons représentent à elles seules **{pct_top2_cumul} %** des cas documentés : ",
  "**{tolower(raison_top1$raison_pas_lbl[1])}** ({pct_fr(raison_top1$pct[1])} %) et ",
  "**{tolower(raison_top2$raison_pas_lbl[1])}** ({pct_fr(raison_top2$pct[1])} %). ",
  "Ces deux causes constituent les leviers d'action prioritaires pour les prochaines campagnes."
), "warning")
```

::: {.content-visible when-format="html"}
Sur les `r if (!is.na(n_non_vax_total)) n_fr(n_non_vax_total) else n_fr(n_non_vax)` enfants non vaccinés documentés dans l'enquête, **`r n_fr(n_non_vax)`** (`r if (!is.na(n_non_vax_total)) pct_fr(100*n_non_vax/n_non_vax_total) else "100"` %) ont déclaré une raison de non-vaccination — les `r if (!is.na(n_sans_raison)) n_fr(n_sans_raison) else "autres"` restants n'en ont pas indiqué. Les analyses ci-dessous portent sur ces **`r n_fr(n_non_vax)` enfants**. Le diagramme de Pareto illustre l'effet de concentration des raisons : en agissant sur les deux premières causes, on pourrait théoriquement réduire les cas de non-vaccination de moitié.
:::

```{r fig-pareto, fig.cap="Diagramme de Pareto des raisons de non-vaccination — les barres bleues représentent les raisons cumulant 80 % des cas", fig.height=9}
show_fig("13_pareto_raisons.png")
```

```{r tab-raisons-top}
raisons_det %>%
  head(10) %>%
  mutate(
    pct = paste0(pct_fr(pct), " %"),
    n = n_fr(n)
  ) %>%
  rename(Raison = raison_pas_lbl, Effectif = n, `% non-vaccinés` = pct) %>%
  afficher_tableau(
    caption = glue("Top 10 des raisons de non-vaccination (n = {n_fr(n_non_vax)} enfants ayant déclaré une raison)")
  ) %>%
  row_spec(1:2, bold = TRUE, background = "#FFF8E1")
```

```{r note-pop-raisons, results='asis'}
note_pop("Raisons de non-vaccination")
```

## Analyse par groupe thématique (taxonomie OMS)

::: {.content-visible when-format="html"}
L'OMS classe les raisons de non-vaccination en groupes thématiques pour orienter les réponses programmatiques. Cette classification révèle que la **majorité des cas de non-vaccination dans cette enquête auraient pu être évités** avec une meilleure planification opérationnelle et une mobilisation sociale renforcée.
:::

```{r fig-groupes-province, fig.cap="Groupes de raisons de non-vaccination par province (taxonomie OMS) — proportions relatives", fig.height=6}
show_fig("14_groupes_raisons_province.png")
```

```{r tab-groupes-raisons}
rg %>%
  mutate(n = n_fr(n), pct = paste0(pct_fr(pct), " %")) %>%
  rename(`Groupe thématique (OMS)` = raison_groupe,
         Effectif = n, `Proportion (%)` = pct) %>%
  afficher_tableau(
    caption = "Répartition des non-vaccinés par groupe thématique OMS"
  ) %>%
  row_spec(1:2, bold = TRUE, background = "#FFF8E1")
```

```{r interp-groupes, results='asis'}
callout(glue(
  "Les **{gr_top1}** ({pct_fr(gr_top1_pct)} %) et le **{gr_top2}** ({pct_fr(gr_top2_pct)} %) ",
  "représentent ensemble **{pct_fr(gr_top1_pct + gr_top2_pct)} %** des non-vaccinés. ",
  "Ces deux groupes relèvent de causes sur lesquelles les équipes de terrain ont ",
  "une prise directe : mieux planifier les horaires et itinéraires d'équipe, ",
  "et intensifier la communication avant la campagne."
), "info")
```

```{r interp-groupes-detail, results='asis'}
# Description dynamique des 2 groupes dominants — sans fraction fixe
desc_groupe <- function(groupe, pct) {
  base <- glue("**{groupe}** ({pct_fr(pct)} %)")
  conseil <- dplyr::case_when(
    grepl("pratique|accès|Accès", groupe) ~
      " — les familles n'ont pas pu être atteintes au bon moment ou au bon endroit. Ce résultat pointe vers des horaires de vaccination inadaptés, des zones difficiles d'accès ou une mobilité des ménages non anticipée.",
    grepl("information|Information", groupe) ~
      " — les familles non informées sont mécaniquement moins susceptibles d'amener leur enfant se faire vacciner. Renforcer la mobilisation sociale préalable est la réponse directe.",
    grepl("refus|hésitation|Refus|Hésitation", groupe) ~
      " — une fraction des familles a délibérément renoncé à la vaccination. Des messages ciblés sur la sécurité du vaccin et l'engagement des leaders communautaires sont nécessaires.",
    grepl("logistique|offre|Offre", groupe) ~
      " — la disponibilité du vaccin ou du vaccinateur a été mise en cause. Une meilleure planification logistique et une supervision renforcée peuvent y remédier.",
    TRUE ~ " — ce groupe appelle une investigation qualitative complémentaire pour en identifier les leviers d'action spécifiques."
  )
  paste0(base, conseil)
}
narratif(paste0(
  desc_groupe(gr_top1, gr_top1_pct), "\n\n",
  desc_groupe(gr_top2, gr_top2_pct)
))
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Dénominateur :** {n_fr(n_non_vax)} enfants ayant déclaré au moins une raison ",
  "de non-vaccination (sur {n_fr(n_non_vax_total)} non-vaccinés au total — ",
  "{n_fr(n_sans_raison)} sans raison déclarée exclus).\n\n",
  "**Taxonomie utilisée :** classification OMS adaptée au contexte RDC — ",
  "5 groupes : Contraintes pratiques, Manque d'information, Refus/Hésitation, ",
  "Raisons logistiques, Autres. Les raisons individuelles sont assignées à chaque ",
  "groupe selon la codification du plan d'analyse.\n\n",
  "**Diagramme de Pareto :** les pourcentages sont calculés sur les {n_fr(n_non_vax)} ",
  "enfants avec raison connue (pas sur les {n_fr(n_non_vax_total)} non-vaccinés totaux). ",
  "Les deux premiers motifs représentent {pct_fr(pct_top2_cumul)} % des cas documentés.\n\n",
  "**Limites :** un enfant peut cumuler plusieurs raisons (questionnaire multi-réponses). ",
  "Les données qualitatives (groupes de discussion) complètent utilement cette analyse ",
  "en révélant les mécanismes sous-jacents non capturés par les codes quantitatifs."
), "info")
```

## Réponse opérationnelle ciblée par province

::: {.content-visible when-format="html"}
La taxonomie OMS ne prend tout son sens que si elle oriente une **réponse différenciée selon la province**. La cause dominante de non-vaccination n'est pas la même partout : une province dont le problème principal est le manque d'information appelle une réponse de communication, tandis qu'une province confrontée à des ruptures d'approvisionnement appelle une réponse logistique. Le tableau ci-dessous identifie, pour chaque province (classée par couverture croissante), le groupe de raisons dominant et la réponse de santé publique recommandée.
:::

```{r tab-reponse-province}
if (nrow(reponse_province) > 0) {
  reponse_province %>%
    mutate(
      `Couverture (%)` = paste0(pct_fr(`Couverture (%)`), " %"),
      `Part chez non-vaccinés (%)` = paste0(pct_fr(`Part chez non-vaccinés (%)`), " %")
    ) %>%
    afficher_tableau(
      caption = "Réponse opérationnelle recommandée selon la raison dominante, par province"
    )
}
```

```{r fig-matrice-province-raison, fig.cap="Matrice province × groupe de raisons — identifie la cause dominante propre à chaque province pour cibler la réponse", fig.height=6}
show_fig("14b_matrice_province_raison.png")
```

```{r tab-matrice-complete}
if (exists("matrice_complete_t") && nrow(matrice_complete_t) > 0) {
  matrice_complete_t %>%
    afficher_tableau(
      caption = "Matrice complète province × groupe de raisons (% des non-vaccinés, toutes valeurs)"
    )
}
```

```{r interp-reponse-province, results='asis'}
if (nrow(reponse_province) > 0) {
  prov_faible <- reponse_province %>% slice(1)  # déjà triée par couverture croissante
  callout(glue(
    "**Lecture opérationnelle :** à **{prov_faible$Province}** ",
    "(couverture la plus basse, {pct_fr(prov_faible$`Couverture (%)`)} %), ",
    "la cause dominante de non-vaccination relève du groupe ",
    "« {prov_faible$`Raison dominante (groupe OMS)`} ». ",
    "La réponse prioritaire recommandée est donc : ",
    "**{tolower(prov_faible$`Réponse de santé publique recommandée`)}**. ",
    "Chaque province doit ainsi recevoir une réponse adaptée à sa cause dominante, ",
    "plutôt qu'une stratégie uniforme."
  ), "info")
}
```

::: {.content-visible when-format="html"}
Cette approche évite l'écueil d'une stratégie nationale uniforme : elle alloue les ressources là où elles auront le plus d'impact, en fonction du **profil de barrières propre à chaque province**.
:::

`r newpage()`

# Confirmation de la vaccination par la carte

::: {.content-visible when-format="html"}
La vérification documentaire est essentielle pour valider la déclaration de vaccination. L'enquêteur a demandé à voir la carte de vaccination de l'enfant.
:::

```{r tab-confirmation}
confirmation %>%
  mutate(
    n = n_fr(n),
    pct = paste0(pct_fr(pct), " %")
  ) %>%
  rename(`Mode de confirmation` = confirmation_carte,
         Effectif = n, `Proportion (%)` = pct) %>%
  afficher_tableau(
    caption = "Mode de confirmation de la vaccination par la carte"
  ) %>%
  row_spec(which(confirmation$confirmation_carte == "Carte vue"),
           background = "#E8F5E9", bold = TRUE) %>%
  row_spec(which(confirmation$confirmation_carte == "Pas de carte reçue"),
           background = "#FFEBEE")
```

```{r fig-confirmation, fig.cap="Répartition des modes de confirmation de la vaccination", fig.height=4.5}
show_fig("08_confirmation_carte.png")
```

```{r note-pop-confirmation, results='asis'}
note_pop("Confirmation par carte")
```

```{r interp-confirmation, results='asis'}
callout(glue(
  "Seulement **{pct_fr(conf_vue_pct)} % des vaccinés** ont pu présenter ",
  "une carte que l'enquêteur a vue et vérifiée. Les **{pct_fr(100 - conf_vue_pct - conf_pas_pct)} %** ",
  "restants déclarent avoir une carte mais ne l'ont pas présentée lors de l'enquête. ",
  "Cela limite la capacité à valider objectivement le taux de couverture : ",
  "une partie de la couverture repose sur des déclarations non vérifiées."
), "warning")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Deux mesures distinctes :** (1) « Carte vue » = carte/jeton présenté et vérifié ",
  "par l'enquêteur au moment de l'entretien — mesure objective mais sous-estimée ",
  "(les familles ne retrouvent pas toujours la carte). (2) « Reçu, non présenté » = ",
  "déclaration que la carte a été reçue sans pouvoir la montrer.\n\n",
  "**Biais de désirabilité sociale :** l'écart entre couverture déclarative (~95 %) ",
  "et couverture par preuve (~15 %) ne signifie pas que 80 % des enfants ne sont pas ",
  "vaccinés. Il signifie que la grande majorité des déclarations n'est pas vérifiable ",
  "documentairement. Le biais de désirabilité (sur-déclaration) est probable mais non ",
  "quantifiable sans étude séparée.\n\n",
  "**Indicateur VCQI correspondant :** RI-QUAL-03 (taux de cartes présentées). ",
  "La cible OMS de 85 % de cartes remises est atteinte ({pct_fr(pct_cartes)} %), ",
  "mais il s'agit de cartes remises — pas nécessairement conservées ni présentées."
), "info")
```

::: {.content-visible when-format="html"}
La remise systématique des cartes de vaccination et la sensibilisation des familles à leur conservation sont des **enjeux clés** pour améliorer la vérification documentaire. L'indicateur de cartes reçues (`r pct_fr(pct_cartes)` %, cible 85 % : **atteinte**) montre que la distribution de cartes s'est bien déroulée, mais leur présentation lors de l'enquête reste limitée.
:::

## Couverture déclarative vs couverture par preuve physique

::: {.content-visible when-format="html"}
Pour mesurer l'ampleur du **biais de désirabilité sociale** (tendance des répondants à déclarer un enfant vacciné pour satisfaire l'enquêteur), nous confrontons deux estimateurs de couverture calculés sur la même base d'enfants :
:::

- la **couverture déclarative** (carte + histoire orale) — l'estimation usuelle ;
- la **couverture par preuve physique** (carte effectivement vue par l'enquêteur) — rigoureuse mais nécessairement plus basse.

```{r tab-carte-vs-decl}
if (nrow(carte_vs_decl) > 0) {
  carte_vs_decl %>%
    mutate(`Estimation (%)` = paste0(pct_fr(`Estimation (%)`), " %"),
           Enfants = n_fr(Enfants)) %>%
    select(Indicateur, `Estimation (%)`, `IC 95% (%)`, Enfants) %>%
    afficher_tableau(
      caption = "Couverture nationale : déclaratif vs preuve physique (carte vue)"
    )
}
```

```{r fig-carte-vs-decl, fig.cap="Couverture déclarative vs preuve physique par province — l'écart entre les deux points mesure le biais potentiel", fig.height=6}
show_fig("08b_carte_vs_declaratif.png")
```

```{r interp-carte-decl, results='asis'}
if (nrow(carte_vs_decl) >= 2) {
  cv_decl_v  <- carte_vs_decl$`Estimation (%)`[1]
  cv_carte_v <- carte_vs_decl$`Estimation (%)`[2]
  biais_v    <- carte_vs_decl$biais_pp[1]
  callout(glue(
    "**Écart de {pct_fr(biais_v)} points** entre la couverture déclarative ",
    "({pct_fr(cv_decl_v)} %) et la couverture vérifiée par carte ({pct_fr(cv_carte_v)} %). ",
    "Un écart de cette ampleur indique que la couverture officielle repose très ",
    "majoritairement sur du déclaratif non vérifié. Il ne signifie pas que la couverture ",
    "réelle est de {pct_fr(cv_carte_v)} % — beaucoup de cartes existent sans être présentées — ",
    "mais il appelle à la **prudence** dans l'interprétation et à un renforcement ",
    "de la vérification documentaire lors des prochaines enquêtes."
  ), "danger")
}
```

`r newpage()`

# Déterminants de la vaccination

## Ce que révèle l'analyse statistique

::: {.content-visible when-format="html"}
Pour identifier les facteurs qui **favorisent ou freinent** la vaccination, une analyse de régression logistique a été conduite sur **`r n_fr(modele_adj$nobs[1])` enfants** pour lesquels toutes les variables nécessaires étaient disponibles. Cette technique statistique permet de mesurer l'effet de chaque facteur **en isolant l'influence des autres** : on parle d'Odds Ratio ajusté (ORa). Un ORa supérieur à 1 signifie que le facteur augmente les chances d'être vacciné ; un ORa inférieur à 1 signifie qu'il les diminue.
:::

::: {.content-visible when-format="html"}
Certaines provinces ayant une couverture quasi totale comptent très peu d'enfants non vaccinés, ce qui peut rendre les estimations instables (phénomène de « quasi-séparation »). Pour y remédier, le modèle applique une **régression pénalisée de Firth** (Firth, 1993), qui corrige ce biais et resserre les intervalles de confiance, garantissant des résultats plus fiables même pour les provinces à très haute couverture.
:::

```{r fig-forest, fig.cap="Forest plot des Odds Ratios ajustés — les points verts (ORa > 1) sont des facteurs protecteurs, les points rouges (ORa < 1) des facteurs de risque. La barre verticale en pointillés représente l'absence d'effet.", fig.height=8}
show_fig("16_forest_plot_determinants.png")
```

## Résultats complets du modèle de régression

```{r tab-determinants-complet}
determinants %>%
  mutate(`OR ajusté` = as.character(`OR ajusté`),
         `p-value` = as.character(`p-value`)) %>%
  rename(`Facteur` = Variable, `IC 95 %` = `IC 95%`, `p-valeur` = `p-value`) %>%
  afficher_tableau(
    caption = "Modèle de régression logistique — tous les Odds Ratios ajustés (les modalités de référence ne sont pas affichées : elles servent de comparaison)"
  )
```

```{r interp-determinants-modele, results='asis'}
# Lecture (silencieuse) de la méthode d'estimation et de la taille effective
.meth_path <- file.path("..", "tableaux", ".methode_modele.txt")
.meth <- if (file.exists(.meth_path)) readLines(.meth_path, warn = FALSE) else c("non spécifiée", "—")
methode_lbl <- .meth[1]
n_obs_modele <- .meth[2]
ponderation_txt <- if (grepl("pond", methode_lbl, ignore.case = TRUE)) {
  paste0("**Les poids d'échantillonnage ont été appliqués** au modèle (`weights = poids_final` dans ",
         "`glm( ... , method = brglm2::brglmFit)`). Les OR ajustés sont donc des estimations ",
         "représentatives au niveau national/provincial, cohérentes avec le plan de sondage ",
         "complexe utilisé pour calculer la couverture vaccinale.")
} else if (grepl("non pond", methode_lbl, ignore.case = TRUE)) {
  paste0("**Les poids d'échantillonnage n'ont pas pu être appliqués** dans cette implémentation ",
         "(le package `logistf` ne supporte pas les poids). Les OR doivent alors être interprétés ",
         "comme des associations conditionnelles à l'échantillon, et non comme des estimations ",
         "représentatives au niveau population.")
} else if (grepl("svyglm", methode_lbl, ignore.case = TRUE)) {
  paste0("**Le modèle est ajusté par `svyglm` sur le design pondéré** (clusters = aires de santé, ",
         "poids = `poids_final`). Les OR sont des estimations représentatives au niveau ",
         "national/provincial.")
} else {
  paste0("Méthode utilisée : `glm` non pondéré (repli). Les OR sont conditionnels à l'échantillon ",
         "et non extrapolables à la population sans précaution.")
}
narratif(paste0(
  "Le tableau ci-dessus liste l'ensemble des variables du modèle. Chaque Odds Ratio ",
  "se lit **par rapport à la modalité de référence** de la variable concernée ",
  "(par exemple, pour la province, la référence est la première province par ordre ",
  "alphabétique ; pour le sexe, la modalité non affichée). Un OR supérieur à 1 indique ",
  "une probabilité de vaccination plus élevée que la référence, un OR inférieur à 1 ",
  "une probabilité plus faible."
))
callout(paste0(
  "**Note méthodologique — estimation et pondération.** ",
  "Méthode d'estimation employée : **", methode_lbl, "** (taille effective : ",
  n_obs_modele, " observations après suppression des valeurs manquantes sur les ",
  "covariables du modèle).<br><br>",
  "**Pourquoi la pénalisation de Firth ?** La quasi-séparation (faibles effectifs dans ",
  "certaines modalités croisées avec le résultat vaccinal) biaise et déstabilise la ",
  "maximum-vraisemblance standard. La pénalisation de Firth (1993) introduit un terme ",
  "correcteur qui produit des estimations finies et de biais réduit, même en présence ",
  "de séparation.<br><br>",
  "**Et la pondération ?** ", ponderation_txt, "<br><br>",
  "**Limite à connaître :** Firth pondéré (via `brglm2::brglmFit` avec ",
  "`type = \"AS_mean\"`) corrige le biais de séparation et utilise les poids dans la ",
  "vraisemblance, mais ne tient pas compte du clustering par aire de santé pour le ",
  "calcul des écarts-types. Les IC 95 % rapportés peuvent donc être légèrement ",
  "anticonservateurs (intervalles un peu trop étroits). Pour les indicateurs ",
  "principaux (couverture), nous utilisons `svyciprop` qui prend en compte le plan ",
  "complet (poids + clusters + strates)."),
  "info")
```

## Facteurs significativement favorables

```{r tab-determinants-fav}
sig_fav <- determinants %>%
  filter(as.numeric(`p-value`) < 0.05, as.numeric(`OR ajusté`) > 1) %>%
  arrange(desc(as.numeric(`OR ajusté`)))

sig_fav %>%
  mutate(`OR ajusté` = as.character(`OR ajusté`)) %>%
  rename(
    `Facteur` = Variable,
    `OR ajusté` = `OR ajusté`,
    `IC 95 %` = `IC 95%`,
    `p-valeur` = `p-value`
  ) %>%
  afficher_tableau(caption = "Déterminants significativement favorables à la vaccination") %>%
  row_spec(1, background = "#E8F5E9", bold = TRUE)
```

```{r interp-determinants-info, results='asis'}
callout(glue(
  "**Information préalable = levier n° 1 :** Un enfant dont les parents ont été ",
  "informés avant le démarrage de la campagne a environ ",
  "**{round(as.numeric(or_info$'OR ajusté'[1]))} fois plus de chances** d'être vacciné ",
  "qu'un enfant dont la famille n'avait pas reçu cette information. ",
  "C'est le facteur le plus puissant identifié par l'analyse — et le plus actionnable."
), "success")
```

::: {.content-visible when-format="html"}
En langage clair, les résultats de l'analyse disent :
:::

```{r interp-determinants-list, results='asis'}
# Identifier dynamiquement les facteurs favorables les plus forts
det_num <- determinants %>%
  mutate(or_num = suppressWarnings(as.numeric(`OR ajusté`)),
         p_num  = suppressWarnings(as.numeric(`p-value`)))
fav_prov <- det_num %>%
  filter(grepl("Province", Variable), p_num < 0.05, or_num > 1) %>%
  arrange(desc(or_num))
def_prov <- det_num %>%
  filter(grepl("Province", Variable), p_num < 0.05, or_num < 1) %>%
  arrange(or_num)

# Construction des lignes narratives
lignes <- c()
lignes <- c(lignes, paste0(
  "- **Être informé avant la campagne** (", or_info$`OR ajusté`[1], "× plus de chances, ",
  "p < 0,001) : c'est le facteur le plus puissant. Investir dans la mobilisation ",
  "sociale avant le lancement d'une campagne est le moyen le plus efficace d'augmenter la couverture."))

if (nrow(fav_prov) >= 1) {
  prov_lbl <- gsub("Province : ", "", fav_prov$Variable[1])
  lignes <- c(lignes, paste0(
    "- **", prov_lbl, "** (", fav_prov$or_num[1], "× plus de chances, IC 95 % : ",
    fav_prov$`IC 95%`[1], ") : les caractéristiques organisationnelles de cette ",
    "province constituent une référence de bonnes pratiques à documenter et à partager."))
}
if (nrow(fav_prov) >= 2) {
  prov_lbl2 <- gsub("Province : ", "", fav_prov$Variable[2])
  lignes <- c(lignes, paste0(
    "- **", prov_lbl2, "** (", fav_prov$or_num[2], "× plus de chances, p = ",
    signif(fav_prov$p_num[2], 3), ") : performance également supérieure, méritant ",
    "une analyse des facteurs de succès."))
}
if (nrow(or_imp) > 0) {
  lignes <- c(lignes, paste0(
    "- **Importance perçue des vaccins** (", or_imp$`OR ajusté`[1], "× par niveau ",
    "Likert supplémentaire) : plus un parent croit en l'utilité des vaccins, plus il ",
    "amène son enfant. Cela souligne l'importance de la communication sur les bénéfices."))
}
# Tranches d'âge favorables
fav_age <- det_num %>% filter(grepl("Âge enfant", Variable), p_num < 0.05, or_num > 1) %>%
  arrange(desc(or_num))
if (nrow(fav_age) >= 1) {
  age_lbl <- gsub("Âge enfant : ", "", fav_age$Variable[1])
  lignes <- c(lignes, paste0(
    "- **Âge ", age_lbl, "** (", fav_age$or_num[1], "× plus de chances) : ",
    "les enfants de cette tranche présentent une meilleure couverture."))
}
# Déterminants BeSD additionnels significatifs (parents/amis, vaccins souhaités, etc.)
besd_extra <- det_num %>%
  filter(grepl("parents/amis|Vaccins souhait|Connaît lieu|Milieu", Variable),
         p_num < 0.05) %>%
  arrange(desc(abs(log(or_num))))
if (nrow(besd_extra) >= 1) {
  effet <- if (besd_extra$or_num[1] > 1) "favorable" else "défavorable"
  lignes <- c(lignes, paste0(
    "- **", besd_extra$Variable[1], "** (", besd_extra$or_num[1], "× ; p = ",
    signif(besd_extra$p_num[1], 3), ") : déterminant BeSD ", effet, " identifié."))
}

cat(paste(lignes, collapse = "\n"), "\n")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Modèle :** régression logistique pondérée, {n_obs_modele} observations, ",
  "méthode {methode_lbl}. Variables : {nrow(determinants)} lignes dans le modèle couvrant ",
  "4 domaines BeSD (OMS) + caractéristiques individuelles et contextuelles.\n\n",
  "**OR ajustés :** chaque ORa est estimé en contrôlant mutuellement toutes les ",
  "autres variables du modèle. La province de référence est la première par ordre ",
  "alphabétique. Les ORa provinciaux reflètent l'effet résiduel de la province ",
  "après contrôle des variables socio-comportementales.\n\n",
  "**Interprétation causale :** les ORa ne sont pas des effets causaux mais des ",
  "associations ajustées. La causalité inverse est possible (ex. les familles qui ",
  "cherchent l'information sont aussi celles qui vaccinent).\n\n",
  "**Cadre BeSD :** Thinking & Feeling (importance_vaccins, facilite_paiement) ; ",
  "Social Processes (parents_amis_vaccin) ; Motivation (vaccins_souhaites) ; ",
  "Practical Issues (info_campagne, connait_lieu, milieu). Référence : Dube et al., 2021."
), "info")
```

## Facteur significativement défavorable

```{r tab-determinants-def}
sig_def <- determinants %>%
  filter(as.numeric(`p-value`) < 0.05, as.numeric(`OR ajusté`) < 1)

if (nrow(sig_def) > 0) {
  sig_def %>%
    rename(Facteur = Variable, `IC 95 %` = `IC 95%`, `p-valeur` = `p-value`) %>%
    afficher_tableau(
      caption = "Déterminants significativement défavorables à la vaccination"
    ) %>%
    row_spec(1, background = "#FFEBEE", bold = TRUE)
}
```

```{r interp-defavorable, results='asis'}
def_prov_top <- det_num %>%
  filter(grepl("Province", Variable), p_num < 0.05, or_num < 1) %>%
  arrange(or_num) %>% slice(1)
if (nrow(def_prov_top) > 0) {
  prov_lbl <- gsub("Province : ", "", def_prov_top$Variable[1])
  cv_prov_val <- cv_province$`Couverture (%)`[cv_province$Province == prov_lbl]
  cv_txt <- if (length(cv_prov_val) > 0)
    paste0(" Avec une couverture de seulement ", pct_fr(cv_prov_val[1]), " %,") else ""
  callout(paste0(
    "**Province ", prov_lbl, " :** les enfants y ont environ ",
    "**", round(1 / def_prov_top$or_num[1], 1), " fois moins de chances** ",
    "d'être vaccinés comparativement à la province de référence ",
    "(ORa = ", def_prov_top$or_num[1], ", IC 95 % : ", def_prov_top$`IC 95%`[1],
    ", p = ", signif(def_prov_top$p_num[1], 3), ").",
    cv_txt, " cette province nécessite une investigation approfondie et un plan d'action ciblé."
  ), "danger")
} else {
  callout(
    "Aucune province ne ressort significativement défavorable dans le modèle ajusté.",
    "info")
}
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Interprétation de l'ORa défavorable :** un ORa < 1 pour une province signifie que, ",
  "à caractéristiques individuelles et comportementales identiques, un enfant de cette ",
  "province a moins de chances d'être vacciné que dans la province de référence. ",
  "Cet effet résiduel provincial capture les facteurs non mesurés par le questionnaire : ",
  "organisation logistique, accessibilité géographique, qualité de supervision, ",
  "contexte socio-politique local.\n\n",
  "**Quasi-séparation :** les provinces à couverture quasi-totale génèrent une ",
  "quasi-séparation (très peu de non-vaccinés), ce qui nécessite la régression de Firth. ",
  "Les ORa des provinces à forte couverture peuvent être instables — se concentrer ",
  "sur les IC 95 % plutôt que sur les valeurs ponctuelles.\n\n",
  "**Action :** une province avec ORa défavorable significatif doit faire l'objet d'une ",
  "investigation qualitative approfondie avant toute intervention normative."
), "info")
```

## Facteurs non significatifs

```{r interp-non-signif, results='asis'}
# Identifier dynamiquement les facteurs non significatifs (p >= 0.05)
non_sig <- det_num %>%
  filter(p_num >= 0.05) %>%
  mutate(famille = stringr::str_remove(Variable, " : .*$")) %>%
  distinct(famille) %>%
  pull(famille)
# Exclure les familles dont au moins une modalité est significative
sig_fam <- det_num %>%
  filter(p_num < 0.05) %>%
  mutate(famille = stringr::str_remove(Variable, " : .*$")) %>%
  distinct(famille) %>% pull(famille)
non_sig_uniq <- setdiff(non_sig, sig_fam)
if (length(non_sig_uniq) > 0) {
  liste <- paste0("**", non_sig_uniq, "**", collapse = ", ")
  cat(paste0(
    "L'analyse confirme que ", liste, " ne sont pas des déterminants statistiquement ",
    "significatifs de la couverture vaccinale dans cette enquête (p > 0,05). ",
    "Ces résultats, parfois contre-intuitifs, méritent interprétation : ils ne signifient ",
    "pas que ces facteurs sont sans importance, mais plutôt que leur effet est diffus ou ",
    "masqué par d'autres variables dans le modèle.\n"))
}
```

`r newpage()`

# Mobilisation sociale et sources d'information

## Niveau d'information

```{r kpi-info, results='asis'}
cible_info <- 90
info_atteinte <- pct_info >= cible_info
callout(glue(
  "**{pct_fr(pct_info)} % des répondants** déclarent avoir été informés ",
  "avant le démarrage de la campagne — la cible de {cible_info} % est ",
  "**{if (info_atteinte) 'dépassée' else 'non atteinte'}** ",
  "{if (info_atteinte) '\u2705' else '\u26a0\ufe0f'}. ",
  "Cela représente {fraction_lisible(pct_info)} des familles touchées."
), if (info_atteinte) "success" else "warning")
```

**`r pct_fr(pct_info)` %** des répondants déclarent avoir reçu une information sur la campagne avant son lancement. `r if (pct_info >= 90) "C'est une performance remarquable qui dépasse la cible de 90 % et constitue un facteur clé de la bonne couverture nationale." else "Ce résultat reste en deçà de la cible de 90 % et identifie un axe d'amélioration prioritaire."` Ce résultat est d'autant plus important que l'analyse des déterminants montre que l'information préalable est le levier le plus puissant pour augmenter la couverture.

## Canaux d'information utilisés

```{r tab-canaux}
canaux %>%
  mutate(
    pct_lbl = paste0(pct_fr(100 * pct), " %"),
    n = n_fr(n)
  ) %>%
  arrange(desc(as.numeric(str_remove(pct_lbl, " %")))) %>%
  head(8) %>%
  rename(Canal = canal_info_lbl, Effectif = n, `Proportion (%)` = pct_lbl) %>%
  afficher_tableau(
    caption = glue("Canaux d'information sur la campagne (parmi les {pct_fr(pct_info)} % informés)")
  ) %>%
  row_spec(1:2, bold = TRUE, background = "#E3F2FD")
```

```{r fig-canaux, fig.cap="Distribution des canaux d'information sur la campagne", fig.height=6}
show_fig("17_canaux_information.png")
```

```{r note-pop-canaux, results='asis'}
note_pop("Canaux d.information")
```

```{r interp-canaux, results='asis'}
callout(glue(
  "**Les crieurs et les agents de santé communautaires (ASC)** représentent ensemble ",
  "**{pct_fr(pct_can12)} %** de tous les canaux d'information utilisés. ",
  "Ce sont des acteurs de terrain, proches des communautés, peu coûteux et très efficaces. ",
  "Leur rôle central dans la mobilisation sociale de cette campagne doit être reconnu, ",
  "valorisé et renforcé pour les prochaines interventions."
), "success")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Variable multi-réponses :** un répondant peut avoir cité plusieurs canaux. ",
  "Les pourcentages sont calculés sur la base des ménages informés ",
  "({n_fr(sum(canaux$n, na.rm=TRUE))} ménages, soit {pct_fr(pct_info)} % de l'échantillon). ",
  "Ils ne se totalisent pas à 100 %.\n\n",
  "**Couverture par canal :** la figure suivante présente la couverture vaccinale ",
  "observée selon le canal d'information reçu. Cet indicateur est observationnel : ",
  "les familles qui reçoivent l'information par un leader religieux ou un agent de santé ",
  "peuvent être systématiquement différentes (plus motivées, plus accessibles) des familles ",
  "qui reçoivent l'information par la télévision. La différence de couverture par canal ",
  "reflète donc une association, pas nécessairement un effet causal du canal.\n\n",
  "**Indicateur VCQI correspondant :** SIA-QUAL-03 (proportion de ménages informés ",
  "avant la campagne). Cible OMS : ≥ 90 % — {if (pct_info >= 90) 'atteinte' else 'non atteinte'} ",
  "({pct_fr(pct_info)} %)."
), "info")
```

```{r fig-canal-couv, fig.cap="Couverture vaccinale observée selon le canal d'information reçu — la ligne rouge représente la couverture nationale", fig.height=5.5}
show_fig("18_couverture_par_canal.png")
```

`r newpage()`

# Organisation et mise en œuvre de la vaccination

## Sites de vaccination

```{r tab-lieux}
lieu_vacc %>%
  mutate(pct_lbl = paste0(pct_fr(100 * pct), " %"), n = n_fr(n)) %>%
  rename(
    `Lieu de vaccination` = lieu_vaccination_lbl,
    Effectif = n, `Proportion (%)` = pct_lbl
  ) %>%
  afficher_tableau(
    caption = "Sites de vaccination utilisés lors de la campagne"
  ) %>%
  row_spec(1, bold = TRUE, background = "#E3F2FD")
```

```{r fig-lieux, fig.cap="Répartition des lieux de vaccination — l'école est le principal site de vaccination (46 %)", fig.height=5}
show_fig("20_lieu_vaccination.png")
```

```{r note-pop-lieu, results='asis'}
note_pop("Lieu de vaccination")
```

```{r interp-lieux, results='asis'}
callout(glue(
  "L'**{lieu_top1$lieu_vaccination_lbl}** est le principal site de vaccination ",
  "({lieu_top1$pct_lbl} des vaccinés), suivi de la formation sanitaire ",
  "({pct_fr(100 * lieu_vacc$pct[lieu_vacc$lieu_vaccination_lbl == 'Formation sanitaire'])} %). ",
  "La vaccination scolaire permet d'atteindre massivement les enfants d'âge scolaire, ",
  "mais elle exclut mécaniquement les enfants non scolarisés et les moins de 6 ans. ",
  "Les sites communautaires (domicile, marché, église) ne représentent que ",
  "{pct_fr(100 * sum(lieu_vacc$pct[lieu_vacc$lieu_vaccination_lbl %in% c('Domicile', 'Marché/Communauté', 'Église')]))} %."
), "info")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Variable :** lieu_vaccination — question fermée à modalités multiples. ",
  "Un enfant peut avoir été vacciné dans un seul lieu déclaré par le tuteur. ",
  "Les analyses croisées lieux × tranche d'âge sont disponibles en Annexe.\n\n",
  "**Lien avec la couverture par âge :** la prédominance de l'école comme site ",
  "explique mécaniquement la meilleure couverture des 5–14 ans. Les 6–59 mois ",
  "dépendent presque entièrement des stratégies avancées et du passage en ménage. ",
  "Analyser conjointement les lieux de vaccination et les tranches d'âge permet ",
  "d'identifier les segments mal couverts par la stratégie fixe.\n\n",
  "**Indicateur VCQI correspondant :** SIA-QUAL-04 (organisation de la prestation). ",
  "La répartition des sites reflète l'efficacité de la micro-planification opérationnelle."
), "info")
```

## Gratuité de la campagne et dépenses anormales

::: {.content-visible when-format="html"}
La campagne est officiellement gratuite. Pourtant, une proportion non négligeable de ménages déclare avoir engagé des dépenses liées à la vaccination de leur enfant.
:::

```{r tab-depenses}
depenses %>%
  mutate(
    pct_lbl = paste0(pct_fr(pct_dep), " %"),
    n = n_fr(n)
  ) %>%
  rename(Province = province_clean, Enquêtés = n, `% déclarant une dépense` = pct_lbl) %>%
  arrange(desc(pct_dep)) %>%
  afficher_tableau(
    caption = "Part des ménages déclarant des dépenses liées à la vaccination, par province"
  ) %>%
  row_spec(1:2, bold = TRUE, background = "#FFEBEE")
```

```{r fig-depenses, fig.cap="Dépenses de vaccination déclarées par province (campagne théoriquement gratuite)", fig.height=5}
show_fig("19_depenses_par_province.png")
```

```{r note-pop-depenses, results='asis'}
note_pop("Dépenses liées")
```

```{r alerte-depenses, results='asis'}
callout(glue(
  "🚨 **Alerte — principe de gratuité compromis :** ",
  "**{pct_fr(dep_top2$pct_dep[1])} % des ménages à {dep_top2$province_clean[1]}** et ",
  "**{pct_fr(dep_top2$pct_dep[2])} % à {dep_top2$province_clean[2]}** ",
  "déclarent avoir engagé des dépenses liées à la vaccination. ",
  "Ces dépenses peuvent constituer une barrière à l'accès, en particulier pour les ménages pauvres, ",
  "et sont incompatibles avec le principe de gratuité de la campagne. ",
  "Une investigation approfondie est nécessaire dans ces deux provinces."
), "danger")
```

### À quoi ces dépenses étaient-elles liées ?

```{r tab-depenses-motifs}
if (exists("depenses_motifs_t") && nrow(depenses_motifs_t) > 0) {
  depenses_motifs_t %>%
    transmute(Motif, Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(caption = "Motifs des dépenses déclarées (réponses multiples, parmi les ménages ayant déclaré une dépense)")
}
```

```{r fig-depenses-motifs, fig.cap="Motifs des dépenses liées à la vaccination (réponses multiples)", fig.height=4}
if (exists("depenses_motifs_t") && nrow(depenses_motifs_t) > 0) show_fig("19b_depenses_motifs.png")
```

```{r interp-depenses-motifs, results='asis'}
if (exists("depenses_motifs_t") && nrow(depenses_motifs_t) > 0) {
  dm <- depenses_motifs_t %>% arrange(desc(`Pourcentage (%)`))
  top1 <- dm[1, ]
  if (nrow(dm) >= 2) {
    top2 <- dm[2, ]
    phrase_top <- glue(
      "Parmi les ménages ayant déclaré une dépense, le motif le plus fréquent est ",
      "**{tolower(top1$Motif)}** ({fmt_pct1(top1$`Pourcentage (%)`)} %), suivi de ",
      "**{tolower(top2$Motif)}** ({fmt_pct1(top2$`Pourcentage (%)`)} %)."
    )
  } else {
    phrase_top <- glue(
      "Parmi les ménages ayant déclaré une dépense, le motif déclaré est ",
      "**{tolower(top1$Motif)}** ({fmt_pct1(top1$`Pourcentage (%)`)} %)."
    )
  }
  narratif(glue(
    "{phrase_top} Comme un même ménage pouvait citer plusieurs motifs, les pourcentages ",
    "ne totalisent pas 100 %. Ces postes de dépense — souvent modestes individuellement — ",
    "n'en restent pas moins incompatibles avec le principe de gratuité de la campagne, et ",
    "peuvent dissuader les ménages les plus pauvres de faire vacciner leur enfant. ",
    "Ils désignent des leviers d'action concrets : prise en charge du transport vers les sites, ",
    "gratuité effective de la délivrance des cartes et contrôle des pratiques sur le terrain."
  ))
  callout(glue(
    "**Note technique — pour les spécialistes.**\n\n",
    "**Variable :** depense_items_1..3 + _96 — question multi-réponses (4.06b). ",
    "Dénominateur : ménages déclarant au moins une dépense ({pct_fr(dep_top2$pct_dep[1])} % ",
    "à {dep_top2$province_clean[1]}, {pct_fr(dep_top2$pct_dep[2])} % à {dep_top2$province_clean[2]}).\n\n",
    "**Limites :** (1) les dépenses déclarées peuvent inclure des coûts opportunité ",
    "(transport, temps de travail perdu) difficiles à distinguer des paiements directs. ",
    "(2) La déclaration de dépenses peut être sous-estimée (crainte de représailles) ou ",
    "surestimée (attente d'une compensation). (3) Aucun montant n'a été collecté — ",
    "impossible d'évaluer l'ampleur financière réelle.\n\n",
    "**Interprétation :** les dépenses de transport peuvent être inévitables dans les zones ",
    "éloignées (coût d'opportunité géographique), tandis que les paiements pour la carte ",
    "ou la seringue sont clairement illicites et doivent faire l'objet d'une enquête de ",
    "responsabilisation. Ces deux types de dépenses appellent des réponses différentes."
  ), "info")
}
```

## Tolérance vaccinale et effets secondaires

```{r tab-symptomes}
symptomes %>%
  mutate(pct = paste0(pct_fr(pct), " %"), n = n_fr(n)) %>%
  rename(`Symptômes signalés` = measles_symptom_f, Effectif = n, `Proportion (%)` = pct) %>%
  afficher_tableau(
    caption = "Symptômes post-vaccinaux déclarés par les répondants"
  )
```

```{r interp-symptomes, results='asis'}
callout(glue(
  "**{pct_fr(symp_pct)} % des répondants** déclarent que leur enfant a présenté des ",
  "symptômes après la vaccination — au-dessus de la cible de ≤ {kpi_symp_cible} % ✗. ",
  "Ces déclarations sont **non vérifiées cliniquement** et peuvent inclure des ",
  "réactions bénignes normales (légère fièvre, douleur au site d'injection). ",
  "La peur des effets secondaires est par ailleurs citée parmi les raisons de non-vaccination ",
  "— il est donc prioritaire d'améliorer la communication sur la tolérance du vaccin."
), "warning")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Variable :** measles_symptom_f — déclaration du tuteur (binaire : symptômes oui/non) ",
  "et measles_symptom_detail (liste multi-réponses des symptômes observés).\n\n",
  "**Dénominateur :** ensemble de l'échantillon analytique (vaccinés et non vaccinés). ",
  "Il est attendu que les non-vaccinés puissent déclarer des symptômes non liés au vaccin.\n\n",
  "**Biais de mémoire et d'attribution :** les tuteurs peuvent attribuer au vaccin des ",
  "symptômes survenus pour d'autres raisons (paludisme, IRAS), surtout dans les semaines ",
  "suivant la vaccination. La fièvre — symptôme le plus fréquent — est une réaction ",
  "attendue et bénigne de la réponse immunitaire au vaccin RR.\n\n",
  "**Indicateur AEFI :** les événements indésirables suivant la vaccination (AEFI) ",
  "sévères ou inhabituels relèvent d'un système de surveillance distinct (SAGE/OMS). ",
  "Les déclarations de cette enquête ne peuvent pas se substituer à ce système — ",
  "elles mesurent la perception des familles, pas l'incidence clinique des AEFI.\n\n",
  "**Cible OMS :** ≤ 5 % de déclarations de symptômes pour les enquêtes ECP. ",
  "Cette cible est indicative et reflète un niveau de tolérance acceptable."
), "info")
```

```{r tab-symptomes-detail}
if (exists("symptomes_detail_t") && nrow(symptomes_detail_t) > 0) {
  symptomes_detail_t %>%
    transmute(Symptôme,
              Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(
      caption = "Liste des symptômes post-vaccinaux déclarés (détail par type de symptôme)"
    )
}
```

```{r fig-symptomes-detail, fig.cap="Symptômes post-vaccinaux déclarés, par type", fig.height=5}
if (exists("symptomes_detail_t") && nrow(symptomes_detail_t) > 0) show_fig("21_symptomes_detail.png")
```

```{r note-pop-symptomes, results='asis'}
note_pop("Symptômes post-vaccinaux")
```

::: {.content-visible when-format="html"}
Le tableau résumé et le tableau détaillé des symptômes reposent sur la **même population** (échantillon analytique), ce qui rend leurs pourcentages directement comparables.
:::

`r newpage()`

# Effet de plan (DEFF) et corrélation intraclasse — VCQI QUAL-04

::: {.content-visible when-format="html"}
**Une question simple : l'échantillon est-il aussi précis qu'on le croit ?** Lorsqu'une enquête ne sélectionne pas les individus un par un, au hasard parmi toute la population, mais d'abord des zones géographiques puis des ménages au sein de ces zones (c'est ce qu'on appelle un sondage en grappes), les résultats sont un peu moins précis qu'un tirage purement aléatoire de même taille. Le DEFF mesure cet écart de précision, et l'ICC en explique la cause.
:::

```{r tab-deff-icc}
deff_icc_t <- read_t("deff_icc_couverture")
if (nrow(deff_icc_t) > 0) {
  deff_icc_t %>%
    transmute(Niveau, Province,
              `N enquêtés` = fmt_n(as.integer(n)),
              DEFF = format(suppressWarnings(as.numeric(DEFF)), nsmall = 2, decimal.mark = ","),
              `Taille moy. cluster` = `m̄ (taille moy. cluster)`,
              ICC = format(suppressWarnings(as.numeric(ICC)), nsmall = 4, decimal.mark = ",")) %>%
    afficher_tableau(caption = "Effet de plan (DEFF) et corrélation intraclasse (ICC) par province — VCQI QUAL-04")
}
```

```{r fig-deff, fig.cap="Effet de plan (DEFF) de la couverture vaccinale par province. Plus le DEFF est élevé, plus l'enquête perd de sa précision dans cette province.", fig.height=5}
show_fig("13g_deff_par_province.png")
```

```{r interp-deff-icc, results='asis'}
deff_icc_t <- read_t("deff_icc_couverture")
if (!is.na(deff_global) && !is.null(deff_global) && nrow(deff_icc_t) > 0) {

  # Valeurs nationales
  nat <- deff_icc_t %>% filter(Niveau == "National")
  n_enquetes  <- suppressWarnings(as.integer(nat$n[1]))
  deff_nat    <- suppressWarnings(as.numeric(nat$DEFF[1]))
  icc_nat     <- suppressWarnings(as.numeric(nat$ICC[1]))
  m_bar_nat   <- suppressWarnings(as.numeric(nat$`m̄ (taille moy. cluster)`[1]))
  n_eff       <- if (!is.na(n_enquetes) && deff_nat > 0) round(n_enquetes / deff_nat) else NA_integer_

  # Provinces avec DEFF les plus extrêmes
  prov_data <- deff_icc_t %>%
    filter(Niveau == "Provincial") %>%
    mutate(deff_num = suppressWarnings(as.numeric(DEFF)),
           icc_num  = suppressWarnings(as.numeric(ICC)))
  prov_min_d <- prov_data %>% slice_min(deff_num, n = 1)
  prov_max_d <- prov_data %>% slice_max(deff_num, n = 1)
  n_prov_eleve <- prov_data %>% filter(deff_num >= 5) %>% nrow()

  # Qualifier le DEFF national en langage accessible
  qualif_deff <- dplyr::case_when(
    deff_nat < 1.5 ~ list(
      mot = "faible",
      explication = "La structure en grappes de cette enquête n'a entraîné qu'une perte de précision minimale. Les intervalles de confiance sont proches de ceux qu'on obtiendrait avec un tirage purement aléatoire.",
      action = "Ce DEFF satisfaisant valide le plan de sondage actuel. Il peut être maintenu pour les prochaines enquêtes."
    ),
    deff_nat < 3.0 ~ list(
      mot = "modéré",
      explication = glue("La structure en grappes réduit l'efficacité statistique de l'enquête. ",
                         "Concrètement, les {n_fr(n_enquetes)} enfants effectivement enquêtés ",
                         "n'ont pas la même valeur statistique que si chacun avait été tiré au sort ",
                         "indépendamment : la taille d'échantillon effective est d'environ ",
                         "{n_fr(n_eff)} enfants — soit la taille qu'un sondage aléatoire simple ",
                         "aurait besoin pour être aussi précis."),
      action = "Le plan de sondage est acceptable. Pour améliorer la précision, on peut augmenter le nombre de clusters plutôt que la taille de chaque cluster."
    ),
    TRUE ~ list(
      mot = "élevé",
      explication = glue("La structure en grappes réduit fortement l'efficacité statistique. ",
                         "Sur les {n_fr(n_enquetes)} enfants enquêtés, seuls environ ",
                         "{n_fr(n_eff)} apportent une information véritablement indépendante — ",
                         "le reste est redondant statistiquement, car les enfants d'une même ",
                         "aire de santé partagent trop les mêmes caractéristiques. ",
                         "Les intervalles de confiance sont plus larges qu'on ne le croit."),
      action = glue("Pour les prochaines enquêtes, envisager d'augmenter le nombre d'aires de santé tirées ",
                    "(clusters) en réduisant le nombre d'enfants par cluster, ou d'adopter un ",
                    "plan de sondage stratifié plus fin. Cela réduira le DEFF et améliorera la ",
                    "précision des estimations.")
    )
  )

  narratif(glue(
    "**Comprendre le DEFF en une phrase.** Un DEFF de {format(deff_nat, nsmall=1, decimal.mark=',')} ",
    "signifie que l'enquête est {format(deff_nat, nsmall=1, decimal.mark=',')} fois moins précise ",
    "qu'un sondage aléatoire simple de même taille. Autrement dit, il faudrait enquêter ",
    "{format(deff_nat, nsmall=1, decimal.mark=',')} fois plus d'enfants sans clustering pour ",
    "obtenir la même précision. Le DEFF national est **{qualif_deff$mot}**.\n\n",
    qualif_deff$explication, "\n\n",
    "**Pourquoi ce DEFF est-il si élevé ?** L'ICC de **{format(icc_nat, nsmall=4, decimal.mark=',')}** ",
    "explique le mécanisme : il mesure à quel point les enfants d'une même aire de santé se ",
    "ressemblent sur le plan de leur statut vaccinal. Un ICC de ",
    "{format(icc_nat, nsmall=2, decimal.mark=',')} signifie que ",
    "{pct_fr(icc_nat * 100)} % des différences de couverture observées s'expliquent par des ",
    "différences entre aires de santé — et seulement {pct_fr((1-icc_nat) * 100)} % par des ",
    "différences entre enfants au sein d'une même aire. En clair : dans cette enquête, savoir ",
    "dans quelle aire de santé vit un enfant donne déjà beaucoup d'information sur ses chances ",
    "d'être vacciné — ce qui limite la valeur informationnelle de chaque enfant supplémentaire ",
    "enquêté dans la même aire.\n\n",
    "**Ce que cela change pour la lecture des résultats.** Les intervalles de confiance à 95 % ",
    "présentés dans ce rapport sont calculés en tenant compte du DEFF (méthode svyciprop). ",
    "Ils sont donc corrects. En revanche, les comparaisons entre sous-groupes de taille réduite ",
    "(par exemple, une tranche d'âge dans une province donnée) doivent être interprétées avec ",
    "prudence car leur puissance statistique est limitée.\n\n",
    "**Variation entre provinces.** ",
    "{prov_min_d$Province[1]} présente le DEFF le plus faible (",
    "{format(prov_min_d$deff_num[1], nsmall=2, decimal.mark=',')}), ",
    "indiquant une relative hétérogénéité entre aires de santé favorisant la précision. ",
    "À l'opposé, {prov_max_d$Province[1]} atteint un DEFF de ",
    "{format(prov_max_d$deff_num[1], nsmall=2, decimal.mark=',')} — ",
    "le plus élevé du bloc — signalant que les aires de santé de cette province sont très ",
    "homogènes entre elles sur le plan vaccinal."
  ))

  # ── Callout décisionnel ───────────────────────────────────────────────────
  if (deff_nat >= 5) {
    callout(glue(
      "**Attention — DEFF très élevé ({format(deff_nat, nsmall=1, decimal.mark=',')}). ",
      "Pour les décideurs : ce chiffre ne remet pas en cause les conclusions du rapport, ",
      "mais il signifie que l'enquête a une précision réelle équivalente à celle d'un ",
      "échantillon de seulement {n_fr(n_eff)} enfants, malgré les {n_fr(n_enquetes)} enquêtés. ",
      "Pour les prochaines enquêtes, augmenter le nombre d'aires de santé (clusters) ",
      "sera plus efficace qu'augmenter le nombre d'enfants par aire.**"
    ), "warning")
  } else if (deff_nat >= 2.5) {
    callout(glue(
      "**DEFF modéré à élevé ({format(deff_nat, nsmall=1, decimal.mark=',')}). ",
      "Les résultats sont fiables mais les intervalles de confiance sont plus larges ",
      "qu'ils ne le paraissent. Taille d'échantillon effective : environ {n_fr(n_eff)} enfants. ",
      "Recommandation : diversifier les clusters dans les prochains plans de sondage.**"
    ), "info")
  }

  # ── Note technique — spécialistes ────────────────────────────────────────
  callout(glue(
    "**Note technique (VCQI QUAL-04) — pour les spécialistes.**\n\n",
    "**Indicateur :** VCQI QUAL-04 — Effet de plan (DEFF) et corrélation intraclasse (ICC) ",
    "pour l'indicateur de couverture vaccinale.\n\n",
    "**Définitions formelles :**\n",
    "- DEFF = Var(plan complexe) / Var(SRS à même n). ",
    "Approximé ici par : DEFF = [(IC_haut - IC_bas) / (2 × 1,96)]² / [p(1-p)/n], ",
    "où p et n sont issus de svyciprop (méthode logit). ",
    "DEFF national = {format(deff_nat, nsmall=2, decimal.mark=',')} | ",
    "Taille effective = {n_fr(n_enquetes)} / {format(deff_nat, nsmall=2, decimal.mark=',')} ",
    "= {n_fr(n_eff)} enfants.\n",
    "- ICC (ρ) = (DEFF - 1) / (m̄ - 1) avec m̄ = taille moyenne des clusters. ",
    "ICC national = {format(icc_nat, nsmall=4, decimal.mark=',')} | ",
    "m̄ national = {format(m_bar_nat, nsmall=1, decimal.mark=',')} enfants / cluster.\n\n",
    "**Interprétation statistique :**\n",
    "Un ICC de {format(icc_nat, nsmall=3, decimal.mark=',')} signifie que {pct_fr(icc_nat*100)} % ",
    "de la variance totale du statut vaccinal est attribuable aux différences entre clusters ",
    "(variance inter) et {pct_fr((1-icc_nat)*100)} % aux différences intra-cluster. ",
    "L'ICC est élevé typiquement dans les enquêtes de couverture vaccinale en Afrique ",
    "sub-saharienne (valeurs de référence : ρ = 0,10 à 0,40 — Kish, 1965 ; ",
    "Bennett et al., 1991).\n\n",
    "**Variation provinciale :**\n",
    "DEFF min = {format(prov_min_d$deff_num[1], nsmall=2, decimal.mark=',')} ",
    "({prov_min_d$Province[1]}, ICC = {format(prov_min_d$icc_num[1], nsmall=4, decimal.mark=',')}) | ",
    "DEFF max = {format(prov_max_d$deff_num[1], nsmall=2, decimal.mark=',')} ",
    "({prov_max_d$Province[1]}, ICC = {format(prov_max_d$icc_num[1], nsmall=4, decimal.mark=',')}).\n\n",
    "**Impact sur les IC 95 % :** les intervalles de confiance sont calculés par svyciprop ",
    "(méthode logit, package survey de Lumley, 2010), qui intègre pleinement le plan de ",
    "sondage complexe (poids + clusters). Ils sont donc corrects malgré le DEFF élevé. ",
    "En revanche, la puissance des tests de comparaison sur sous-groupes fins (ex. une ",
    "tranche d'âge dans une province spécifique) est réduite.\n\n",
    "**Recommandation pour les prochains sondages :** pour réduire le DEFF, ",
    "augmenter le nombre de clusters k (aires de santé) en diminuant m̄ (enfants / cluster), ",
    "plutôt que d'augmenter n total. La relation DEFF ≈ 1 + (m̄ - 1) × ICC montre qu'avec ",
    "ICC = {format(icc_nat, nsmall=3, decimal.mark=',')}, ",
    "réduire m̄ de {format(m_bar_nat, nsmall=1, decimal.mark=',')} à 10 enfants/cluster ",
    "ramènerait le DEFF à environ {format(1 + (10-1)*icc_nat, nsmall=2, decimal.mark=',')}."
  ), "info")
} else {
  narratif("DEFF et ICC non disponibles pour cette enquête.")
}
```

`r newpage()`

# Analyse des non-répondants et biais potentiel (VCQI ES-03)

::: {.content-visible when-format="html"}
Un biais de non-réponse existe si les ménages non enquêtés (absents, refus) ont un profil de couverture vaccinale systématiquement différent de ceux enquêtés. Il est impossible de mesurer directement la couverture des non-répondants, mais nous pouvons quantifier les taux de non-réponse par province et analyser leur corrélation avec la couverture estimée.
:::

```{r tab-non-repondants}
nr_t <- read_t("non_repondants_analyse")
if (nrow(nr_t) > 0) {
  nr_t %>%
    transmute(Province,
              `Dénombrés`          = fmt_n(n_denombres),
              `Enquêtés`           = fmt_n(n_enquetes),
              `Non-répondants`     = fmt_n(n_non_repondants),
              `Taux de réponse (%)` = paste0(fmt_pct1(taux_reponse), " %"),
              `Taux de NR (%)`     = paste0(fmt_pct1(taux_non_rep), " %"),
              `CV estimée (%)`     = if_else(is.na(cv_pct), "—",
                                              paste0(fmt_pct1(cv_pct), " %"))) %>%
    afficher_tableau(caption = "Analyse des non-répondants par province — VCQI ES-03")
}
```

```{r fig-non-repondants, fig.cap="Taux de non-réponse par province (VCQI ES-03)", fig.height=5}
show_fig("16b_non_repondants.png")
```

```{r interp-non-repondants, results='asis'}
nr_t <- read_t("non_repondants_analyse")
if (nrow(nr_t) > 0) {
  tot_nr  <- nr_t %>% filter(Province == "TOTAL / Moyenne")
  prov_nr <- nr_t %>% filter(Province != "TOTAL / Moyenne")
  prov_max_nr <- prov_nr %>% slice_max(taux_non_rep, n = 1)
  prov_min_nr <- prov_nr %>% slice_min(taux_non_rep, n = 1)
  cor_txt <- if (!is.na(cor_nr_cv)) {
    paste0(" La corrélation entre le taux de non-réponse et la couverture estimée est de ",
           "**r = ", format(cor_nr_cv, nsmall=3, decimal.mark=","), "** — ",
           dplyr::case_when(
             abs(cor_nr_cv) < 0.3  ~ "faible, ce qui suggère que la non-réponse n'introduit pas de biais systématique détectable.",
             abs(cor_nr_cv) < 0.6  ~ "modérée : une tendance est perceptible entre non-réponse et couverture, à surveiller.",
             TRUE                   ~ "forte : les zones à forte non-réponse ont une couverture systématiquement différente, signal d'un biais potentiel."
           ))
  } else ""
  narratif(glue(
    "Le taux de non-réponse national est de **{tot_nr$taux_non_rep[1]} %**, ",
    "ce qui est {if(tot_nr$taux_non_rep[1] <= 5) 'très satisfaisant (< 5 %)' else if(tot_nr$taux_non_rep[1] <= 15) 'acceptable (5–15 %)' else 'élevé (> 15 %) — à surveiller'}. ",
    "Le taux varie de **{prov_min_nr$taux_non_rep[1]} %** ({prov_min_nr$Province[1]}) ",
    "à **{prov_max_nr$taux_non_rep[1]} %** ({prov_max_nr$Province[1]}).{cor_txt}"
  ))
  callout(glue(
    "**Note technique — pour les spécialistes (VCQI ES-03).**\n\n",
    "**Dénominateur :** enfants dénombrés dans le registre de dénombrement (n_denombres). ",
    "Numérateur : enfants effectivement enquêtés (n_enquetes).\n\n",
    "**Taux de non-réponse :** N_non_répondants / N_dénombrés × 100. ",
    "Ce taux inclut : absences le jour de l'enquête, refus, questionnaires incomplets. ",
    "Il ne doit pas être confondu avec le taux de non-vaccination.\n\n",
    "**Corrélation NR × couverture :** r = {format(cor_nr_cv, nsmall=3, decimal.mark=',')}. ",
    "Une corrélation négative forte (r < -0,6) signalerait que les provinces à forte ",
    "non-réponse ont aussi une couverture plus basse — ce qui constituerait un biais ",
    "de sélection conservateur (sous-estimation de la couverture). Une corrélation ",
    "proche de zéro, comme ici, suggère que la non-réponse est aléatoire par rapport ",
    "au statut vaccinal et n'introduit pas de biais systématique.\n\n",
    "**Méthode :** la corrélation est calculée par province (n = 7 points). ",
    "Avec un si petit n, l'interprétation doit rester prudente. Une analyse par aire ",
    "de santé offrirait une meilleure puissance statistique.\n\n",
    "**Indicateur VCQI :** ES-03 (analyse des non-répondants). Référence : ",
    "Groves & Peytcheva (2008). The Impact of Nonresponse Rates on Nonresponse Bias."
  ), "info")
} else {
  narratif("Analyse des non-répondants : données de dénombrement insuffisantes.")
}
```

`r newpage()`

# Qualité des données

::: {.content-visible when-format="html"}
La qualité des données collectées sur le terrain a été évaluée par un **contrôle de cohérence entre les enquêtes principales et les re-interviews (backcheck)**. Sur `r nrow(concordance)` variables controlées, les résultats suivants ont été obtenus :
:::

```{r tab-concordance}
concordance %>%
  mutate(Accord_pct = paste0(pct_fr(Accord_pct), " %")) %>%
  rename(
    Variable = Variable,
    Type = Type,
    `N paires` = N_comparaisons,
    `Accord (%)` = Accord_pct,
    Interprétation = Interpretation
  ) %>%
  select(Variable, Type, `N paires`, `Accord (%)`) %>%
  afficher_tableau(
    caption = "Taux d'accord entre l'enquête principale et le backcheck par variable"
  ) %>%
  row_spec(which(as.numeric(str_remove(
    concordance$Accord_pct |> round(1) |> as.character(), "%")) >= 90),
    background = "#E8F5E9")
```

```{r tab-qualite}
qualite %>%
  rename(
    `Type de variable` = Type,
    `Nb variables` = `Nb variables`,
    `Accord moyen (%)` = `Accord moyen (%)`,
    `Taux d'erreur moyen (%)` = `Taux d'erreur moyen (%)`,
    `Seuil OMS (%)` = `Seuil acceptable (%)`,
    Statut = Statut
  ) %>%
  mutate(
    `Accord moyen (%)` = paste0(pct_fr(`Accord moyen (%)`), " %"),
    `Taux d'erreur moyen (%)` = paste0(pct_fr(`Taux d'erreur moyen (%)`), " %")
  ) %>%
  afficher_tableau(
    caption = "Synthèse de la qualité par type de variable (seuils OMS)"
  ) %>%
  row_spec(1:3, background = "#FFF8E1")
```

```{r fig-concordance, fig.cap="Taux d'accord par variable (enquête principale vs backcheck) — au-delà de 90 % est considéré satisfaisant", fig.height=7}
show_fig("10_concordance_enfant.png")
```

```{r interp-qualite, results='asis'}
callout(glue(
  "**Tous les types de variables dépassent leur seuil d'erreur acceptable.** ",
  "Les variables de type T1 (supposées très stables comme le sexe) affichent un ",
  "taux d'erreur de {pct_fr(qt_t1$'Taux d\\'erreur moyen (%)'[1])} % (seuil : 5 %), ",
  "et les variables de type T2 (attitudes) de {pct_fr(qt_t2$'Taux d\\'erreur moyen (%)'[1])} % ",
  "(seuil : 15 %). Ces résultats suggèrent des problèmes de formation ou de supervision ",
  "des enquêteurs qui doivent être corrigés avant la prochaine collecte."
), "warning")
callout(glue(
  "**Note technique — pour les spécialistes.**\n\n",
  "**Méthode backcheck :** un sous-échantillon d'enfants a fait l'objet d'un re-interview ",
  "par un enquêteur différent, à bref délai (même journée ou lendemain). ",
  "Pour chaque variable, le taux d'accord est calculé comme la proportion de paires ",
  "(enquête principale, backcheck) avec la même réponse.\n\n",
  "**Coefficient Kappa (κ) :** mesure la concordance au-delà du hasard. ",
  "κ < 0,20 = médiocre | 0,20–0,40 = faible | 0,41–0,60 = modéré | ",
  "0,61–0,80 = substantiel | > 0,80 = quasi parfait (Landis & Koch, 1977). ",
  "Un κ médiocre sur une variable T1 (censée être stable) indique une erreur ",
  "systématique d'enquêteur, pas un problème de mémoire du répondant.\n\n",
  "**Classification OMS des types de variables :**\n",
  "- T1 (stable) : sexe, âge, province — seuil d'erreur ≤ 5 %\n",
  "- T2 (attitude/comportement) : importance vaccins, refus — seuil ≤ 15 %\n",
  "- T3 (mémoire) : statut vaccinal antérieur, dépenses — seuil ≤ 25 %\n\n",
  "**Impact sur les résultats :** le taux d'erreur élevé sur les variables T1 et T2 ",
  "n'affecte pas les indicateurs de couverture principale (vaccine_bin est une variable T3 ",
  "dont les seuils sont moins stricts), mais il affecte la fiabilité des analyses ",
  "de sous-groupes et des déterminants comportementaux.\n\n",
  "**Action corrective :** révision du protocole de formation des enquêteurs, ",
  "augmentation de la fréquence des supervisions de terrain, introduction d'un ",
  "test de maîtrise du questionnaire avant le déploiement."
), "info")
```

`r newpage()`

# Recommandations

::: {.content-visible when-format="html"}
Les recommandations qui suivent sont directement issues des résultats de l'enquête. Elles sont classées par **priorité** et assignées à un **responsable principal** avec un délai et un indicateur de suivi mesurable.
:::

---

```{r rec1, results='asis'}
# Provinces les plus basses (jusqu'à 2)
cv_prov_bas <- cv_province %>% arrange(`Couverture (%)`) %>% slice(1:2)
prov_focus <- if (nrow(cv_prov_bas) >= 2) {
  paste(cv_prov_bas$Province[1], "et", cv_prov_bas$Province[2])
} else {
  cv_prov_bas$Province[1]
}
prov_focus_detail <- if (nrow(cv_prov_bas) >= 2) {
  paste0(cv_prov_bas$Province[1], " (", pct_fr(cv_prov_bas$`Couverture (%)`[1]), " %) ",
         "et ", cv_prov_bas$Province[2], " (", pct_fr(cv_prov_bas$`Couverture (%)`[2]), " %)")
} else {
  paste0(cv_prov_bas$Province[1], " (", pct_fr(cv_prov_bas$`Couverture (%)`[1]), " %)")
}
callout(paste0(
  "**R1 — PRIORITÉ HAUTE : Renforcer la mobilisation sociale préalable, avec accent sur ",
  prov_focus, "**<br><br>",
  "**Constat :** L'information préalable est le déterminant le plus puissant de la vaccination (ORa = ",
  or_info$"OR ajusté"[1], "). ", prov_focus_detail,
  " affiche", if (nrow(cv_prov_bas) >= 2) "nt" else "", " les couvertures les plus basses.<br><br>",
  "**Action :** Démarrer la mobilisation sociale au minimum **14 jours avant** le lancement de toute ",
  "campagne. Déployer massivement les crieurs et les ASC (qui représentent déjà ", pct_fr(pct_can12), " % ",
  "des canaux) en renforçant leur formation sur les messages clés. ",
  "Cibler prioritairement les zones à risque identifiées.<br><br>",
  "**Responsable :** OMS + UNICEF (communication) / Équipes des zones de santé concernées<br>",
  "**Délai :** Avant toute prochaine campagne<br>",
  "**Indicateur :** Taux d'information pré-campagne ≥ ", cible_oms, " % dans ", prov_focus
), "danger")
```

```{r rec2, results='asis'}
# Province la plus basse (dynamique) + ORa défavorable si disponible
prov_bas <- cv_province %>% arrange(`Couverture (%)`) %>% slice(1)
prov_bas_nom <- prov_bas$Province[1]
prov_bas_cv  <- prov_bas$`Couverture (%)`[1]
# ORa de la province la plus défavorable (depuis le modèle si présent)
ora_def_txt <- if (exists("det_num")) {
  dd <- det_num %>% dplyr::filter(grepl("Province", Variable), p_num < 0.05, or_num < 1) %>%
    dplyr::arrange(or_num) %>% dplyr::slice(1)
  if (nrow(dd) > 0)
    glue(", soit une probabilité de vaccination environ {round(1/dd$or_num[1],1)} fois plus faible ",
         "que la province de référence (ORa = {dd$or_num[1]})")
  else ""
} else ""
callout(glue(
  "**R2 — PRIORITÉ HAUTE : Lancer une investigation et un plan d'action ciblé pour {prov_bas_nom}**<br><br>",
  "**Constat :** {prov_bas_nom} affiche la couverture la plus basse du bloc ",
  "({pct_fr(prov_bas_cv)} %){ora_def_txt}. Cette province cumule les facteurs de risque ",
  "et concentre une part importante des enfants non atteints.<br><br>",
  "**Action :** Conduire une enquête qualitative rapide (entretiens individuels et de groupe) ",
  "dans les zones les plus en retard pour identifier les barrières spécifiques. Élaborer un ",
  "plan de rattrapage ciblé avant la prochaine campagne (vaccination à domicile, horaires décalés, ",
  "équipes mobiles).<br><br>",
  "**Responsable :** Bureau provincial de la santé de {prov_bas_nom} / OMS RDC<br>",
  "**Délai :** Dans les 90 jours suivant la réception de ce rapport<br>",
  "**Indicateur :** Couverture en hausse significative à {prov_bas_nom} lors de la prochaine évaluation"
), "danger")
```

```{r rec3, results='asis'}
prov_dep1 <- if (exists("dep_top2") && nrow(dep_top2) >= 1) dep_top2$province_clean[1] else "les provinces concernées"
prov_dep2 <- if (exists("dep_top2") && nrow(dep_top2) >= 2) dep_top2$province_clean[2] else NA
titre_dep <- if (!is.na(prov_dep2)) glue("{prov_dep1} et {prov_dep2}") else prov_dep1
callout(paste0(
  "**R3 — PRIORITÉ HAUTE : Enquêter et sanctionner les cas de paiements illicites à ", titre_dep, "**<br><br>",
  "**Constat :** ",
  pct_fr(dep_top2$pct_dep[1]), " % des ménages à ", dep_top2$province_clean[1],
  " et ", pct_fr(dep_top2$pct_dep[2]), " % à ", dep_top2$province_clean[2],
  " déclarent avoir effectué des dépenses liées à la vaccination, ",
  "alors que la campagne est officiellement gratuite. Cette violation du principe de gratuité ",
  "est une barrière financière à l'accès et une faute grave.<br><br>",
  "**Action :** Ouvrir une investigation administrative dans ces deux provinces. ",
  "Renforcer la communication sur la gratuité auprès des équipes vaccinatrices ET des communautés. ",
  "Mettre en place un mécanisme de plainte accessible (numéro vert, boîte anonyme).<br><br>",
  "**Responsable :** Inspection provinciale de la santé / DPS / OMS (supervision)<br>",
  "**Délai :** Immédiat (dans les 30 jours)<br>",
  "**Indicateur :** Taux de ménages déclarant des dépenses < 2 % lors de la prochaine ECP"
), "danger")
```

```{r rec4, results='asis'}
callout(paste0(
  "**R4 — PRIORITÉ HAUTE : Intensifier la vaccination de routine pour réduire les « zéro dose »**<br><br>",
  "**Constat :** ", pct_fr(sat_zero_pct), " % des enfants de moins de 5 ans n'avaient reçu ",
  "aucune dose de vaccin contre la rougeole avant la campagne. Ces enfants constituent ",
  "un réservoir de susceptibles et dépendent entièrement des campagnes supplémentaires.<br><br>",
  "**Action :** Renforcer les séances de vaccination de routine dans les formations sanitaires. ",
  "Introduire des séances de vaccination communautaires mensuelles dans les zones à faible ",
  "couverture de routine. Mettre en place un suivi nominatif des enfants non vaccinés par aire.<br><br>",
  "**Responsable :** Programme Élargi de Vaccination (PEV) / MSP<br>",
  "**Délai :** Mise en œuvre dans les 6 mois<br>",
  "**Indicateur :** Réduction du taux de « zéro dose » sous 5 % en 24 mois"
), "warning")
```

```{r rec5, results='asis'}
if (cv_age_min_sous_cible) {
  callout(paste0(
    "**R5 — PRIORITÉ HAUTE : Adapter les stratégies pour la tranche ", cv_age_min, "**<br><br>",
    "**Constat :** La tranche **", cv_age_min, "** affiche la couverture la plus basse (",
    pct_fr(cv_age_min_pct), " %), sous la cible OMS de ", cible_oms, " %. ",
    "Ces enfants sont moins accessibles via les sites fixes habituels.<br><br>",
    "**Action :** Déployer des équipes mobiles spécifiquement dédiées à cette tranche, ",
    "avec passage systématique dans les marchés, les lieux de rassemblement et à domicile. ",
    "Renforcer la coordination avec les relais communautaires pour localiser les enfants concernés.<br><br>",
    "**Responsable :** Équipes de vaccination / Superviseurs de district<br>",
    "**Délai :** À intégrer dans la planification de la prochaine campagne<br>",
    "**Indicateur :** Couverture ≥ ", cible_oms, " % chez les ", cv_age_min, " lors de la prochaine ECP"
  ), "warning")
} else {
  callout(paste0(
    "**R5 — PRIORITÉ MOYENNE : Consolider l'équité entre tranches d'âge**<br><br>",
    "**Constat :** Toutes les tranches d'âge atteignent la cible OMS, la plus basse étant ",
    "les **", cv_age_min, "** (", pct_fr(cv_age_min_pct), " %).<br><br>",
    "**Action :** Maintenir une stratégie d'atteinte équitable lors des prochaines campagnes ",
    "pour éviter tout décrochage de cette tranche relativement plus basse.<br><br>",
    "**Responsable :** Équipes de vaccination / Superviseurs de district<br>",
    "**Délai :** Prochaine campagne<br>",
    "**Indicateur :** Couverture ≥ ", cible_oms, " % maintenue dans toutes les tranches d'âge"
  ), "info")
}
```

```{r rec6, results='asis'}
callout(paste0(
  "**R6 — PRIORITÉ MOYENNE : Mener une campagne active contre la peur des effets secondaires**<br><br>",
  "**Constat :** ", pct_fr(symp_pct), " % des vaccinés présentent des symptômes déclarés (cible ≤ ",
  kpi_symp_cible, " %). La peur des effets secondaires est la 3e raison de non-vaccination. ",
  "Ces deux éléments se renforcent mutuellement et érodent la confiance dans le vaccin.<br><br>",
  "**Action :** Former les vaccinateurs et les ASC à informer systématiquement les parents ",
  "sur les réactions normales post-vaccinales (légère fièvre, pleurs) et les distinguer ",
  "des effets indésirables graves. Produire des supports de communication visuelle adaptés ",
  "aux communautés (BD, affiches, messages radio).<br><br>",
  "**Responsable :** OMS + UNICEF (communication) / Programme de pharmacovigilance<br>",
  "**Délai :** Avant la prochaine campagne<br>",
  "**Indicateur :** Taux de non-vaccination pour peur des effets secondaires < 3 %"
), "warning")
```

```{r rec7, results='asis'}
callout(paste0(
  "**R7 — PRIORITÉ MOYENNE : Améliorer la conservation et la présentation des cartes de vaccination**<br><br>",
  "**Constat :** Seulement ", pct_fr(conf_vue_pct), " % des vaccinés ont pu présenter ",
  "une carte vérifiable à l'enquêteur. Or, ", pct_fr(conf_pas_pct), " % déclarent n'avoir ",
  "reçu aucune carte. La carte de vaccination est un outil essentiel de suivi de l'enfant.<br><br>",
  "**Action :** Systématiser la remise d'une carte lors de chaque vaccination. ",
  "Sensibiliser les ménages à la valeur et à la conservation de la carte. ",
  "Explorer les solutions numériques (registre SMS, QR-code) pour les zones à faible alphabétisation.<br><br>",
  "**Responsable :** Équipes vaccinatrices / PEV<br>",
  "**Délai :** Dans les 6 mois<br>",
  "**Indicateur :** Taux de vérification documentaire (carte vue) ≥ 50 % lors de la prochaine ECP"
), "info")
```

```{r rec8, results='asis'}
callout(paste0(
  "**R8 — PRIORITÉ MOYENNE : Renforcer la supervision et la formation des enquêteurs**<br><br>",
  "**Constat :** Tous les types de variables du backcheck dépassent leur seuil d'erreur OMS. ",
  "Le taux d'erreur moyen est de ", pct_fr(qt_t1$"Taux d'erreur moyen (%)"[1]),
  " % pour les variables stables (T1, seuil 5 %). ",
  "Ces erreurs peuvent biaiser les estimations de couverture et réduire la confiance dans les données.<br><br>",
  "**Action :** Réviser le protocole de formation des enquêteurs avec simulation pratique. ",
  "Augmenter la proportion de backcheck à 15 % (contre ~5 % actuellement). ",
  "Introduire des contrôles de cohérence automatisés dans l'application SurveyCTO.<br><br>",
  "**Responsable :** Équipe de supervision de l'ECP / OMS<br>",
  "**Délai :** Avant la prochaine collecte de données<br>",
  "**Indicateur :** Taux d'erreur T1 < 5 %, T2 < 15 %, T3 < 25 % lors du prochain backcheck"
), "info")
```

`r newpage()`

# Conclusion

::: {.content-visible when-format="html"}
Cette enquête de couverture post-campagne confirme que la **campagne de vaccination contre la rougeole et la rubéole de novembre–décembre 2025 a atteint un résultat globalement satisfaisant** : `r fraction_lisible(cv_nat_pct)` des enfants éligibles ont été vaccinés dans les `r n_provinces` provinces enquêtées, plaçant la couverture nationale à **`r pct_fr(cv_nat_pct)` %** — `r if (cv_atteinte) paste0("au niveau de") else paste0("à ", pct_fr(cible_oms - cv_nat_pct), " point en dessous de")` la cible OMS de `r cible_oms` %.
:::

::: {.content-visible when-format="html"}
Ce succès global ne doit pas masquer **trois défis majeurs** qui conditionnent l'efficacité des prochaines campagnes :
:::

1. **Des inégalités géographiques persistantes** : l'écart de `r ecart_prov` points entre la province la plus performante (`r cv_prov_max$Province[1]` : `r pct_fr(cv_prov_max$"Couverture (%)")` %) et la moins performante (`r cv_prov_min$Province[1]` : `r pct_fr(cv_prov_min$"Couverture (%)")` %) indique que certaines populations restent insuffisamment protégées.

2. **Un accès à l'information inégal** : bien que `r pct_fr(pct_info)` % des familles aient été informées, les `r pct_fr(100 - pct_info)` % restants concentrent une part disproportionnée des non-vaccinés — l'information préalable est le facteur protecteur le plus puissant identifié.

3. **Des problèmes de qualité des données et d'intégrité opérationnelle** : les taux d'erreur du backcheck et les dépenses anormales signalées à `r dep_top2$province_clean[1]` et `r dep_top2$province_clean[2]` appellent des mesures correctives immédiates.

::: {.content-visible when-format="html"}
Les `r nrow(kpis)` indicateurs clés de performance synthétisent ces résultats :
:::

```{r tab-kpis-final}
kpis %>%
  mutate(
    valeur = paste0(pct_fr(valeur), " %"),
    cible  = paste0(cible, " %"),
    Statut = if_else(atteinte, "✅ Atteinte", "⚠️ Non atteinte")
  ) %>%
  rename(
    Indicateur = indicateur,
    `Valeur observée` = valeur,
    `Cible` = cible
  ) %>%
  select(Indicateur, `Valeur observée`, Cible, Statut) %>%
  afficher_tableau(
    caption = "Tableau de bord des indicateurs clés de performance (KPI)"
  ) %>%
  row_spec(which(!kpis$atteinte), background = "#FFF8E1") %>%
  row_spec(which(kpis$atteinte), background = "#E8F5E9")
```

```{r interp-kpi-bilan, results='asis'}
n_atteint <- sum(kpis$atteinte, na.rm = TRUE)
n_total_kpi <- nrow(kpis)
n_non_atteint <- n_total_kpi - n_atteint
kpi_atteints <- kpis %>% filter(atteinte) %>% pull(indicateur)
kpi_non_atteints <- kpis %>% filter(!atteinte) %>% pull(indicateur)
callout(glue(
  "**{n_atteint} indicateur{if(n_atteint>1) 's' else ''} sur {n_total_kpi} ",
  "{if(n_atteint>1) 'atteignent' else 'atteint'} la cible OMS.** ",
  "Indicateurs atteints : {paste(kpi_atteints, collapse = ' ; ')}. ",
  if (n_non_atteint > 0)
    glue("Indicateurs non atteints : {paste(kpi_non_atteints, collapse = ' ; ')}.")
  else
    "Tous les indicateurs atteignent leur cible."
), if (n_atteint == n_total_kpi) "success" else if (n_atteint >= n_total_kpi / 2) "info" else "warning")
```

```{r fig-kpi-final, fig.cap="Tableau de bord visuel des indicateurs clés — vert = cible atteinte, rouge = cible non atteinte, losange = valeur cible", fig.height=5}
show_fig("21_kpi_dashboard.png")
```

::: {.content-visible when-format="html"}
`r local({ n_at <- sum(kpis$atteinte, na.rm=TRUE); n_tot <- nrow(kpis); glue::glue("Sur {n_tot} indicateurs suivis, {n_at} atteignent la cible OMS. La mobilisation remarquable des équipes de terrain — qui ont informé {fraction_lisible(pct_info)} des familles avant le lancement — est un acquis précieux à consolider. Les recommandations formulées dans ce rapport constituent une feuille de route concrète pour maintenir et étendre ces résultats lors des prochaines campagnes.") })`
:::

`r newpage()`

# Caractéristiques de l'échantillon — profil des ménages et des enfants

::: {.content-visible when-format="html"}
Cette section décrit le profil de l'échantillon enquêté : sa répartition géographique, le profil des enfants (sexe, âge) et celui des ménages et tuteurs. Ces caractéristiques permettent de contextualiser les résultats de couverture présentés précédemment.
:::

## Répartition de l'échantillon par province

```{r tab-profil-province}
if (nrow(profil_province_t) > 0) {
  profil_province_t %>%
    transmute(Province, Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(caption = "Répartition de l'échantillon analytique par province")
}
```

```{r fig-profil-province, fig.cap="Répartition de l'échantillon par province", fig.height=5}
show_fig("28_profil_echantillon_province.png")
```

```{r interp-profil-province, results='asis'}
if (nrow(profil_province_t) > 0) {
  pmax <- profil_province_t %>% slice_max(n, n = 1)
  pmin <- profil_province_t %>% slice_min(n, n = 1)
  total <- sum(profil_province_t$n)
  narratif(glue(
    "L'échantillon analytique compte **{n_fr(total)} enfants** répartis sur les sept provinces. ",
    "La province la plus représentée est **{pmax$Province[1]}** ",
    "({n_fr(pmax$n[1])} enfants, {fmt_pct1(pmax$`Pourcentage (%)`[1])} %), ",
    "et la moins représentée **{pmin$Province[1]}** ",
    "({n_fr(pmin$n[1])} enfants, {fmt_pct1(pmin$`Pourcentage (%)`[1])} %). ",
    "Cette répartition reflète la taille démographique et le nombre d'aires de santé ",
    "couvertes dans chaque province."
  ))
}
```

## Profil des enfants enquêtés

```{r tab-profil-enfant}
if (nrow(profil_enfant_t) > 0) {
  profil_enfant_t %>%
    transmute(Caractéristique, Modalité, Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(caption = "Profil des enfants enquêtés (sexe et tranche d'âge)")
}
```

```{r interp-profil-enfant, results='asis'}
if (nrow(profil_enfant_t) > 0) {
  sexe_tbl <- profil_enfant_t %>% filter(Caractéristique == "Sexe de l'enfant")
  age_tbl  <- profil_enfant_t %>% filter(Caractéristique == "Tranche d'âge")
  txt_sexe <- if (nrow(sexe_tbl) >= 2) {
    ecart <- abs(diff(sexe_tbl$`Pourcentage (%)`[1:2]))
    equilibre <- if (ecart <= 5) "globalement équilibrée" else "déséquilibrée"
    paste0("La répartition par sexe est **", equilibre, "** (",
           paste(sprintf("%s : %s %%", sexe_tbl$Modalité, fmt_pct1(sexe_tbl$`Pourcentage (%)`)),
                 collapse = " ; "), "). ")
  } else ""
  txt_age <- if (nrow(age_tbl) > 0) {
    amax <- age_tbl %>% slice_max(`Pourcentage (%)`, n = 1)
    paste0("La tranche d'âge la plus représentée est **", amax$Modalité[1],
           "** (", fmt_pct1(amax$`Pourcentage (%)`[1]),
           " %), conformément à la pyramide des âges de la population cible.")
  } else ""
  narratif(paste0(txt_sexe, txt_age))
}
```

## Profil des tuteurs

```{r tab-profil-tuteur}
if (exists("profil_tuteur_t") && nrow(profil_tuteur_t) > 0) {
  profil_tuteur_t %>%
    transmute(Caractéristique, Modalité, Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(caption = "Profil des tuteurs (instruction et situation matrimoniale)")
}
```

```{r interp-profil-tuteur, results='asis'}
if (exists("profil_tuteur_t") && nrow(profil_tuteur_t) > 0) {
  instr <- profil_tuteur_t %>% filter(grepl("instruction", Caractéristique, ignore.case = TRUE))
  txt <- "Le profil des tuteurs varie selon les provinces. "
  if (nrow(instr) > 0) {
    imax <- instr %>% slice_max(`Pourcentage (%)`, n = 1)
    txt <- paste0(txt, "Le niveau d'instruction le plus fréquent est **",
                  imax$Modalité[1], "** (", fmt_pct1(imax$`Pourcentage (%)`[1]), " %). ")
  }
  narratif(paste0(txt,
    "Le détail de la couverture vaccinale selon ces caractéristiques est présenté en annexe G."))
}
```

## Caractéristiques des ménages

```{r tab-carac-menage}
if (nrow(carac_menage_t) > 0) {
  carac_menage_t %>%
    transmute(Caractéristique, Modalité,
              Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(pct), " %")) %>%
    afficher_tableau(caption = "Caractéristiques des ménages enquêtés")
}
```

```{r fig-carac-menage, fig.cap="Répartition des ménages selon leurs caractéristiques (sexe et âge du chef, taille)", fig.height=8}
show_fig("22_caracteristiques_menage.png")
```

```{r interp-carac-menage, results='asis'}
if (nrow(carac_menage_t) > 0) {
  chef_sexe <- carac_menage_t %>% filter(Caractéristique == "Sexe du chef de ménage")
  taille <- carac_menage_t %>% filter(Caractéristique == "Taille du ménage")
  txt <- ""
  if (nrow(chef_sexe) > 0) {
    cmax <- chef_sexe %>% slice_max(pct, n = 1)
    txt <- paste0("La majorité des ménages sont dirigés par **", tolower(cmax$Modalité[1]),
                  "** (", fmt_pct1(cmax$pct[1]), " %). ")
  }
  if (nrow(taille) > 0) {
    tmax <- taille %>% slice_max(pct, n = 1)
    txt <- paste0(txt, "La taille de ménage la plus fréquente est **", tmax$Modalité[1],
                  "** (", fmt_pct1(tmax$pct[1]), " %). ")
  }
  narratif(paste0(txt,
    "Ces caractéristiques contextualisent les analyses de couverture par sous-groupe ",
    "présentées en annexe G."))
}
```

`r newpage()`

# Résultats complémentaires par province

```{r interp-resultats-comp-intro, results='asis'}
narratif(paste0(
  "Cette section ventile par province trois indicateurs clés du dispositif de campagne : ",
  "(i) les **canaux d'information** ayant atteint les ménages, ",
  "(ii) la **confirmation par carte/jeton** au moment de l'enquête, ",
  "et (iii) le **lieu de vaccination** déclaré. ",
  "Ces résultats permettent de calibrer finement la communication, la traçabilité ",
  "et l'offre de service province par province."
))
```

## Canaux d'information par province

```{r tab-canaux-prov}
if (nrow(canaux_prov_t) > 0) {
  canaux_prov_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, `Canal d'information` = modalite,
           Effectif = n, `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Canaux d'information par province (avec ensemble)")
}
```

```{r fig-canaux-prov, fig.cap="Canaux d'information par province et ensemble", fig.height=6}
show_fig("17b_canaux_information_province.png")
```

```{r interp-canaux-prov, results='asis'}
if (exists("canaux_prov_t") && nrow(canaux_prov_t) > 0) {
  canal_top <- canaux_prov_t %>% filter(Zone == "Ensemble") %>%
    slice_max(pct, n = 1)
  if (nrow(canal_top) > 0) {
    narratif(glue(
      "Au niveau national, le canal d'information le plus cité est **{canal_top$modalite[1]}** ",
      "({fmt_pct1(canal_top$pct[1])} % des répondants informés). ",
      "Les variations inter-provinciales reflètent les habitudes médiatiques locales : ",
      "les provinces où certains canaux sont moins représentés méritent une diversification ",
      "des supports lors des prochaines campagnes."
    ))
  }
}
```

## Confirmation par carte par province

```{r tab-carte-prov}
if (nrow(carte_prov_t) > 0) {
  carte_prov_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, `Confirmation` = modalite,
           Effectif = n, `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Confirmation par carte, par province (avec ensemble)")
}
```

```{r fig-carte-prov-fig, fig.cap="Confirmation de la vaccination par carte, par province", fig.height=6}
show_fig("08c_confirmation_carte_province.png")
```

```{r interp-carte-prov, results='asis'}
if (exists("carte_prov_t") && nrow(carte_prov_t) > 0) {
  # Proportion "carte vue" au niveau Ensemble
  carte_vue <- carte_prov_t %>%
    filter(Zone == "Ensemble", grepl("vue|carte", modalite, ignore.case = TRUE)) %>%
    slice_max(pct, n = 1)
  if (nrow(carte_vue) > 0) {
    narratif(glue(
      "Au niveau national, **{fmt_pct1(carte_vue$pct[1])} %** des enfants vaccinés ",
      "présentent une confirmation par carte ({carte_vue$modalite[1]}). ",
      "Les provinces affichant des taux de confirmation plus faibles nécessitent ",
      "un renforcement de la délivrance et de la conservation des cartes/jetons ",
      "pour assurer la traçabilité et la qualité des données."
    ))
  }
}
```

## Lieu de vaccination par province

```{r tab-lieu-prov}
if (nrow(lieu_prov_t) > 0) {
  lieu_prov_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, `Lieu` = modalite,
           Effectif = n, `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Lieu de vaccination, par province (avec ensemble)")
}
```

```{r fig-lieu-prov-fig, fig.cap="Lieu de vaccination, par province", fig.height=6}
show_fig("20b_lieu_vaccination_province.png")
```

```{r interp-lieu-prov, results='asis'}
if (exists("lieu_prov_t") && nrow(lieu_prov_t) > 0) {
  lieu_top <- lieu_prov_t %>% filter(Zone == "Ensemble") %>%
    slice_max(pct, n = 1)
  if (nrow(lieu_top) > 0) {
    narratif(glue(
      "Au niveau national, le lieu de vaccination le plus fréquemment déclaré est ",
      "**{lieu_top$modalite[1]}** ({fmt_pct1(lieu_top$pct[1])} %). ",
      "La répartition entre stratégies fixes, avancées et mobiles varie selon les provinces ; ",
      "elle informe le dimensionnement optimal du dispositif pour les prochaines campagnes."
    ))
  }
}
```

`r newpage()`

# Annexes {-}

## Annexe A — Tableau des indicateurs KPI {-}

```{r annexe-kpi}
kpis %>%
  mutate(
    Valeur = paste0(pct_fr(valeur), " %"),
    Cible = paste0(cible, " %"),
    `Sens (↑/↓)` = if_else(sens == "haut", "↑ plus c'est mieux", "↓ moins c'est mieux"),
    Statut = if_else(atteinte, "Atteinte", "Non atteinte"),
    Écart = paste0(if_else(atteinte, "+", ""), pct_fr(if_else(sens=="haut", valeur-cible, cible-valeur)), " pp")
  ) %>%
  rename(Indicateur = indicateur) %>%
  select(Indicateur, Valeur, Cible, `Sens (↑/↓)`, Statut, Écart) %>%
  afficher_tableau(caption = "Indicateurs clés de performance — tableau détaillé")
```

## Annexe B — Limites de l'étude {-}

::: {.content-visible when-format="html"}
Toute enquête, quelle que soit sa rigueur, comporte des limites qui doivent être connues pour une juste interprétation des résultats :
:::

1. **Biais de déclaration :** La couverture vaccinale est mesurée principalement sur déclaration du répondant, avec vérification documentaire limitée (`r pct_fr(conf_vue_pct)` % de cartes vues). Une partie des enfants déclarés vaccinés pourrait ne pas l'avoir été.

2. **Biais de rappel :** Le statut vaccinal antérieur est basé sur la mémoire du répondant. Le taux de réponse « Ne sait pas » de `r pct_fr(statut_ant$pct[statut_ant$statut_vaccinal_ant == "Ne sait pas"])` % sur cette variable en témoigne.

3. **Qualité des données :** Comme documenté dans la section contrôle qualité, les taux d'erreur du backcheck dépassent les seuils OMS pour tous les types de variables. Ce constat doit inciter à la prudence dans l'interprétation des indicateurs secondaires.

4. **Représentativité :** L'enquête est représentative des zones de santé enquêtées. Elle ne couvre pas l'ensemble du territoire national ; les résultats ne peuvent pas être extrapolés aux provinces non incluses.

5. **Symptômes déclaratifs :** Les `r pct_fr(symp_pct)` % de symptômes post-vaccinaux sont des déclarations non vérifiées cliniquement et peuvent inclure des réactions normales sans gravité.

## Annexe C — Glossaire {-}

| Terme | Définition accessible |
|-------|----------------------|
| **Couverture vaccinale** | Proportion d'enfants éligibles ayant reçu au moins une dose lors de la campagne |
| **IC 95 %** | Plage dans laquelle se situe le vrai résultat avec 95 % de certitude |
| **Estimation pondérée** | Résultat ajusté pour tenir compte de la probabilité de sélection de chaque ménage |
| **Odds Ratio ajusté (ORa)** | Mesure de l'association entre un facteur et la vaccination, en contrôlant les autres facteurs. ORa > 1 = facteur favorisant ; ORa < 1 = facteur défavorisant |
| **Chi-2 (p-valeur)** | Test statistique mesurant si une différence entre groupes peut être due au hasard. p < 0,05 = différence réelle |
| **Backcheck** | Re-interview d'une sélection de ménages pour vérifier la cohérence des réponses collectées |
| **Kappa de Cohen** | Mesure de concordance entre deux interviews. Valeur entre 0 (aucun accord) et 1 (accord parfait) |
| **ECP** | Enquête de Couverture Post-Campagne |
| **PEV** | Programme Élargi de Vaccination |
| **ASC** | Agent de Santé Communautaire |
| **Taxonomie OMS** | Classification standardisée des raisons de non-vaccination en groupes thématiques |
| **Zéro dose** | Enfant n'ayant reçu aucune vaccination avant la campagne |
| **Régression de Firth** | Méthode de régression pénalisée qui stabilise les estimations lorsque certaines catégories ont très peu d'observations (quasi-séparation) |
| **IC logit** | Intervalle de confiance calculé par transformation logit, mathématiquement borné dans [0 % ; 100 %] |
| **Biais de désirabilité sociale** | Tendance d'un répondant à donner la réponse qu'il pense attendue (ex. déclarer un enfant vacciné) |

## Annexe D — Note méthodologique détaillée {-}

::: {.content-visible when-format="html"}
Cette annexe reprend, de façon plus technique, les méthodes employées tout au long du rapport. Elle est destinée aux lecteurs souhaitant comprendre ou reproduire les analyses.
:::

### D.1 Pondération {-}

::: {.content-visible when-format="html"}
Les poids de sondage initiaux reflètent la probabilité de sélection à chaque étape : tirage **PPS** (probabilité proportionnelle à la taille) au niveau de la zone de dénombrement, tirage aléatoire simple à l'îlot, puis tirage aléatoire simple au ménage. Ils sont **ajustés par un facteur de non-réponse** calculé au niveau de l'aire de santé. Les estimations sont produites avec le package R `survey` (Lumley, 2010).
:::

### D.2 Intervalles de confiance {-}

::: {.content-visible when-format="html"}
Les intervalles de confiance des proportions sont calculés par **transformation logit** (`survey::svyciprop(method = "logit")`), qui contraint mathématiquement les bornes à rester dans l'intervalle [0 % ; 100 %]. Cette méthode corrige les anomalies de l'approximation classique de Wald, qui peut produire des bornes supérieures aberrantes (> 100 %) lorsque la couverture est très élevée. En l'absence de plan de sondage, l'intervalle de Wilson (score) est utilisé.
:::

### D.3 Test de disparité géographique {-}

::: {.content-visible when-format="html"}
La comparaison de la couverture entre provinces utilise le **test de Rao-Scott** (`survey::svychisq`), une adaptation du test du Chi-2 de Pearson aux plans de sondage complexes (Rao & Scott, 1984). Il tient compte de l'effet de grappe et de la pondération, contrairement au Chi-2 classique qui supposerait un échantillonnage aléatoire simple.
:::

### D.4 Déterminants de la vaccination {-}

::: {.content-visible when-format="html"}
Les facteurs associés à la vaccination sont estimés par **régression logistique pondérée** (`survey::svyglm`). Lorsqu'une quasi-séparation est détectée (très faibles effectifs dans certaines modalités, rendant les estimations instables), le modèle bascule sur une **régression pénalisée de Firth** (Firth, 1993 ; packages `brglm2` ou `logistf`), qui corrige le biais et resserre les intervalles de confiance.
:::

### D.5 Mesures de concordance (contrôle qualité) {-}

::: {.content-visible when-format="html"}
Le pourcentage d'accord simple et le coefficient **Kappa de Cohen** (`irr::kappa2`) sont calculés pour chaque variable commune entre l'enquête principale et le re-contrôle (backcheck). L'interprétation suit l'échelle de **Landis & Koch (1977)** :
:::

| Valeur du Kappa | Niveau d'accord |
|-----------------|------------------|
| < 0,00          | Pauvre (désaccord) |
| 0,00 – 0,20     | Léger |
| 0,21 – 0,40     | Passable |
| 0,41 – 0,60     | Modéré |
| 0,61 – 0,80     | Substantiel |
| 0,81 – 1,00     | Presque parfait |

::: {.content-visible when-format="html"}
Les variables sont classées en trois types, selon leur stabilité attendue et le seuil d'erreur acceptable correspondant :
:::

- **T1** — variables très stables (sexe, statut matrimonial…) : seuil d'erreur acceptable de **5 %** ;
- **T2** — connaissances et attitudes (importance perçue, information…) : seuil de **15 %** ;
- **T3** — variables sensibles ou sujettes au rappel (statut vaccinal antérieur, dépenses…) : seuil de **25 %**.

### D.6 Dérivation du milieu de résidence {-}

::: {.content-visible when-format="html"}
Le milieu (urbain/rural) n'étant pas collecté dans le questionnaire, il est dérivé de la base de sondage (`oag_base_sondage.xlsx`) : pour chaque zone de santé, on détermine le milieu prédominant selon la population recensée, puis on l'affecte aux enfants de cette zone. Cette approximation est fiable pour les zones nettement urbaines ou rurales mais peut être imprécise pour les zones mixtes.
:::

## Annexe E — Références bibliographiques {-}

1. Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Hoboken, NJ : Wiley. Disponible : https://r-survey.r-forge.r-project.org/survey/ — méthode d'estimation pondérée et intervalles de confiance logit (`svyciprop`).
2. Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159-174. https://pubmed.ncbi.nlm.nih.gov/843571/ — échelle d'interprétation du coefficient Kappa de concordance.
3. Rao, J. N. K., & Scott, A. J. (1984). On chi-squared tests for multiway contingency tables with cell proportions estimated from survey data. *Annals of Statistics*, 12(1), 46-60. https://www.jstor.org/stable/2241031 — test du Chi-2 adapté aux plans de sondage complexes (test de Rao-Scott).
4. Institut National de la Statistique (INS) — RDC. *Données démographiques et enquêtes MICS*. https://www.ins.cd/ — données de cadrage démographique pour la République Démocratique du Congo.
5. Firth, D. (1993). Bias reduction of maximum likelihood estimates. *Biometrika*, 80(1), 27-38. https://academic.oup.com/biomet/article/80/1/27/224184 — régression logistique pénalisée corrigeant la quasi-séparation.
6. Organisation mondiale de la Santé (OMS). *Vaccination coverage cluster surveys: reference manual*. Genève : OMS — méthodologie de référence pour les enquêtes de couverture vaccinale post-campagne.

## Annexe F — Attitudes vis-à-vis de la vaccination, par province {-}

```{r interp-annexe-F-intro, results='asis'}
narratif(paste0(
  "Cette annexe présente les distributions des **attitudes vis-à-vis de la vaccination** ",
  "(cadre BeSD : Thinking & Feeling + Motivation + Social Processes) ventilées par province. ",
  "Les trois indicateurs — importance perçue, soutien des parents/amis et vaccins souhaités — ",
  "complètent le portrait socio-comportemental des répondants."
))
```

### F.1 Importance perçue de la vaccination {-}

```{r annexe-importance}
if (nrow(dist_importance_t) > 0) {
  dist_importance_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, Modalité = modalite, Effectif = n,
           `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Importance perçue de la vaccination, par province")
}
```

```{r fig-annexe-importance, fig.cap="Importance perçue de la vaccination, par province et ensemble", fig.height=6}
show_fig("A01_importance_vaccins_province.png")
```

```{r interp-annexe-F1, results='asis'}
if (exists("dist_importance_t") && nrow(dist_importance_t) > 0) {
  # Modalité la plus favorable au niveau Ensemble
  mod_fav <- dist_importance_t %>%
    filter(Zone == "Ensemble") %>% slice_max(pct, n = 1)
  if (nrow(mod_fav) > 0) {
    narratif(glue(
      "Au niveau national, la modalité dominante est **{mod_fav$modalite[1]}** ",
      "({fmt_pct1(mod_fav$pct[1])} % des répondants). ",
      "La variabilité inter-provinciale révèle des différences de perception qui peuvent ",
      "orienter les messages de mobilisation sociale province par province."
    ))
  }
}
```

### F.2 Soutien des parents et amis proches {-}

```{r annexe-pression}
if (nrow(dist_pression_t) > 0) {
  dist_pression_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, Modalité = modalite, Effectif = n,
           `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Parents et amis favorables à la vaccination, par province")
}
```

```{r fig-annexe-pression, fig.cap="Soutien des parents et amis proches à la vaccination, par province", fig.height=6}
show_fig("A02_parents_amis_province.png")
```

```{r interp-annexe-F2, results='asis'}
if (exists("dist_pression_t") && nrow(dist_pression_t) > 0) {
  oui_ensemble <- dist_pression_t %>%
    filter(Zone == "Ensemble", grepl("^Oui", modalite, ignore.case = TRUE)) %>%
    slice(1)
  if (nrow(oui_ensemble) > 0) {
    narratif(glue(
      "Au niveau national, **{fmt_pct1(oui_ensemble$pct[1])} %** des répondants déclarent ",
      "que leurs parents et amis sont favorables à la vaccination. ",
      "Cette dimension sociale (domaine BeSD *Social Processes*) influence directement ",
      "la décision de vacciner ; les provinces affichant un soutien plus faible méritent ",
      "un engagement renforcé des leaders communautaires et religieux."
    ))
  }
}
```

### F.3 Vaccins souhaités par le tuteur {-}

```{r annexe-souhaits}
if (nrow(dist_souhaits_t) > 0) {
  dist_souhaits_t %>%
    mutate(`Pourcentage (%)` = paste0(fmt_pct1(pct), " %"),
           `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    select(Province = Zone, Modalité = modalite, Effectif = n,
           `Pourcentage (%)`, `IC 95%`) %>%
    afficher_tableau(caption = "Vaccins souhaités par le tuteur, par province")
}
```

```{r fig-annexe-souhaits, fig.cap="Vaccins souhaités par le tuteur pour l'enfant, par province", fig.height=6}
show_fig("A03_vaccins_souhaites_province.png")
```

```{r interp-annexe-F3, results='asis'}
if (exists("dist_souhaits_t") && nrow(dist_souhaits_t) > 0) {
  tous_ensemble <- dist_souhaits_t %>%
    filter(Zone == "Ensemble", grepl("Tous", modalite, ignore.case = TRUE)) %>%
    slice(1)
  if (nrow(tous_ensemble) > 0) {
    narratif(glue(
      "Au niveau national, **{fmt_pct1(tous_ensemble$pct[1])} %** des tuteurs souhaitent ",
      "que leur enfant reçoive **tous les vaccins** recommandés. ",
      "Cet indicateur de *Motivation* (cadre BeSD) est un prédicteur direct de l'adhésion ",
      "vaccinale future ; un suivi des provinces où cette proportion est plus basse permettrait ",
      "d'anticiper les zones nécessitant un effort éducatif accru."
    ))
  }
}
```

## Annexe G — Couverture selon les caractéristiques socio-démographiques {-}

```{r interp-annexe-G-intro, results='asis'}
narratif(paste0(
  "Cette annexe ventile la couverture vaccinale selon les caractéristiques ",
  "**du chef de ménage** (G.1) et **du tuteur de l'enfant** (G.2), afin d'identifier ",
  "d'éventuels gradients socio-démographiques (sexe, âge, niveau d'instruction) ",
  "associés à la non-vaccination."
))
```

### G.1 Selon les caractéristiques du chef de ménage {-}

```{r annexe-cv-chef}
if (nrow(cv_chef_t) > 0) {
  cv_chef_t %>%
    transmute(Caractéristique, Modalité, Effectif = fmt_n(N),
              `Couverture (%)` = paste0(fmt_pct1(`Couverture (%)`), " %"),
              `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    afficher_tableau(caption = "Couverture vaccinale selon les caractéristiques du chef de ménage")
}
```

```{r interp-annexe-G1, results='asis'}
if (exists("cv_chef_t") && nrow(cv_chef_t) > 0) {
  hors_ens <- cv_chef_t %>% filter(Caractéristique != "Ensemble")
  if (nrow(hors_ens) > 0) {
    mod_min <- hors_ens %>% slice_min(`Couverture (%)`, n = 1)
    mod_max <- hors_ens %>% slice_max(`Couverture (%)`, n = 1)
    ecart <- mod_max$`Couverture (%)`[1] - mod_min$`Couverture (%)`[1]
    narratif(glue(
      "La couverture varie de **{fmt_pct1(mod_min$`Couverture (%)`[1])} %** ",
      "({mod_min$Caractéristique[1]} : {mod_min$Modalité[1]}) à ",
      "**{fmt_pct1(mod_max$`Couverture (%)`[1])} %** ",
      "({mod_max$Caractéristique[1]} : {mod_max$Modalité[1]}), soit un écart de ",
      "**{fmt_pct1(ecart)} points** entre les modalités extrêmes."
    ))
  }
}
```

### G.2 Selon les caractéristiques du tuteur {-}

```{r annexe-cv-tuteur}
if (nrow(cv_tuteur_t) > 0) {
  cv_tuteur_t %>%
    transmute(Caractéristique, Modalité, Effectif = fmt_n(N),
              `Couverture (%)` = paste0(fmt_pct1(`Couverture (%)`), " %"),
              `IC 95%` = fmt_ic(ic_bas, ic_haut)) %>%
    afficher_tableau(caption = "Couverture vaccinale selon les caractéristiques du tuteur")
}
```

```{r interp-annexe-G2, results='asis'}
if (exists("cv_tuteur_t") && nrow(cv_tuteur_t) > 0) {
  hors_ens <- cv_tuteur_t %>% filter(Caractéristique != "Ensemble")
  if (nrow(hors_ens) > 0) {
    mod_min <- hors_ens %>% slice_min(`Couverture (%)`, n = 1)
    mod_max <- hors_ens %>% slice_max(`Couverture (%)`, n = 1)
    ecart <- mod_max$`Couverture (%)`[1] - mod_min$`Couverture (%)`[1]
    narratif(glue(
      "Du côté du tuteur, la couverture s'étend de **{fmt_pct1(mod_min$`Couverture (%)`[1])} %** ",
      "({mod_min$Caractéristique[1]} : {mod_min$Modalité[1]}) à ",
      "**{fmt_pct1(mod_max$`Couverture (%)`[1])} %** ",
      "({mod_max$Caractéristique[1]} : {mod_max$Modalité[1]}), avec un écart de ",
      "**{fmt_pct1(ecart)} points** — utile pour identifier les profils de tuteurs prioritaires ",
      "à atteindre lors des prochaines campagnes."
    ))
  }
}
```

## Annexe H — Couverture selon les caractéristiques de l'enfant, par province {-}

```{r annexe-cv-enfant-prov}
if (nrow(cv_enfant_prov_t) > 0) {
  cv_enfant_prov_t %>%
    afficher_tableau(caption = "Couverture vaccinale (%) selon les caractéristiques de l'enfant, par province")
}
```

```{r interp-annexe-enfant, results='asis'}
if (exists("cv_enfant_prov_t") && nrow(cv_enfant_prov_t) > 0) {
  # Tente d'identifier dynamiquement les cellules min/max sur la colonne Ensemble
  if ("Ensemble" %in% names(cv_enfant_prov_t)) {
    val_ens <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "",
                                  gsub(",", ".", cv_enfant_prov_t$Ensemble))))
    if (any(!is.na(val_ens))) {
      idx_min <- which.min(val_ens); idx_max <- which.max(val_ens)
      label_col <- names(cv_enfant_prov_t)[1]
      narratif(glue(
        "Ce tableau unique croise les principales caractéristiques de l'enfant ",
        "(sexe, tranche d'âge) avec la province. Au niveau **Ensemble**, la couverture ",
        "la plus basse est observée pour **{cv_enfant_prov_t[[label_col]][idx_min]}** ",
        "({format(round(val_ens[idx_min],1), decimal.mark=',')} %) et la plus élevée pour ",
        "**{cv_enfant_prov_t[[label_col]][idx_max]}** ",
        "({format(round(val_ens[idx_max],1), decimal.mark=',')} %). ",
        "Les pourcentages sont pondérés et exprimés avec une décimale."
      ))
    } else {
      narratif("Ce tableau croise les caractéristiques de l'enfant avec la province. Les pourcentages sont pondérés.")
    }
  }
}
```

## Annexe I — Statut vaccinal antérieur détaillé {-}

```{r annexe-statut-detail}
if (nrow(statut_detail_t) > 0) {
  statut_detail_t %>%
    transmute(`Statut antérieur (détaillé)` = statut_vaccinal_ant,
              Effectif = fmt_n(n),
              `Pourcentage (%)` = paste0(fmt_pct1(pct), " %")) %>%
    afficher_tableau(caption = "Statut vaccinal antérieur détaillé (6-59 mois) — toutes modalités")
}
```

```{r interp-annexe-statut, results='asis'}
if (exists("statut_detail_t") && nrow(statut_detail_t) > 0) {
  modal_top <- statut_detail_t %>% slice_max(pct, n = 1)
  narratif(glue(
    "La modalité la plus fréquente est **{modal_top$statut_vaccinal_ant[1]}** ",
    "({fmt_pct1(modal_top$pct[1])} % des enfants concernés). ",
    "Cette version détaillée distingue la modalité « Une dose ou plus (n.p.) » ",
    "(nombre de doses non précisé), qui est fusionnée avec « Une dose » dans le ",
    "tableau principal du corps du rapport."
  ))
}
```

## Annexe J — Qualité des données : concordance backcheck {-}

Cette annexe complémentaire présente en détail la concordance entre l'enquête principale et le re-contrôle (backcheck) pour les enfants, ainsi que le coefficient Kappa de Cohen par variable.

```{r annexe-concordance}
if (nrow(concordance) > 0) {
  concordance %>%
    mutate(`Accord (%)` = paste0(fmt_pct1(Accord_pct), " %"),
           Kappa = if ("Kappa" %in% names(.)) format(round(Kappa, 3), decimal.mark = ",") else NA) %>%
    rename(`N paires` = N_comparaisons, Interprétation = Interpretation) %>%
    select(any_of(c("Variable", "Type", "N paires", "Accord (%)", "Kappa", "Interprétation"))) %>%
    afficher_tableau(caption = "Concordance backcheck (enfants) — accord et Kappa par variable")
}
```

```{r fig-annexe-concordance, fig.cap="Taux d'accord par variable (enquête principale vs backcheck)", fig.height=7}
show_fig("10_concordance_enfant.png")
```

```{r fig-annexe-kappa, fig.cap="Coefficient Kappa de Cohen par variable (concordance backcheck enfants)", fig.height=7}
show_fig("11_kappa_enfant.png")
```

```{r interp-annexe-kappa, results='asis'}
if (exists("concordance") && nrow(concordance) > 0) {
  # Statistiques dynamiques
  acc_moy <- mean(concordance$Accord_pct, na.rm = TRUE)
  var_min <- concordance %>% slice_min(Accord_pct, n = 1)
  var_max <- concordance %>% slice_max(Accord_pct, n = 1)
  kappa_txt <- if ("Kappa" %in% names(concordance)) {
    k_moy <- mean(concordance$Kappa, na.rm = TRUE)
    paste0(" Le **Kappa moyen** est de **", format(round(k_moy, 2), decimal.mark = ","),
           "** sur l'ensemble des variables comparées.")
  } else ""
  narratif(glue(
    "Le **taux d'accord moyen** entre l'enquête principale et le backcheck est de ",
    "**{fmt_pct1(acc_moy)} %**, avec un minimum à **{fmt_pct1(var_min$Accord_pct[1])} %** ",
    "(*{var_min$Variable[1]}*) et un maximum à **{fmt_pct1(var_max$Accord_pct[1])} %** ",
    "(*{var_max$Variable[1]}*).{kappa_txt} ",
    "Le coefficient Kappa corrige l'accord observé de l'accord attendu par le seul ",
    "hasard ; son interprétation suit l'échelle de Landis & Koch (1977) présentée en ",
    "annexe D.5. Les variables affichant un Kappa faible (proche de zéro ou négatif) ",
    "appellent un renforcement de la formation des enquêteurs sur les items concernés."
  ))
}
```

## Annexe K — Registre des effectifs par indicateur {-}

Ce registre récapitule, pour chaque indicateur du rapport, la **population concernée**, la définition du **numérateur** et du **dénominateur**, l'effectif effectif du dénominateur et le nombre d'enfants exclus de l'échantillon analytique faute d'information sur la variable considérée. Il garantit la transparence sur la base de calcul de chaque résultat.

```{r annexe-registre-effectifs}
if (exists("registre_effectifs_t") && nrow(registre_effectifs_t) > 0) {
  registre_effectifs_t %>%
    transmute(Indicateur = indicateur, Population = population,
              Numérateur = numerateur, Dénominateur = denominateur,
              `n (dénom.)` = fmt_n(n_denom),
              `Exclus` = fmt_n(n_exclus)) %>%
    afficher_tableau(caption = "Registre des effectifs : population, numérateur et dénominateur de chaque indicateur")
}
```

## Annexe L — Tableaux d'accompagnement des graphiques {-}

Cette annexe fournit, sous forme de tableaux, les données chiffrées des graphiques du rapport qui n'étaient pas accompagnés d'un tableau dans le corps du texte.

### L.1 Distribution des enfants par tranche d'âge {-}

```{r annexe-distribution-age}
if (nrow(distribution_age_t) > 0) {
  distribution_age_t %>%
    transmute(`Tranche d'âge`, Effectif = fmt_n(Effectif),
              `Pourcentage (%)` = paste0(fmt_pct1(`Pourcentage (%)`), " %")) %>%
    afficher_tableau(caption = "Distribution par tranche d'âge — même population que la couverture par âge (section 3.4)")
}
```

```{r interp-annexe-K1, results='asis'}
if (exists("distribution_age_t") && nrow(distribution_age_t) > 0) {
  tr_top <- distribution_age_t %>% slice_max(`Pourcentage (%)`, n = 1)
  narratif(glue(
    "La tranche d'âge la plus représentée dans l'échantillon est **{tr_top$`Tranche d'âge`[1]}** ",
    "({fmt_pct1(tr_top$`Pourcentage (%)`[1])} % des enfants enquêtés). ",
    "Cette répartition reflète la pyramide des âges des enfants ciblés par la campagne (6 mois – 14 ans)."
  ))
}
```

### L.2 Couverture vaccinale par canal d'information {-}

```{r annexe-couverture-canal}
if (nrow(couverture_canal_t) > 0) {
  couverture_canal_t %>%
    mutate(couv = if ("couv" %in% names(.)) 100 * couv else `Couverture (%)`) %>%
    arrange(desc(couv)) %>%
    transmute(`Canal d'information` = if ("canal_info_lbl" %in% names(.)) canal_info_lbl else `Canal d'information`,
              Effectif = fmt_n(n),
              `Couverture (%)` = paste0(fmt_pct1(couv), " %")) %>%
    afficher_tableau(caption = "Couverture vaccinale par canal d'information (accompagne la figure correspondante)")
}
```

```{r interp-annexe-K2, results='asis'}
if (exists("couverture_canal_t") && nrow(couverture_canal_t) > 0) {
  cc <- couverture_canal_t %>%
    mutate(couv = if ("couv" %in% names(.)) 100 * couv else `Couverture (%)`,
           canal_lbl = if ("canal_info_lbl" %in% names(.)) canal_info_lbl else `Canal d'information`)
  cmax <- cc %>% slice_max(couv, n = 1)
  cmin <- cc %>% slice_min(couv, n = 1)
  narratif(glue(
    "La couverture est la plus élevée pour les enfants exposés au canal ",
    "**{cmax$canal_lbl[1]}** ({fmt_pct1(cmax$couv[1])} %), et la plus basse pour ",
    "**{cmin$canal_lbl[1]}** ({fmt_pct1(cmin$couv[1])} %). ",
    "Ces résultats orientent le choix des canaux à privilégier pour la mobilisation sociale ",
    "des prochaines campagnes."
  ))
}
```

### L.3 Couverture vaccinale par zone de santé {-}

```{r annexe-couverture-zone}
if (nrow(couverture_zone_t) > 0) {
  couverture_zone_t %>%
    arrange(province_clean, desc(pct)) %>%
    transmute(Province = province_clean,
              `Zone de santé` = zone_sante_clean,
              `Enfants (N)` = fmt_n(N),
              Vaccinés = fmt_n(Nvax),
              `Couverture (%)` = paste0(fmt_pct1(pct), " %")) %>%
    afficher_tableau(caption = "Couverture vaccinale par zone de santé (accompagne le graphique par zone de santé)")
}
```

```{r interp-annexe-K3, results='asis'}
if (exists("couverture_zone_t") && nrow(couverture_zone_t) > 0) {
  n_zs <- nrow(couverture_zone_t)
  n_cible <- sum(couverture_zone_t$pct >= cible_oms, na.rm = TRUE)
  n_sous <- n_zs - n_cible
  zs_min <- couverture_zone_t %>% slice_min(pct, n = 1)
  narratif(glue(
    "Sur **{n_zs} zones de santé** enquêtées, **{n_cible}** atteignent ou dépassent la ",
    "cible OMS de {cible_oms} % et **{n_sous}** sont sous la cible. ",
    "La zone affichant la couverture la plus basse est **{zs_min$zone_sante_clean[1]}** ",
    "(province {zs_min$province_clean[1]}, {fmt_pct1(zs_min$pct[1])} %), à prioriser pour le rattrapage."
  ))
}
```

---

*Rapport généré automatiquement par le système d'analyse ECP — OMS Bureau RDC*  
*Date de génération : `r format(Sys.Date(), "%d %B %Y")`*  
*Version : finale | Données : ECP Rougeole-Rubéole Nov–Déc 2025*]--"

writeLines(qmd_content, qmd_path)
cli_alert_success("Document Quarto : {.path {qmd_path}}")



# --- 4. RESUME ---------------------------------------------------------------

cli_h1("Reporting terminé")
cli_alert_info("Word  (officer) : {.path {word_path}}")
cli_alert_info("QMD   (Quarto)  : {.path {qmd_path}}")
cli_rule("Rendu HTML avec TOC — commandes Quarto")
cli_alert_info("Terminal : quarto render {qmd_path} --to html")
cli_alert_info("RStudio  : cliquer 'Render' avec format HTML selectionne")
cli_alert_info("Le TOC fixe a gauche requiert Quarto CLI (quarto.org/docs/get-started/)")
