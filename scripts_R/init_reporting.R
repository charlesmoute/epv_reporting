# =============================================================================
# ORCHESTRATEUR : lance le pipeline complet
# Enquête de Couverture Post-Vaccinale (ECP) Rougeole-Rubéole en RDC
# OMS RDC - 2025/2026
# =============================================================================
# Auteur : Charles Mouté (révisé)
# Date   : Mai 2026
#
# Ce script enchaîne :
#   1. Traitement / nettoyage des données       -> script_treatment.R
#   2. Analyses et production des figures/tab.  -> script_analyses.R
#   3. Reporting (Word + Quarto)                -> script_reporting.R
#   4. Rendu Quarto en HTML                     -> quarto::quarto_render(...)
#
# Note : le script suppose que le working directory est la racine du projet
# (le dossier qui contient `data/`, `scripts_R/`, `outputs/`, ...).
# =============================================================================

# (Re)définir le working directory si besoin :
# setwd("chemin/vers/le/projet")

# 1. Traitement des données (online SurveyCTO OU offline .dta selon contexte)
source("scripts_R/script_treatment.R")

# 2. Analyses + production des graphiques et tableaux
source("scripts_R/script_analyses.R")

# 3. Génération du rapport Word + fichier .qmd
source("scripts_R/script_reporting.R")

# 4. Rendu du rapport Quarto en HTML (si quarto + binaire dispo)
if (requireNamespace("quarto", quietly = TRUE)) {
  qmd_file <- "outputs/rapport/rapport_ecp_rdc.qmd"
  if (file.exists(qmd_file)) {
    tryCatch({
      quarto::quarto_render(qmd_file, output_format = "html")
      html_out <- "outputs/rapport/rapport_ecp_rdc.html"
      if (file.exists(html_out)) {
        message("Rapport HTML : ", normalizePath(html_out))
        if (interactive()) try(browseURL(normalizePath(html_out)), silent = TRUE)
      }
    }, error = function(e) {
      message("Rendu Quarto non effectué : ", e$message)
    })
  }
} else {
  message("Package 'quarto' non installé - rendu HTML non effectué.")
  message("Pour l'installer : install.packages('quarto')")
}
