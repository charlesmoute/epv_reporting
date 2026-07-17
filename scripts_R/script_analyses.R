# =============================================================================
# SCRIPT D'ANALYSE UNIFIE
# Enquête de Couverture Post-Vaccinale (ECP) Rougeole-Rubéole en RDC
# OMS RDC - 2025/2026
# =============================================================================
# Auteur  : Charles Mouté (révisé)
# Date    : Mai 2026
# Objet   :
#   Ce script unifie l'ensemble des analyses (descriptives, pondérées,
#   concordance backcheck/Kappa) et produit des graphiques et tableaux
#   professionnels, lisibles et reproductibles.
#
#   Il consomme les données STATA (.dta) produites par `script_treatment.R`
#   (ou les fichiers .dta présents dans `data/`).
#
# Sorties :
#   outputs/graphiques/  - figures PNG haute résolution
#   outputs/tableaux/    - tableaux HTML (gt) et CSV
#   outputs/cartes/      - carte choroplèthe (si shapefile dispo)
#   outputs/datasets/    - jeux de données analytiques (RDS)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. CONFIGURATION ET CHARGEMENT DES PACKAGES
# -----------------------------------------------------------------------------

# Installation automatique des packages manquants
pkgs_required <- c(
  "tidyverse", "haven", "labelled", "scales", "glue",
  "survey", "flextable",
  "ggplot2", "ggtext", "patchwork",
  "RColorBrewer", "viridis", "cli"
)
pkgs_optional <- c(
  "srvyr", "irr", "gt", "gtExtras", "ggrepel", "paletteer",
  "janitor", "sf", "ggspatial", "rnaturalearth", "rnaturalearthdata",
  "gtsummary", "broom", "DescTools", "logistf", "brglm2"
)

# Helper d'installation/chargement silencieux
.install_if_missing <- function(pkgs, optional = FALSE) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      if (optional) {
        message("Package optionnel non disponible (ignoré) : ", p)
      } else {
        try(install.packages(p, quiet = TRUE,
                             repos = "https://cloud.r-project.org"),
            silent = TRUE)
      }
    }
  }
}
.install_if_missing(pkgs_required, optional = FALSE)
.install_if_missing(pkgs_optional, optional = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(labelled)
  library(scales)
  library(glue)
  library(survey)
  library(flextable)
  library(ggtext)
  library(patchwork)
  library(cli)
})

# Packages optionnels (chargés s'ils sont disponibles)
has_gt       <- requireNamespace("gt", quietly = TRUE)
has_irr      <- requireNamespace("irr", quietly = TRUE)
has_sf       <- requireNamespace("sf", quietly = TRUE)
has_ggspat   <- requireNamespace("ggspatial", quietly = TRUE)
has_paletteer<- requireNamespace("paletteer", quietly = TRUE)
has_gtsummary<- requireNamespace("gtsummary", quietly = TRUE)
has_broom    <- requireNamespace("broom", quietly = TRUE)
has_desctools<- requireNamespace("DescTools", quietly = TRUE)
has_ggrepel  <- requireNamespace("ggrepel", quietly = TRUE)
if (has_gt)     suppressPackageStartupMessages(library(gt))
if (has_sf)     suppressPackageStartupMessages(library(sf))
if (has_ggspat) suppressPackageStartupMessages(library(ggspatial))
if (has_gtsummary) suppressPackageStartupMessages(library(gtsummary))
if (has_broom)     suppressPackageStartupMessages(library(broom))

# Désactiver la notation scientifique
options(scipen = 999, dplyr.summarise.inform = FALSE)


# -----------------------------------------------------------------------------
# 1. CHEMINS ET DOSSIERS DE SORTIE
# -----------------------------------------------------------------------------

# Le script suppose que le working directory est la racine du projet
# (le dossier qui contient `data/`, `scripts_R/`, etc.).
# Adapter ces chemins si nécessaire.

PATH_DATA       <- if (dir.exists("outputs/datasets/clean")) "outputs/datasets/clean" else if (dir.exists("outputs/datasets")) "outputs/datasets" else "data"
PATH_OUTPUTS    <- "outputs"
PATH_GRAPHIQUES <- file.path(PATH_OUTPUTS, "graphiques")
PATH_TABLEAUX   <- file.path(PATH_OUTPUTS, "tableaux")
PATH_CARTES     <- file.path(PATH_OUTPUTS, "cartes")
PATH_DATASETS   <- file.path(PATH_OUTPUTS, "datasets")
PATH_ANALYSIS   <- file.path(PATH_DATASETS, "analysis")
PATH_RAPPORT    <- file.path(PATH_OUTPUTS, "rapport")

# Création silencieuse des dossiers
walk(
  c(PATH_OUTPUTS, PATH_GRAPHIQUES, PATH_TABLEAUX, PATH_CARTES,
    PATH_DATASETS, PATH_ANALYSIS, PATH_RAPPORT),
  ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE)
)


# -----------------------------------------------------------------------------
# 2. THEME GRAPHIQUE ET HELPERS DE PRESENTATION
# -----------------------------------------------------------------------------

# Palette de couleurs WHO/OMS
oms_colors <- c(
  bleu_fonce = "#003366",
  bleu_who   = "#0093D5",  # bleu de l'OMS
  bleu_clair = "#3399FF",
  vert       = "#33A02C",
  vert_clair = "#A6D96A",
  orange     = "#FF7F00",
  jaune      = "#FECC5C",
  rouge      = "#E31A23",
  violet     = "#6A3D9A",
  gris_fonce = "#3A3A3A",
  gris       = "#737373",
  gris_clair = "#D9D9D9"
)

# Palette catégorielle cohérente (utilisée pour la plupart des graphiques)
palette_oms_cat <- c(
  oms_colors[["bleu_who"]],
  oms_colors[["orange"]],
  oms_colors[["vert"]],
  oms_colors[["violet"]],
  oms_colors[["rouge"]],
  oms_colors[["jaune"]],
  oms_colors[["bleu_fonce"]]
)

# Thème ggplot2 unifié, professionnel et reproductible
theme_oms <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                      color = oms_colors[["bleu_fonce"]],
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(size = rel(1.0), hjust = 0,
                                      color = oms_colors[["gris"]],
                                      margin = margin(b = 12)),
      plot.caption     = element_text(size = rel(0.8), color = oms_colors[["gris"]],
                                      hjust = 1, margin = margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title       = element_text(face = "bold", size = rel(0.95),
                                      color = oms_colors[["gris_fonce"]]),
      axis.title.x     = element_text(margin = margin(t = 8)),
      axis.title.y     = element_text(margin = margin(r = 8)),
      axis.text        = element_text(size = rel(0.85),
                                      color = oms_colors[["gris_fonce"]]),
      axis.line.x      = element_line(color = oms_colors[["gris_clair"]],
                                      linewidth = 0.3),
      axis.ticks.x     = element_line(color = oms_colors[["gris_clair"]],
                                      linewidth = 0.3),
      axis.ticks.y     = element_blank(),
      panel.grid.major.y = element_line(color = oms_colors[["gris_clair"]],
                                        linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold", size = rel(0.85),
                                        color = oms_colors[["gris_fonce"]]),
      legend.text        = element_text(size = rel(0.85)),
      legend.key.height  = unit(0.4, "cm"),
      legend.margin      = margin(t = 5),
      strip.background   = element_rect(fill = oms_colors[["bleu_fonce"]],
                                        color = NA),
      strip.text         = element_text(face = "bold", color = "white",
                                        size = rel(0.9),
                                        margin = margin(4, 4, 4, 4)),
      plot.margin        = margin(14, 14, 14, 14),
      plot.background    = element_rect(fill = "white", color = NA)
    )
}
theme_set(theme_oms())

# Sauvegarde de graphique (largeur, hauteur en pouces; dpi haute résolution)
save_plot <- function(plot, filename, width = 11, height = 7, dpi = 320) {
  ggsave(
    filename = filename, plot = plot,
    width = width, height = height, dpi = dpi,
    bg = "white", device = grDevices::png
  )
  cli_alert_success("Figure sauvegardée : {.path {filename}}")
  invisible(filename)
}

# Caption standard pour toutes les figures
.caption_default <- function(suffix = NULL) {
  base <- "Source : ECP Rougeole-Rubéole, OMS RDC 2025-2026"
  if (!is.null(suffix)) paste(base, suffix, sep = " | ") else base
}

# ── Helpers de formatage FRANÇAIS (virgule décimale) ─────────────────────────
# Pourcentage arrondi à 1 décimale, virgule française (ex. "94,5")
fmt_pct1 <- function(x) {
  ifelse(is.na(x), "—", format(round(x, 1), nsmall = 1, decimal.mark = ",",
                               big.mark = " ", trim = TRUE))
}
# Effectif entier avec séparateur de milliers espace
fmt_n <- function(x) {
  ifelse(is.na(x), "—", format(as.integer(round(x)), big.mark = " ",
                               scientific = FALSE, trim = TRUE))
}
# Intervalle de confiance "[bas - haut]" en % à 1 décimale, virgule
fmt_ic <- function(bas, haut) {
  ifelse(is.na(bas) | is.na(haut), "—",
         sprintf("[%s - %s]", format(round(bas, 1), nsmall = 1, decimal.mark = ","),
                 format(round(haut, 1), nsmall = 1, decimal.mark = ",")))
}


# Helper pour créer un tableau gt stylisé OMS (ou flextable en repli)
# Retourne :
#  - un objet gt_tbl si gt est disponible (export en .html)
#  - un objet flextable sinon
gt_styled <- function(data, title = NULL, subtitle = NULL,
                      source_note = "Source : ECP Rougeole-Rubéole, OMS RDC 2025-2026") {
  if (has_gt) {
    out <- gt::gt(data)
    if (!is.null(title)) {
      out <- out %>%
        gt::tab_header(
          title = gt::md(glue("**{title}**")),
          subtitle = if (!is.null(subtitle)) gt::md(glue("*{subtitle}*")) else NULL
        )
    }
    out %>%
      gt::tab_source_note(source_note = source_note) %>%
      gt::tab_options(
        heading.title.font.size = 16,
        heading.subtitle.font.size = 12,
        column_labels.font.weight = "bold",
        column_labels.background.color = oms_colors[["bleu_fonce"]],
        column_labels.text_transform = "uppercase",
        column_labels.font.size = 11,
        table.font.size = 11,
        table.border.top.width = gt::px(2),
        table.border.top.color = oms_colors[["bleu_fonce"]],
        table.border.bottom.width = gt::px(2),
        table.border.bottom.color = oms_colors[["bleu_fonce"]],
        row.striping.background_color = "#F5F5F5",
        data_row.padding = gt::px(6)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "white"),
        locations = gt::cells_column_labels()
      ) %>%
      gt::opt_row_striping()
  } else {
    # Fallback : flextable
    ft <- flextable::flextable(data) %>%
      flextable::theme_vanilla() %>%
      flextable::bg(part = "header", bg = oms_colors[["bleu_fonce"]]) %>%
      flextable::color(part = "header", color = "white") %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(part = "all", size = 10) %>%
      flextable::autofit()
    if (!is.null(title)) {
      ft <- flextable::set_caption(ft, caption = title)
    }
    ft
  }
}

# Export combiné tableau (HTML + CSV) - chemin de base sans extension
export_table <- function(data_or_obj, basename_no_ext, raw_data = NULL) {
  # Export du CSV (avec raw_data si fourni, sinon avec data_or_obj si data.frame)
  csv_done <- FALSE
  if (!is.null(raw_data) && is.data.frame(raw_data)) {
    csv_path <- paste0(basename_no_ext, ".csv")
    write_csv(raw_data, csv_path, na = "")
    cli_alert_success("Tableau CSV  : {.path {csv_path}}")
    csv_done <- TRUE
  } else if (is.data.frame(data_or_obj)) {
    csv_path <- paste0(basename_no_ext, ".csv")
    write_csv(data_or_obj, csv_path, na = "")
    cli_alert_success("Tableau CSV  : {.path {csv_path}}")
    csv_done <- TRUE
  }
  # Export HTML
  html_path <- paste0(basename_no_ext, ".html")
  tryCatch({
    if (has_gt && inherits(data_or_obj, "gt_tbl")) {
      gt::gtsave(data_or_obj, filename = html_path)
    } else if (inherits(data_or_obj, "flextable")) {
      flextable::save_as_html(data_or_obj, path = html_path)
    } else if (is.data.frame(data_or_obj)) {
      # repli ultime : simple table HTML via flextable
      flextable::save_as_html(flextable::flextable(data_or_obj), path = html_path)
    }
    if (file.exists(html_path))
      cli_alert_success("Tableau HTML : {.path {html_path}}")
  }, error = function(e) {
    cli_alert_warning("Export HTML KO ({e$message}) - CSV uniquement.")
  })
  invisible(basename_no_ext)
}

# Coloration conditionnelle d'une cellule (gt) : no-op si gt non dispo
# Usage : gt_color_cells(tbl, column, rows, fill = "#abc...", alpha = 0.25)
gt_color_cells <- function(tbl, column, rows, fill, alpha = 0.25) {
  if (!has_gt || !inherits(tbl, "gt_tbl")) return(tbl)
  tryCatch(
    tbl %>%
      gt::tab_style(
        style = gt::cell_fill(color = fill, alpha = alpha),
        locations = gt::cells_body(columns = !!column, rows = !!rows)
      ),
    error = function(e) tbl
  )
}
gt_text_style <- function(tbl, column, rows, color, weight = "bold") {
  if (!has_gt || !inherits(tbl, "gt_tbl")) return(tbl)
  tryCatch(
    tbl %>%
      gt::tab_style(
        style = gt::cell_text(color = color, weight = weight),
        locations = gt::cells_body(columns = !!column, rows = !!rows)
      ),
    error = function(e) tbl
  )
}

# Helper: convertir un objet labellisé/factor en numérique sécurisé
as_num <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) return(x)
  if (inherits(x, "haven_labelled")) return(as.numeric(haven::zap_labels(x)))
  if (is.factor(x)) return(as.numeric(as.character(x)))
  suppressWarnings(as.numeric(as.character(x)))
}

cli_h1("Configuration terminée")


# -----------------------------------------------------------------------------
# 3. CHARGEMENT DES DONNEES
# -----------------------------------------------------------------------------

cli_h1("Chargement des données")

# Helper: lecture robuste d'un .dta, retourne NULL si absent (avec un warning)
.read_dta_safe <- function(path) {
  if (!file.exists(path)) {
    cli_alert_warning("Fichier introuvable : {.path {path}}")
    return(NULL)
  }
  tryCatch({
    d <- haven::read_dta(path)
    cli_alert_success("{.path {path}} ({nrow(d)} lignes, {ncol(d)} colonnes)")
    d
  }, error = function(e) {
    cli_alert_danger("Erreur lecture {.path {path}} : {e$message}")
    NULL
  })
}

denombrement       <- .read_dta_safe(file.path(PATH_DATA, "denombrement.dta"))
menage_main        <- .read_dta_safe(file.path(PATH_DATA, "menage_main.dta"))
menage_backcheck   <- .read_dta_safe(file.path(PATH_DATA, "menage_backchek.dta"))
enfant_main        <- .read_dta_safe(file.path(PATH_DATA, "enfant_main.dta"))
enfant_backcheck   <- .read_dta_safe(file.path(PATH_DATA, "enfant_backchek.dta"))
monitoring         <- .read_dta_safe(file.path(PATH_DATA, "monitoring.dta"))
supervision        <- .read_dta_safe(file.path(PATH_DATA, "supervision.dta"))

# Vérifications minimales : on a besoin au moins de enfant_main et denombrement
if (is.null(enfant_main)) {
  cli_abort("La base {.field enfant_main.dta} est requise et introuvable dans {.path {PATH_DATA}}")
}

# -----------------------------------------------------------------------------
# 3b. ENRICHISSEMENT : MILIEU DE RESIDENCE (urbain / rural)
# -----------------------------------------------------------------------------
# Le milieu (urbain/rural) n'est pas collecté directement dans le questionnaire.
# On le dérive de la base de sondage (oag_base_sondage.xlsx, colonne milres_name)
# en déterminant le milieu PREDOMINANT de chaque zone de santé (pondéré par la
# population recensée preEA_Popn), puis on l'affecte à chaque enfant.
# JOINTURE PAR IDENTIFIANT : les 5 premiers chiffres du zd_id de la base
# correspondent au zone_sante_id de l'enquête (vérifié : 42/42 zones appariées,
# 0 valeur manquante). Cette clé numérique est insensible aux écarts
# d'orthographe des noms de zones (ex. « Malemba-Nkulu » vs « Malemba Nkulu »).
.enrichir_milieu <- function(df) {
  # La base de sondage se trouve dans data/ (jamais dans outputs/datasets/)
  base_candidates <- c(file.path("data", "oag_base_sondage.xlsx"),
                       file.path(PATH_DATA, "oag_base_sondage.xlsx"))
  base_path <- base_candidates[file.exists(base_candidates)][1]
  if (is.na(base_path)) {
    cli_alert_warning("Base de sondage absente : milieu non dérivé.")
    df$milieu <- factor(NA, levels = c("Urbain", "Rural"))
    return(df)
  }
  base <- tryCatch(readxl::read_excel(base_path), error = function(e) NULL)
  if (is.null(base) || !all(c("zd_id", "milres_name") %in% names(base))) {
    cli_alert_warning("Colonnes zd_id/milres_name absentes de la base de sondage.")
    df$milieu <- factor(NA, levels = c("Urbain", "Rural"))
    return(df)
  }
  pop_col <- if ("preEA_Popn" %in% names(base)) "preEA_Popn" else NULL

  # JOINTURE PAR IDENTIFIANT NUMÉRIQUE (robuste aux écarts d'orthographe) :
  # le zd_id de la base (11 chiffres) est hiérarchique ; ses 5 premiers chiffres
  # correspondent exactement au zone_sante_id de l'enquête (vérifié : 42/42 zones).
  #   zd_id = [zone_sante_id (5)] [ordre ZD (6)]
  # On détermine le milieu prédominant de chaque zone de santé (pondéré par la
  # population recensée si disponible) puis on l'affecte à chaque enfant via son
  # zone_sante_id.
  milieu_zone <- base %>%
    mutate(.zsid = suppressWarnings(as.integer(substr(
      as.character(format(zd_id, scientific = FALSE, trim = TRUE)), 1, 5)))) %>%
    filter(!is.na(.zsid)) %>%
    group_by(.zsid) %>%
    summarise(
      pop_urb = if (!is.null(pop_col))
        sum(.data[[pop_col]][milres_name == "Urbain"], na.rm = TRUE)
        else sum(milres_name == "Urbain"),
      pop_rur = if (!is.null(pop_col))
        sum(.data[[pop_col]][milres_name == "Rural"], na.rm = TRUE)
        else sum(milres_name == "Rural"),
      .groups = "drop"
    ) %>%
    mutate(milieu = if_else(pop_urb >= pop_rur, "Urbain", "Rural")) %>%
    select(.zsid, milieu)

  if (!"zone_sante_id" %in% names(df)) {
    cli_alert_warning("zone_sante_id absent de l'enquête : milieu non dérivé.")
    df$milieu <- factor(NA, levels = c("Urbain", "Rural"))
    return(df)
  }
  res <- df %>%
    mutate(.zsid = suppressWarnings(as.integer(as_num(zone_sante_id)))) %>%
    left_join(milieu_zone, by = ".zsid") %>%
    mutate(milieu = factor(milieu, levels = c("Urbain", "Rural"))) %>%
    select(-.zsid)

  n_na <- sum(is.na(res$milieu))
  if (n_na > 0)
    cli_alert_warning("Milieu non apparié pour {n_na} enfant(s) (zone_sante_id absent de la base de sondage).")
  else
    cli_alert_success("Milieu de résidence dérivé pour 100 % des enfants (jointure par zone_sante_id).")
  res
}

enfant_main <- .enrichir_milieu(enfant_main)
n_milieu_ok <- sum(!is.na(enfant_main$milieu))
cli_alert_info("Milieu de résidence dérivé pour {n_milieu_ok}/{nrow(enfant_main)} enfants.")

# -----------------------------------------------------------------------------
# 3c. ENRICHISSEMENT : CARACTERISTIQUES DU CHEF DE MENAGE
# -----------------------------------------------------------------------------
# On rattache à chaque enfant les caractéristiques du chef de ménage
# (sexe, âge, taille du ménage) via la jointure hhid (enfant) = caseid (ménage).
.enrichir_chef_menage <- function(df, menage) {
  if (is.null(menage) || !"hhid" %in% names(df)) {
    cli_alert_warning("Données ménage indisponibles : caractéristiques chef non dérivées.")
    return(df)
  }
  mn <- menage %>%
    transmute(
      .hhkey   = as.character(caseid),
      hhh_sex_n  = as_num(hhh_sex),
      hhh_age_n  = as_num(hhh_age),
      hhsize_n   = as_num(hhsize)
    ) %>%
    filter(!is.na(.hhkey)) %>%
    distinct(.hhkey, .keep_all = TRUE)

  df %>%
    mutate(.hhkey = as.character(hhid)) %>%
    left_join(mn, by = ".hhkey") %>%
    mutate(
      chef_sexe = factor(case_when(
        hhh_sex_n == 1 ~ "Femme",
        hhh_sex_n == 2 ~ "Homme",
        TRUE ~ NA_character_), levels = c("Homme", "Femme")),
      chef_age_grp = factor(case_when(
        hhh_age_n < 25            ~ "< 25 ans",
        hhh_age_n >= 25 & hhh_age_n < 35 ~ "25-34 ans",
        hhh_age_n >= 35 & hhh_age_n < 45 ~ "35-44 ans",
        hhh_age_n >= 45 & hhh_age_n < 55 ~ "45-54 ans",
        hhh_age_n >= 55           ~ "55 ans et +",
        TRUE ~ NA_character_),
        levels = c("< 25 ans", "25-34 ans", "35-44 ans", "45-54 ans", "55 ans et +")),
      taille_menage_grp = factor(case_when(
        hhsize_n <= 3            ~ "1-3 personnes",
        hhsize_n >= 4 & hhsize_n <= 6 ~ "4-6 personnes",
        hhsize_n >= 7 & hhsize_n <= 9 ~ "7-9 personnes",
        hhsize_n >= 10           ~ "10 personnes et +",
        TRUE ~ NA_character_),
        levels = c("1-3 personnes", "4-6 personnes", "7-9 personnes",
                   "10 personnes et +"))
    ) %>%
    select(-.hhkey)
}

if (exists("menage_main") && !is.null(menage_main)) {
  enfant_main <- .enrichir_chef_menage(enfant_main, menage_main)
  n_chef_ok <- sum(!is.na(enfant_main$chef_sexe))
  cli_alert_info("Caractéristiques du chef de ménage dérivées pour {n_chef_ok}/{nrow(enfant_main)} enfants.")
}


# -----------------------------------------------------------------------------
# 4. PREPARATION DES DONNEES
# -----------------------------------------------------------------------------
# IMPORTANT : ce script suppose que les .dta ont été produits par
# script_treatment.R, qui appelle .normalize_codes() pour aligner les codes
# sur le XLSForm de référence :
#   - oui/non/dk   : 0 = Non, 1 = Oui, -99 = Ne sait pas
#   - vaccin_anterieur : 0 = Non jamais, 1 = Oui ≥1 dose, -99 = NSP
#   - nombre_doses     : 1, 2, 3, -99 = NSP
#   - sex              : 1 = Féminin, 2 = Masculin
#   - finger_marked    : 0=Non, 1=marque vue, 2=marque non vue, 3=enfant absent, -99=NSP
#   - lieu_vaccination : 1=École, 2=FOSA, 3=Domicile, 4=Marché, 5=Eglise,
#                        -99=NSP, -96=Autre
#
# Travailler sur les CODES (numériques) est plus rapide et indépendant de la
# langue des labels. En cas de doute sur le codage source, lancer d'abord
# script_treatment.R qui normalise les jeux.
# -----------------------------------------------------------------------------

cli_h1("Préparation des données")

# Helper : convertit une variable haven_labelled en character via ses labels
# (utilisé pour les variables textuelles : province_name, raison_pas_vaccine)
.lbl_to_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.factor(x)) return(as.character(x))
  if (inherits(x, c("haven_labelled", "haven_labelled_spss"))) {
    return(as.character(haven::as_factor(x)))
  }
  as.character(x)
}

prepare_enfant <- function(df) {
  # Robustesse : créer en NA les colonnes attendues mais absentes
  # (ex. le backcheck ne contient pas toutes les variables de l'enquête principale)
  cols_attendues <- c(
    "age_months", "child_eligible", "sex", "vaccine_campagne", "consent",
    "vaccin_anterieur", "nombre_doses", "recu_carte", "vu_carte",
    "finger_marked", "vitamine_a", "depense_vaccination", "lieu_vaccination",
    "interviewed", "part_status", "raison_pas_vaccine",
    "caregiver_sex", "caregiver_age", "caregiver_nivins", "caregiver_maritalStatus",
    "importance_vaccins", "facilite_paiement", "info_campagne",
    "connait_lieu_vaccination", "measles_symptom", "canal_info", "delai_info",
    "zone_sante_name", "province_name",
    "parents_amis_vaccin", "vaccins_souhaites"
  )
  for (col in cols_attendues) if (!col %in% names(df)) df[[col]] <- NA

  df %>%
    mutate(
      # Conversions numériques (codes XLSForm)
      age_months_num   = as_num(age_months),
      child_eligible_n = as_num(child_eligible),
      sex_n            = as_num(sex),
      vaccine_camp_n   = as_num(vaccine_campagne),
      consent_n        = as_num(consent),
      vaccin_ant_n     = as_num(vaccin_anterieur),
      nombre_doses_n   = as_num(nombre_doses),
      recu_carte_n     = as_num(recu_carte),
      vu_carte_n       = as_num(vu_carte),
      finger_marked_n  = as_num(finger_marked),
      vitamine_a_n     = as_num(vitamine_a),
      depense_vacc_n   = as_num(depense_vaccination),
      lieu_vacc_n      = as_num(lieu_vaccination),
      interviewed_n    = as_num(interviewed),
      part_status_n    = as_num(part_status),

      # ── Sexe (1=F, 2=M) ──────────────────────────────────────────────────
      sexe = factor(
        case_when(
          sex_n == 1 ~ "Féminin",
          sex_n == 2 ~ "Masculin",
          TRUE       ~ NA_character_
        ),
        levels = c("Masculin", "Féminin")
      ),

      # ── Tranches d'âge ──────────────────────────────────────────────────
      # Tranches en ANNÉES (présentation principale)
      tranche_age = factor(
        case_when(
          age_months_num >= 6   & age_months_num < 12  ~ "6-11 mois",
          age_months_num >= 12  & age_months_num < 60  ~ "1-4 ans",
          age_months_num >= 60  & age_months_num < 120 ~ "5-9 ans",
          age_months_num >= 120 & age_months_num < 180 ~ "10-14 ans",
          TRUE                                          ~ NA_character_
        ),
        levels = c("6-11 mois", "1-4 ans", "5-9 ans", "10-14 ans")
      ),
      # Conservée en mois pour le statut vaccinal antérieur (6-59 mois)
      tranche_age_mois = factor(
        case_when(
          age_months_num >= 6   & age_months_num < 60  ~ "6-59 mois",
          age_months_num >= 60  & age_months_num < 120 ~ "60-119 mois",
          age_months_num >= 120 & age_months_num < 180 ~ "120-179 mois",
          TRUE                                          ~ NA_character_
        ),
        levels = c("6-59 mois", "60-119 mois", "120-179 mois")
      ),
      tranche_age_detail = factor(
        case_when(
          age_months_num >= 6   & age_months_num < 12  ~ "6-11 mois",
          age_months_num >= 12  & age_months_num < 24  ~ "12-23 mois",
          age_months_num >= 24  & age_months_num < 60  ~ "24-59 mois",
          age_months_num >= 60  & age_months_num < 120 ~ "5-9 ans",
          age_months_num >= 120 & age_months_num < 180 ~ "10-14 ans",
          TRUE ~ NA_character_
        ),
        levels = c("6-11 mois", "12-23 mois", "24-59 mois", "5-9 ans", "10-14 ans")
      ),

      # ── Couverture vaccinale ────────────────────────────────────────────
      # vaccine_bin : NSP exclu du dénominateur
      vaccine_bin = case_when(
        vaccine_camp_n == 1   ~ 1L,   # Oui
        vaccine_camp_n == 0   ~ 0L,   # Non
        TRUE                  ~ NA_integer_
      ),
      # Variante : NSP traité comme "non vacciné"
      vaccine_bin_inc_nsp = case_when(
        vaccine_camp_n == 1                ~ 1L,
        vaccine_camp_n %in% c(0, -99)      ~ 0L,
        TRUE                                ~ NA_integer_
      ),
      # ── Couverture par PREUVE PHYSIQUE (carte vue) ──────────────────────────
      # Indicateur objectif : vacciné ET carte effectivement vue par l'enquêteur.
      # Le dénominateur reste l'ensemble des enfants éligibles (même base que
      # vaccine_bin), afin que la confrontation carte-seule vs déclaratif mesure
      # l'amplitude du biais de désirabilité sociale.
      vaccine_bin_carte = case_when(
        vaccine_camp_n == 1 & vu_carte_n == 1 ~ 1L,  # vacciné + carte vue
        vaccine_camp_n == 1 & (is.na(vu_carte_n) | vu_carte_n != 1) ~ 0L, # déclaré sans preuve
        vaccine_camp_n == 0 ~ 0L,                    # non vacciné
        TRUE ~ NA_integer_
      ),

      # ── Statut vaccinal antérieur ───────────────────────────────────────
      statut_vaccinal_ant = factor(
        case_when(
          vaccin_ant_n == 0                                ~ "Zéro dose",
          vaccin_ant_n == 1 & nombre_doses_n == 1          ~ "Une dose",
          vaccin_ant_n == 1 & nombre_doses_n %in% c(2, 3)  ~ "Deux doses ou plus",
          vaccin_ant_n == 1 & nombre_doses_n == -99        ~ "Une dose ou plus (n.p.)",
          vaccin_ant_n == 1 & is.na(nombre_doses_n)        ~ "Une dose ou plus (n.p.)",
          vaccin_ant_n == -99                              ~ "Ne sait pas",
          TRUE                                              ~ NA_character_
        ),
        levels = c("Zéro dose", "Une dose", "Deux doses ou plus",
                   "Une dose ou plus (n.p.)", "Ne sait pas")
      ),
      # Variante fusionnée : "Une dose ou plus (n.p.)" regroupée avec "Une dose"
      statut_vaccinal_ant_fus = factor(
        case_when(
          vaccin_ant_n == 0                                ~ "Zéro dose",
          vaccin_ant_n == 1 & nombre_doses_n == 1          ~ "Une dose",
          vaccin_ant_n == 1 & nombre_doses_n == -99        ~ "Une dose",
          vaccin_ant_n == 1 & is.na(nombre_doses_n)        ~ "Une dose",
          vaccin_ant_n == 1 & nombre_doses_n %in% c(2, 3)  ~ "Deux doses ou plus",
          vaccin_ant_n == -99                              ~ "Ne sait pas",
          TRUE                                              ~ NA_character_
        ),
        levels = c("Zéro dose", "Une dose", "Deux doses ou plus", "Ne sait pas")
      ),

      # ── Confirmation par carte ──────────────────────────────────────────
      confirmation_carte = factor(
        case_when(
          vu_carte_n == 1                                       ~ "Carte vue",
          recu_carte_n == 1 & (is.na(vu_carte_n) | vu_carte_n == 0) ~ "Carte reçue mais non vue",
          recu_carte_n == 0                                     ~ "Pas de carte reçue",
          recu_carte_n == -99                                   ~ "NSP",
          TRUE                                                   ~ NA_character_
        ),
        levels = c("Carte vue", "Carte reçue mais non vue",
                   "Pas de carte reçue", "NSP")
      ),

      # ── Lieu de vaccination ─────────────────────────────────────────────
      lieu_vaccination_lbl = factor(
        case_when(
          lieu_vacc_n == 1   ~ "École",
          lieu_vacc_n == 2   ~ "Formation sanitaire",
          lieu_vacc_n == 3   ~ "Domicile",
          lieu_vacc_n == 4   ~ "Marché/Communauté",
          lieu_vacc_n == 5   ~ "Église",
          lieu_vacc_n == -96 ~ "Autre",
          lieu_vacc_n == -99 ~ "Ne sait pas",
          TRUE                ~ NA_character_
        ),
        levels = c("Formation sanitaire", "École", "Domicile",
                   "Marché/Communauté", "Église", "Autre", "Ne sait pas")
      ),

      # ── Raison de non-vaccination (texte du formulaire) ─────────────────
      # On garde le label tel quel (multi-codes propres au XLSForm)
      raison_pas_n   = as_num(raison_pas_vaccine),
      raison_pas_lbl = {
        raison_chr <- .lbl_to_chr(raison_pas_vaccine)
        if_else(is.na(raison_chr) | raison_chr == "", NA_character_, raison_chr)
      },

      # ── Taxonomie OMS des raisons de non-vaccination ────────────────────
      raison_groupe = factor(
        case_when(
          raison_pas_n %in% c(1, 5, 11)            ~ "Manque d'information",
          raison_pas_n %in% c(4, 6, 7, 16, 17)     ~ "Barrières d'accès",
          raison_pas_n %in% c(3, 8, 10, 12, 13, 15)~ "Contraintes pratiques",
          raison_pas_n %in% c(9, 14, 18, 19, 21)   ~ "Refus / Hésitation",
          raison_pas_n == 20                       ~ "Refus d'autorisation",
          raison_pas_n == 2                        ~ "Déjà vacciné",
          raison_pas_n %in% c(22, -96)             ~ "Autre",
          TRUE                                      ~ NA_character_
        ),
        levels = c("Manque d'information", "Barrières d'accès",
                   "Contraintes pratiques", "Refus / Hésitation",
                   "Déjà vacciné", "Refus d'autorisation", "Autre")
      ),

      # ── Groupes d'âge épidémiologiques (régression/sous-groupes) ────────
      age_annees = age_months_num / 12,
      age_groupe = cut(age_annees,
        breaks = c(-Inf, 1, 5, 10, 15),
        labels = c("< 1 an", "1-4 ans", "5-9 ans", "10-14 ans"),
        right = FALSE),

      # ── Variables explicatives (facteurs étiquetés pour modèles/tableaux) ─
      caregiver_sex_f = factor(
        case_when(as_num(caregiver_sex) == 1 ~ "Féminin",
                  as_num(caregiver_sex) == 2 ~ "Masculin",
                  TRUE ~ NA_character_),
        levels = c("Féminin", "Masculin")),
      caregiver_age_num = as_num(caregiver_age),
      niveau_instruction = factor(
        case_when(
          as_num(caregiver_nivins) == 0 ~ "Sans niveau",
          as_num(caregiver_nivins) == 1 ~ "Primaire",
          as_num(caregiver_nivins) == 2 ~ "Secondaire",
          as_num(caregiver_nivins) == 3 ~ "Supérieur/universitaire",
          TRUE ~ NA_character_),
        levels = c("Sans niveau", "Primaire", "Secondaire",
                   "Supérieur/universitaire")),
      situation_matrimoniale = factor(
        case_when(
          as_num(caregiver_maritalStatus) == 1 ~ "Célibataire",
          as_num(caregiver_maritalStatus) == 2 ~ "En union",
          as_num(caregiver_maritalStatus) == 3 ~ "Marié(e) monogame",
          as_num(caregiver_maritalStatus) == 4 ~ "Marié(e) polygame",
          as_num(caregiver_maritalStatus) == 5 ~ "Divorcé(e)/Séparé(e)",
          as_num(caregiver_maritalStatus) == 6 ~ "Veuf(ve)",
          TRUE ~ NA_character_)),
      importance_vaccins_f = factor(
        case_when(
          as_num(importance_vaccins) == 1 ~ "Pas du tout important",
          as_num(importance_vaccins) == 2 ~ "Quelque peu important",
          as_num(importance_vaccins) == 3 ~ "Moyennement important",
          as_num(importance_vaccins) == 4 ~ "Très important",
          TRUE ~ NA_character_),
        levels = c("Pas du tout important", "Quelque peu important",
                   "Moyennement important", "Très important"),
        ordered = TRUE),
      importance_vaccins_num = as_num(importance_vaccins),  # 1-4 pour modèle
      # Pression sociale : parents/amis souhaitent la vaccination
      parents_amis_vaccin_f = factor(
        case_when(
          as_num(parents_amis_vaccin) == 1 ~ "Oui",
          as_num(parents_amis_vaccin) == 0 ~ "Non",
          TRUE ~ NA_character_),
        levels = c("Non", "Oui")),
      # Vaccins souhaités par le tuteur pour l'enfant
      vaccins_souhaites_f = factor(
        case_when(
          as_num(vaccins_souhaites) == 0 ~ "Aucun vaccin",
          as_num(vaccins_souhaites) == 1 ~ "Certains vaccins",
          as_num(vaccins_souhaites) == 3 ~ "Tous les vaccins",
          TRUE ~ NA_character_),
        levels = c("Aucun vaccin", "Certains vaccins", "Tous les vaccins")),
      facilite_paiement_f = factor(
        case_when(
          as_num(facilite_paiement) == 1 ~ "Pas du tout facile",
          as_num(facilite_paiement) == 2 ~ "Assez peu facile",
          as_num(facilite_paiement) == 3 ~ "Moyennement facile",
          as_num(facilite_paiement) == 4 ~ "Très facile",
          TRUE ~ NA_character_),
        levels = c("Pas du tout facile", "Assez peu facile",
                   "Moyennement facile", "Très facile"),
        ordered = TRUE),
      facilite_paiement_num = as_num(facilite_paiement),
      info_campagne_f = factor(
        case_when(as_num(info_campagne) == 1 ~ "Oui",
                  as_num(info_campagne) == 0 ~ "Non",
                  TRUE ~ NA_character_),
        levels = c("Non", "Oui")),
      connait_lieu_f = factor(
        case_when(as_num(connait_lieu_vaccination) == 1 ~ "Oui",
                  as_num(connait_lieu_vaccination) == 0 ~ "Non",
                  TRUE ~ NA_character_),
        levels = c("Non", "Oui")),
      depense_vacc_bin = case_when(
        as_num(depense_vaccination) == 1 ~ 1L,
        as_num(depense_vaccination) == 0 ~ 0L,
        TRUE ~ NA_integer_),
      measles_symptom_f = factor(
        case_when(as_num(measles_symptom) == 1 ~ "Oui",
                  as_num(measles_symptom) == 0 ~ "Non",
                  as_num(measles_symptom) == -99 ~ "Ne sait pas",
                  TRUE ~ NA_character_),
        levels = c("Non", "Oui", "Ne sait pas")),
      canal_info_lbl = .lbl_to_chr(canal_info),
      delai_info_lbl = .lbl_to_chr(delai_info),
      zone_sante_clean = as.character(.lbl_to_chr(zone_sante_name)),

      # Province (texte)
      province_clean = as.character(province_name),
      province_clean = ifelse(is.na(province_clean) | province_clean == "",
                              NA_character_, province_clean)
    )
}

enfant_main      <- prepare_enfant(enfant_main)
if (!is.null(enfant_backcheck)) enfant_backcheck <- prepare_enfant(enfant_backcheck)

cli_alert_info("Echantillon enfant principal : {nrow(enfant_main)} obs")

# Filtre standard : enfants éligibles et effectivement enquêtés
enfant_anal <- enfant_main %>%
  filter(
    !is.na(interviewed_n) & interviewed_n == 1,
    !is.na(child_eligible_n) & child_eligible_n == 1
  )
cli_alert_info("Echantillon analytique (interviewés et éligibles) : {nrow(enfant_anal)} obs")

# Préparation menage_main / denombrement
# Codes après .normalize_codes() :
#   consent, hh_eligible, enumerated_hh, occupation, hhh_sex (1/2 inchangé)
#   - oui/non    : 0 = Non,  1 = Oui
if (!is.null(menage_main)) {
  menage_main <- menage_main %>%
    mutate(
      consent_n      = as_num(consent),
      hh_eligible_n  = as_num(hh_eligible),
      count_elig_n   = as_num(count_eligibles),
      interviewed_n  = as_num(interviewed),
      part_status_n  = as_num(part_status),
      hhh_sex_n      = as_num(hhh_sex)
    )
}
if (!is.null(denombrement)) {
  denombrement <- denombrement %>%
    mutate(
      occupation_n        = as_num(occupation),
      enumerated_hh_n     = as_num(enumerated_hh),
      hh_eligible_n       = as_num(hh_eligible),
      total_hh_n          = as_num(total_hh),
      total_hh_elig_n     = as_num(total_hh_eligible),
      total_pop_n         = as_num(total_pop),
      total_pop_elig_n    = as_num(total_pop_eligible)
    )
}



# -----------------------------------------------------------------------------
# 5. PLAN DE SONDAGE ET POIDS
# -----------------------------------------------------------------------------
# Stratégie :
#  - Si une variable `poids_final` ou `poids_sondage` existe dans enfant_main,
#    on l'utilise.
#  - Sinon on calcule un poids de non-réponse simple = (nb éligibles dénombrés)
#    / (nb enfants enquêtés) par aire de santé.
#  - À défaut, on bascule en estimation non pondérée (proportions brutes).
# -----------------------------------------------------------------------------

cli_h1("Calcul / récupération des poids de sondage")

if (!"poids_final" %in% names(enfant_anal)) {
  if ("poids_sondage" %in% names(enfant_anal)) {
    enfant_anal <- enfant_anal %>% mutate(poids_final = as_num(poids_sondage))
    cli_alert_info("Poids issus de la variable {.field poids_sondage}")
  } else if (!is.null(denombrement)) {
    # Poids de non-réponse simplifié par aire de santé
    elig_par_aire <- denombrement %>%
      filter(!is.na(hh_eligible_n) & hh_eligible_n == 1) %>%
      group_by(aire_sante_id) %>%
      summarise(n_elig_denom = n(), .groups = "drop")
    enq_par_aire <- enfant_anal %>%
      group_by(aire_sante_id) %>%
      summarise(n_enq = n(), .groups = "drop")
    poids_aire <- elig_par_aire %>%
      left_join(enq_par_aire, by = "aire_sante_id") %>%
      mutate(
        poids_final = if_else(!is.na(n_enq) & n_enq > 0,
                              n_elig_denom / n_enq, 1)
      )
    enfant_anal <- enfant_anal %>%
      left_join(poids_aire %>% select(aire_sante_id, poids_final),
                by = "aire_sante_id") %>%
      mutate(poids_final = if_else(is.na(poids_final), 1, poids_final))
    cli_alert_info("Poids ajustés calculés via le dénombrement.")
  } else {
    enfant_anal <- enfant_anal %>% mutate(poids_final = 1)
    cli_alert_warning("Aucun poids disponible : estimations non pondérées (poids = 1).")
  }
}

# Création du design global (utilisé par VCQI QUAL-04 et svychisq)
# Recréé ici une fois dans l'environnement global pour éviter de le recalculer
# à chaque appel de estim_prop().
svy_design <- tryCatch({
  args <- list(ids = ~aire_sante_id, weights = ~poids_final,
               data = enfant_anal %>% filter(!is.na(vaccine_bin)), nest = TRUE)
  if ("bloc" %in% names(enfant_anal) && dplyr::n_distinct(enfant_anal$bloc, na.rm = TRUE) > 1)
    args$strata <- ~bloc
  do.call(survey::svydesign, args)
}, error = function(e) {
  cli_alert_warning("Design global non créé : {e$message}")
  NULL
})

# -----------------------------------------------------------------------------
# Helper d'estimation pondérée + IC95 (avec repli non pondéré si pas de design)
# -----------------------------------------------------------------------------
# Arguments :
#   df      : data.frame (utilisé pour le repli non pondéré et pour les effectifs)
#   var_bin : nom (chaîne) de la variable binaire 0/1 (NA admis)
#   by      : nom (chaîne) de la variable de stratification (ou NULL)
# Retourne : tibble (groupe, n, prop, ic_bas, ic_haut)
#
# MÉTHODE D'INTERVALLE DE CONFIANCE :
#   On utilise svyciprop(method = "logit") du package survey (Lumley, 2010).
#   Contrairement à l'approximation de Wald (svymean + confint), la transformation
#   logit contraint MATHÉMATIQUEMENT l'intervalle à rester dans [0 ; 1], ce qui
#   élimine les bornes aberrantes (> 100 %) lorsque la proportion p est proche de 1.
#   En l'absence de plan de sondage, on utilise l'IC de Wilson (prop.test), lui
#   aussi borné dans [0 ; 1].
# -----------------------------------------------------------------------------
estim_prop <- function(df, var_bin, by = NULL) {
  if (!var_bin %in% names(df)) {
    cli_alert_warning("Variable {.field {var_bin}} absente.")
    return(NULL)
  }
  # Effectifs de référence
  if (is.null(by)) {
    df2 <- df %>% filter(!is.na(.data[[var_bin]]))
  } else {
    df2 <- df %>% filter(!is.na(.data[[var_bin]]), !is.na(.data[[by]]))
  }
  if (nrow(df2) == 0) return(NULL)

  # Branche pondérée : on (re)crée un design sur df2 directement
  poids_ok <- "poids_final" %in% names(df2) &&
              any(!is.na(df2$poids_final) & df2$poids_final > 0) &&
              dplyr::n_distinct(df2$aire_sante_id) > 1
  if (poids_ok) {
    return(tryCatch({
      args <- list(ids = ~aire_sante_id, weights = ~poids_final,
                   data = df2, nest = TRUE)
      if ("bloc" %in% names(df2) && dplyr::n_distinct(df2$bloc) > 1)
        args$strata <- ~bloc
      des <- do.call(survey::svydesign, args)
      f_bin <- as.formula(paste0("~", var_bin))

      if (is.null(by)) {
        # IC logit borné dans [0 ; 1] (et non Wald qui peut dépasser 1)
        ci_obj <- survey::svyciprop(f_bin, des, method = "logit", level = 0.95)
        tibble(groupe = "Total", n = nrow(df2),
               prop    = as.numeric(ci_obj),
               ic_bas  = as.numeric(attr(ci_obj, "ci")[1]),
               ic_haut = as.numeric(attr(ci_obj, "ci")[2]))
      } else {
        # Estimation logit par sous-groupe (boucle sur chaque modalité)
        niveaux <- df2 %>% filter(!is.na(.data[[by]])) %>%
          pull(.data[[by]]) %>% as.character() %>% unique()
        res <- purrr::map_dfr(niveaux, function(lv) {
          sub <- tryCatch(
            subset(des, as.character(des$variables[[by]]) == lv),
            error = function(e) NULL)
          if (is.null(sub) || nrow(sub$variables) == 0) return(NULL)
          ci_obj <- tryCatch(
            survey::svyciprop(f_bin, sub, method = "logit", level = 0.95),
            error = function(e) NULL)
          n_lv <- sum(df2[[by]] == lv & !is.na(df2[[var_bin]]), na.rm = TRUE)
          if (is.null(ci_obj)) {
            # repli Wilson si svyciprop échoue (ex. groupe trop petit)
            x <- sum(df2[[var_bin]][df2[[by]] == lv], na.rm = TRUE)
            ic <- stats::prop.test(x, n_lv, conf.level = 0.95,
                                   correct = FALSE)$conf.int
            tibble(groupe = lv, n = n_lv, prop = x / n_lv,
                   ic_bas = ic[1], ic_haut = ic[2])
          } else {
            tibble(groupe = lv, n = n_lv,
                   prop    = as.numeric(ci_obj),
                   ic_bas  = as.numeric(attr(ci_obj, "ci")[1]),
                   ic_haut = as.numeric(attr(ci_obj, "ci")[2]))
          }
        })
        res
      }
    }, error = function(e) {
      cli_alert_warning("Estimation pondérée KO ({e$message}) - repli non pondéré.")
      .estim_prop_unweighted(df2, var_bin, by)
    }))
  }
  .estim_prop_unweighted(df2, var_bin, by)
}

# Fonction interne : estimation non pondérée + IC Wilson (borné dans [0 ; 1])
.estim_prop_unweighted <- function(df2, var_bin, by) {
  # Wilson via binom.test serait exact ; prop.test donne l'IC de Wilson (score)
  wilson_ci <- function(x, n) {
    if (n == 0) return(c(NA_real_, NA_real_))
    as.numeric(stats::prop.test(x, n, conf.level = 0.95,
                                correct = FALSE)$conf.int)
  }
  if (is.null(by)) {
    x <- sum(df2[[var_bin]], na.rm = TRUE)
    n <- sum(!is.na(df2[[var_bin]]))
    ic <- wilson_ci(x, n)
    tibble(groupe = "Total", n = n, prop = if (n > 0) x / n else NA_real_,
           ic_bas = ic[1], ic_haut = ic[2])
  } else {
    df2 %>%
      group_by(.g = .data[[by]]) %>%
      summarise(x = sum(.data[[var_bin]], na.rm = TRUE),
                n = sum(!is.na(.data[[var_bin]])),
                .groups = "drop") %>%
      rowwise() %>%
      mutate(
        .ic     = list(wilson_ci(x, n)),
        prop    = if (n > 0) x / n else NA_real_,
        ic_bas  = .ic[[1]],
        ic_haut = .ic[[2]]
      ) %>%
      ungroup() %>%
      transmute(groupe = as.character(.g), n, prop, ic_bas, ic_haut)
  }
}


# -----------------------------------------------------------------------------
# 6. STATISTIQUES DESCRIPTIVES SUR LE DENOMBREMENT
# -----------------------------------------------------------------------------

cli_h1("Statistiques descriptives - Dénombrement")

if (!is.null(denombrement)) {
  resume_denom <- denombrement %>%
    filter(!is.na(occupation_n) & occupation_n == 1,
           !is.na(enumerated_hh_n) & enumerated_hh_n == 1) %>%
    group_by(province_name) %>%
    summarise(
      `Ménages dénombrés` = n(),
      `Ménages éligibles` = sum(!is.na(hh_eligible_n) & hh_eligible_n == 1, na.rm = TRUE),
      `Taux d'éligibilité (%)` = round(
        100 * sum(!is.na(hh_eligible_n) & hh_eligible_n == 1, na.rm = TRUE) / n(), 1
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(`Ménages dénombrés`))

  # Ligne totale
  resume_denom_tot <- bind_rows(
    resume_denom,
    tibble(
      province_name = "TOTAL",
      `Ménages dénombrés` = sum(resume_denom$`Ménages dénombrés`),
      `Ménages éligibles` = sum(resume_denom$`Ménages éligibles`),
      `Taux d'éligibilité (%)` = round(
        100 * sum(resume_denom$`Ménages éligibles`) /
              sum(resume_denom$`Ménages dénombrés`), 1
      )
    )
  ) %>% rename(Province = province_name)

  export_table(
    gt_styled(resume_denom_tot,
              title = "Résumé du dénombrement par province",
              subtitle = "Ménages dénombrés, éligibles et taux d'éligibilité"),
    file.path(PATH_TABLEAUX, "denombrement_par_province"),
    raw_data = resume_denom_tot
  )

  # Graphique en barres horizontales
  p_denom <- resume_denom %>%
    mutate(
      etiq = comma(`Ménages dénombrés`, big.mark = " ")
    ) %>%
    ggplot(aes(x = `Ménages dénombrés`,
               y = reorder(province_name, `Ménages dénombrés`))) +
    geom_col(fill = oms_colors[["bleu_who"]], width = 0.7, alpha = 0.9) +
    geom_text(aes(label = etiq),
              hjust = -0.15, size = 3.6, color = oms_colors[["gris_fonce"]]) +
    scale_x_continuous(labels = label_comma(big.mark = " "),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Ménages dénombrés par province",
      subtitle = "Phase de dénombrement de l'ECP RR",
      x = "Nombre de ménages dénombrés", y = NULL,
      caption  = .caption_default()
    )
  save_plot(p_denom, file.path(PATH_GRAPHIQUES, "01_denombrement_par_province.png"),
            width = 11, height = 6.5)
}


# -----------------------------------------------------------------------------
# 7. COUVERTURE VACCINALE - INDICATEUR GLOBAL
# -----------------------------------------------------------------------------

cli_h1("Couverture vaccinale globale")

cv_global_df <- estim_prop(enfant_anal, "vaccine_bin")
if (!is.null(cv_global_df)) {
  cv_global_pct <- 100 * cv_global_df$prop
  cv_global_ic  <- 100 * c(cv_global_df$ic_bas, cv_global_df$ic_haut)
  n_global      <- cv_global_df$n

  # Tableau récapitulatif
  tbl_cv_global <- tibble(
    Indicateur = "Couverture vaccinale Rougeole-Rubéole",
    `Estimation (%)` = round(cv_global_pct, 1),
    `IC 95% (%)` = sprintf("[%.1f - %.1f]", cv_global_ic[1], cv_global_ic[2]),
    `Enfants enquêtés` = n_global
  )
  export_table(
    gt_styled(tbl_cv_global,
              title = "Couverture vaccinale nationale post-campagne",
              subtitle = "Estimation pondérée avec intervalle de confiance à 95%"),
    file.path(PATH_TABLEAUX, "cv_global"),
    raw_data = tbl_cv_global
  )

  # Visualisation : barre horizontale unique avec IC, plus lisible qu'un gauge
  p_cv_global <- tibble(
    label    = "Couverture\nnationale",
    valeur   = cv_global_pct,
    ic_bas   = cv_global_ic[1],
    ic_haut  = cv_global_ic[2]
  ) %>%
    ggplot(aes(x = label, y = valeur)) +
    geom_col(fill = oms_colors[["bleu_who"]], width = 0.4, alpha = 0.9) +
    geom_errorbar(aes(ymin = pmax(0, ic_bas), ymax = pmin(100, ic_haut)),
                  width = 0.12, color = oms_colors[["gris_fonce"]], linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", valeur)),
              nudge_y = 4, size = 7, fontface = "bold",
              color = oms_colors[["bleu_fonce"]]) +
    geom_text(aes(label = sprintf("IC 95%% : [%.1f - %.1f] | n = %s",
                                  ic_bas, ic_haut, comma(n_global))),
              y = 2, size = 3.6, color = oms_colors[["gris"]], vjust = 0) +
    geom_hline(yintercept = 95, linetype = "dashed",
               color = oms_colors[["vert"]], linewidth = 0.5) +
    annotate("text", x = 0.55, y = 96.5, label = "Cible OMS ≥ 95%",
             color = oms_colors[["vert"]], hjust = 0,
             fontface = "italic", size = 3.5) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = "Couverture vaccinale Rougeole-Rubéole - Niveau national",
      subtitle = "Estimation pondérée, intervalle de confiance à 95%",
      x = NULL, y = "Couverture vaccinale (%)",
      caption  = .caption_default()
    ) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", size = rel(1.0)))
  save_plot(p_cv_global, file.path(PATH_GRAPHIQUES, "02_cv_globale.png"),
            width = 7.5, height = 6)
}


# -----------------------------------------------------------------------------
# 8. COUVERTURE VACCINALE - PAR PROVINCE
# -----------------------------------------------------------------------------

cli_h1("Couverture vaccinale par province")

cv_prov_df <- estim_prop(enfant_anal, "vaccine_bin", by = "province_clean") %>%
  rename(Province = groupe) %>%
  mutate(
    pct      = round(100 * prop, 1),
    pct_bas  = round(100 * ic_bas, 1),
    pct_haut = round(100 * ic_haut, 1),
    categorie = case_when(
      pct >= 95 ~ "≥ 95% (cible OMS atteinte)",
      pct >= 80 ~ "80-94% (proche cible)",
      pct >= 70 ~ "70-79% (insuffisant)",
      TRUE      ~ "< 70% (critique)"
    ),
    categorie = factor(categorie,
      levels = c("≥ 95% (cible OMS atteinte)", "80-94% (proche cible)",
                 "70-79% (insuffisant)", "< 70% (critique)"))
  ) %>%
  arrange(desc(pct))

# Tableau
tbl_cv_prov <- cv_prov_df %>%
  transmute(
    Province,
    `n (enfants)` = n,
    `Couverture (%)` = pct,
    `IC 95%` = sprintf("[%.1f - %.1f]", pct_bas, pct_haut),
    Catégorie = as.character(categorie)
  )
export_table(
  gt_styled(tbl_cv_prov,
            title = "Couverture vaccinale par province",
            subtitle = "Estimations pondérées avec intervalles de confiance à 95%") %>%
    gt_color_cells("Couverture (%)", `Couverture (%)` >= 95,
                   fill = oms_colors[["vert"]]) %>%
    gt_color_cells("Couverture (%)", `Couverture (%)` < 70,
                   fill = oms_colors[["rouge"]]),
  file.path(PATH_TABLEAUX, "cv_par_province"),
  raw_data = tbl_cv_prov
)

# Couleurs alignées sur les catégories
couleurs_cat <- c(
  "≥ 95% (cible OMS atteinte)" = oms_colors[["vert"]],
  "80-94% (proche cible)"      = oms_colors[["bleu_who"]],
  "70-79% (insuffisant)"       = oms_colors[["orange"]],
  "< 70% (critique)"           = oms_colors[["rouge"]]
)

p_cv_prov <- cv_prov_df %>%
  ggplot(aes(x = pct, y = reorder(Province, pct), fill = categorie)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(xmin = pmax(0, pct_bas), xmax = pmin(100, pct_haut)),
                 orientation = "y",
                 width = 0.25, color = oms_colors[["gris_fonce"]],
                 linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.15, size = 3.4, color = oms_colors[["gris_fonce"]]) +
  geom_vline(xintercept = 95, linetype = "dashed",
             color = oms_colors[["vert"]], linewidth = 0.5) +
  scale_fill_manual(values = couleurs_cat, drop = FALSE, name = NULL) +
  scale_x_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Couverture vaccinale Rougeole-Rubéole par province",
    subtitle = "Estimations pondérées avec intervalles de confiance à 95%",
    x = "Couverture vaccinale (%)", y = NULL,
    caption  = .caption_default("Cible OMS ≥ 95% (ligne verte)")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(panel.grid.major.x = element_line(color = oms_colors[["gris_clair"]],
                                          linewidth = 0.25),
        panel.grid.major.y = element_blank())
save_plot(p_cv_prov, file.path(PATH_GRAPHIQUES, "03_cv_par_province.png"),
          width = 11, height = 6.5)


# -----------------------------------------------------------------------------
# 9. COUVERTURE PAR TRANCHE D'AGE, PAR SEXE
# -----------------------------------------------------------------------------

cli_h1("Couverture par sous-groupes (âge, sexe)")

# Par tranche d'âge (en années)
cv_age_df <- estim_prop(enfant_anal, "vaccine_bin", by = "tranche_age") %>%
  rename(`Tranche d'âge` = groupe) %>%
  mutate(`Tranche d'âge` = factor(`Tranche d'âge`,
    levels = c("6-11 mois", "1-4 ans", "5-9 ans", "10-14 ans")),
    pct = 100 * prop, ic_bas_pct = 100 * ic_bas, ic_haut_pct = 100 * ic_haut) %>%
  arrange(`Tranche d'âge`)

p_cv_age <- cv_age_df %>%
  ggplot(aes(x = `Tranche d'âge`, y = pct, fill = `Tranche d'âge`)) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, ic_bas_pct), ymax = pmin(100, ic_haut_pct)),
                width = 0.12, color = oms_colors[["gris_fonce"]],
                linewidth = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
            vjust = -0.6, size = 4.2, fontface = "bold",
            color = oms_colors[["bleu_fonce"]]) +
  geom_text(aes(label = paste0("n=", comma(n))),
            y = 2, size = 3, color = "white", fontface = "bold") +
  geom_hline(yintercept = 95, linetype = "dashed",
             color = oms_colors[["vert"]], linewidth = 0.5) +
  scale_fill_manual(values = c(oms_colors[["bleu_who"]],
                               oms_colors[["orange"]],
                               oms_colors[["violet"]],
                               oms_colors[["vert"]])) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Couverture vaccinale par tranche d'âge",
    subtitle = "Estimations pondérées avec IC 95%",
    x = NULL, y = "Couverture vaccinale (%)",
    caption  = .caption_default()
  ) +
  theme(legend.position = "none")
save_plot(p_cv_age, file.path(PATH_GRAPHIQUES, "04_cv_par_age.png"),
          width = 9, height = 6)

# Tableau âge
tbl_cv_age <- cv_age_df %>%
  transmute(`Tranche d'âge`, `n` = n,
            `Couverture (%)` = round(pct, 1),
            `IC 95%` = sprintf("[%.1f - %.1f]", ic_bas_pct, ic_haut_pct))
export_table(
  gt_styled(tbl_cv_age,
            title = "Couverture par tranche d'âge",
            subtitle = "Estimations pondérées avec IC 95%"),
  file.path(PATH_TABLEAUX, "cv_par_age"), raw_data = tbl_cv_age
)

# Par sexe
cv_sexe_df <- estim_prop(enfant_anal, "vaccine_bin", by = "sexe") %>%
  rename(Sexe = groupe) %>%
  mutate(pct = 100 * prop,
         ic_bas_pct = 100 * ic_bas,
         ic_haut_pct = 100 * ic_haut)

p_cv_sexe <- cv_sexe_df %>%
  ggplot(aes(x = Sexe, y = pct, fill = Sexe)) +
  geom_col(width = 0.45, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, ic_bas_pct), ymax = pmin(100, ic_haut_pct)),
                width = 0.1, color = oms_colors[["gris_fonce"]],
                linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.6, size = 5, fontface = "bold",
            color = oms_colors[["bleu_fonce"]]) +
  scale_fill_manual(values = c("Masculin" = oms_colors[["bleu_who"]],
                               "Féminin"  = oms_colors[["rouge"]])) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Couverture vaccinale par sexe",
       subtitle = "Estimations pondérées avec IC 95%",
       x = NULL, y = "Couverture vaccinale (%)",
       caption = .caption_default()) +
  theme(legend.position = "none")
save_plot(p_cv_sexe, file.path(PATH_GRAPHIQUES, "05_cv_par_sexe.png"),
          width = 7, height = 6)

tbl_cv_sexe <- cv_sexe_df %>%
  transmute(Sexe, n,
            `Couverture (%)` = round(pct, 1),
            `IC 95%` = sprintf("[%.1f - %.1f]", ic_bas_pct, ic_haut_pct))
export_table(
  gt_styled(tbl_cv_sexe,
            title = "Couverture par sexe",
            subtitle = "Estimations pondérées avec IC 95%"),
  file.path(PATH_TABLEAUX, "cv_par_sexe"), raw_data = tbl_cv_sexe
)


# -----------------------------------------------------------------------------
# 9b. COUVERTURE VACCINALE PAR MILIEU DE RESIDENCE (urbain / rural)
# -----------------------------------------------------------------------------

cli_h1("Couverture par milieu de résidence (urbain / rural)")

if ("milieu" %in% names(enfant_anal) &&
    sum(!is.na(enfant_anal$milieu)) > 0) {

  cv_milieu_df <- estim_prop(enfant_anal, "vaccine_bin", by = "milieu") %>%
    rename(Milieu = groupe) %>%
    mutate(pct = 100 * prop,
           ic_bas_pct = 100 * ic_bas,
           ic_haut_pct = 100 * ic_haut)

  if (!is.null(cv_milieu_df) && nrow(cv_milieu_df) > 0) {
    p_cv_milieu <- cv_milieu_df %>%
      ggplot(aes(x = Milieu, y = pct, fill = Milieu)) +
      geom_col(width = 0.45, alpha = 0.9) +
      geom_errorbar(aes(ymin = pmax(0, ic_bas_pct), ymax = pmin(100, ic_haut_pct)),
                    width = 0.1, color = oms_colors[["gris_fonce"]],
                    linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", pct)),
                vjust = -0.6, size = 5, fontface = "bold",
                color = oms_colors[["bleu_fonce"]]) +
      geom_hline(yintercept = 95, linetype = "dashed",
                 color = oms_colors[["vert"]], linewidth = 0.5) +
      scale_fill_manual(values = c("Urbain" = oms_colors[["bleu_who"]],
                                   "Rural"  = oms_colors[["orange"]])) +
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
                         labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Couverture vaccinale par milieu de résidence",
           subtitle = "Estimations pondérées avec IC 95% (ligne verte = cible OMS 95%)",
           x = NULL, y = "Couverture vaccinale (%)",
           caption = .caption_default("Milieu dérivé de la base de sondage (zone de santé)")) +
      theme(legend.position = "none")
    save_plot(p_cv_milieu, file.path(PATH_GRAPHIQUES, "05b_cv_par_milieu.png"),
              width = 7, height = 6)

    tbl_cv_milieu <- cv_milieu_df %>%
      transmute(Milieu, n,
                `Couverture (%)` = round(pct, 1),
                `IC 95%` = sprintf("[%.1f - %.1f]", ic_bas_pct, ic_haut_pct))
    export_table(
      gt_styled(tbl_cv_milieu,
                title = "Couverture par milieu de résidence",
                subtitle = "Estimations pondérées avec IC 95%"),
      file.path(PATH_TABLEAUX, "cv_par_milieu"), raw_data = tbl_cv_milieu
    )
    cli_alert_success("Couverture par milieu calculée ({nrow(cv_milieu_df)} modalités).")
  }
} else {
  cli_alert_warning("Milieu de résidence non disponible : section ignorée.")
}


# -----------------------------------------------------------------------------
# 10. STATUT VACCINAL ANTERIEUR
# -----------------------------------------------------------------------------

cli_h1("Statut vaccinal antérieur (6-59 mois)")

# Variante fusionnée ("Une dose ou plus (n.p.)" regroupée avec "Une dose")
statut_df <- enfant_anal %>%
  filter(tranche_age_mois == "6-59 mois", !is.na(statut_vaccinal_ant_fus)) %>%
  count(statut_vaccinal_ant_fus) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  rename(statut_vaccinal_ant = statut_vaccinal_ant_fus)

# Variante détaillée (toutes modalités) — conservée pour annexe
statut_detail_df <- enfant_anal %>%
  filter(tranche_age_mois == "6-59 mois", !is.na(statut_vaccinal_ant)) %>%
  count(statut_vaccinal_ant) %>%
  mutate(pct = round(100 * n / sum(n), 1))

if (nrow(statut_df) > 0) {
  p_statut <- statut_df %>%
    ggplot(aes(x = "", y = pct, fill = statut_vaccinal_ant)) +
    geom_col(width = 0.5, color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%s\nn=%s (%s%%)",
                                  statut_vaccinal_ant, comma(n),
                                  format(pct, decimal.mark = ","))),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3.5,
              lineheight = 0.9) +
    coord_flip() +
    scale_fill_manual(values = palette_oms_cat) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Répartition par statut vaccinal antérieur",
      subtitle = "Enfants de 6 à 59 mois - Doses reçues avant la campagne",
      x = NULL, y = "Pourcentage", fill = NULL,
      caption = .caption_default("« Une dose » inclut les cas « une dose ou plus » non précisés")
    ) +
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          panel.grid = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  save_plot(p_statut, file.path(PATH_GRAPHIQUES, "06_statut_vaccinal_anterieur.png"),
            width = 11, height = 5.5)

  export_table(
    gt_styled(statut_df %>%
                rename(`Statut antérieur` = statut_vaccinal_ant,
                       `Effectif` = n, `Pourcentage (%)` = pct),
              title = "Statut vaccinal antérieur (6-59 mois)"),
    file.path(PATH_TABLEAUX, "statut_vaccinal_anterieur"),
    raw_data = statut_df
  )
  # Version détaillée en annexe
  export_table(
    gt_styled(statut_detail_df %>%
                rename(`Statut antérieur (détaillé)` = statut_vaccinal_ant,
                       `Effectif` = n, `Pourcentage (%)` = pct),
              title = "Statut vaccinal antérieur détaillé (6-59 mois)"),
    file.path(PATH_TABLEAUX, "statut_vaccinal_anterieur_detail"),
    raw_data = statut_detail_df
  )
}


# -----------------------------------------------------------------------------
# 10b. COUVERTURE PARMI LES ENFANTS ZÉRO DOSE (SIA-COVG-03 / VCQI)
# -----------------------------------------------------------------------------
# Indicateur VCQI SIA-COVG-03 : parmi les enfants n'ayant reçu aucun vaccin
# contre la rougeole avant la campagne (« zéro dose »), quelle proportion a
# été atteinte par la campagne ? C'est un indicateur de rattrapage critique.
# Dénominateur : enfants 6-59 mois au statut antérieur connu (zéro dose).
# Numérateur : parmi eux, ceux déclarés vaccinés lors de la campagne.

cli_h1("SIA-COVG-03 : Couverture parmi les enfants zéro dose (VCQI)")

if (all(c("statut_vaccinal_ant_fus", "vaccine_bin", "tranche_age_mois")
        %in% names(enfant_anal))) {
  # Base : enfants 6-59 mois au statut antérieur connu
  zero_dose_base <- enfant_anal %>%
    filter(tranche_age_mois == "6-59 mois",
           !is.na(statut_vaccinal_ant_fus),
           !is.na(vaccine_bin))

  # Sous-groupe zéro dose
  zero_dose_df <- zero_dose_base %>%
    filter(grepl("zéro dose|zero dose|aucun|no dose|0 dose",
                 statut_vaccinal_ant_fus, ignore.case = TRUE))

  # Couverture globale parmi zéro dose
  n_zero <- nrow(zero_dose_df)
  if (n_zero >= 10) {
    pct_campagne_zero <- round(100 * mean(zero_dose_df$vaccine_bin == 1, na.rm = TRUE), 1)
    pct_campagne_zero_nonvax <- round(100 - pct_campagne_zero, 1)

    # Par province
    zero_prov <- zero_dose_df %>%
      filter(!is.na(province_clean)) %>%
      group_by(Province = province_clean) %>%
      summarise(
        n_zero_dose = n(),
        n_vaccines   = sum(vaccine_bin == 1, na.rm = TRUE),
        cv_pct       = round(100 * n_vaccines / n_zero_dose, 1),
        .groups = "drop"
      ) %>%
      arrange(cv_pct)

    # Tableau croisé statut antérieur × statut campagne (tous 6-59 mois)
    croise_statut <- zero_dose_base %>%
      mutate(
        statut_ant_grp = case_when(
          grepl("zéro dose|zero dose|aucun|no dose|0 dose",
                statut_vaccinal_ant_fus, ignore.case = TRUE) ~ "Zéro dose",
          TRUE ~ "Au moins une dose"
        ),
        vax_camp = if_else(vaccine_bin == 1, "Vacciné (campagne)", "Non vacciné")
      ) %>%
      count(statut_ant_grp, vax_camp) %>%
      group_by(statut_ant_grp) %>%
      mutate(pct = round(100 * n / sum(n), 1)) %>%
      ungroup()

    export_table(
      gt_styled(
        zero_prov %>%
          transmute(Province,
                    `N (zéro dose)` = fmt_n(n_zero_dose),
                    `Vaccinés campagne` = fmt_n(n_vaccines),
                    `Taux de rattrapage (%)` = fmt_pct1(cv_pct)),
        title = "Couverture de la campagne parmi les enfants zéro dose (6-59 mois)",
        subtitle = glue("Total zéro dose enquêtés : {comma(n_zero)} enfants | ",
                        "Taux de rattrapage national : {fmt_pct1(pct_campagne_zero)} %")
      ),
      file.path(PATH_TABLEAUX, "zero_dose_couverture_campagne"),
      raw_data = zero_prov
    )

    export_table(
      gt_styled(
        croise_statut %>%
          transmute(`Statut vaccinal antérieur` = statut_ant_grp,
                    `Statut campagne` = vax_camp,
                    Effectif = fmt_n(n),
                    `Pourcentage (%)` = fmt_pct1(pct)),
        title = "Croisement statut vaccinal antérieur × statut campagne (6-59 mois)",
        subtitle = "Permet de mesurer l'apport de la campagne pour les enfants non vaccinés auparavant"
      ),
      file.path(PATH_TABLEAUX, "zero_dose_croise_campagne"),
      raw_data = croise_statut
    )

    # Graphique : taux de rattrapage par province
    p_zero <- zero_prov %>%
      ggplot(aes(x = cv_pct, y = reorder(Province, cv_pct),
                 fill = cv_pct >= 80)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_vline(xintercept = 80, color = oms_colors[["orange"]], linetype = "dashed", linewidth = 0.8) +
      geom_text(aes(label = paste0(fmt_pct1(cv_pct), " % (n=", n_zero_dose, ")")),
                hjust = -0.05, size = 3, color = oms_colors[["gris_fonce"]]) +
      scale_fill_manual(values = c(`TRUE` = oms_colors[["vert"]], `FALSE` = oms_colors[["rouge"]]),
                        guide = "none") +
      scale_x_continuous(limits = c(0, 115), labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Taux de rattrapage des enfants zéro dose par la campagne",
           subtitle = glue("Par province | Ligne orange = seuil indicatif 80 % | ",
                           "Total : {comma(n_zero)} enfants zéro dose (6-59 mois)"),
           x = "Part des enfants zéro dose vaccinés lors de la campagne",
           y = NULL, caption = .caption_default())
    save_plot(p_zero, file.path(PATH_GRAPHIQUES, "10b_zero_dose_rattrapage.png"),
              width = 10, height = 5)

    cli_alert_success("SIA-COVG-03 : {comma(n_zero)} enfants zéro dose ; rattrapage = {fmt_pct1(pct_campagne_zero)} %.")
  } else {
    cli_alert_warning("SIA-COVG-03 : effectifs zéro dose insuffisants (n < 10) — indicateur non calculé.")
    pct_campagne_zero <- NA_real_
    zero_prov <- tibble()
  }
} else {
  cli_alert_warning("SIA-COVG-03 : variables statut antérieur ou vaccine_bin manquantes.")
  pct_campagne_zero <- NA_real_
  n_zero <- NA_integer_
  zero_prov <- tibble()
}


# -----------------------------------------------------------------------------
# 11. MOTIFS DE NON VACCINATION
# -----------------------------------------------------------------------------

cli_h1("Motifs de non-vaccination")

raisons_df <- enfant_anal %>%
  filter(!is.na(raison_pas_lbl)) %>%
  count(raison_pas_lbl) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

if (nrow(raisons_df) > 0) {
  p_raisons <- raisons_df %>%
    ggplot(aes(x = pct, y = reorder(raison_pas_lbl, pct))) +
    geom_col(fill = oms_colors[["rouge"]], alpha = 0.85, width = 0.75) +
    geom_text(aes(label = sprintf("%s (%.1f%%)", comma(n), pct)),
              hjust = -0.1, size = 3.3, color = oms_colors[["gris_fonce"]]) +
    scale_x_continuous(limits = c(0, max(raisons_df$pct) * 1.25),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Motifs de non-vaccination déclarés",
      subtitle = glue("Sur {comma(sum(raisons_df$n))} enfants non vaccinés"),
      x = "Pourcentage", y = NULL,
      caption = .caption_default()
    )
  save_plot(p_raisons, file.path(PATH_GRAPHIQUES, "07_raisons_non_vaccination.png"),
            width = 11, height = max(5, 0.4 * nrow(raisons_df) + 2))

  export_table(
    gt_styled(raisons_df %>% rename(Motif = raison_pas_lbl,
                                    Effectif = n, `Pourcentage (%)` = pct),
              title = "Motifs de non-vaccination"),
    file.path(PATH_TABLEAUX, "raisons_non_vaccination"),
    raw_data = raisons_df
  )
}


# -----------------------------------------------------------------------------
# 12. CONFIRMATION PAR CARTE / DOIGT MARQUE
# -----------------------------------------------------------------------------

cli_h1("Confirmation par carte et marquage du doigt")

# Confirmation par carte (chez les enfants déclarés vaccinés)
carte_df <- enfant_anal %>%
  filter(!is.na(confirmation_carte), vaccine_bin == 1) %>%
  count(confirmation_carte) %>%
  mutate(pct = round(100 * n / sum(n), 1))

if (nrow(carte_df) > 0) {
  p_carte <- carte_df %>%
    ggplot(aes(x = reorder(confirmation_carte, -n), y = pct,
               fill = confirmation_carte)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", pct, comma(n))),
              vjust = -0.3, lineheight = 0.9, size = 3.5,
              color = oms_colors[["gris_fonce"]]) +
    scale_fill_manual(values = palette_oms_cat) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Confirmation de la vaccination par la carte",
      subtitle = "Enfants déclarés vaccinés durant la campagne",
      x = NULL, y = "Pourcentage",
      caption = .caption_default()
    ) +
    theme(legend.position = "none")
  save_plot(p_carte, file.path(PATH_GRAPHIQUES, "08_confirmation_carte.png"),
            width = 9, height = 6)

  export_table(
    gt_styled(carte_df %>% rename(`Confirmation` = confirmation_carte,
                                  Effectif = n, `Pourcentage (%)` = pct),
              title = "Confirmation de la vaccination par la carte"),
    file.path(PATH_TABLEAUX, "confirmation_carte"),
    raw_data = carte_df
  )
}


# -----------------------------------------------------------------------------
# 12b. COUVERTURE AJUSTEE SUR LA PREUVE PHYSIQUE (carte vs declaratif)
# -----------------------------------------------------------------------------
# On calcule DEUX estimateurs de couverture, au niveau national et provincial :
#   1) Couverture déclarative  : vaccine_bin (carte + histoire) — estimation usuelle
#   2) Couverture par carte     : vaccine_bin_carte (preuve physique vue uniquement)
# L'écart entre les deux mesure l'amplitude du biais de désirabilité sociale.

cli_h1("Couverture : déclaratif vs preuve physique (carte)")

cv_decl_nat  <- estim_prop(enfant_anal, "vaccine_bin")
cv_carte_nat <- estim_prop(enfant_anal, "vaccine_bin_carte")

if (!is.null(cv_decl_nat) && !is.null(cv_carte_nat)) {
  comp_nat <- tibble(
    Indicateur = c("Couverture déclarative (carte + histoire)",
                   "Couverture par preuve physique (carte vue)"),
    `Estimation (%)` = c(round(100 * cv_decl_nat$prop, 1),
                         round(100 * cv_carte_nat$prop, 1)),
    `IC 95% (%)` = c(
      sprintf("[%.1f - %.1f]", 100 * cv_decl_nat$ic_bas, 100 * cv_decl_nat$ic_haut),
      sprintf("[%.1f - %.1f]", 100 * cv_carte_nat$ic_bas, 100 * cv_carte_nat$ic_haut)
    ),
    `Enfants` = c(cv_decl_nat$n, cv_carte_nat$n)
  )
  biais_nat <- round(100 * (cv_decl_nat$prop - cv_carte_nat$prop), 1)

  export_table(
    gt_styled(comp_nat,
              title = "Couverture nationale : déclaratif vs preuve physique",
              subtitle = glue("Écart (biais potentiel) = {biais_nat} points de pourcentage")),
    file.path(PATH_TABLEAUX, "couverture_carte_vs_declaratif"),
    raw_data = comp_nat %>% mutate(biais_pp = biais_nat))

  # Comparaison par province (deux estimateurs côte à côte)
  cv_decl_prov  <- estim_prop(enfant_anal, "vaccine_bin", by = "province_clean")
  cv_carte_prov <- estim_prop(enfant_anal, "vaccine_bin_carte", by = "province_clean")

  if (!is.null(cv_decl_prov) && !is.null(cv_carte_prov)) {
    comp_prov <- cv_decl_prov %>%
      transmute(Province = groupe,
                cv_declaratif = 100 * prop,
                decl_bas = 100 * ic_bas, decl_haut = 100 * ic_haut) %>%
      left_join(
        cv_carte_prov %>% transmute(Province = groupe,
                                    cv_carte = 100 * prop,
                                    carte_bas = 100 * ic_bas,
                                    carte_haut = 100 * ic_haut),
        by = "Province"
      ) %>%
      mutate(biais_pp = round(cv_declaratif - cv_carte, 1)) %>%
      arrange(desc(cv_declaratif))

    # Graphique : deux courbes (déclaratif vs carte) par province
    comp_long <- comp_prov %>%
      select(Province, cv_declaratif, cv_carte) %>%
      pivot_longer(c(cv_declaratif, cv_carte),
                   names_to = "type", values_to = "pct") %>%
      mutate(type = recode(type,
        cv_declaratif = "Déclaratif (carte + histoire)",
        cv_carte      = "Preuve physique (carte vue)"))

    p_comp <- ggplot(comp_long,
           aes(x = pct, y = reorder(Province, pct), color = type, group = type)) +
      geom_line(aes(group = Province), color = oms_colors[["gris_clair"]],
                linewidth = 3, alpha = 0.5) +
      geom_point(size = 4, alpha = 0.9) +
      scale_color_manual(values = c(
        "Déclaratif (carte + histoire)" = oms_colors[["bleu_who"]],
        "Preuve physique (carte vue)"   = oms_colors[["orange"]]), name = NULL) +
      scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                         limits = c(0, 105), expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = "Couverture déclarative vs preuve physique, par province",
        subtitle = "L'écart entre les deux points mesure le biais de désirabilité sociale potentiel",
        x = "Couverture vaccinale", y = NULL,
        caption = .caption_default("Écart = couverture déclarative − couverture par carte vue")
      ) +
      theme(legend.position = "bottom")
    save_plot(p_comp, file.path(PATH_GRAPHIQUES, "08b_carte_vs_declaratif.png"),
              width = 11, height = 6.5)

    export_table(
      gt_styled(comp_prov %>%
                  transmute(Province,
                            `Déclaratif (%)` = round(cv_declaratif, 1),
                            `Carte vue (%)`  = round(cv_carte, 1),
                            `Biais (pp)`     = biais_pp),
                title = "Couverture déclarative vs preuve physique par province",
                subtitle = "Biais = écart en points de pourcentage (désirabilité sociale potentielle)") %>%
        gt_color_cells("Biais (pp)", `Biais (pp)` >= 40, fill = oms_colors[["rouge"]]),
      file.path(PATH_TABLEAUX, "couverture_carte_vs_declaratif_province"),
      raw_data = comp_prov)
  }

  cli_alert_info("Biais déclaratif national : {biais_nat} points (déclaratif {round(100*cv_decl_nat$prop,1)}% vs carte {round(100*cv_carte_nat$prop,1)}%)")
}


# -----------------------------------------------------------------------------
# 13. DISTRIBUTION DE L'AGE DES ENFANTS
# -----------------------------------------------------------------------------

cli_h1("Distribution de l'âge des enfants")

age_anal <- enfant_anal %>% filter(!is.na(age_months_num))
if (nrow(age_anal) > 0) {
  age_stats <- age_anal %>%
    summarise(moyenne = mean(age_months_num),
              mediane = median(age_months_num),
              n = n())

  p_age <- age_anal %>%
    ggplot(aes(x = age_months_num)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 6,
                   fill = oms_colors[["bleu_who"]], alpha = 0.75,
                   color = "white", linewidth = 0.2) +
    geom_density(color = oms_colors[["orange"]], linewidth = 1.1) +
    geom_vline(xintercept = age_stats$moyenne,
               color = oms_colors[["rouge"]], linetype = "dashed",
               linewidth = 0.6) +
    geom_vline(xintercept = age_stats$mediane,
               color = oms_colors[["vert"]], linetype = "dashed",
               linewidth = 0.6) +
    annotate("label", x = age_stats$moyenne, y = Inf,
             label = sprintf("Moyenne : %.1f mois", age_stats$moyenne),
             vjust = 1.5, hjust = -0.05, fill = oms_colors[["rouge"]],
             color = "white", fontface = "bold", size = 3.2) +
    annotate("label", x = age_stats$mediane, y = Inf,
             label = sprintf("Médiane : %.0f mois", age_stats$mediane),
             vjust = 3.0, hjust = 1.05, fill = oms_colors[["vert"]],
             color = "white", fontface = "bold", size = 3.2) +
    scale_x_continuous(breaks = seq(0, 180, 24)) +
    labs(title = "Distribution de l'âge des enfants enquêtés",
         subtitle = glue("Histogramme et densité (n = {comma(age_stats$n)})"),
         x = "Âge (mois)", y = "Densité",
         caption = .caption_default())
  save_plot(p_age, file.path(PATH_GRAPHIQUES, "09_distribution_age.png"),
            width = 10, height = 6)

  # Tableau d'accompagnement : répartition par tranche d'âge + statistiques
  # IMPORTANT : pour garantir la cohérence avec le tableau de COUVERTURE par
  # tranche d'âge (section 3.4), on restreint la distribution à EXACTEMENT la
  # même population : enfants dont la tranche d'âge ET le statut vaccinal
  # (vaccine_bin) sont renseignés. Ainsi les effectifs par tranche coïncident
  # entre les deux tableaux.
  base_age_distrib <- age_anal %>%
    filter(!is.na(tranche_age), !is.na(vaccine_bin))
  n_base_age <- nrow(base_age_distrib)
  age_distrib_tbl <- base_age_distrib %>%
    count(tranche_age, name = "n") %>%
    mutate(`Pourcentage (%)` = round(100 * n / sum(n), 1)) %>%
    rename(`Tranche d'âge` = tranche_age, Effectif = n)
  age_stats_distrib <- base_age_distrib %>%
    summarise(moyenne = mean(age_months_num, na.rm = TRUE),
              mediane = median(age_months_num, na.rm = TRUE),
              n = n())
  export_table(
    gt_styled(age_distrib_tbl,
              title = "Distribution des enfants par tranche d'âge",
              subtitle = glue("Population : enfants au statut vaccinal et à la tranche d'âge connus ",
                              "(n = {comma(age_stats_distrib$n)}) | Âge moyen : ",
                              "{format(round(age_stats_distrib$moyenne,1), decimal.mark=',')} mois | ",
                              "médian : {format(round(age_stats_distrib$mediane,1), decimal.mark=',')} mois")),
    file.path(PATH_TABLEAUX, "distribution_age"),
    raw_data = age_distrib_tbl)
}


# -----------------------------------------------------------------------------
# 13b. DESCRIPTION SOCIO-DEMOGRAPHIQUE (stratifiee par province)  [Module A2]
# -----------------------------------------------------------------------------

cli_h1("Description socio-démographique de l'échantillon")

vars_desc <- intersect(
  c("caregiver_sex_f", "caregiver_age_num", "niveau_instruction",
    "situation_matrimoniale", "sexe", "age_groupe",
    "info_campagne_f", "connait_lieu_f", "importance_vaccins_f"),
  names(enfant_anal)
)

if (has_gtsummary && length(vars_desc) > 0) {
  tryCatch({
    labels_desc <- list(
      caregiver_sex_f        ~ "Sexe du répondant",
      caregiver_age_num      ~ "Âge du répondant (ans)",
      niveau_instruction     ~ "Niveau d'instruction",
      situation_matrimoniale ~ "Situation matrimoniale",
      sexe                   ~ "Sexe de l'enfant",
      age_groupe             ~ "Groupe d'âge de l'enfant",
      info_campagne_f        ~ "Informé avant la campagne",
      connait_lieu_f         ~ "Connaît le lieu de vaccination",
      importance_vaccins_f   ~ "Importance perçue des vaccins"
    )
    labels_desc <- labels_desc[sapply(labels_desc,
                     function(f) all.vars(f)[1] %in% vars_desc)]

    tbl_desc <- enfant_anal %>%
      select(any_of(c("province_clean", vars_desc))) %>%
      gtsummary::tbl_summary(
        by = province_clean,
        label = labels_desc,
        statistic = list(
          gtsummary::all_continuous()  ~ "{median} ({p25}-{p75})",
          gtsummary::all_categorical() ~ "{n} ({p}%)"),
        digits = list(gtsummary::all_categorical() ~ c(0, 1)),
        missing = "no"
      ) %>%
      gtsummary::add_overall() %>%
      gtsummary::bold_labels() %>%
      gtsummary::modify_caption(
        "Caractéristiques socio-démographiques de l'échantillon")

    # Export (gtsummary -> gt -> html / -> tibble -> csv)
    gt_obj <- gtsummary::as_gt(tbl_desc)
    gt::gtsave(gt_obj, file.path(PATH_TABLEAUX, "00_description_echantillon.html"))
    readr::write_csv(gtsummary::as_tibble(tbl_desc),
                     file.path(PATH_TABLEAUX, "00_description_echantillon.csv"), na = "")
    cli_alert_success("Tableau descriptif (gtsummary) exporté.")
  }, error = function(e) cli_alert_warning("Tableau descriptif KO : {e$message}"))
} else {
  # Repli sans gtsummary : tableau simple effectifs/% par variable catégorielle
  desc_simple <- map_dfr(vars_desc, function(v) {
    x <- enfant_anal[[v]]
    if (is.numeric(x)) {
      tibble(Variable = v, Modalite = "médiane (Q1-Q3)",
             Valeur = sprintf("%.1f (%.1f-%.1f)",
                              median(x, na.rm = TRUE),
                              quantile(x, .25, na.rm = TRUE),
                              quantile(x, .75, na.rm = TRUE)))
    } else {
      tt <- enfant_anal %>% filter(!is.na(.data[[v]])) %>% count(.data[[v]])
      tibble(Variable = v, Modalite = as.character(tt[[1]]),
             Valeur = sprintf("%d (%.1f%%)", tt$n, 100 * tt$n / sum(tt$n)))
    }
  })
  export_table(gt_styled(desc_simple, title = "Description de l'échantillon"),
               file.path(PATH_TABLEAUX, "00_description_echantillon"),
               raw_data = desc_simple)
}


# -----------------------------------------------------------------------------
# 13c. COUVERTURE PAR SOUS-GROUPES SOCIO-DEMOGRAPHIQUES  [Module B3]
# -----------------------------------------------------------------------------

cli_h1("Couverture par sous-groupes socio-démographiques")

# Helper : couverture + IC Wilson par groupe (>= 15 obs)
.cov_subgroup <- function(data, grp, label_grp, min_n = 15) {
  if (!grp %in% names(data)) return(NULL)
  data %>%
    filter(!is.na(.data[[grp]]), !is.na(vaccine_bin)) %>%
    group_by(.lab = as.character(.data[[grp]])) %>%
    summarise(N = n(), Nvax = sum(vaccine_bin == 1), .groups = "drop") %>%
    filter(N >= min_n) %>%
    rowwise() %>%
    mutate(
      pct  = Nvax / N,
      ic   = list(prop.test(Nvax, N, correct = FALSE)$conf.int),
      ic_l = ic[[1]], ic_h = ic[[2]],
      groupe = label_grp
    ) %>%
    ungroup() %>%
    select(groupe, label = .lab, N, Nvax, pct, ic_l, ic_h)
}

df_sg <- bind_rows(
  .cov_subgroup(enfant_anal, "niveau_instruction", "Niveau d'instruction"),
  .cov_subgroup(enfant_anal, "age_groupe",        "Groupe d'âge enfant"),
  .cov_subgroup(enfant_anal, "info_campagne_f",   "Informé avant campagne"),
  .cov_subgroup(enfant_anal, "importance_vaccins_f", "Importance perçue")
)

if (!is.null(df_sg) && nrow(df_sg) > 0) {
  cv_global_ref <- if (exists("cv_global_df") && !is.null(cv_global_df))
    cv_global_df$prop else mean(enfant_anal$vaccine_bin, na.rm = TRUE)

  p_sg <- ggplot(df_sg, aes(x = pct, y = reorder(label, pct))) +
    geom_point(aes(size = N), color = oms_colors[["bleu_who"]], alpha = 0.85) +
    geom_errorbar(aes(xmin = ic_l, xmax = ic_h), orientation = "y", width = 0.3,
                   linewidth = 0.7, color = oms_colors[["bleu_who"]]) +
    geom_vline(xintercept = cv_global_ref, linetype = "dashed",
               color = oms_colors[["rouge"]], linewidth = 0.7) +
    scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                       limits = c(min(0.3, min(df_sg$ic_l)), 1.0)) +
    scale_size_continuous(name = "Effectif (N)", range = c(2, 7)) +
    facet_wrap(~groupe, scales = "free_y", ncol = 2) +
    labs(title = "Couverture vaccinale par sous-groupes",
         subtitle = "Points = couverture, barres = IC 95% Wilson ; ligne rouge = taux global",
         x = "Taux de couverture", y = NULL,
         caption = .caption_default()) +
    theme(legend.position = "bottom")
  save_plot(p_sg, file.path(PATH_GRAPHIQUES, "12_couverture_sousgroupes.png"),
            width = 11, height = 7)

  export_table(
    gt_styled(df_sg %>%
                transmute(`Sous-groupe` = groupe, Modalité = label, N, Vaccinés = Nvax,
                          `Couverture (%)` = round(100 * pct, 1),
                          `IC 95%` = sprintf("[%.1f - %.1f]", 100*ic_l, 100*ic_h)),
              title = "Couverture par sous-groupe socio-démographique"),
    file.path(PATH_TABLEAUX, "couverture_sousgroupes"),
    raw_data = df_sg)
}


# -----------------------------------------------------------------------------
# 13d. RAISONS DE NON-VACCINATION : PARETO + TAXONOMIE OMS  [Module C]
# -----------------------------------------------------------------------------

cli_h1("Raisons de non-vaccination : Pareto et taxonomie OMS")

enfant_nonvax <- enfant_anal %>% filter(!is.na(vaccine_bin) & vaccine_bin == 0)
n_nonvax       <- nrow(enfant_nonvax)          # 374 : tous les non vaccinés
n_nonvax_raison <- sum(!is.na(enfant_nonvax$raison_pas_lbl))  # 313 : ceux ayant déclaré une raison
# Note : n_nonvax - n_nonvax_raison = enfants non vaccinés sans raison déclarée (NA)
writeLines(c(as.character(n_nonvax), as.character(n_nonvax_raison)),
           file.path(PATH_TABLEAUX, ".effectifs_nonvax.txt"))
# Exposer ces deux compteurs au reporting (lecture via un petit fichier texte)
writeLines(c(as.character(n_nonvax), as.character(n_nonvax_raison)),
           file.path(PATH_TABLEAUX, ".effectifs_nonvax.txt"))

# --- Diagramme de Pareto (raisons détaillées) ---
df_pareto <- enfant_nonvax %>%
  filter(!is.na(raison_pas_lbl)) %>%
  count(raison_pas_lbl, sort = TRUE) %>%
  mutate(
    pct     = n / sum(n),           # % relatif parmi ceux ayant déclaré une raison
    cum_pct = cumsum(pct),
    raison  = factor(raison_pas_lbl, levels = raison_pas_lbl),
    prioritaire = cum_pct <= 0.80 | lag(cum_pct, default = 0) < 0.80
  )

if (nrow(df_pareto) > 0) {
  p_pareto <- ggplot(df_pareto, aes(x = raison, y = pct)) +
    geom_col(aes(fill = prioritaire), width = 0.75, alpha = 0.9) +
    geom_line(aes(y = cum_pct, group = 1),
              color = oms_colors[["rouge"]], linewidth = 1, linetype = "dashed") +
    geom_point(aes(y = cum_pct), color = oms_colors[["rouge"]], size = 2) +
    geom_hline(yintercept = 0.80, linetype = "dotted",
               color = oms_colors[["orange"]], linewidth = 0.8) +
    annotate("label", x = 1, y = 0.80, label = "Seuil 80%",
             color = oms_colors[["orange"]], fill = "white",
             hjust = 0.5, vjust = -0.2, size = 3.2,
             fontface = "bold") +
    scale_y_continuous(labels = function(x) paste0(round(100*x), "%"),
                       limits = c(0, 1.02),
                       expand = expansion(mult = c(0, 0.02)),
                       sec.axis = sec_axis(~., name = "% cumulé",
                                  labels = function(x) paste0(round(100*x), "%"))) +
    scale_fill_manual(
      values = c("TRUE" = oms_colors[["bleu_who"]], "FALSE" = oms_colors[["gris"]]),
      labels = c("TRUE" = "Prioritaires (≤80% cumulé)", "FALSE" = "Secondaires"),
      name = NULL) +
    coord_flip(clip = "off") +
    labs(title = "Raisons de non-vaccination — Diagramme de Pareto",
         subtitle = glue("Parmi {comma(n_nonvax_raison)} enfants non vaccinés ayant déclaré une raison ",
                         "({comma(n_nonvax)} non vaccinés au total ; ",
                         "{comma(n_nonvax - n_nonvax_raison)} sans raison déclarée)"),
         x = NULL, y = "Fréquence relative",
         caption = .caption_default()) +
    theme(legend.position = "bottom",
          plot.margin = margin(t = 10, r = 20, b = 10, l = 10))
  save_plot(p_pareto, file.path(PATH_GRAPHIQUES, "13_pareto_raisons.png"),
            width = 11, height = max(6, 0.4 * nrow(df_pareto) + 2))
}

# --- Groupes de raisons (taxonomie OMS) par province ---
df_gp <- enfant_nonvax %>%
  filter(!is.na(raison_groupe), !is.na(province_clean)) %>%
  count(province_clean, raison_groupe) %>%
  group_by(province_clean) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

if (nrow(df_gp) > 0) {
  pal_groupe <- c(
    "Manque d'information"  = oms_colors[["orange"]],
    "Barrières d'accès"     = oms_colors[["violet"]],
    "Contraintes pratiques" = oms_colors[["vert"]],
    "Refus / Hésitation"    = oms_colors[["rouge"]],
    "Déjà vacciné"          = oms_colors[["bleu_who"]],
    "Refus d'autorisation"  = oms_colors[["gris_fonce"]],
    "Autre"                 = oms_colors[["gris"]])

  p_gp <- ggplot(df_gp, aes(x = province_clean, y = pct, fill = raison_groupe)) +
    geom_col(position = "fill", width = 0.7, color = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(pct > 0.06, sprintf("%.0f%%", 100*pct), "")),
              position = position_fill(vjust = 0.5),
              size = 2.8, color = "white", fontface = "bold") +
    scale_y_continuous(labels = function(x) paste0(round(100*x), "%")) +
    scale_fill_manual(values = pal_groupe, name = "Groupe de raisons", drop = FALSE) +
    coord_flip() +
    labs(title = "Groupes de raisons de non-vaccination par province",
         subtitle = "Taxonomie OMS — proportions relatives (barres normalisées)",
         x = NULL, y = "Proportion",
         caption = .caption_default()) +
    guides(fill = guide_legend(nrow = 3)) +
    theme(legend.position = "bottom")
  save_plot(p_gp, file.path(PATH_GRAPHIQUES, "14_groupes_raisons_province.png"),
            width = 11, height = 7)

  # Tableau détaillé groupe x raison
  tab_rai <- enfant_nonvax %>%
    filter(!is.na(raison_pas_lbl)) %>%
    count(raison_groupe, raison_pas_lbl, name = "n") %>%
    arrange(raison_groupe, desc(n)) %>%
    # Dénominateur = enfants ayant déclaré une raison (313), pas tous les non-vaccinés (374)
    mutate(`% non-vaccinés` = round(100 * n / n_nonvax_raison, 1))
  export_table(
    gt_styled(tab_rai %>% rename(Groupe = raison_groupe, Raison = raison_pas_lbl,
                                 Effectif = n),
              title = "Raisons de non-vaccination par groupe thématique (OMS)"),
    file.path(PATH_TABLEAUX, "raisons_par_groupe"),
    raw_data = tab_rai)
}

# --- Matrice province × raison dominante + réponse de santé publique [D] -----
# Pour chaque province, on identifie le GROUPE de raisons dominant chez les
# non-vaccinés et on lui associe le TYPE de réponse opérationnelle recommandée.
# Cela opérationnalise la taxonomie : la réponse diffère selon la cause.
if (nrow(df_gp) > 0) {
  # Couverture provinciale (pour contextualiser l'urgence)
  cv_prov_lookup <- if (exists("cv_prov_df")) {
    cv_prov_df %>% transmute(province_clean = Province, cv = pct)
  } else {
    enfant_anal %>% filter(!is.na(vaccine_bin), !is.na(province_clean)) %>%
      group_by(province_clean) %>%
      summarise(cv = round(100 * mean(vaccine_bin == 1), 1), .groups = "drop")
  }

  # Table de correspondance groupe -> réponse de santé publique
  reponse_sante_publique <- function(groupe) {
    dplyr::case_when(
      groupe == "Manque d'information"   ~ "Communication / mobilisation sociale renforcée",
      groupe == "Barrières d'accès"      ~ "Logistique : approvisionnement, chaîne de froid, sites",
      groupe == "Contraintes pratiques"  ~ "Adaptation horaires + stratégie avancée/porte-à-porte",
      groupe == "Refus / Hésitation"     ~ "Communication de proximité, dialogue communautaire",
      groupe == "Refus d'autorisation"   ~ "Engagement des leaders communautaires et religieux",
      groupe == "Déjà vacciné"           ~ "Vérification documentaire + sensibilisation au rappel",
      TRUE                                ~ "Investigation locale spécifique"
    )
  }

  matrice_prov_raison <- df_gp %>%
    group_by(province_clean) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(cv_prov_lookup, by = "province_clean") %>%
    mutate(
      `Réponse recommandée` = reponse_sante_publique(raison_groupe),
      pct = round(100 * pct, 1)
    ) %>%
    arrange(cv) %>%
    transmute(
      Province = province_clean,
      `Couverture (%)` = cv,
      `Raison dominante (groupe OMS)` = raison_groupe,
      `Part chez non-vaccinés (%)` = pct,
      `Réponse de santé publique recommandée` = `Réponse recommandée`
    )

  export_table(
    gt_styled(matrice_prov_raison,
              title = "Réponse opérationnelle par province selon la raison dominante",
              subtitle = "Opérationnalisation de la taxonomie OMS — provinces triées par couverture croissante") %>%
      gt_color_cells("Couverture (%)", `Couverture (%)` < 90, fill = oms_colors[["orange"]]) %>%
      gt_color_cells("Couverture (%)", `Couverture (%)` < 80, fill = oms_colors[["rouge"]]),
    file.path(PATH_TABLEAUX, "reponse_par_province"),
    raw_data = matrice_prov_raison)

  # Heatmap province × groupe de raisons (proportion au sein de chaque province)
  p_matrice <- df_gp %>%
    mutate(pct100 = 100 * pct,
           lbl = ifelse(pct > 0, paste0(format(round(pct100, 1), decimal.mark = ","), "%"), "")) %>%
    ggplot(aes(x = raison_groupe, y = reorder(province_clean, -pct),
               fill = pct100)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = lbl, color = pct100 > 30),
              size = 2.9, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c(`TRUE` = "white",
                                  `FALSE` = oms_colors[["gris_fonce"]])) +
    scale_fill_gradientn(
      colors = c(oms_colors[["bleu_clair"]], oms_colors[["jaune"]],
                 oms_colors[["orange"]], oms_colors[["rouge"]]),
      name = "% des\nnon-vaccinés") +
    labs(title = "Matrice province × groupe de raisons de non-vaccination",
         subtitle = "Identifie la cause dominante propre à chaque province pour cibler la réponse",
         x = "Groupe de raisons (taxonomie OMS)", y = "Province",
         caption = .caption_default()) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          panel.grid = element_blank())
  save_plot(p_matrice, file.path(PATH_GRAPHIQUES, "14b_matrice_province_raison.png"),
            width = 11, height = 6.5)

  # Tableau complet de la matrice (toutes les valeurs) pour annexe
  matrice_complete <- df_gp %>%
    mutate(pct100 = 100 * pct) %>%
    select(province_clean, raison_groupe, n, pct100) %>%
    tidyr::pivot_wider(names_from = raison_groupe,
                       values_from = pct100, values_fill = 0,
                       id_cols = province_clean)
  # Reformater en % français
  matrice_complete_fmt <- df_gp %>%
    mutate(cellule = paste0(fmt_pct1(100 * pct), " %")) %>%
    select(province_clean, raison_groupe, cellule) %>%
    tidyr::pivot_wider(names_from = raison_groupe, values_from = cellule,
                       values_fill = "0,0 %") %>%
    rename(Province = province_clean)
  export_table(
    gt_styled(matrice_complete_fmt,
              title = "Matrice province × groupe de raisons (% des non-vaccinés)",
              subtitle = "Répartition complète des raisons de non-vaccination par province"),
    file.path(PATH_TABLEAUX, "matrice_province_raison"),
    raw_data = matrice_complete_fmt)

  cli_alert_success("Matrice province × raison dominante générée.")
}


# -----------------------------------------------------------------------------
# 13e. DISPARITES GEOGRAPHIQUES : CHI-2 + CV + HEATMAP  [Module D]
# -----------------------------------------------------------------------------

cli_h1("Disparités géographiques (Chi-2, coefficient de variation, heatmap)")

# Test du Chi-2 : couverture ~ province
# On privilégie le test de Rao-Scott (svychisq), adapté aux plans de sondage
# complexes (Rao & Scott, 1984). Repli sur le Chi-2 de Pearson classique si le
# plan pondéré n'est pas disponible.
chi2_geo <- suppressWarnings(tryCatch({
  design_ok <- "poids_final" %in% names(enfant_anal) &&
               dplyr::n_distinct(enfant_anal$aire_sante_id) > 1
  df_chi <- enfant_anal %>%
    filter(!is.na(province_clean), !is.na(vaccine_bin)) %>%
    mutate(province_clean = factor(as.character(province_clean)),
           vaccine_bin = factor(as.integer(vaccine_bin)))

  if (design_ok) {
    args <- list(ids = ~aire_sante_id, weights = ~poids_final,
                 data = df_chi, nest = TRUE)
    if ("bloc" %in% names(df_chi) && dplyr::n_distinct(df_chi$bloc) > 1)
      args$strata <- ~bloc
    des_chi <- do.call(survey::svydesign, args)
    ct <- survey::svychisq(~province_clean + vaccine_bin, des_chi, statistic = "F")
    tibble(test = "Rao-Scott (couverture ~ province, plan complexe)",
           statistique = round(unname(ct$statistic), 2),
           ddl = paste(round(unname(ct$parameter), 1), collapse = " ; "),
           p_value = signif(ct$p.value, 4),
           conclusion = if (ct$p.value < 0.05)
             "Différence significative entre provinces (alpha=5%)"
             else "Pas de différence significative")
  } else {
    tab_geo <- table(df_chi$province_clean, df_chi$vaccine_bin)
    ct <- chisq.test(tab_geo)
    tibble(test = "Chi-2 de Pearson (couverture ~ province)",
           statistique = round(unname(ct$statistic), 2),
           ddl = as.character(unname(ct$parameter)),
           p_value = signif(ct$p.value, 4),
           conclusion = if (ct$p.value < 0.05)
             "Différence significative entre provinces (alpha=5%)"
             else "Pas de différence significative")
  }
}, error = function(e) { cli_alert_warning("Chi-2 KO : {e$message}"); NULL }))

if (!is.null(chi2_geo)) {
  cli_alert_info("Chi-2 : stat={chi2_geo$statistique}, ddl={chi2_geo$ddl}, p={chi2_geo$p_value}")
  export_table(gt_styled(chi2_geo, title = "Test de disparité géographique (Chi-2)"),
               file.path(PATH_TABLEAUX, "test_chi2_geographique"),
               raw_data = chi2_geo)
}

# Couverture par zone de santé + coefficient de variation
if ("zone_sante_clean" %in% names(enfant_anal)) {
  cov_zone <- enfant_anal %>%
    filter(!is.na(zone_sante_clean), !is.na(vaccine_bin)) %>%
    group_by(province_clean, zone_sante_clean) %>%
    summarise(N = n(), Nvax = sum(vaccine_bin == 1), .groups = "drop") %>%
    filter(N >= 10) %>%
    mutate(pct = 100 * Nvax / N)

  if (nrow(cov_zone) > 1) {
    cv_inter_zone <- 100 * sd(cov_zone$pct) / mean(cov_zone$pct)
    cli_alert_info("Coefficient de variation inter-zones : {round(cv_inter_zone,1)}%")
    writeLines(format(round(cv_inter_zone, 1), decimal.mark = ","),
               file.path(PATH_TABLEAUX, ".cv_inter_zone.txt"))

    # Barres horizontales facettées par province (bien plus lisible qu'une heatmap
    # creuse, car chaque zone n'appartient qu'à une seule province).
    n_zones <- dplyr::n_distinct(cov_zone$zone_sante_clean)
    p_heat <- cov_zone %>%
      ggplot(aes(x = pct, y = reorder(zone_sante_clean, pct), fill = pct)) +
      geom_col(width = 0.72, alpha = 0.95) +
      geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
                hjust = -0.12, size = 2.7, fontface = "bold",
                color = oms_colors[["gris_fonce"]]) +
      geom_vline(xintercept = 95, linetype = "dashed",
                 color = oms_colors[["rouge"]], linewidth = 0.6) +
      facet_grid(province_clean ~ ., scales = "free_y", space = "free_y",
                 switch = "y") +
      scale_fill_gradientn(
        colors = c(oms_colors[["rouge"]], oms_colors[["orange"]],
                   oms_colors[["jaune"]], oms_colors[["vert"]]),
        limits = c(0, 100), name = "Couverture (%)") +
      scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                         labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(title = "Couverture vaccinale par zone de santé",
           subtitle = glue("Regroupées par province | Coefficient de variation inter-zones : {format(round(cv_inter_zone,1), decimal.mark=',')}% (ligne rouge = cible 95%)"),
           x = "Couverture vaccinale", y = NULL,
           caption = .caption_default("Zones avec au moins 10 enfants enquêtés")) +
      theme(panel.grid.major.y = element_blank(),
            strip.text.y.left = element_text(angle = 0, face = "bold",
                                             size = 8, hjust = 1),
            strip.placement = "outside",
            axis.text.y = element_text(size = 7),
            legend.position = "bottom",
            panel.spacing = unit(0.3, "lines"))
    save_plot(p_heat, file.path(PATH_GRAPHIQUES, "15_heatmap_zone_sante.png"),
              width = 11, height = max(8, 0.22 * n_zones + 3))

    export_table(
      gt_styled(cov_zone %>%
                  transmute(Province = province_clean, `Zone de santé` = zone_sante_clean,
                            N, Vaccinés = Nvax, `Couverture (%)` = round(pct, 1)),
                title = "Couverture par zone de santé",
                subtitle = glue("CV inter-zones = {round(cv_inter_zone,1)}%")),
      file.path(PATH_TABLEAUX, "couverture_par_zone"),
      raw_data = cov_zone)
  }
}



# -----------------------------------------------------------------------------
# 13f_bis. EFFET DE PLAN (DEFF) ET CORRÉLATION INTRACLASSE (ICC) — VCQI QUAL-04
# -----------------------------------------------------------------------------
# Le DEFF (Design Effect) mesure la perte d'efficacité statistique due au
# clustering : DEFF = Var(plan complexe) / Var(SRS à même n). Un DEFF > 1
# signifie que le plan en grappes est moins précis qu'un aléatoire simple.
# ICC (ρ) = ressemblance des individus au sein d'un même cluster :
#   ICC = (DEFF - 1) / (m_bar - 1)   avec m_bar = taille moyenne des clusters.
# Référence : Kish (1965). Survey Sampling.

cli_h1("VCQI QUAL-04 : Effet de plan (DEFF) et corrélation intraclasse (ICC)")

if (exists("svy_design") && !is.null(svy_design) &&
    exists("cv_global_pct") && !is.na(cv_global_pct) &&
    "vaccine_bin" %in% names(enfant_anal)) {
  tryCatch({
    # ── DEFF GLOBAL ────────────────────────────────────────────────────────
    # Variance SRS (approximation binomiale)
    p0 <- cv_global_pct / 100
    srs_var_global <- p0 * (1 - p0) / n_global

    # Variance pondérée depuis les IC logit déjà calculés
    ic_bas_p  <- cv_global_df$ic_bas
    ic_haut_p <- cv_global_df$ic_haut
    svy_var_global <- ((ic_haut_p - ic_bas_p) / (2 * 1.96))^2
    deff_global <- round(svy_var_global / srs_var_global, 2)

    # Taille moyenne des clusters (aires de santé)
    m_bar <- enfant_anal %>%
      filter(!is.na(vaccine_bin)) %>%
      count(aire_sante_id) %>%
      pull(n) %>% mean()
    icc_global <- if (m_bar > 1) round((deff_global - 1) / (m_bar - 1), 4) else NA_real_

    # ── DEFF PAR PROVINCE ──────────────────────────────────────────────────
    deff_prov <- cv_prov_df %>%
      ungroup() %>%
      select(Province, n, prop, ic_bas, ic_haut) %>%
      mutate(
        p_prov = prop,
        srs_v  = p_prov * (1 - p_prov) / pmax(n, 1),
        svy_v  = ((ic_haut - ic_bas) / (2 * 1.96))^2,
        deff_p = round(svy_v / srs_v, 2)
      ) %>%
      left_join(
        enfant_anal %>%
          filter(!is.na(vaccine_bin), !is.na(province_clean)) %>%
          count(province_clean, aire_sante_id) %>%
          group_by(province_clean) %>%
          summarise(m_p = mean(n), .groups = "drop"),
        by = c("Province" = "province_clean")
      ) %>%
      mutate(icc_p = if_else(!is.na(m_p) & m_p > 1,
                              round((deff_p - 1) / (m_p - 1), 4), NA_real_)) %>%
      ungroup()

    deff_tbl <- bind_rows(
      tibble(Niveau = "National", Province = "Ensemble",
             n = n_global,
             DEFF = deff_global,
             `m̄ (taille moy. cluster)` = round(m_bar, 1),
             ICC = icc_global),
      deff_prov %>%
        transmute(Niveau = "Provincial", Province,
                  n, DEFF = deff_p,
                  `m̄ (taille moy. cluster)` = round(m_p, 1),
                  ICC = icc_p)
    )

    export_table(
      gt_styled(
        deff_tbl %>%
          transmute(Niveau, Province,
                    `N enquêtés` = fmt_n(n),
                    DEFF = format(DEFF, nsmall = 2, decimal.mark = ","),
                    `Taille moy. cluster` = format(`m̄ (taille moy. cluster)`,
                                                    nsmall = 1, decimal.mark = ","),
                    ICC = format(ICC, nsmall = 4, decimal.mark = ",")),
        title = "Effet de plan (DEFF) et corrélation intraclasse (ICC)",
        subtitle = glue("Variable : couverture vaccinale (vaccine_bin) | ",
                        "DEFF national = {format(deff_global, nsmall=2, decimal.mark=',')} | ",
                        "ICC = {format(icc_global, nsmall=4, decimal.mark=',')}")
      ),
      file.path(PATH_TABLEAUX, "deff_icc_couverture"),
      raw_data = deff_tbl
    )

    # ── GRAPHIQUE DEFF PAR PROVINCE (avec traces de diagnostic) ───────────
    cli_alert_info("DEFF trace 1 : deff_prov nrow={nrow(deff_prov)}, colonnes={paste(names(deff_prov), collapse=',')}")
    cli_alert_info("DEFF trace 2 : deff_p values={paste(round(deff_prov$deff_p, 2), collapse=', ')}")
    cli_alert_info("DEFF trace 3 : Province values={paste(deff_prov$Province, collapse=', ')}")

    deff_prov_plot <- deff_prov %>% filter(!is.na(deff_p) & !is.na(Province))
    cli_alert_info("DEFF trace 4 : deff_prov_plot nrow={nrow(deff_prov_plot)}")

    if (nrow(deff_prov_plot) == 0) {
      cli_alert_warning("VCQI QUAL-04 : aucune province avec DEFF calculé — graphique non produit.")
    } else {
      cli_alert_info("DEFF trace 5 : construction ggplot...")
      p_deff <- ggplot(deff_prov_plot,
                       aes(x = reorder(as.character(Province), deff_p), y = deff_p)) +
        geom_col(aes(fill = deff_p), width = 0.7, alpha = 0.88) +
        geom_hline(yintercept = 1, linetype = "dashed",
                   color = oms_colors[["gris"]], linewidth = 0.7) +
        geom_hline(yintercept = 2, linetype = "dashed",
                   color = oms_colors[["orange"]], linewidth = 0.9) +
        geom_text(aes(label = format(round(deff_p, 2), nsmall = 2, decimal.mark = ",")),
                  hjust = -0.1, size = 3.2, color = oms_colors[["gris_fonce"]]) +
        coord_flip() +
        scale_fill_gradient(low = oms_colors[["bleu_who"]], high = oms_colors[["rouge"]],
                            guide = "none") +
        scale_y_continuous(
          limits = c(0, max(c(deff_prov_plot$deff_p, 1), na.rm = TRUE) * 1.25),
          expand = expansion(mult = c(0, 0))) +
        labs(title = "Effet de plan (DEFF) de la couverture vaccinale par province",
             subtitle = "Ligne grise = DEFF 1 (SRS) | Ligne orange = DEFF 2 (seuil d'alerte)",
             x = NULL, y = "DEFF", caption = .caption_default())
      cli_alert_info("DEFF trace 6 : ggplot construit, sauvegarde...")
      save_plot(p_deff, file.path(PATH_GRAPHIQUES, "13g_deff_par_province.png"),
                width = 9, height = 5)
      cli_alert_info("DEFF trace 7 : figure sauvegardée avec succès.")
    }

    cli_alert_success("VCQI QUAL-04 : DEFF global = {deff_global} | ICC = {icc_global}")
  }, error = function(e) {
    cli_alert_warning("VCQI QUAL-04 : calcul DEFF/ICC échoué — {e$message}")
    cli_alert_info("DEFF debug : rlang::last_trace() pour détails. Vérifier deff_prov en console.")
    # NE PAS écraser deff_global/icc_global s'ils ont déjà été calculés
    if (!exists("deff_global") || is.null(deff_global)) deff_global <<- NA_real_
    if (!exists("icc_global")  || is.null(icc_global))  icc_global  <<- NA_real_
    if (!exists("deff_tbl")    || is.null(deff_tbl))    deff_tbl    <<- tibble()
  })
} else {
  cli_alert_warning("VCQI QUAL-04 : svy_design ou cv_global_pct indisponible.")
  deff_global <- NA_real_; icc_global <- NA_real_; deff_tbl <- tibble()
}


# -----------------------------------------------------------------------------
# 13f. DETERMINANTS : REGRESSION LOGISTIQUE + FOREST PLOT  [Module E]
# -----------------------------------------------------------------------------

# Variables candidates couvrant les 4 domaines BeSD (OMS) :
#   1. Thinking & Feeling : importance_vaccins, facilite_paiement (perception)
#   2. Social Processes   : parents_amis_vaccin (influence de l'entourage)
#   3. Motivation         : vaccins_souhaites (intention de vacciner)
#   4. Practical Issues   : info_campagne, connait_lieu (accès à l'information),
#                           milieu (urbain/rural, accès aux services)
# + Caractéristiques individuelles & contextuelles : province, sexe, âge,
#   instruction du tuteur, sexe et âge du chef de ménage, taille du ménage.
vars_modele <- intersect(
  c("province_clean", "niveau_instruction", "info_campagne_f",
    "importance_vaccins_num", "connait_lieu_f", "facilite_paiement_num",
    "parents_amis_vaccin_f", "vaccins_souhaites_f",
    "milieu", "chef_sexe", "chef_age_grp", "taille_menage_grp",
    "sexe", "age_groupe"),
  names(enfant_anal)
)

df_mod <- enfant_anal %>%
  filter(!is.na(vaccine_bin)) %>%
  select(vaccine_bin, poids_final, aire_sante_id,
         any_of(c(vars_modele, "bloc"))) %>%
  tidyr::drop_na(any_of(vars_modele))

if (nrow(df_mod) > 50 && length(vars_modele) >= 2) {
  formule <- as.formula(paste("vaccine_bin ~", paste(vars_modele, collapse = " + ")))

  # ── Détection de quasi-séparation ───────────────────────────────────────────
  # Une province (ou modalité) avec très peu de non-vaccinés provoque une
  # quasi-séparation : l'OR explose et l'IC devient gigantesque (ex. Haut Lomami
  # ORa=24 [3-198]). On détecte les modalités de province à risque (< 5 cas
  # dans l'une des deux classes 0/1).
  detecter_quasi_separation <- function(df, vars) {
    risque <- FALSE
    for (v in vars) {
      if (is.factor(df[[v]]) || is.character(df[[v]])) {
        tab <- table(df[[v]], df$vaccine_bin)
        if (any(tab < 5)) risque <- TRUE
      }
    }
    risque
  }
  quasi_sep <- detecter_quasi_separation(df_mod, vars_modele)
  if (quasi_sep) {
    cli_alert_warning(paste(
      "Quasi-séparation détectée (faibles effectifs dans certaines modalités).",
      "Application de la régression pénalisée de Firth (Firth, 1993) pour",
      "stabiliser les estimations."))
  }

  # ── Choix de la méthode d'estimation ────────────────────────────────────────
  # 1. svyglm pondéré (plan complexe) — méthode de référence si pas de séparation
  # 2. Firth pénalisé (logistf ou brglm2) — si quasi-séparation détectée
  # 3. glm classique — repli ultime
  has_logistf <- requireNamespace("logistf", quietly = TRUE)
  has_brglm2  <- requireNamespace("brglm2", quietly = TRUE)
  methode_modele <- "svyglm"

  modele <- tryCatch({
    design_ok <- "poids_final" %in% names(df_mod) &&
                 dplyr::n_distinct(df_mod$aire_sante_id) > 1

    # Les poids de sondage non entiers déclenchent un avertissement bénin
    # "non-integer #successes in a binomial glm!" : l'estimation reste correcte
    # (vraisemblance pondérée). On le neutralise via suppressWarnings.
    suppressWarnings(
    if (quasi_sep && (has_logistf || has_brglm2)) {
      # Régression de Firth pénalisée — corrige le biais de séparation.
      # On l'applique de façon PONDÉRÉE pour rester cohérent avec le plan complexe.
      if (has_brglm2) {
        # brglm2 : Firth (reduced-bias) compatible avec des poids de sondage.
        methode_modele <<- "Firth (brglm2, pondéré)"
        glm(formule, data = df_mod,
            family = binomial(link = "logit"),
            weights = df_mod$poids_final,
            method = brglm2::brglmFit, type = "AS_mean")
      } else {
        # logistf : Firth classique (non pondéré ; on prévient l'utilisateur).
        methode_modele <<- "Firth (logistf, non pondéré)"
        cli_alert_info("logistf ne gère pas les poids ; modèle Firth non pondéré.")
        logistf::logistf(formule, data = df_mod)
      }
    } else if (design_ok) {
      methode_modele <<- "svyglm (pondéré)"
      args <- list(ids = ~aire_sante_id, weights = ~poids_final,
                   data = df_mod, nest = TRUE)
      if ("bloc" %in% names(df_mod) && dplyr::n_distinct(df_mod$bloc) > 1)
        args$strata <- ~bloc
      des <- do.call(survey::svydesign, args)
      survey::svyglm(formule, design = des,
                     family = quasibinomial(link = "logit"))
    } else {
      methode_modele <<- "glm"
      glm(formule, data = df_mod, family = binomial(link = "logit"))
    }
    )
  }, error = function(e) {
    cli_alert_warning("Modèle principal KO ({e$message}), repli glm.")
    methode_modele <<- "glm (repli)"
    tryCatch(glm(formule, data = df_mod, family = binomial(link = "logit")),
             error = function(e2) { cli_alert_danger("glm KO : {e2$message}"); NULL })
  })

  if (!is.null(modele)) {
    # ── Extraction des OR ajustés (compatible svyglm / glm / Firth) ────────────
    extraire_or <- function(mod) {
      # logistf : objet spécifique
      if (inherits(mod, "logistf")) {
        tibble(
          term     = names(coef(mod)),
          estimate = exp(coef(mod)),
          conf.low = exp(mod$ci.lower),
          conf.high= exp(mod$ci.upper),
          p.value  = mod$prob
        )
      } else if (has_broom) {
        broom::tidy(mod, conf.int = TRUE, exponentiate = TRUE)
      } else {
        # repli manuel
        s <- summary(mod)$coefficients
        ci <- suppressMessages(confint.default(mod))
        tibble(term = rownames(s), estimate = exp(s[, 1]),
               conf.low = exp(ci[, 1]), conf.high = exp(ci[, 2]),
               p.value = s[, ncol(s)])
      }
    }

    or_df <- extraire_or(modele) %>%
        filter(term != "(Intercept)") %>%
        mutate(
          significatif = p.value < 0.05,
          direction = case_when(
            estimate > 1 & p.value < 0.05 ~ "Favorise la vaccination",
            estimate < 1 & p.value < 0.05 ~ "Défavorise la vaccination",
            TRUE                          ~ "Non significatif"),
          term_lbl = term %>%
            str_replace("province_clean", "Province : ") %>%
            str_replace("niveau_instruction", "Instruction tuteur : ") %>%
            str_replace("info_campagne_f", "Informé campagne : ") %>%
            str_replace("importance_vaccins_num", "Importance perçue vaccins") %>%
            str_replace("connait_lieu_f", "Connaît lieu vaccination : ") %>%
            str_replace("facilite_paiement_num", "Facilité paiement perçue") %>%
            str_replace("parents_amis_vaccin_f", "Soutien parents/amis : ") %>%
            str_replace("vaccins_souhaites_f", "Vaccins souhaités : ") %>%
            str_replace("milieu", "Milieu de résidence : ") %>%
            str_replace("chef_sexe", "Sexe chef de ménage : ") %>%
            str_replace("chef_age_grp", "Âge chef de ménage : ") %>%
            str_replace("taille_menage_grp", "Taille ménage : ") %>%
            str_replace("sexe", "Sexe enfant : ") %>%
            str_replace("age_groupe", "Âge enfant : ")
        )

      # Tableau exporté
      tab_or <- or_df %>%
        transmute(Variable = term_lbl,
                  `OR ajusté` = round(estimate, 2),
                  `IC 95%` = sprintf("[%.2f - %.2f]", conf.low, conf.high),
                  `p-value` = signif(p.value, 3))
      export_table(
        gt_styled(tab_or, title = "Déterminants de la vaccination (OR ajustés)",
                  subtitle = glue("Méthode : {methode_modele}")) %>%
          gt_color_cells("p-value", `p-value` < 0.05, fill = oms_colors[["vert"]]),
        file.path(PATH_TABLEAUX, "determinants_or_ajustes"),
        raw_data = tab_or)

      # Forest plot
      p_forest <- ggplot(or_df,
             aes(x = estimate, y = reorder(term_lbl, estimate), color = direction)) +
        geom_vline(xintercept = 1, linetype = "dashed",
                   color = oms_colors[["gris"]], linewidth = 0.7) +
        geom_point(size = 3, alpha = 0.9) +
        geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                       orientation = "y", width = 0.3, linewidth = 0.7) +
        geom_text(aes(label = sprintf("%.2f", estimate)),
                  hjust = -0.3, size = 2.8, color = oms_colors[["gris_fonce"]]) +
        scale_x_log10() +
        scale_color_manual(values = c(
          "Favorise la vaccination"   = oms_colors[["vert"]],
          "Défavorise la vaccination" = oms_colors[["rouge"]],
          "Non significatif"          = oms_colors[["gris"]]), name = NULL) +
        labs(title = "Forest plot — Déterminants de la vaccination (OR ajustés)",
             subtitle = glue("Méthode : {methode_modele} | OR > 1 favorise, OR < 1 défavorise (échelle log)"),
             x = "Odds Ratio ajusté (échelle logarithmique)", y = NULL,
             caption = .caption_default()) +
        theme(legend.position = "bottom")
      save_plot(p_forest, file.path(PATH_GRAPHIQUES, "16_forest_plot_determinants.png"),
                width = 11, height = max(6, 0.4 * nrow(or_df) + 2))

      # Indicateurs d'ajustement
      modele_glance <- tryCatch(
        broom::glance(modele) %>% mutate(across(where(is.numeric), ~round(., 2))),
        error = function(e) NULL)
      if (!is.null(modele_glance))
        readr::write_csv(modele_glance,
                         file.path(PATH_TABLEAUX, "modele_ajustement.csv"), na = "")
    cli_alert_success("Régression logistique terminée — méthode : {methode_modele} ({nrow(df_mod)} obs).")
    # Exposer la méthode et les paramètres clés au reporting (note méthodologique)
    writeLines(c(methode_modele, as.character(nrow(df_mod))),
               file.path(PATH_TABLEAUX, ".methode_modele.txt"))
  }
} else {
  cli_alert_warning("Pas assez de données complètes pour la régression logistique.")
}


# -----------------------------------------------------------------------------
# 13g. SOURCES D'INFORMATION  [Module F]
# -----------------------------------------------------------------------------

cli_h1("Sources et canaux d'information")

if ("canal_info_lbl" %in% names(enfant_anal)) {
  canaux <- enfant_anal %>%
    filter(!is.na(canal_info_lbl)) %>%
    count(canal_info_lbl, name = "n") %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(n))

  if (nrow(canaux) > 0) {
    p_canaux <- ggplot(canaux, aes(x = pct, y = reorder(canal_info_lbl, pct))) +
      geom_col(fill = oms_colors[["bleu_who"]], alpha = 0.85, width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%% (n=%s)", 100*pct, comma(n))),
                hjust = -0.1, size = 3.2, color = oms_colors[["gris_fonce"]]) +
      scale_x_continuous(labels = function(x) paste0(round(100*x), "%"),
                         limits = c(0, max(canaux$pct) * 1.25),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Canaux d'information sur la campagne",
           subtitle = "Parmi les répondants ayant déclaré un canal d'information",
           x = "Proportion", y = NULL, caption = .caption_default())
    save_plot(p_canaux, file.path(PATH_GRAPHIQUES, "17_canaux_information.png"),
              width = 11, height = max(5, 0.45 * nrow(canaux) + 2))

    export_table(
      gt_styled(canaux %>% transmute(Canal = canal_info_lbl, Effectif = n,
                                     `Pourcentage (%)` = round(100*pct, 1)),
                title = "Canaux d'information"),
      file.path(PATH_TABLEAUX, "canaux_information"),
      raw_data = canaux)
  }

  # Couverture selon le canal
  cov_canal <- enfant_anal %>%
    filter(!is.na(canal_info_lbl), !is.na(vaccine_bin)) %>%
    group_by(canal_info_lbl) %>%
    summarise(n = n(), couv = mean(vaccine_bin == 1), .groups = "drop") %>%
    filter(n >= 10)
  if (nrow(cov_canal) > 0) {
    cv_ref <- mean(enfant_anal$vaccine_bin, na.rm = TRUE)
    p_cc <- ggplot(cov_canal, aes(x = couv, y = reorder(canal_info_lbl, couv))) +
      geom_col(fill = oms_colors[["vert"]], alpha = 0.85, width = 0.65) +
      geom_text(aes(label = sprintf("%.0f%%", 100*couv)),
                hjust = -0.15, size = 3.2, color = oms_colors[["gris_fonce"]]) +
      geom_vline(xintercept = cv_ref, linetype = "dashed",
                 color = oms_colors[["rouge"]], linewidth = 0.7) +
      scale_x_continuous(labels = function(x) paste0(round(100*x), "%"),
                         limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
      labs(title = "Couverture vaccinale par canal d'information",
           subtitle = "Ligne rouge = taux global ; min. 10 obs. par canal",
           x = "Taux de couverture", y = NULL, caption = .caption_default())
    save_plot(p_cc, file.path(PATH_GRAPHIQUES, "18_couverture_par_canal.png"),
              width = 11, height = max(5, 0.45 * nrow(cov_canal) + 2))

    # Tableau d'accompagnement de la couverture par canal
    export_table(
      gt_styled(cov_canal %>%
                  arrange(desc(couv)) %>%
                  transmute(`Canal d'information` = canal_info_lbl,
                            Effectif = n,
                            `Couverture (%)` = round(100 * couv, 1)),
                title = "Couverture vaccinale par canal d'information",
                subtitle = "Canaux comptant au moins 10 enfants enquêtés"),
      file.path(PATH_TABLEAUX, "couverture_par_canal"),
      raw_data = cov_canal)
  }
}


# -----------------------------------------------------------------------------
# 13h. PROFIL DES VACCINES : depenses + symptomes  [Module G]
# -----------------------------------------------------------------------------

cli_h1("Profil des enfants vaccinés (dépenses, symptômes)")

# Dépenses de vaccination par province
if ("depense_vacc_bin" %in% names(enfant_anal)) {
  dep_prov <- enfant_anal %>%
    filter(!is.na(depense_vacc_bin)) %>%
    group_by(province_clean) %>%
    summarise(n = n(), pct_dep = 100 * mean(depense_vacc_bin == 1), .groups = "drop") %>%
    filter(!is.na(province_clean)) %>%
    arrange(desc(pct_dep))
  if (nrow(dep_prov) > 0) {
    p_dep <- ggplot(dep_prov, aes(x = pct_dep, y = reorder(province_clean, pct_dep))) +
      geom_col(fill = oms_colors[["orange"]], alpha = 0.85, width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", pct_dep)),
                hjust = -0.15, size = 3.2, color = oms_colors[["gris_fonce"]]) +
      scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                         limits = c(0, max(dep_prov$pct_dep) * 1.2),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Dépenses déclarées pour la vaccination, par province",
           subtitle = "Part des ménages déclarant une dépense liée à la vaccination",
           x = "Proportion déclarant une dépense", y = NULL,
           caption = .caption_default())
    save_plot(p_dep, file.path(PATH_GRAPHIQUES, "19_depenses_par_province.png"),
              width = 11, height = max(5, 0.5 * nrow(dep_prov) + 2))
    export_table(
      gt_styled(dep_prov %>% transmute(Province = province_clean, N = n,
                                       `Dépense déclarée (%)` = round(pct_dep, 1)),
                title = "Dépenses de vaccination par province"),
      file.path(PATH_TABLEAUX, "depenses_par_province"),
      raw_data = dep_prov)
  }
}

# Types de dépenses (motifs) — question multi-réponses 4.06b
# Variables depense_items_1..3 + __96 (1/0 après normalisation : 1 = Oui)
items_dep <- c(
  depense_items_1   = "Seringue",
  depense_items_2   = "Carte de vaccination",
  depense_items_3   = "Transport vers le site",
  depense_items__96 = "Autre motif"
)
items_dispo <- intersect(names(items_dep), names(enfant_anal))
if (length(items_dispo) > 0 && "depense_vacc_bin" %in% names(enfant_anal)) {
  # Dénominateur : enfants ayant déclaré une dépense (depense_vacc_bin == 1)
  base_dep <- enfant_anal %>% filter(!is.na(depense_vacc_bin) & depense_vacc_bin == 1)
  n_dep_decl <- nrow(base_dep)
  if (n_dep_decl > 0) {
    depense_items_tbl <- purrr::map_dfr(items_dispo, function(v) {
      val <- as_num(base_dep[[v]])
      # "Oui" = 1 après normalisation XLSForm ; fallback = 2 si non normalisé (1=No/2=Yes)
      oui <- if (any(val == 2, na.rm = TRUE) && !any(val == 0, na.rm = TRUE)) 2 else 1
      tibble(Motif = unname(items_dep[v]),
             n = sum(val == oui, na.rm = TRUE))
    }) %>%
      filter(n > 0) %>%
      mutate(`Pourcentage (%)` = round(100 * n / n_dep_decl, 1)) %>%
      arrange(desc(n))

    if (nrow(depense_items_tbl) > 0) {
      p_dep_items <- ggplot(depense_items_tbl,
                            aes(x = `Pourcentage (%)`, y = reorder(Motif, `Pourcentage (%)`))) +
        geom_col(fill = oms_colors[["orange"]], alpha = 0.85, width = 0.65) +
        geom_text(aes(label = sprintf("%s (%.1f%%)", comma(n), `Pourcentage (%)`)),
                  hjust = -0.1, size = 3.2, color = oms_colors[["gris_fonce"]]) +
        scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                           limits = c(0, max(depense_items_tbl$`Pourcentage (%)`) * 1.25),
                           expand = expansion(mult = c(0, 0))) +
        labs(title = "Motifs des dépenses liées à la vaccination",
             subtitle = glue("Parmi {comma(n_dep_decl)} ménages ayant déclaré une dépense ",
                             "(réponses multiples possibles)"),
             x = "Part des ménages ayant déclaré une dépense", y = NULL,
             caption = .caption_default())
      save_plot(p_dep_items, file.path(PATH_GRAPHIQUES, "19b_depenses_motifs.png"),
                width = 10, height = max(4, 0.6 * nrow(depense_items_tbl) + 2))
      export_table(
        gt_styled(depense_items_tbl %>% transmute(Motif, Effectif = n,
                    `Pourcentage (%)` = `Pourcentage (%)`),
                  title = "Motifs des dépenses liées à la vaccination",
                  subtitle = glue("Base : {comma(n_dep_decl)} ménages ayant déclaré une dépense ; ",
                                  "réponses multiples")),
        file.path(PATH_TABLEAUX, "depenses_motifs"), raw_data = depense_items_tbl)
      cli_alert_success("Motifs de dépenses : {nrow(depense_items_tbl)} catégories ({n_dep_decl} ménages).")
    }
  }
}

# Symptômes post-vaccinaux
if ("measles_symptom_f" %in% names(enfant_anal)) {
  symp <- enfant_anal %>%
    filter(!is.na(measles_symptom_f)) %>%
    count(measles_symptom_f, name = "n") %>%
    mutate(pct = round(100 * n / sum(n), 1))
  if (nrow(symp) > 0) {
    export_table(
      gt_styled(symp %>% rename(`Symptômes signalés` = measles_symptom_f,
                                Effectif = n, `Pourcentage (%)` = pct),
                title = "Symptômes post-vaccinaux signalés"),
      file.path(PATH_TABLEAUX, "symptomes_post_vaccinaux"),
      raw_data = symp)
  }
}

# Liste détaillée des symptômes post-vaccinaux (select_multiple symptoms_1..12, _96)
symptomes_labels <- c(
  symptoms_1 = "Diarrhée", symptoms_2 = "Fièvre",
  symptoms_3 = "Apparition de boutons sur la peau", symptoms_4 = "Convulsion",
  symptoms_5 = "Paralysie", symptoms_6 = "Plaie avec ou sans pus",
  symptoms_7 = "Rougeur", symptoms_8 = "Gonflement au point d'injection",
  symptoms_9 = "Évanouissement", symptoms_10 = "Démangeaisons",
  symptoms_11 = "Maux de tête", symptoms_12 = "Nausées / Vomissements",
  symptoms__96 = "Autres")

symptomes_dispo <- intersect(names(symptomes_labels), names(enfant_anal))
if (length(symptomes_dispo) > 0) {
  # Base : échantillon analytique (enfant_anal), cohérent avec le tableau résumé
  # des symptômes ci-dessus. Le dénominateur est l'ensemble des enfants analysés.
  base_symp <- enfant_anal
  n_base <- nrow(base_symp)
  symp_detail <- purrr::map_dfr(symptomes_dispo, function(v) {
    val <- as_num(base_symp[[v]])
    # 1 = Oui (Yes) selon le codage XLSForm
    nb <- sum(val == 1, na.rm = TRUE)
    tibble(Symptôme = unname(symptomes_labels[v]), n = nb)
  }) %>%
    filter(n > 0) %>%
    mutate(`Pourcentage (%)` = round(100 * n / n_base, 1)) %>%
    arrange(desc(n))
  if (nrow(symp_detail) > 0) {
    p_symp_det <- symp_detail %>%
      ggplot(aes(x = `Pourcentage (%)`, y = reorder(Symptôme, `Pourcentage (%)`))) +
      geom_col(fill = oms_colors[["orange"]], width = 0.7, alpha = 0.9) +
      geom_text(aes(label = paste0(format(`Pourcentage (%)`, decimal.mark = ","), "%")),
                hjust = -0.15, size = 3.1, color = oms_colors[["gris_fonce"]]) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         limits = c(0, max(symp_detail$`Pourcentage (%)`) * 1.2),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Symptômes post-vaccinaux déclarés (détail)",
           subtitle = "Parmi l'ensemble des enfants enquêtés",
           x = "Pourcentage", y = NULL, caption = .caption_default()) +
      theme(panel.grid.major.y = element_blank())
    save_plot(p_symp_det, file.path(PATH_GRAPHIQUES, "21_symptomes_detail.png"),
              width = 10, height = max(5, 0.45 * nrow(symp_detail) + 2))
    export_table(
      gt_styled(symp_detail %>% rename(Effectif = n),
                title = "Liste des symptômes post-vaccinaux déclarés",
                subtitle = "Symptômes spécifiques signalés par les répondants"),
      file.path(PATH_TABLEAUX, "symptomes_detail"),
      raw_data = symp_detail)
    cli_alert_success("Liste détaillée des symptômes générée ({nrow(symp_detail)} symptômes).")
  }
}


# -----------------------------------------------------------------------------
# 13i. LIEU DE VACCINATION  [Module G1]
# -----------------------------------------------------------------------------

cli_h1("Lieu de vaccination")

if ("lieu_vaccination_lbl" %in% names(enfant_anal)) {
  lieu_df <- enfant_anal %>%
    filter(!is.na(lieu_vaccination_lbl), vaccine_bin == 1) %>%
    count(lieu_vaccination_lbl, name = "n") %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(n))
  if (nrow(lieu_df) > 0) {
    p_lieu <- ggplot(lieu_df, aes(x = pct, y = reorder(lieu_vaccination_lbl, pct))) +
      geom_col(fill = oms_colors[["bleu_fonce"]], alpha = 0.85, width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%% (n=%s)", 100*pct, comma(n))),
                hjust = -0.1, size = 3.2, color = oms_colors[["gris_fonce"]]) +
      scale_x_continuous(labels = function(x) paste0(round(100*x), "%"),
                         limits = c(0, max(lieu_df$pct) * 1.25),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = "Lieu de vaccination lors de la campagne",
           subtitle = "Parmi les enfants vaccinés durant la campagne",
           x = "Proportion", y = NULL, caption = .caption_default())
    save_plot(p_lieu, file.path(PATH_GRAPHIQUES, "20_lieu_vaccination.png"),
              width = 10, height = max(5, 0.5 * nrow(lieu_df) + 2))
    export_table(
      gt_styled(lieu_df %>% transmute(`Lieu` = lieu_vaccination_lbl, Effectif = n,
                                      `Pourcentage (%)` = round(100*pct, 1)),
                title = "Lieu de vaccination"),
      file.path(PATH_TABLEAUX, "lieu_vaccination"),
      raw_data = lieu_df)
  }
}


# -----------------------------------------------------------------------------
# 13j. TABLEAU DE BORD DES INDICATEURS CLES (KPI)  [Synthèse]
# -----------------------------------------------------------------------------

cli_h1("Tableau de bord des indicateurs clés")

.safe_mean_oui <- function(var, code_oui = 1) {
  if (!var %in% names(enfant_anal)) return(NA_real_)
  x <- as_num(enfant_anal[[var]])
  if (all(is.na(x))) return(NA_real_)
  100 * mean(x == code_oui, na.rm = TRUE)
}

kpis <- tibble(
  indicateur = c("Couverture vaccinale globale",
                 "Taux d'information pré-campagne",
                 "Cartes de vaccination reçues",
                 "Symptômes post-vaccinaux signalés"),
  valeur = c(
    # Utiliser la couverture PONDÉRÉE (cv_global_pct = 95.5%) comme valeur de référence.
    # La moyenne brute ne tient pas compte du plan de sondage et peut tomber sous 95 %.
    if (exists("cv_global_pct") && !is.na(cv_global_pct)) cv_global_pct
    else 100 * mean(enfant_anal$vaccine_bin == 1, na.rm = TRUE),
    .safe_mean_oui("info_campagne"),
    .safe_mean_oui("recu_carte"),
    .safe_mean_oui("measles_symptom")
  ),
  cible = c(95, 90, 85, 5),
  sens  = c("haut", "haut", "haut", "bas")
) %>%
  filter(!is.na(valeur)) %>%
  mutate(atteinte = if_else(sens == "haut", valeur >= cible, valeur <= cible))

if (nrow(kpis) > 0) {
  p_kpi <- ggplot(kpis, aes(x = reorder(indicateur, valeur), y = valeur)) +
    geom_col(aes(fill = atteinte), width = 0.6, alpha = 0.9) +
    geom_point(aes(y = cible), shape = 18, size = 5, color = oms_colors[["orange"]]) +
    geom_text(aes(label = sprintf("%.1f%%", valeur)),
              hjust = -0.2, size = 3.5, fontface = "bold",
              color = oms_colors[["gris_fonce"]]) +
    scale_fill_manual(values = c("TRUE" = oms_colors[["vert"]],
                                 "FALSE" = oms_colors[["rouge"]]),
                      labels = c("TRUE" = "Cible atteinte", "FALSE" = "Cible non atteinte"),
                      name = NULL) +
    scale_y_continuous(limits = c(0, 115), expand = expansion(mult = c(0, 0))) +
    coord_flip() +
    labs(title = "Indicateurs clés de performance (KPI)",
         subtitle = "Losange orange = valeur cible ; vert = cible atteinte",
         x = NULL, y = "Valeur observée (%)",
         caption = .caption_default()) +
    theme(legend.position = "bottom")
  save_plot(p_kpi, file.path(PATH_GRAPHIQUES, "21_kpi_dashboard.png"),
            width = 10, height = 5.5)
  export_table(
    gt_styled(kpis %>% transmute(Indicateur = indicateur,
                                 `Valeur (%)` = round(valeur, 1),
                                 `Cible (%)` = cible,
                                 Statut = if_else(atteinte,
                                                  "Atteinte", "Non atteinte")),
              title = "Indicateurs clés de performance") %>%
      gt_color_cells("Statut", Statut == "Atteinte", fill = oms_colors[["vert"]]) %>%
      gt_color_cells("Statut", Statut == "Non atteinte", fill = oms_colors[["rouge"]]),
    file.path(PATH_TABLEAUX, "kpi_dashboard"),
    raw_data = kpis)
}




# -----------------------------------------------------------------------------
# 14. CARTE CHOROPLETHE PAR PROVINCE (optionnel - dépend de sf + shapefile)
# -----------------------------------------------------------------------------

cli_h1("Carte choroplèthe de la couverture")

shapefile_path <- "shapefiles/gadm41_COD_1.shp"
if (has_sf && file.exists(shapefile_path) && exists("cv_prov_df")) {
  tryCatch({
    rdc <- sf::st_read(shapefile_path, quiet = TRUE)
    rdc <- rdc %>%
      mutate(
        province_match = case_when(
          NAME_1 == "Kasaï"          ~ "Kasai",
          NAME_1 == "Kasaï Central"  ~ "Kasai Central",
          NAME_1 == "Kasaï Oriental" ~ "Kasai Oriental",
          NAME_1 == "Équateur"       ~ "Equateur",
          NAME_1 == "Tanganyika"     ~ "Tanganyka",
          NAME_1 == "Haut-Katanga"   ~ "Haut Katanga",
          NAME_1 == "Haut-Lomami"    ~ "Haut Lomami",
          NAME_1 == "Haut-Uele"      ~ "Haut Uele",
          NAME_1 == "Bas-Uele"       ~ "Bas Uele",
          TRUE                        ~ NAME_1
        )
      ) %>%
      left_join(cv_prov_df %>% select(Province, pct, pct_bas, pct_haut),
                by = c("province_match" = "Province"))

    # Centroïdes pour le placement des étiquettes (évite les chevauchements)
    rdc_lab <- rdc %>%
      mutate(.cx = sf::st_coordinates(suppressWarnings(
                     sf::st_point_on_surface(sf::st_zm(geometry))))[, 1],
             .cy = sf::st_coordinates(suppressWarnings(
                     sf::st_point_on_surface(sf::st_zm(geometry))))[, 2])
    lab_enq <- rdc_lab %>% filter(!is.na(pct))   # provinces enquêtées
    lab_oth <- rdc_lab %>% filter(is.na(pct))    # autres provinces

    p_carte <- ggplot(rdc) +
      geom_sf(aes(fill = pct), color = "white", linewidth = 0.4)
    # Noms discrets des provinces non enquêtées (petits, gris, sans chevauchement)
    if (has_ggrepel && nrow(lab_oth) > 0) {
      p_carte <- p_carte +
        ggrepel::geom_text_repel(
          data = lab_oth, aes(x = .cx, y = .cy, label = province_match),
          size = 2.3, color = oms_colors[["gris"]], fontface = "plain",
          min.segment.length = 0, segment.size = 0.2,
          segment.color = oms_colors[["gris_clair"]],
          max.overlaps = Inf, seed = 42, box.padding = 0.3)
    }
    # Étiquettes des provinces enquêtées (encadré blanc + couverture)
    if (has_ggrepel && nrow(lab_enq) > 0) {
      p_carte <- p_carte +
        ggrepel::geom_label_repel(
          data = lab_enq,
          aes(x = .cx, y = .cy,
              label = sprintf("%s\n%s%%", province_match,
                              format(round(pct, 1), decimal.mark = ","))),
          size = 3.5, color = oms_colors[["bleu_fonce"]], fontface = "bold",
          lineheight = 0.9, fill = scales::alpha("white", 0.85),
          label.size = 0.2, label.padding = unit(0.18, "lines"),
          min.segment.length = 0, segment.size = 0.3,
          segment.color = oms_colors[["gris_fonce"]],
          max.overlaps = Inf, seed = 42, box.padding = 0.4)
    } else {
      # Repli sans ggrepel
      p_carte <- p_carte +
        geom_sf_label(aes(label = ifelse(is.na(pct), province_match,
                                    sprintf("%s\n%s%%", province_match,
                                            format(round(pct, 1), decimal.mark = ",")))),
                     size = 3.2, color = oms_colors[["bleu_fonce"]],
                     fontface = "bold", lineheight = 0.9,
                     fill = scales::alpha("white", 0.78),
                     label.size = 0, label.padding = unit(0.12, "lines"))
    }
    p_carte <- p_carte +
      scale_fill_gradientn(
        colors = c("#BD0026", "#FC4E2A", "#FEB24C", "#FFEDA0",
                   "#A6D96A", "#33A02C"),
        values = scales::rescale(c(0, 50, 70, 80, 90, 100)),
        limits = c(0, 100), na.value = "grey90", name = "Couverture (%)",
        guide = guide_colorbar(title.position = "top", barwidth = 16,
                               barheight = 0.9, label.position = "bottom")
      ) +
      labs(title = "Couverture vaccinale Rougeole-Rubéole par province",
           subtitle = "République Démocratique du Congo - ECP 2025-2026",
           caption = .caption_default()) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                                      color = oms_colors[["bleu_fonce"]],
                                      margin = margin(b = 6)),
            plot.subtitle = element_text(hjust = 0.5, color = oms_colors[["gris"]],
                                         margin = margin(b = 12)),
            plot.caption = element_text(size = 9, color = oms_colors[["gris"]],
                                        hjust = 1, margin = margin(t = 8)),
            legend.position = "bottom", legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 11),
            plot.margin = margin(15, 15, 15, 15),
            plot.background = element_rect(fill = "white", color = NA))
    suppressWarnings(
      save_plot(p_carte, file.path(PATH_CARTES, "carte_cv_par_province.png"),
                width = 11, height = 11))
  }, error = function(e) {
    cli_alert_warning("Carte non générée : {e$message}")
  })
} else {
  cli_alert_info(
    "Carte non générée (sf disponible: {has_sf}, shapefile présent: {file.exists(shapefile_path)})"
  )
}


# -----------------------------------------------------------------------------
# 15. CONCORDANCE BACKCHECK (KAPPA DE COHEN)
# -----------------------------------------------------------------------------
# Méthodologie :
# - On joint l'enquête principale et le backcheck sur `caseid`.
# - Pour chaque variable, on calcule l'accord observé en % et le Kappa de
#   Cohen (irr::kappa2) avec IC 95% asymptotique.
# - Échelle d'interprétation de Landis & Koch (1977).
# - Classification par type de variable (T1 stable, T2 connaissances, T3 autre)
#   avec seuils d'alerte (5%, 15%, 25%).
# -----------------------------------------------------------------------------

cli_h1("Concordance backcheck (Kappa de Cohen)")

# Interprétation Landis & Koch
interpreter_kappa <- function(k) {
  case_when(
    is.na(k)   ~ NA_character_,
    k < 0      ~ "Pauvre",
    k <= 0.20  ~ "Léger",
    k <= 0.40  ~ "Passable",
    k <= 0.60  ~ "Modéré",
    k <= 0.80  ~ "Substantiel",
    TRUE       ~ "Presque parfait"
  )
}

# Types de variables : T1 (très stable), T2 (assez stable), T3 (sensible)
type_variable <- function(var) {
  case_when(
    var %in% c("sex", "caregiver_sex", "hhh_sex", "caregiver_maritalStatus") ~ "T1",
    var %in% c("hh_eligible", "count_eligibles", "child_eligible",
               "importance_vaccins", "connait_lieu_vaccination",
               "facilite_paiement", "caregiver_nivins")             ~ "T2",
    TRUE                                                            ~ "T3"
  )
}

# Mode pour agréger les éventuels doublons (mêmes caseid)
mode_func <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Fonction principale : calcule un tableau (Variable / N / Accord / Kappa / IC / Interpretation)
calculer_concordance <- function(main_df, bc_df, vars,
                                 id_var = "caseid", entite = "enfant") {
  if (is.null(main_df) || is.null(bc_df)) {
    cli_alert_warning("Données manquantes pour la concordance ({entite}).")
    return(NULL)
  }
  vars <- intersect(vars, intersect(names(main_df), names(bc_df)))
  if (length(vars) == 0) {
    cli_alert_warning("Aucune variable commune ({entite}).")
    return(NULL)
  }

  # Conversion en numérique pour comparer sur les codes
  to_num_safe <- function(d, v) as_num(d[[v]])

  m_u <- main_df %>%
    group_by(.id = .data[[id_var]]) %>%
    summarise(across(all_of(vars), ~ mode_func(as_num(.x))), .groups = "drop") %>%
    mutate(across(all_of(vars), ~ as.numeric(haven::zap_labels(.x))))
  b_u <- bc_df %>%
    group_by(.id = .data[[id_var]]) %>%
    summarise(across(all_of(vars), ~ mode_func(as_num(.x))), .groups = "drop") %>%
    mutate(across(all_of(vars), ~ as.numeric(haven::zap_labels(.x))))

  jdf <- suppressWarnings(
    inner_join(m_u, b_u, by = ".id", suffix = c("_main", "_bc")))
  if (nrow(jdf) == 0) {
    cli_alert_warning("Aucun cas commun ({entite}).")
    return(NULL)
  }
  cli_alert_info("Cas communs ({entite}) : {nrow(jdf)}")

  out <- map_dfr(vars, function(v) {
    a <- jdf[[paste0(v, "_main")]]
    b <- jdf[[paste0(v, "_bc")]]
    ok <- !is.na(a) & !is.na(b)
    n_c <- sum(ok)
    if (n_c < 2) return(NULL)

    accord <- mean(a[ok] == b[ok])
    kr <- tryCatch(
      suppressWarnings(irr::kappa2(cbind(a[ok], b[ok]))),
      error   = function(e) list(value = NA, statistic = NA, p.value = NA),
      warning = function(w) list(value = NA, statistic = NA, p.value = NA)
    )
    k  <- if (is.null(kr$value) || (length(kr$value) == 1 && is.nan(kr$value))) NA else kr$value
    # SE asymptotique de Kappa (Fleiss & al.) : approximation via les marges
    # de la table 2x2 ou plus. Pour un IC pragmatique on utilise la formule
    # se(kappa) ~ sqrt(p0*(1-p0) / (n*(1-pe)^2)) (formule de Fleiss simplifiée).
    se_k <- tryCatch({
      # Table carrée : on impose les mêmes niveaux (union) aux deux évaluateurs,
      # sinon rowSums/colSums ont des longueurs différentes (warning).
      lev <- sort(union(unique(a[ok]), unique(b[ok])))
      tab <- table(factor(a[ok], levels = lev), factor(b[ok], levels = lev))
      n   <- sum(tab)
      p0  <- sum(diag(tab)) / n
      pe  <- sum(rowSums(tab) * colSums(tab)) / (n^2)
      if (pe < 1) sqrt(p0 * (1 - p0) / (n * (1 - pe)^2)) else NA_real_
    }, error = function(e) NA_real_)
    tibble(
      Variable        = v,
      Type            = type_variable(v),
      N_comparaisons  = n_c,
      Accord_pct      = round(100 * accord, 1),
      Kappa           = if (is.na(k)) NA_real_ else round(k, 3),
      IC95_bas        = if (is.na(k) || is.na(se_k)) NA_real_
                        else round(k - 1.96 * se_k, 3),
      IC95_haut       = if (is.na(k) || is.na(se_k)) NA_real_
                        else round(k + 1.96 * se_k, 3),
      Interpretation  = interpreter_kappa(k)
    )
  })
  out
}

vars_enfant <- c("sex", "caregiver_sex", "caregiver_nivins",
                 "caregiver_maritalStatus",
                 "importance_vaccins", "connait_lieu_vaccination",
                 "facilite_paiement",
                 "vaccin_anterieur", "nombre_doses", "vaccine_campagne",
                 "recu_carte", "vu_carte", "finger_marked", "vitamine_a",
                 "lieu_vaccination", "depense_vaccination",
                 "raison_pas_vaccine", "child_eligible")

vars_menage <- c("consent", "hhh_sex", "count_eligibles", "hh_eligible")

concordance_enf <- calculer_concordance(enfant_main, enfant_backcheck,
                                        vars_enfant, "caseid", "enfant")
concordance_men <- calculer_concordance(menage_main, menage_backcheck,
                                        vars_menage, "caseid", "menage")

# Sauvegarde + visualisation
if (!is.null(concordance_enf) && nrow(concordance_enf) > 0) {
  export_table(
    gt_styled(concordance_enf %>%
                mutate(`IC 95% Kappa` = ifelse(
                  is.na(Kappa), NA_character_,
                  sprintf("[%.3f - %.3f]", IC95_bas, IC95_haut))) %>%
                select(Variable, Type, N_comparaisons, Accord_pct,
                       Kappa, `IC 95% Kappa`, Interpretation),
              title = "Concordance enquête principale / backcheck - Enfants",
              subtitle = "Pourcentage d'accord et Kappa de Cohen") %>%
      gt_color_cells("Interpretation",
                     Interpretation == "Presque parfait",
                     fill = oms_colors[["vert"]]) %>%
      gt_color_cells("Interpretation",
                     Interpretation %in% c("Pauvre", "Léger", "Passable"),
                     fill = oms_colors[["rouge"]]),
    file.path(PATH_TABLEAUX, "concordance_enfant"),
    raw_data = concordance_enf
  )

  # Graphique en barres - accord %
  p_concord <- concordance_enf %>%
    mutate(short = str_wrap(Variable, 20)) %>%
    ggplot(aes(x = Accord_pct, y = reorder(short, Accord_pct), fill = Type)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_vline(xintercept = c(75, 85, 95),
               linetype = "dashed", color = oms_colors[["gris"]],
               linewidth = 0.4, alpha = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", Accord_pct)),
              hjust = -0.15, size = 3.2, color = oms_colors[["gris_fonce"]]) +
    scale_fill_manual(
      values = c("T1" = oms_colors[["bleu_who"]],
                 "T2" = oms_colors[["orange"]],
                 "T3" = oms_colors[["violet"]]),
      labels = c("T1" = "Stable (T1)",
                 "T2" = "Connaissances (T2)",
                 "T3" = "Sensible (T3)"),
      name = "Type"
    ) +
    scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Taux d'accord par variable (Enquête principale vs Backcheck)",
      subtitle = "Enquête enfant - Seuils : 75% (minimum), 85% (bon), 95% (excellent)",
      x = "Pourcentage d'accord", y = NULL,
      caption = .caption_default()
    )
  save_plot(p_concord, file.path(PATH_GRAPHIQUES, "10_concordance_enfant.png"),
            width = 11, height = max(6, 0.35 * nrow(concordance_enf) + 2))

  # Graphique Kappa avec interprétation Landis & Koch
  p_kappa <- concordance_enf %>%
    filter(!is.na(Kappa)) %>%
    mutate(short = str_wrap(Variable, 20)) %>%
    ggplot(aes(x = Kappa, y = reorder(short, Kappa), fill = Kappa)) +
    geom_col(width = 0.7, alpha = 0.95) +
    geom_errorbar(aes(xmin = pmax(-1, IC95_bas), xmax = pmin(1, IC95_haut)),
                   orientation = "y",
                   width = 0.25, color = oms_colors[["gris_fonce"]],
                   linewidth = 0.35) +
    geom_vline(xintercept = c(0.21, 0.41, 0.61, 0.81),
               linetype = "dotted", color = oms_colors[["gris"]],
               alpha = 0.7) +
    geom_text(aes(label = sprintf("%.2f", Kappa)),
              hjust = -0.15, size = 3.1, color = oms_colors[["gris_fonce"]]) +
    scale_fill_gradient2(low = oms_colors[["rouge"]], mid = oms_colors[["jaune"]],
                         high = oms_colors[["vert"]], midpoint = 0.5,
                         limits = c(-0.1, 1)) +
    scale_x_continuous(limits = c(-0.1, 1.15),
                       breaks = seq(0, 1, 0.2),
                       expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Coefficient Kappa de Cohen par variable",
      subtitle = "Échelle de Landis & Koch (0.21 Passable, 0.41 Modéré, 0.61 Substantiel, 0.81 Presque parfait)",
      x = "Kappa", y = NULL, fill = "Kappa",
      caption = .caption_default()
    ) +
    theme(legend.position = "none")
  save_plot(p_kappa, file.path(PATH_GRAPHIQUES, "11_kappa_enfant.png"),
            width = 11, height = max(6, 0.35 * nrow(concordance_enf) + 2))
}

if (!is.null(concordance_men) && nrow(concordance_men) > 0) {
  export_table(
    gt_styled(concordance_men %>%
                mutate(`IC 95% Kappa` = ifelse(
                  is.na(Kappa), NA_character_,
                  sprintf("[%.3f - %.3f]", IC95_bas, IC95_haut))) %>%
                select(Variable, Type, N_comparaisons, Accord_pct,
                       Kappa, `IC 95% Kappa`, Interpretation),
              title = "Concordance enquête principale / backcheck - Ménages",
              subtitle = "Pourcentage d'accord et Kappa de Cohen"),
    file.path(PATH_TABLEAUX, "concordance_menage"),
    raw_data = concordance_men
  )
}


# -----------------------------------------------------------------------------
# 16. SYNTHESE QUALITE DES DONNEES PAR TYPE DE VARIABLE
# -----------------------------------------------------------------------------

cli_h1("Synthèse qualité des données par type de variable")

if (!is.null(concordance_enf) && nrow(concordance_enf) > 0) {
  taux_erreur <- concordance_enf %>%
    group_by(Type) %>%
    summarise(
      `Nb variables`         = n(),
      `Accord moyen (%)`     = round(mean(Accord_pct, na.rm = TRUE), 1),
      `Taux d'erreur moyen (%)` = round(100 - mean(Accord_pct, na.rm = TRUE), 1),
      `Kappa moyen`          = round(mean(Kappa, na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    mutate(
      `Seuil acceptable (%)` = case_when(
        Type == "T1" ~ 5,
        Type == "T2" ~ 15,
        Type == "T3" ~ 25
      ),
      Statut = if_else(`Taux d'erreur moyen (%)` <= `Seuil acceptable (%)`,
                       "Conforme", "À surveiller")
    )

  export_table(
    gt_styled(taux_erreur,
              title = "Qualité des données par type de variable",
              subtitle = "Taux d'erreur moyen vs seuil acceptable") %>%
      gt_color_cells("Statut", Statut == "Conforme",
                     fill = oms_colors[["vert"]]) %>%
      gt_color_cells("Statut", Statut == "À surveiller",
                     fill = oms_colors[["rouge"]]),
    file.path(PATH_TABLEAUX, "qualite_par_type"),
    raw_data = taux_erreur
  )
}


# =============================================================================
# 16b. SORTIES SUPPLEMENTAIRES (ANNEXES) — distributions et couvertures
# =============================================================================

cli_h1("Sorties supplémentaires pour annexes")

# -----------------------------------------------------------------------------
# Helper : distribution d'une variable catégorielle PAR PROVINCE + ENSEMBLE,
#          en % avec IC 95% (Wilson). Retourne un tibble long prêt à exporter.
# -----------------------------------------------------------------------------
distribution_prov <- function(data, var, var_label) {
  if (!var %in% names(data)) return(NULL)
  d <- data %>% filter(!is.na(.data[[var]]), !is.na(province_clean))
  if (nrow(d) == 0) return(NULL)

  # Par province + ensemble
  calc <- function(sub, zone) {
    tot <- nrow(sub)
    sub %>%
      count(modalite = .data[[var]]) %>%
      rowwise() %>%
      mutate(
        Zone = zone, N = tot,
        pct = 100 * n / tot,
        ic  = list(stats::prop.test(n, tot, correct = FALSE)$conf.int),
        ic_bas = 100 * ic[[1]], ic_haut = 100 * ic[[2]]
      ) %>% ungroup() %>%
      select(Zone, modalite, n, N, pct, ic_bas, ic_haut)
  }
  par_prov <- d %>% group_split(province_clean) %>%
    map_dfr(~ calc(.x, as.character(.x$province_clean[1])))
  ensemble <- calc(d, "Ensemble")
  res <- bind_rows(par_prov, ensemble) %>%
    mutate(Variable = var_label,
           modalite = as.character(modalite))
  res
}

# Helper : export d'une distribution en tableau large (modalités en colonnes %)
exporter_distribution <- function(dist, basename, titre) {
  if (is.null(dist) || nrow(dist) == 0) return(invisible(NULL))
  # Tableau % (1 décimale, virgule) avec IC en sous-ligne
  large <- dist %>%
    mutate(cellule = paste0(fmt_pct1(pct), "% ", fmt_ic(ic_bas, ic_haut))) %>%
    select(Zone, modalite, cellule) %>%
    pivot_wider(names_from = modalite, values_from = cellule) %>%
    arrange(Zone != "Ensemble", Zone)
  export_table(
    gt_styled(large, title = titre,
              subtitle = "Pourcentages pondérés (1 décimale) avec IC 95% Wilson"),
    file.path(PATH_TABLEAUX, basename), raw_data = dist)
  invisible(large)
}

# Helper : graphique en barres empilées (distribution par province)
graphe_distribution <- function(dist, titre, fichier) {
  if (is.null(dist) || nrow(dist) == 0) return(invisible(NULL))
  n_mod <- dplyr::n_distinct(dist$modalite)
  # Palette adaptée au nombre de modalités (interpolation si > base)
  pal <- if (n_mod <= length(palette_oms_cat)) {
    unname(palette_oms_cat)[seq_len(n_mod)]
  } else {
    grDevices::colorRampPalette(unname(palette_oms_cat))(n_mod)
  }
  g <- dist %>%
    mutate(Zone = factor(Zone, levels = c(setdiff(sort(unique(Zone)), "Ensemble"),
                                          "Ensemble"))) %>%
    ggplot(aes(x = Zone, y = pct, fill = modalite)) +
    geom_col(width = 0.7, color = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(pct >= 7, paste0(fmt_pct1(pct), "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 2.7, color = "white", fontface = "bold") +
    scale_fill_manual(values = pal, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_flip() +
    labs(title = titre, subtitle = "Répartition (%) par province et ensemble",
         x = NULL, y = "Pourcentage", caption = .caption_default()) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = max(2, ceiling(n_mod / 4)), byrow = TRUE))
  save_plot(g, file.path(PATH_GRAPHIQUES, fichier),
            width = 11, height = max(5.5, 0.45 * dplyr::n_distinct(dist$Zone) + 2))
  invisible(g)
}

# --- Distribution : importance perçue de la vaccination ----------------------
dist_importance <- distribution_prov(enfant_anal, "importance_vaccins_f",
                                     "Importance perçue de la vaccination")
exporter_distribution(dist_importance, "annexe_importance_vaccins_province",
  "Importance perçue de la vaccination, par province")
graphe_distribution(dist_importance,
  "Importance perçue de la vaccination", "A01_importance_vaccins_province.png")

# --- Distribution : pression sociale (parents/amis) --------------------------
dist_pression <- distribution_prov(enfant_anal, "parents_amis_vaccin_f",
                                   "Parents/amis favorables à la vaccination")
exporter_distribution(dist_pression, "annexe_parents_amis_vaccin_province",
  "Parents et amis proches favorables à la vaccination, par province")
graphe_distribution(dist_pression,
  "Parents/amis favorables à la vaccination", "A02_parents_amis_province.png")

# --- Distribution : vaccins souhaités ----------------------------------------
dist_souhaits <- distribution_prov(enfant_anal, "vaccins_souhaites_f",
                                   "Vaccins souhaités pour l'enfant")
exporter_distribution(dist_souhaits, "annexe_vaccins_souhaites_province",
  "Vaccins souhaités par le tuteur pour l'enfant, par province")
graphe_distribution(dist_souhaits,
  "Vaccins souhaités pour l'enfant", "A03_vaccins_souhaites_province.png")


# -----------------------------------------------------------------------------
# Couverture vaccinale selon les caractéristiques socio-démographiques
# -----------------------------------------------------------------------------
# Helper : couverture (%) + IC95 par modalité d'une variable + ligne Ensemble
couverture_par_carac <- function(data, vars_labels) {
  res <- purrr::imap_dfr(vars_labels, function(lbl, var) {
    if (!var %in% names(data)) return(NULL)
    cv <- suppressWarnings(estim_prop(data, "vaccine_bin", by = var))
    if (is.null(cv)) return(NULL)
    cv %>% transmute(
      Caractéristique = lbl,
      Modalité = as.character(groupe),
      N = n,
      `Couverture (%)` = 100 * prop,
      ic_bas = 100 * ic_bas, ic_haut = 100 * ic_haut)
  })
  # Ligne Ensemble
  cvg <- suppressWarnings(estim_prop(data, "vaccine_bin"))
  if (!is.null(cvg)) {
    res <- bind_rows(res, tibble(
      Caractéristique = "Ensemble", Modalité = "Tous les enfants",
      N = cvg$n, `Couverture (%)` = 100 * cvg$prop,
      ic_bas = 100 * cvg$ic_bas, ic_haut = 100 * cvg$ic_haut))
  }
  res
}

# Caractéristiques du CHEF DE MENAGE
cv_chef <- couverture_par_carac(enfant_anal, c(
  chef_sexe          = "Sexe du chef de ménage",
  chef_age_grp       = "Âge du chef de ménage",
  taille_menage_grp  = "Taille du ménage"))
if (!is.null(cv_chef) && nrow(cv_chef) > 0) {
  export_table(
    gt_styled(cv_chef %>% transmute(Caractéristique, Modalité, N = fmt_n(N),
                `Couverture (%)` = fmt_pct1(`Couverture (%)`),
                `IC 95%` = fmt_ic(ic_bas, ic_haut)),
              title = "Couverture vaccinale selon les caractéristiques du chef de ménage",
              subtitle = "Estimations pondérées, IC 95%"),
    file.path(PATH_TABLEAUX, "annexe_couverture_chef_menage"), raw_data = cv_chef)
}

# Caractéristiques du TUTEUR (répondant)
cv_tuteur <- couverture_par_carac(enfant_anal, c(
  caregiver_sex_f        = "Sexe du tuteur",
  niveau_instruction     = "Niveau d'instruction du tuteur",
  situation_matrimoniale = "Situation matrimoniale du tuteur"))
if (!is.null(cv_tuteur) && nrow(cv_tuteur) > 0) {
  export_table(
    gt_styled(cv_tuteur %>% transmute(Caractéristique, Modalité, N = fmt_n(N),
                `Couverture (%)` = fmt_pct1(`Couverture (%)`),
                `IC 95%` = fmt_ic(ic_bas, ic_haut)),
              title = "Couverture vaccinale selon les caractéristiques du tuteur",
              subtitle = "Estimations pondérées, IC 95%"),
    file.path(PATH_TABLEAUX, "annexe_couverture_tuteur"), raw_data = cv_tuteur)
}


# -----------------------------------------------------------------------------
# Tableau UNIQUE : couverture selon caractéristiques de l'enfant, par province
# -----------------------------------------------------------------------------
# Pour chaque caractéristique (sexe, âge) : couverture par province + ensemble
couverture_enfant_prov <- function(data) {
  carac <- list(
    "Sexe" = "sexe",
    "Tranche d'âge" = "tranche_age")
  provinces <- sort(unique(na.omit(as.character(data$province_clean))))
  # Couverture pondérée par cellule (svyciprop via estim_prop, repli moyenne simple)
  cov_cell <- function(sub) {
    sub <- sub %>% filter(!is.na(vaccine_bin))
    if (nrow(sub) < 1) return(NA_real_)
    # Cellules à couverture 0% ou 100% : la moyenne simple suffit et évite les
    # avertissements de non-convergence des modèles sur données séparées.
    m <- mean(sub$vaccine_bin == 1)
    if (m == 0 || m == 1 || nrow(sub) < 10) return(100 * m)
    est <- suppressWarnings(tryCatch(estim_prop(sub, "vaccine_bin"),
                                     error = function(e) NULL))
    if (!is.null(est) && nrow(est) >= 1 && !is.na(est$prop[1]))
      100 * est$prop[1] else 100 * m
  }
  out <- purrr::imap_dfr(carac, function(var, lbl) {
    if (!var %in% names(data)) return(NULL)
    mods <- levels(factor(data[[var]]))
    purrr::map_dfr(mods, function(m) {
      sub_all <- data %>% filter(as.character(.data[[var]]) == m)
      row <- tibble(Caractéristique = lbl, Modalité = m)
      for (p in provinces) {
        sp <- sub_all %>% filter(as.character(province_clean) == p)
        v <- cov_cell(sp)
        row[[p]] <- if (is.na(v)) "—" else paste0(fmt_pct1(v), " %")
      }
      v <- cov_cell(sub_all)
      row[["Ensemble"]] <- if (is.na(v)) "—" else paste0(fmt_pct1(v), " %")
      row
    })
  })
  out
}
cv_enfant_prov <- couverture_enfant_prov(enfant_anal)
if (!is.null(cv_enfant_prov) && nrow(cv_enfant_prov) > 0) {
  export_table(
    gt_styled(cv_enfant_prov,
              title = "Couverture vaccinale (%) selon les caractéristiques de l'enfant, par province",
              subtitle = "Couverture pondérée en pourcentage (1 décimale) ; colonne Ensemble = national"),
    file.path(PATH_TABLEAUX, "annexe_couverture_enfant_province"),
    raw_data = cv_enfant_prov)
}


# -----------------------------------------------------------------------------
# Versions PAR PROVINCE (+ ensemble) : canaux info, confirmation carte, lieu vacc
# -----------------------------------------------------------------------------
# Canaux d'information
dist_canal <- distribution_prov(enfant_anal, "canal_info_lbl", "Canal d'information")
exporter_distribution(dist_canal, "canaux_information_province",
  "Canaux d'information, par province")
graphe_distribution(dist_canal, "Canaux d'information",
  "17b_canaux_information_province.png")

# Confirmation par carte
dist_carte <- distribution_prov(enfant_anal, "confirmation_carte", "Confirmation par carte")
exporter_distribution(dist_carte, "confirmation_carte_province",
  "Confirmation de la vaccination par carte, par province")
graphe_distribution(dist_carte, "Confirmation par carte",
  "08c_confirmation_carte_province.png")

# Lieu de vaccination
if ("lieu_vaccination_lbl" %in% names(enfant_anal)) {
  dist_lieu <- distribution_prov(
    enfant_anal %>% filter(vaccine_bin == 1),
    "lieu_vaccination_lbl", "Lieu de vaccination")
  exporter_distribution(dist_lieu, "lieu_vaccination_province",
    "Lieu de vaccination, par province")
  graphe_distribution(dist_lieu, "Lieu de vaccination",
    "20b_lieu_vaccination_province.png")
}


# -----------------------------------------------------------------------------
# Caractéristiques du ménage (section descriptive)
# -----------------------------------------------------------------------------
cli_h1("Caractéristiques du ménage")

if ("chef_sexe" %in% names(enfant_anal)) {
  # Distribution des caractéristiques ménage (au niveau enfant, pondéré simple)
  carac_menage <- bind_rows(
    enfant_anal %>% filter(!is.na(chef_sexe)) %>% count(Caractéristique = "Sexe du chef de ménage", Modalité = as.character(chef_sexe)),
    enfant_anal %>% filter(!is.na(chef_age_grp)) %>% count(Caractéristique = "Âge du chef de ménage", Modalité = as.character(chef_age_grp)),
    enfant_anal %>% filter(!is.na(taille_menage_grp)) %>% count(Caractéristique = "Taille du ménage", Modalité = as.character(taille_menage_grp))
  ) %>%
    group_by(Caractéristique) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

  if (nrow(carac_menage) > 0) {
    export_table(
      gt_styled(carac_menage %>% transmute(Caractéristique, Modalité,
                  Effectif = fmt_n(n), `Pourcentage (%)` = fmt_pct1(pct)),
                title = "Caractéristiques des ménages enquêtés",
                subtitle = "Répartition des enfants selon les caractéristiques du ménage"),
      file.path(PATH_TABLEAUX, "caracteristiques_menage"), raw_data = carac_menage)

    # Graphique
    p_menage <- carac_menage %>%
      ggplot(aes(x = reorder(Modalité, pct), y = pct, fill = Caractéristique)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = paste0(fmt_pct1(pct), "%")),
                hjust = -0.1, size = 3, color = oms_colors[["gris_fonce"]]) +
      facet_wrap(~Caractéristique, scales = "free_y", ncol = 1) +
      scale_fill_manual(values = unname(palette_oms_cat)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(title = "Caractéristiques des ménages enquêtés",
           x = NULL, y = "Pourcentage", caption = .caption_default()) +
      theme(legend.position = "none")
    save_plot(p_menage, file.path(PATH_GRAPHIQUES, "22_caracteristiques_menage.png"),
              width = 10, height = 8)
  }
}


# -----------------------------------------------------------------------------
# Profil de l'échantillon : répartition province / sexe-âge enfant / tuteur
# (alimente la section "Caractéristiques de l'échantillon")
# -----------------------------------------------------------------------------
cli_h1("Profil de l'échantillon (annexe descriptive)")

# (1) Répartition de l'échantillon par province (effectifs + %)
profil_province <- enfant_anal %>%
  filter(!is.na(province_clean)) %>%
  count(Province = province_clean, name = "n") %>%
  mutate(`Pourcentage (%)` = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
export_table(
  gt_styled(profil_province %>% transmute(Province, Effectif = fmt_n(n), `Pourcentage (%)` = fmt_pct1(`Pourcentage (%)`)),
            title = "Répartition de l'échantillon analytique par province",
            subtitle = glue("Enfants analysés : {comma(sum(profil_province$n))}")),
  file.path(PATH_TABLEAUX, "profil_echantillon_province"), raw_data = profil_province)

p_prov_ech <- profil_province %>%
  ggplot(aes(x = reorder(Province, n), y = n, fill = Province)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(fmt_n(n), " (", fmt_pct1(`Pourcentage (%)`), "%)")),
            hjust = -0.05, size = 3, color = oms_colors[["gris_fonce"]]) +
  coord_flip() +
  scale_fill_manual(values = unname(palette_oms_cat)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Répartition de l'échantillon par province",
       x = NULL, y = "Nombre d'enfants enquêtés", caption = .caption_default()) +
  theme(legend.position = "none")
save_plot(p_prov_ech, file.path(PATH_GRAPHIQUES, "28_profil_echantillon_province.png"),
          width = 9, height = 5)

# (2) Profil des enfants : sexe et tranche d'âge (effectifs bruts)
profil_enfant <- bind_rows(
  enfant_anal %>% filter(!is.na(sexe)) %>%
    count(Caractéristique = "Sexe de l'enfant", Modalité = as.character(sexe)),
  enfant_anal %>% filter(!is.na(tranche_age)) %>%
    count(Caractéristique = "Tranche d'âge", Modalité = as.character(tranche_age))
) %>%
  group_by(Caractéristique) %>%
  mutate(`Pourcentage (%)` = round(100 * n / sum(n), 1)) %>%
  ungroup()
export_table(
  gt_styled(profil_enfant %>% transmute(Caractéristique, Modalité, Effectif = fmt_n(n),
              `Pourcentage (%)` = fmt_pct1(`Pourcentage (%)`)),
            title = "Profil des enfants enquêtés (sexe et tranche d'âge)",
            subtitle = "Effectifs bruts de l'échantillon analytique"),
  file.path(PATH_TABLEAUX, "profil_echantillon_enfant"), raw_data = profil_enfant)

# (3) Profil des tuteurs : instruction et situation matrimoniale (effectifs bruts)
vars_tuteur <- intersect(c("niveau_instruction", "situation_matrimoniale"), names(enfant_anal))
if (length(vars_tuteur) > 0) {
  profil_tuteur <- purrr::map_dfr(vars_tuteur, function(v) {
    lbl <- if (v == "niveau_instruction") "Niveau d'instruction du tuteur" else "Situation matrimoniale du tuteur"
    enfant_anal %>% filter(!is.na(.data[[v]])) %>%
      count(Modalité = as.character(.data[[v]])) %>%
      mutate(Caractéristique = lbl)
  }) %>%
    group_by(Caractéristique) %>%
    mutate(`Pourcentage (%)` = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    select(Caractéristique, Modalité, n, `Pourcentage (%)`)
  if (nrow(profil_tuteur) > 0) {
    export_table(
      gt_styled(profil_tuteur %>% transmute(Caractéristique, Modalité, Effectif = fmt_n(n),
                  `Pourcentage (%)` = fmt_pct1(`Pourcentage (%)`)),
                title = "Profil des tuteurs des enfants enquêtés",
                subtitle = "Niveau d'instruction et situation matrimoniale (effectifs bruts)"),
      file.path(PATH_TABLEAUX, "profil_echantillon_tuteur"), raw_data = profil_tuteur)
  }
}
cli_alert_success("Profil de l'échantillon : province, enfant, tuteur exportés.")



# -----------------------------------------------------------------------------
# Flowchart de la collecte (diagramme de flux)
# -----------------------------------------------------------------------------
cli_h1("Flowchart de la collecte")

flow_metrics <- list(
  # ZD = aires de santé échantillonnées. La base monitoring liste les ZD
  # planifiées (attendues) ; les ZD visitées = aires de santé où des ménages
  # ont effectivement été enquêtés.
  zd_attendues  = if (exists("monitoring") && !is.null(monitoring) &&
                      "health_area_id" %in% names(monitoring))
                    dplyr::n_distinct(monitoring$health_area_id) else NA,
  zd_visitees   = if (exists("menage_main") && !is.null(menage_main) &&
                      "aire_sante_id" %in% names(menage_main))
                    dplyr::n_distinct(menage_main$aire_sante_id) else NA,
  menages_approches = if (exists("menage_main") && !is.null(menage_main))
                        nrow(menage_main) else NA,
  menages_repondants = if (exists("menage_main") && !is.null(menage_main))
                         sum(as_num(menage_main$part_status) == 1, na.rm = TRUE) else NA,
  menages_refus = if (exists("menage_main") && !is.null(menage_main))
                    sum(as_num(menage_main$head_status) %in% c(4, 5), na.rm = TRUE) else NA,
  menages_eligibles = if (exists("menage_main") && !is.null(menage_main))
                        sum(as_num(menage_main$hh_eligible) == 1, na.rm = TRUE) else NA,
  enfants_visites = nrow(enfant_main),
  enfants_eligibles = sum(as_num(enfant_main$child_eligible_n) == 1, na.rm = TRUE),
  enfants_analyses = nrow(enfant_anal)
)
# Texte combiné "visitées / attendues"
zd_txt <- if (!is.na(flow_metrics$zd_attendues)) {
  sprintf("%s / %s", fmt_n(flow_metrics$zd_visitees), fmt_n(flow_metrics$zd_attendues))
} else {
  fmt_n(flow_metrics$zd_visitees)
}

# Effectifs analytiques en aval (dénominateurs des indicateurs de couverture)
n_cv_flow     <- sum(!is.na(enfant_anal$vaccine_bin))
n_cv_age_flow <- sum(!is.na(enfant_anal$vaccine_bin) & !is.na(enfant_anal$tranche_age))

# Tableau du flux
flow_tbl <- tibble(
  Étape = c("Zones de dénombrement (ZD) visitées / attendues",
            "Ménages approchés",
            "Ménages répondants",
            "Ménages avec refus",
            "Ménages éligibles (≥ 1 enfant cible)",
            "Enfants visités",
            "Enfants éligibles",
            "Enfants analysés (échantillon analytique)",
            "  dont statut vaccinal connu (base couverture nationale/province/sexe/milieu)",
            "  dont tranche d'âge également connue (base couverture par âge)"),
  Effectif_fmt = c(zd_txt, fmt_n(flow_metrics$menages_approches),
               fmt_n(flow_metrics$menages_repondants), fmt_n(flow_metrics$menages_refus),
               fmt_n(flow_metrics$menages_eligibles), fmt_n(flow_metrics$enfants_visites),
               fmt_n(flow_metrics$enfants_eligibles), fmt_n(flow_metrics$enfants_analyses),
               fmt_n(n_cv_flow), fmt_n(n_cv_age_flow))
)
export_table(
  gt_styled(flow_tbl %>% transmute(Étape, Effectif = Effectif_fmt),
            title = "Déroulement de la collecte des données",
            subtitle = glue("Effectifs aux étapes clés. Les deux dernières lignes précisent les ",
                            "dénominateurs effectifs des indicateurs de couverture, après exclusion ",
                            "des statuts « Ne sait pas » ({fmt_n(flow_metrics$enfants_analyses - n_cv_flow)} enfants) ",
                            "puis des tranches d'âge manquantes ({fmt_n(n_cv_flow - n_cv_age_flow)} enfants).")),
  file.path(PATH_TABLEAUX, "flowchart_collecte"),
  raw_data = flow_tbl %>% transmute(Étape, Effectif = Effectif_fmt))

# Diagramme de flux visuel (style organigramme)
.flow_box <- function(x, y, w, h, label, fill, txtcol = "white") {
  list(rect = data.frame(xmin = x - w/2, xmax = x + w/2,
                         ymin = y - h/2, ymax = y + h/2, fill = fill),
       txt  = data.frame(x = x, y = y, label = label, col = txtcol))
}
boxes <- list(
  .flow_box(0.5, 9, 0.8, 1.1,
    sprintf("ZD visitées / attendues\n%s", zd_txt),
    oms_colors[["bleu_who"]]),
  .flow_box(0.5, 7.3, 0.85, 1.1,
    sprintf("Ménages approchés\n%s", fmt_n(flow_metrics$menages_approches)),
    oms_colors[["bleu_who"]]),
  .flow_box(0.5, 5.6, 0.85, 1.1,
    sprintf("Ménages répondants\n%s", fmt_n(flow_metrics$menages_repondants)),
    oms_colors[["bleu_who"]]),
  .flow_box(0.5, 3.9, 0.9, 1.1,
    sprintf("Ménages éligibles\n%s", fmt_n(flow_metrics$menages_eligibles)),
    oms_colors[["bleu_who"]]),
  .flow_box(0.5, 2.2, 0.85, 1.1,
    sprintf("Enfants éligibles\n%s", fmt_n(flow_metrics$enfants_eligibles)),
    oms_colors[["vert"]]),
  .flow_box(0.5, 0.5, 0.85, 1.1,
    sprintf("Enfants analysés\n%s", fmt_n(flow_metrics$enfants_analyses)),
    oms_colors[["vert"]])
)
# Encarts latéraux (exclusions / refus)
side <- list(
  .flow_box(1.55, 6.45, 0.7, 0.9,
    sprintf("Refus : %s", fmt_n(flow_metrics$menages_refus)),
    oms_colors[["rouge"]]),
  .flow_box(1.55, 1.35, 0.75, 0.9,
    sprintf("Non éligibles\nexclus"),
    oms_colors[["gris"]]))

rects <- bind_rows(lapply(c(boxes, side), `[[`, "rect"))
txts  <- bind_rows(lapply(c(boxes, side), `[[`, "txt"))
arrows <- data.frame(x = 0.5, xend = 0.5,
                     y = c(8.45, 6.75, 5.05, 3.35, 1.65),
                     yend = c(7.85, 6.15, 4.45, 2.75, 1.05))

p_flow <- ggplot() +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                              fill = I(fill)), color = "white", linewidth = 0.5,
            alpha = 0.92) +
  geom_segment(data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               color = oms_colors[["gris_fonce"]], linewidth = 0.6) +
  geom_text(data = txts, aes(x = x, y = y, label = label, color = I(col)),
            fontface = "bold", size = 3.1, lineheight = 0.9) +
  scale_x_continuous(limits = c(-0.1, 2.1)) +
  scale_y_continuous(limits = c(-0.2, 9.8)) +
  labs(title = "Diagramme de flux de la collecte des données",
       subtitle = "Effectifs aux étapes clés (ECP Rougeole-Rubéole)",
       caption = .caption_default()) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14,
                                  color = oms_colors[["bleu_fonce"]]),
        plot.subtitle = element_text(size = 10, color = oms_colors[["gris_fonce"]]),
        plot.caption = element_text(size = 8, color = oms_colors[["gris"]], hjust = 1))
save_plot(p_flow, file.path(PATH_GRAPHIQUES, "00_flowchart_collecte.png"),
          width = 8, height = 10)

cli_alert_success("Sorties supplémentaires (annexes) générées.")


# =============================================================================
# 16b_bis. ANALYSE DES NON-RÉPONDANTS — VCQI ES-03
# =============================================================================
# Indicateur VCQI ES-03 : les ménages non enquêtés (absents, refus) différent-
# ils systématiquement de ceux enquêtés ? Un biais de non-réponse existe si
# les non-répondants ont un profil de couverture vaccinale différent.
# Ici, nous ne pouvons pas mesurer directement la couverture des non-répondants
# (par définition non enquêtés), mais nous pouvons :
#   a) Quantifier les taux de non-réponse par province et par aire de santé,
#   b) Analyser si le taux de non-réponse est corrélé à la couverture estimée
#      (signal d'un biais potentiel si les zones à forte non-réponse ont aussi
#      une couverture différente de la moyenne).
# Source : Groves & Peytcheva (2008). The Impact of Nonresponse Rates...

cli_h1("VCQI ES-03 : Analyse des non-répondants et biais potentiel")

if (!is.null(denombrement) && "province_name" %in% names(denombrement) &&
    "province_clean" %in% names(enfant_anal)) {
  tryCatch({
    # Normalisation des noms de province dans le dénombrement
    # (même logique que enfant_anal : province_clean = as.character(province_name))
    den_prov <- denombrement %>%
      filter(!is.na(province_name)) %>%
      mutate(province_clean = as.character(haven::as_factor(province_name))) %>%
      group_by(province_clean) %>%
      summarise(n_denombres = n(), .groups = "drop")

    nb_enquetes <- enfant_anal %>%
      filter(!is.na(province_clean)) %>%
      count(province_clean, name = "n_enquetes")

    non_repondants_tbl <- den_prov %>%
      left_join(nb_enquetes, by = "province_clean") %>%
      mutate(
        n_enquetes      = replace_na(n_enquetes, 0),
        n_non_repondants = n_denombres - n_enquetes,
        taux_reponse    = round(100 * n_enquetes / n_denombres, 1),
        taux_non_rep    = round(100 - taux_reponse, 1)
      ) %>%
      left_join(cv_prov_df %>% select(Province, cv_pct = pct),
                by = c("province_clean" = "Province")) %>%
      rename(Province = province_clean) %>%
      arrange(taux_reponse)

    # Ajout ligne nationale
    tot <- non_repondants_tbl %>%
      summarise(
        Province = "TOTAL / Moyenne",
        n_denombres       = sum(n_denombres),
        n_enquetes        = sum(n_enquetes),
        n_non_repondants  = sum(n_non_repondants),
        taux_reponse      = round(100 * sum(n_enquetes) / sum(n_denombres), 1),
        taux_non_rep      = round(100 - taux_reponse, 1),
        cv_pct            = NA_real_
      )
    non_repondants_export <- bind_rows(non_repondants_tbl, tot)

    # Corrélation taux de réponse × couverture (signal de biais)
    cor_nr_cv <- if (nrow(non_repondants_tbl) >= 4) {
      round(cor(non_repondants_tbl$taux_reponse,
                non_repondants_tbl$cv_pct, use = "complete.obs"), 3)
    } else NA_real_

    export_table(
      gt_styled(
        non_repondants_export %>%
          transmute(Province,
                    `Dénombrés` = fmt_n(n_denombres),
                    `Enquêtés` = fmt_n(n_enquetes),
                    `Non-répondants` = fmt_n(n_non_repondants),
                    `Taux de réponse (%)` = fmt_pct1(taux_reponse),
                    `Taux de NR (%)` = fmt_pct1(taux_non_rep),
                    `CV estimée (%)` = if_else(is.na(cv_pct), "—",
                                               paste0(fmt_pct1(cv_pct), " %"))),
        title = "Analyse des non-répondants par province (VCQI ES-03)",
        subtitle = glue("Taux de réponse national : {tot$taux_reponse} % | ",
                        "Corrélation taux-réponse × CV : r = ",
                        "{ifelse(is.na(cor_nr_cv), 'N/D', cor_nr_cv)}")
      ),
      file.path(PATH_TABLEAUX, "non_repondants_analyse"),
      raw_data = non_repondants_export
    )

    # Graphique
    p_nr <- non_repondants_tbl %>%
      ggplot(aes(x = taux_non_rep, y = reorder(Province, taux_non_rep))) +
      geom_col(fill = oms_colors[["orange"]], width = 0.65, alpha = 0.88) +
      geom_text(aes(label = paste0(fmt_pct1(taux_non_rep), " %",
                                   " (n=", n_non_repondants, ")")),
                hjust = -0.05, size = 3.1, color = oms_colors[["gris_fonce"]]) +
      scale_x_continuous(limits = c(0, max(non_repondants_tbl$taux_non_rep, na.rm=TRUE)*1.3),
                         expand = expansion(mult = c(0,0)),
                         labels = function(x) paste0(round(x),"%")) +
      labs(title = "Taux de non-réponse par province (VCQI ES-03)",
           subtitle = paste0("Taux national : ", tot$taux_non_rep,
                             " % | Corrélation NR × couverture : r = ",
                             ifelse(is.na(cor_nr_cv), "N/D", cor_nr_cv)),
           x = "Taux de non-réponse (%)", y = NULL,
           caption = .caption_default())
    save_plot(p_nr, file.path(PATH_GRAPHIQUES, "16b_non_repondants.png"),
              width = 9, height = 5)

    cli_alert_success("VCQI ES-03 : taux de réponse national = {tot$taux_reponse} %.")
    # Écrire les scalaires VCQI dans un fichier pour le reporting
    writeLines(c(
      as.character(round(pct_campagne_zero, 1)),  # ligne 1 : rattrapage zéro dose (%)
      as.character(n_zero),                        # ligne 2 : effectif zéro dose
      as.character(round(deff_global, 2)),          # ligne 3 : DEFF global
      as.character(round(icc_global, 4)),           # ligne 4 : ICC global
      as.character(round(cor_nr_cv, 3))             # ligne 5 : corrélation NR×CV
    ), file.path(PATH_TABLEAUX, ".vcqi_scalaires.txt"))
  }, error = function(e) {
    cli_alert_warning("VCQI ES-03 : {e$message}")
    non_repondants_tbl <<- tibble(); cor_nr_cv <<- NA_real_
  })
} else {
  cli_alert_warning("VCQI ES-03 : denombrement ou province_clean indisponible.")
  non_repondants_tbl <- tibble(); cor_nr_cv <- NA_real_
}


# =============================================================================
# 16c. COMPARAISON CV ECP vs CV ADMINISTRATIVE vs CV END-PROCESS OMS
# =============================================================================
# Cette section compare nos estimations issues de l'enquête (ECP, pondérées
# svyciprop logit) à deux sources de référence pour le bloc 1 :
#   1) CV administrative agrégée depuis le SNIS (fichier Excel)
#   2) CV End-Process OMS (rapport décembre 2025, n = 124 729 enfants)
# Les comparaisons sont produites au niveau province et au niveau zone de santé.

cli_h1("Comparaison CV : ECP vs Administrative vs End-Process OMS")

# --- 1. Chargement de la CV administrative (Excel) ---------------------------
.lire_cv_admin <- function() {
  f <- file.path("data", "external", "cv_administrative_bloc1.xlsx")
  if (!file.exists(f)) {
    cli_alert_warning("Fichier CV administrative absent : {f}")
    return(NULL)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli_alert_warning("Package readxl absent — comparaison admin ignorée.")
    return(NULL)
  }
  adm <- readxl::read_excel(f) %>%
    rename(Province_raw = 1, ZS_raw = 2, AS_raw = 3,
           cible = 4, vaccinees = 5, cv_admin = 6) %>%
    mutate(
      Province = stringr::str_remove(Province_raw, "^[a-z]{2,3}\\s+") %>%
                 stringr::str_remove("\\s+Province$") %>% stringr::str_trim(),
      ZS = stringr::str_remove(ZS_raw, "^[a-z]{2,3}\\s+") %>%
           stringr::str_remove("\\s+Zone de Santé$") %>% stringr::str_trim()
    ) %>%
    filter(!is.na(cible), !is.na(vaccinees))
  adm
}
cv_admin_raw <- .lire_cv_admin()

# Agrégat administratif par province
cv_admin_prov <- if (!is.null(cv_admin_raw)) {
  cv_admin_raw %>%
    group_by(Province) %>%
    summarise(cible = sum(cible, na.rm = TRUE),
              vaccinees = sum(vaccinees, na.rm = TRUE), .groups = "drop") %>%
    mutate(cv_admin_pct = 100 * vaccinees / cible)
} else NULL

# Agrégat administratif par ZS
cv_admin_zs <- if (!is.null(cv_admin_raw)) {
  cv_admin_raw %>%
    group_by(Province, ZS) %>%
    summarise(cible = sum(cible, na.rm = TRUE),
              vaccinees = sum(vaccinees, na.rm = TRUE), .groups = "drop") %>%
    mutate(cv_admin_pct = 100 * vaccinees / cible)
} else NULL


# --- 2. CV End-Process OMS (rapport décembre 2025, en dur depuis PDF) --------
# Source : Rapport End-Process, OMS RDC, déc. 2025 (bloc 1, n = 124 729)
cv_endprocess_prov <- tibble::tribble(
  ~Province,        ~n_ep,   ~cv_endprocess_pct,
  "Bas Uele",        8801,    99.4,
  "Haut Uele",      13615,    98.9,
  "Tanganyika",      9513,    98.9,   # nommée Tanganyika dans rapport OMS
  "Ituri",          35045,    97.8,
  "Lualaba",        13147,    96.1,
  "Haut Lomami",    16736,    95.8,
  "Haut Katanga",   27872,    91.8
)

# CV End-Process par zone de santé (extrait du PDF, annexes 7)
cv_endprocess_zs <- tibble::tribble(
  ~Province,       ~ZS,                  ~n_ep,  ~cv_endprocess_pct,
  # Aru
  "Ituri","Adi",1384,99.9, "Ituri","Adja",979,99.9, "Ituri","Angumu",887,100.0,
  "Ituri","Ariwara",1246,99.9, "Ituri","Aru",1221,99.8, "Ituri","Aungba",907,100.0,
  "Ituri","Biringi",945,99.9, "Ituri","Kambala",1257,98.8, "Ituri","Laybo",1035,99.8,
  "Ituri","Logo",1038,99.9, "Ituri","Mahagi",868,99.1, "Ituri","Nyarambe",1066,100.0,
  "Ituri","Rimba",1056,99.8,
  # Bunia
  "Ituri","Bambu",1013,69.5, "Ituri","Boga",690,100.0, "Ituri","Bunia",636,58.6,
  "Ituri","Damas",1008,99.2, "Ituri","Drodro",881,100.0, "Ituri","Fataki",767,99.7,
  "Ituri","Gethy",1136,99.9, "Ituri","Jiba",831,99.9, "Ituri","Kilo",1094,99.8,
  "Ituri","Komanda",660,97.7, "Ituri","Linga",1046,99.9, "Ituri","Lita",809,99.8,
  "Ituri","Lolwa",819,99.4, "Ituri","Mambasa",1005,99.9, "Ituri","Mandima",805,100.0,
  "Ituri","Mangala",913,99.2, "Ituri","Mongbwalu",934,99.4, "Ituri","Nia Nia",1153,100.0,
  "Ituri","Nizi",764,86.5, "Ituri","Nyankunde",765,99.1, "Ituri","Rethy",1169,99.8,
  "Ituri","Rwampara",1263,99.9, "Ituri","Tchomia",995,99.4,
  # Buta (Bas Uele)
  "Bas Uele","Aketi",916,99.8, "Bas Uele","Ango",649,100.0, "Bas Uele","Bili",396,98.5,
  "Bas Uele","Bondo",654,99.8, "Bas Uele","Buta",736,97.7, "Bas Uele","Ganga",918,100.0,
  "Bas Uele","Likati",956,98.1, "Bas Uele","Monga",583,100.0, "Bas Uele","Poko",1016,99.7,
  "Bas Uele","Titule",830,99.0, "Bas Uele","Viadana",1147,99.8,
  # Isiro (Haut Uele)
  "Haut Uele","Boma Mangbetu",1080,100.0, "Haut Uele","Doruma",623,100.0,
  "Haut Uele","Dungu",1129,98.8, "Haut Uele","Isiro",894,97.7,
  "Haut Uele","Niangara",560,100.0, "Haut Uele","Pawa",1189,94.4,
  "Haut Uele","Rungu",1207,99.9, "Haut Uele","Wamba",1149,98.9,
  # Kabalo (Tanganyika)
  "Tanganyika","Ankoro",1006,98.8, "Tanganyika","Kabalo",1048,99.5,
  "Tanganyika","Kiambi",794,99.9, "Tanganyika","Kongolo",1132,98.8,
  "Tanganyika","Manono",975,98.9, "Tanganyika","Mbulula",1281,99.3,
  # Kabondo Dianda (Haut Lomami)
  "Haut Lomami","Bukama",1023,97.3, "Haut Lomami","Butumba",901,100.0,
  "Haut Lomami","Kabondo Dianda",740,99.6, "Haut Lomami","Kinkondja",1241,99.4,
  "Haut Lomami","Lwamba",1384,99.7, "Haut Lomami","Malemba Nkulu",963,99.5,
  "Haut Lomami","Mukanga",1882,98.4, "Haut Lomami","Mulongo",1105,99.5,
  # Kalemie
  "Tanganyika","Kalemie",511,97.8, "Tanganyika","Kansimba",769,99.9,
  "Tanganyika","Moba",638,95.5, "Tanganyika","Nyemba",517,99.4,
  "Tanganyika","Nyunzu",842,98.5,
  # Kamina (Haut Lomami)
  "Haut Lomami","Baka",336,99.7, "Haut Lomami","Kabongo",1491,99.8,
  "Haut Lomami","Kamina",977,55.1, "Haut Lomami","Kaniama",879,99.3,
  "Haut Lomami","Kinda",1069,99.9, "Haut Lomami","Kitenge",1316,89.7,
  "Haut Lomami","Songa",1429,97.4,
  # Lubumbashi (Haut Katanga)
  "Haut Katanga","Kafubu",1050,86.6, "Haut Katanga","Kamalondo",329,96.7,
  "Haut Katanga","Kampemba",1206,93.6, "Haut Katanga","Kasenga",922,99.1,
  "Haut Katanga","Kashobwe",397,100.0, "Haut Katanga","Katuba",1105,91.1,
  "Haut Katanga","Kenya",1072,95.3, "Haut Katanga","Kilwa",1924,98.5,
  "Haut Katanga","Kipushi",966,79.1, "Haut Katanga","Kisanga",1516,84.5,
  "Haut Katanga","Kowe",849,94.1, "Haut Katanga","Lubumbashi",914,99.5,
  "Haut Katanga","Lukafu",811,99.6, "Haut Katanga","Mumbunda",632,79.1,
  "Haut Katanga","Pweto",973,97.8, "Haut Katanga","Ruashi",2017,90.8,
  "Haut Katanga","Sakania",2394,79.7, "Haut Katanga","Tshamilemba",950,63.9,
  "Haut Katanga","Vangu",758,94.9,
  # Likasi (Haut Katanga)
  "Haut Katanga","Kambove",992,99.3, "Haut Katanga","Kapolowe",763,98.7,
  "Haut Katanga","Kikula",1303,97.9, "Haut Katanga","Kilela Balanda",695,99.9,
  "Haut Katanga","Likasi",1054,92.1, "Haut Katanga","Mitwaba",644,99.7,
  "Haut Katanga","Mufunga Sampwe",791,96.2, "Haut Katanga","Panda",845,99.6,
  # Kolwezi (Lualaba)
  "Lualaba","Bunkeya",752,92.3, "Lualaba","Dilala",436,93.6,
  "Lualaba","Fungurume",1331,94.1, "Lualaba","Kanzenze",721,93.8,
  "Lualaba","Lualaba",999,94.2, "Lualaba","Lubudi",712,92.7,
  "Lualaba","Manika",1096,91.8, "Lualaba","Mutshatsha",1022,99.0,
  # Kisenge (Lualaba)
  "Lualaba","Dilolo",644,99.8, "Lualaba","Kafakumba",1091,96.5,
  "Lualaba","Kalamba",1217,99.3, "Lualaba","Kapanga",943,99.4,
  "Lualaba","Kasaji",786,98.3, "Lualaba","Sandoa",1397,97.8,
  # Watsa (Haut Uele)
  "Haut Uele","Aba",1061,100.0, "Haut Uele","Faradje",1383,100.0,
  "Haut Uele","Gombari",1094,99.0, "Haut Uele","Makoro",1031,100.0,
  "Haut Uele","Watsa",1215,98.1
)


# --- 3. Comparaison au niveau PROVINCE ---------------------------------------
# Normalisation des noms de province pour la jointure
.norm_prov <- function(x) {
  x <- stringr::str_trim(as.character(x))
  # Standardiser : "Tanganyka" (notre orthographe) vs "Tanganyika"
  dplyr::recode(x, "Tanganyka" = "Tanganyika")
}

if (exists("cv_prov_df") && !is.null(cv_prov_df) && !is.null(cv_admin_prov)) {
  ecp_prov <- cv_prov_df %>%
    transmute(Province = .norm_prov(Province),
              n_ecp = n,
              cv_ecp_pct = 100 * prop,
              ic_bas = 100 * ic_bas,
              ic_haut = 100 * ic_haut)

  comp_prov <- ecp_prov %>%
    full_join(cv_admin_prov %>% mutate(Province = .norm_prov(Province)),
              by = "Province") %>%
    full_join(cv_endprocess_prov %>% mutate(Province = .norm_prov(Province)),
              by = "Province") %>%
    mutate(ecart_admin_ecp = cv_admin_pct - cv_ecp_pct,
           ecart_ep_ecp    = cv_endprocess_pct - cv_ecp_pct) %>%
    arrange(desc(cv_ecp_pct))

  # Tableau export
  comp_prov_fmt <- comp_prov %>%
    transmute(Province,
              `n (ECP)` = fmt_n(n_ecp),
              `CV ECP (%)` = paste0(fmt_pct1(cv_ecp_pct), " %"),
              `IC 95% ECP` = fmt_ic(ic_bas, ic_haut),
              `CV admin. (%)` = paste0(fmt_pct1(cv_admin_pct), " %"),
              `CV End-Process OMS (%)` = paste0(fmt_pct1(cv_endprocess_pct), " %"),
              `Écart admin. − ECP (pp)` = fmt_pct1(ecart_admin_ecp),
              `Écart End-Process − ECP (pp)` = fmt_pct1(ecart_ep_ecp))

  export_table(
    gt_styled(comp_prov_fmt,
              title = "Comparaison des couvertures vaccinales RR par province",
              subtitle = "ECP (svyciprop pondéré, IC 95%) vs CV administrative (SNIS) vs CV End-Process OMS"),
    file.path(PATH_TABLEAUX, "comparaison_cv_province"),
    raw_data = comp_prov)

  # Graphique : barres groupées par province (3 sources)
  comp_long <- comp_prov %>%
    select(Province, ECP = cv_ecp_pct,
           Administrative = cv_admin_pct,
           `End-Process OMS` = cv_endprocess_pct) %>%
    tidyr::pivot_longer(-Province, names_to = "Source", values_to = "CV") %>%
    filter(!is.na(CV)) %>%
    mutate(Source = factor(Source,
                           levels = c("ECP", "Administrative", "End-Process OMS")),
           Province = factor(Province,
                             levels = comp_prov$Province[order(comp_prov$cv_ecp_pct)]))

  p_comp <- ggplot(comp_long, aes(x = CV, y = Province, fill = Source)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65, alpha = 0.92) +
    geom_text(aes(label = paste0(fmt_pct1(CV), " %")),
              position = position_dodge(width = 0.72),
              hjust = -0.1, size = 2.9, color = oms_colors[["gris_fonce"]]) +
    geom_vline(xintercept = 95, linetype = "dashed",
               color = oms_colors[["rouge"]], linewidth = 0.6) +
    scale_fill_manual(values = c(ECP = oms_colors[["bleu_who"]],
                                 Administrative = oms_colors[["orange"]],
                                 `End-Process OMS` = oms_colors[["vert"]])) +
    scale_x_continuous(limits = c(0, 125), breaks = seq(0, 120, 20),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = "Comparaison de la couverture RR par province",
         subtitle = "ECP (notre enquête) vs CV administrative (SNIS) vs End-Process OMS — cible 95% (ligne rouge)",
         x = "Couverture vaccinale (%)", y = NULL, fill = "Source",
         caption = .caption_default(
           "ECP : svyciprop pondéré ; Admin : agrégat SNIS ; End-Process : rapport OMS bloc 1, déc. 2025")) +
    theme(legend.position = "bottom")
  save_plot(p_comp, file.path(PATH_GRAPHIQUES, "23_comparaison_cv_province.png"),
            width = 11, height = 6)
  cli_alert_success("Comparaison CV par province : {nrow(comp_prov)} provinces.")
}


# --- 4. Comparaison au niveau ZONE DE SANTÉ ----------------------------------
if (exists("cov_zone") && !is.null(cv_admin_zs)) {
  # cov_zone (ECP) a colonnes : province_clean, zone_sante_clean, N, Nvax, pct
  ecp_zs <- cov_zone %>%
    transmute(Province = .norm_prov(province_clean),
              ZS = stringr::str_trim(zone_sante_clean),
              n_ecp = N, cv_ecp_pct = pct)

  comp_zs <- ecp_zs %>%
    left_join(cv_admin_zs %>%
                mutate(Province = .norm_prov(Province),
                       ZS = stringr::str_trim(ZS)) %>%
                select(Province, ZS, cv_admin_pct),
              by = c("Province", "ZS")) %>%
    left_join(cv_endprocess_zs %>%
                mutate(Province = .norm_prov(Province),
                       ZS = stringr::str_trim(ZS)) %>%
                select(Province, ZS, cv_endprocess_pct),
              by = c("Province", "ZS")) %>%
    mutate(ecart_admin = cv_admin_pct - cv_ecp_pct,
           ecart_ep    = cv_endprocess_pct - cv_ecp_pct) %>%
    arrange(Province, ZS)

  # Tableau export (toutes les ZS appariées)
  comp_zs_fmt <- comp_zs %>%
    transmute(Province, `Zone de santé` = ZS,
              `n (ECP)` = fmt_n(n_ecp),
              `CV ECP (%)` = paste0(fmt_pct1(cv_ecp_pct), " %"),
              `CV admin. (%)` = paste0(fmt_pct1(cv_admin_pct), " %"),
              `CV End-Process (%)` = paste0(fmt_pct1(cv_endprocess_pct), " %"),
              `Écart admin. − ECP (pp)` = fmt_pct1(ecart_admin),
              `Écart End-Process − ECP (pp)` = fmt_pct1(ecart_ep))
  export_table(
    gt_styled(comp_zs_fmt,
              title = "Comparaison des couvertures RR par zone de santé",
              subtitle = "ECP vs administrative (SNIS) vs End-Process OMS"),
    file.path(PATH_TABLEAUX, "comparaison_cv_zone_sante"),
    raw_data = comp_zs)

  # Scatter ECP vs Admin (et ECP vs End-Process en facette)
  comp_scatter <- comp_zs %>%
    select(Province, ZS, ECP = cv_ecp_pct,
           Administrative = cv_admin_pct,
           `End-Process OMS` = cv_endprocess_pct) %>%
    tidyr::pivot_longer(c(Administrative, `End-Process OMS`),
                        names_to = "Source", values_to = "CV_ref") %>%
    filter(!is.na(CV_ref), !is.na(ECP))

  p_scat <- ggplot(comp_scatter, aes(x = ECP, y = CV_ref, color = Province)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = oms_colors[["gris_fonce"]], linewidth = 0.4) +
    geom_point(alpha = 0.75, size = 2) +
    facet_wrap(~ Source, ncol = 2) +
    scale_x_continuous(limits = c(40, 130), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(40, 130), labels = function(x) paste0(x, "%")) +
    scale_color_manual(values = unname(palette_oms_cat)) +
    labs(title = "Concordance par zone de santé : ECP vs sources de référence",
         subtitle = "Chaque point = une ZS ; ligne pointillée = accord parfait (y = x)",
         x = "CV ECP (%)", y = "CV de référence (%)",
         caption = .caption_default()) +
    theme(legend.position = "bottom")
  save_plot(p_scat, file.path(PATH_GRAPHIQUES, "24_comparaison_cv_zone_sante.png"),
            width = 12, height = 6)
  cli_alert_success("Comparaison CV par ZS : {nrow(comp_zs)} zones appariées.")
}


# -----------------------------------------------------------------------------
# 16d. REGISTRE DES EFFECTIFS PAR INDICATEUR (numérateurs / dénominateurs)
# -----------------------------------------------------------------------------
# Pour documenter de façon DYNAMIQUE la population, le numérateur et le
# dénominateur de chaque résultat, on calcule ici un registre central.
cli_h1("Registre des effectifs par indicateur")

.safe_n <- function(expr) tryCatch(as.integer(expr), error = function(e) NA_integer_)
n_total_anal <- nrow(enfant_anal)

registre_effectifs <- tibble::tibble(
  indicateur = character(), population = character(),
  numerateur = character(), denominateur = character(),
  n_denom = integer(), n_exclus = integer()
)
.add_eff <- function(indicateur, population, numerateur, denominateur,
                     n_denom, n_ref = n_total_anal) {
  registre_effectifs <<- dplyr::bind_rows(registre_effectifs, tibble::tibble(
    indicateur = indicateur, population = population,
    numerateur = numerateur, denominateur = denominateur,
    n_denom = as.integer(n_denom),
    n_exclus = as.integer(n_ref - n_denom)))
}

# Couverture vaccinale (globale / province) : dénominateur = statut connu
n_cv <- .safe_n(sum(!is.na(enfant_anal$vaccine_bin)))
.add_eff("Couverture vaccinale (globale, province)",
         "Enfants analysés dont le statut vaccinal de campagne est connu (réponses « Ne sait pas » exclues)",
         "Enfants déclarés vaccinés pendant la campagne (pondéré)",
         "Enfants analysés au statut vaccinal connu (pondéré)", n_cv)

# Couverture par sexe : statut connu ET sexe connu
n_cv_sexe <- .safe_n(sum(!is.na(enfant_anal$vaccine_bin) & !is.na(enfant_anal$sexe)))
.add_eff("Couverture par sexe",
         "Enfants analysés dont le statut vaccinal ET le sexe sont connus",
         "Enfants vaccinés (pondéré)",
         "Enfants au statut vaccinal et au sexe connus (pondéré)", n_cv_sexe)

# Couverture par milieu : statut connu ET milieu dérivé
n_cv_milieu <- if ("milieu" %in% names(enfant_anal))
  .safe_n(sum(!is.na(enfant_anal$vaccine_bin) & !is.na(enfant_anal$milieu))) else NA_integer_
if (!is.na(n_cv_milieu)) {
  .add_eff("Couverture par milieu de résidence",
           "Enfants analysés dont le statut vaccinal est connu et dont le milieu (urbain/rural) a pu être dérivé de la base de sondage",
           "Enfants vaccinés (pondéré)",
           "Enfants au statut vaccinal connu et au milieu dérivé (pondéré)", n_cv_milieu)
}

# Couverture par tranche d'âge : + tranche connue
n_cv_age <- .safe_n(sum(!is.na(enfant_anal$vaccine_bin) & !is.na(enfant_anal$tranche_age)))
.add_eff("Couverture par tranche d'âge",
         "Enfants analysés dont le statut vaccinal ET la tranche d'âge sont connus",
         "Enfants vaccinés (pondéré)",
         "Enfants au statut vaccinal et à la tranche d'âge connus (pondéré)", n_cv_age)

# Statut vaccinal antérieur (6-59 mois)
if ("statut_vaccinal_ant_fus" %in% names(enfant_anal) &&
    "tranche_age_mois" %in% names(enfant_anal)) {
  n_statut <- .safe_n(sum(enfant_anal$tranche_age_mois == "6-59 mois" &
                          !is.na(enfant_anal$statut_vaccinal_ant_fus), na.rm = TRUE))
  .add_eff("Statut vaccinal antérieur",
           "Enfants de 6 à 59 mois dont le statut vaccinal antérieur est connu",
           "Enfants par catégorie de statut antérieur (zéro dose, une dose, etc.)",
           "Enfants de 6-59 mois au statut antérieur connu", n_statut)
}

# Confirmation par carte (dénominateur = vaccinés)
if ("confirmation_carte" %in% names(enfant_anal)) {
  n_carte <- .safe_n(sum(!is.na(enfant_anal$confirmation_carte) &
                         enfant_anal$vaccine_bin == 1, na.rm = TRUE))
  .add_eff("Confirmation par carte/jeton",
           "Enfants déclarés vaccinés pendant la campagne",
           "Enfants par modalité de preuve (carte vue, reçue non vue, etc.)",
           "Enfants vaccinés au statut de preuve renseigné", n_carte)
}

# Raisons de non-vaccination (dénominateur = non vaccinés ayant déclaré une raison)
if (exists("n_nonvax")) {
  .add_eff("Raisons de non-vaccination",
           paste0("Enfants NON vaccinés ayant déclaré au moins une raison (",
                  n_nonvax - n_nonvax_raison, " enfants non vaccinés sans raison déclarée exclus)"),
           "Enfants par groupe de raisons (taxonomie OMS)",
           "Enfants non vaccinés ayant déclaré au moins une raison",
           n_nonvax_raison, n_ref = n_nonvax)
}

# Lieu de vaccination (dénominateur = vaccinés)
if ("lieu_vaccination_lbl" %in% names(enfant_anal)) {
  n_lieu <- .safe_n(sum(!is.na(enfant_anal$lieu_vaccination_lbl) &
                        enfant_anal$vaccine_bin == 1, na.rm = TRUE))
  .add_eff("Lieu de vaccination",
           "Enfants déclarés vaccinés pendant la campagne",
           "Enfants par lieu déclaré (formation sanitaire, école, etc.)",
           "Enfants vaccinés au lieu renseigné", n_lieu)
}

# Dépenses (dénominateur = statut dépense connu)
if ("depense_vacc_bin" %in% names(enfant_anal)) {
  n_dep <- .safe_n(sum(!is.na(enfant_anal$depense_vacc_bin)))
  .add_eff("Dépenses liées à la vaccination",
           "Enfants dont l'information sur les dépenses est renseignée",
           "Enfants pour lesquels une dépense a été engagée",
           "Enfants au statut de dépense connu", n_dep)
}

# Canaux d'information
if ("canal_info_lbl" %in% names(enfant_anal)) {
  n_canal <- .safe_n(sum(!is.na(enfant_anal$canal_info_lbl)))
  .add_eff("Canaux d'information (réponses multiples)",
           "Enfants dont le canal d'information est renseigné",
           "Mentions de chaque canal (un enfant peut en citer plusieurs)",
           "Enfants au canal d'information renseigné", n_canal)
}

# Symptômes post-vaccinaux
if ("measles_symptom_f" %in% names(enfant_anal)) {
  n_symp <- .safe_n(sum(!is.na(enfant_anal$measles_symptom_f)))
  .add_eff("Symptômes post-vaccinaux",
           "Enfants analysés dont l'information sur les symptômes est renseignée",
           "Enfants ayant présenté chaque symptôme",
           "Enfants analysés au statut de symptômes connu", n_symp)
}

export_table(
  gt_styled(registre_effectifs %>%
              transmute(Indicateur = indicateur, Population = population,
                        Numérateur = numerateur, Dénominateur = denominateur,
                        `n (dénom.)` = fmt_n(n_denom),
                        `Exclus / analysés` = fmt_n(n_exclus)),
            title = "Registre des effectifs par indicateur",
            subtitle = glue("Échantillon analytique de référence : {comma(n_total_anal)} enfants (interviewés et éligibles)")),
  file.path(PATH_TABLEAUX, "registre_effectifs"),
  raw_data = registre_effectifs)
cli_alert_success("Registre des effectifs : {nrow(registre_effectifs)} indicateurs documentés.")


# -----------------------------------------------------------------------------
# 17. SAUVEGARDE DES OBJETS POUR REPORTING
# -----------------------------------------------------------------------------

cli_h1("Sauvegarde des objets pour le reporting")

resultats_globaux <- list(
  timestamp        = Sys.time(),
  n_enfants_analyse = nrow(enfant_anal),
  registre_effectifs = if (exists("registre_effectifs")) registre_effectifs else NULL,
  n_cv_national    = if (exists("n_cv")) n_cv else NULL,
  n_cv_age         = if (exists("n_cv_age")) n_cv_age else NULL,
  n_cv_sexe        = if (exists("n_cv_sexe")) n_cv_sexe else NULL,
  n_cv_milieu      = if (exists("n_cv_milieu")) n_cv_milieu else NULL,
  n_nonvax_total   = if (exists("n_nonvax")) n_nonvax else NULL,
  n_nonvax_raison  = if (exists("n_nonvax_raison")) n_nonvax_raison else NULL,
  # VCQI indicators
  zero_dose_pct_campagne = if (exists("pct_campagne_zero")) pct_campagne_zero else NULL,
  n_zero_dose      = if (exists("n_zero")) n_zero else NULL,
  deff_global      = if (exists("deff_global")) deff_global else NULL,
  icc_global       = if (exists("icc_global")) icc_global else NULL,
  cor_nr_cv        = if (exists("cor_nr_cv")) cor_nr_cv else NULL,
  n_nonvax_total   = if (exists("n_nonvax")) n_nonvax else NULL,
  n_nonvax_raison  = if (exists("n_nonvax_raison")) n_nonvax_raison else NULL,
  cv_global         = if (exists("cv_global_df")) cv_global_df else NULL,
  cv_province       = if (exists("cv_prov_df")) cv_prov_df else NULL,
  cv_age            = if (exists("cv_age_df")) cv_age_df else NULL,
  cv_sexe           = if (exists("cv_sexe_df")) cv_sexe_df else NULL,
  cv_milieu         = if (exists("cv_milieu_df")) cv_milieu_df else NULL,
  comparaison_cv_province  = if (exists("comp_prov")) comp_prov else NULL,
  comparaison_cv_zone_sante = if (exists("comp_zs")) comp_zs else NULL,
  dist_importance   = if (exists("dist_importance")) dist_importance else NULL,
  dist_pression     = if (exists("dist_pression")) dist_pression else NULL,
  dist_souhaits     = if (exists("dist_souhaits")) dist_souhaits else NULL,
  cv_chef_menage    = if (exists("cv_chef")) cv_chef else NULL,
  cv_tuteur         = if (exists("cv_tuteur")) cv_tuteur else NULL,
  cv_enfant_prov    = if (exists("cv_enfant_prov")) cv_enfant_prov else NULL,
  caracteristiques_menage = if (exists("carac_menage")) carac_menage else NULL,
  profil_echantillon_province = if (exists("profil_province")) profil_province else NULL,
  profil_echantillon_enfant = if (exists("profil_enfant")) profil_enfant else NULL,
  profil_echantillon_tuteur = if (exists("profil_tuteur")) profil_tuteur else NULL,
  flowchart         = if (exists("flow_tbl")) flow_tbl else NULL,
  statut_detail     = if (exists("statut_detail_df")) statut_detail_df else NULL,
  statut_anterieur  = if (exists("statut_df")) statut_df else NULL,
  raisons_non_vacc  = if (exists("raisons_df")) raisons_df else NULL,
  confirmation_carte= if (exists("carte_df")) carte_df else NULL,
  concordance_enf   = concordance_enf,
  concordance_men   = concordance_men,
  taux_erreur       = if (exists("taux_erreur")) taux_erreur else NULL,
  couverture_sousgroupes = if (exists("df_sg")) df_sg else NULL,
  pareto_raisons    = if (exists("df_pareto")) df_pareto else NULL,
  raisons_par_groupe= if (exists("tab_rai")) tab_rai else NULL,
  chi2_geographique = if (exists("chi2_geo")) chi2_geo else NULL,
  couverture_zone   = if (exists("cov_zone")) cov_zone else NULL,
  determinants_or   = if (exists("or_df")) or_df else NULL,
  methode_modele    = if (exists("methode_modele")) methode_modele else NULL,
  couverture_carte_vs_decl = if (exists("comp_nat")) comp_nat else NULL,
  couverture_carte_prov    = if (exists("comp_prov")) comp_prov else NULL,
  biais_declaratif_nat     = if (exists("biais_nat")) biais_nat else NULL,
  reponse_par_province     = if (exists("matrice_prov_raison")) matrice_prov_raison else NULL,
  matrice_province_raison  = if (exists("matrice_complete_fmt")) matrice_complete_fmt else NULL,
  symptomes_detail         = if (exists("symp_detail")) symp_detail else NULL,
  distribution_age         = if (exists("age_distrib_tbl")) age_distrib_tbl else NULL,
  couverture_par_canal     = if (exists("cov_canal")) cov_canal else NULL,
  kpis              = if (exists("kpis")) kpis else NULL,
  graphiques        = list.files(PATH_GRAPHIQUES, pattern = "\\.png$",
                                 full.names = FALSE),
  tableaux          = list.files(PATH_TABLEAUX,
                                 pattern = "\\.(csv|html)$",
                                 full.names = FALSE)
)
saveRDS(resultats_globaux, file.path(PATH_DATASETS, "resultats_globaux.rds"))
cli_alert_success("Objets analytiques sauvegardés : {.path {file.path(PATH_DATASETS, 'resultats_globaux.rds')}}")

# Export des bases d'ANALYSE (post-traitement, enrichies) -> outputs/datasets/analysis/
cli_h1("Export des bases d'analyse (outputs/datasets/analysis/)")
.save_analysis <- function(df, basename) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  tryCatch(
    haven::write_dta(df, file.path(PATH_ANALYSIS, paste0(basename, ".dta"))),
    error = function(e) cli_alert_danger("DTA {basename} : {e$message}"))
  tryCatch(
    readr::write_csv(labelled::to_character(df),
                     file.path(PATH_ANALYSIS, paste0(basename, ".csv")), na = ""),
    error = function(e) cli_alert_danger("CSV {basename} : {e$message}"))
  cli_alert_success("Analyse exportée : {.path {file.path(PATH_ANALYSIS, basename)}}")
}
# Bases enrichies utilisées dans les analyses
if (exists("enfant_anal"))   .save_analysis(enfant_anal,   "enfant_analyse")
if (exists("enfant_main"))   .save_analysis(enfant_main,   "enfant_enrichi")
if (exists("menage_main"))   .save_analysis(menage_main,   "menage_enrichi")
if (exists("df_mod"))        .save_analysis(df_mod,        "regression_modele")
if (exists("enfant_nonvax")) .save_analysis(enfant_nonvax, "enfants_non_vaccines")


# -----------------------------------------------------------------------------
# 18. RESUME FINAL
# -----------------------------------------------------------------------------

cli_h1("Fin du script d'analyse")
cli_alert_success("{length(list.files(PATH_GRAPHIQUES, pattern='\\\\.png$'))} graphiques générés dans {.path {PATH_GRAPHIQUES}}")
cli_alert_success("{length(list.files(PATH_TABLEAUX, pattern='\\\\.(csv|html)$'))} tableaux générés dans {.path {PATH_TABLEAUX}}")
if (length(list.files(PATH_CARTES, pattern = "\\.png$")) > 0)
  cli_alert_success("{length(list.files(PATH_CARTES, pattern='\\\\.png$'))} carte(s) dans {.path {PATH_CARTES}}")
cli_alert_info("Objets analytiques : {.path {file.path(PATH_DATASETS, 'resultats_globaux.rds')}}")

# Fin du script
