# =============================================================================
# SCRIPT DE TRAITEMENT DES DONNEES
# Enquête de Couverture Post-Vaccinale (ECP) Rougeole-Rubéole en RDC
# OMS RDC - 2025/2026
# =============================================================================
# Auteur  : Charles Mouté (révisé)
# Date    : Mai 2026
# Objet   :
#   Importer les données SurveyCTO (ou, à défaut, les .dta locaux) pour les
#   formulaires : monitoring, dénombrement, listing, ménage (main + backcheck),
#   enfant (main + backcheck), supervision.
#   Appliquer les filtres temporels et de validité, supprimer les doublons et
#   exporter les jeux nettoyés au format CSV et STATA.
#   Calculer quelques statistiques de pilotage (taux de couverture / refus /
#   non-enquêtés) et exporter le tout dans des fichiers Excel.
#
# Modes de fonctionnement :
#  - Si les credentials SurveyCTO sont disponibles ET que la fonction
#    `label_xlsform()` est sourcée, on récupère les données via l'API.
#  - Sinon (mode hors-ligne / réutilisation), on lit les .dta présents dans
#    `data/`. Ceci permet de re-exécuter le pipeline sans connexion.
# =============================================================================


# --- 0. CONFIGURATION ET CHARGEMENT DES PACKAGES ----------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org", quiet = TRUE)
}
pacman::p_load(
  dplyr, readr, lubridate, stringr, haven, tidyverse, cli, glue, rio,
  labelled, readxl
)
# rsurveycto est optionnel (mode online)
has_scto <- requireNamespace("rsurveycto", quietly = TRUE)

# Évite l'affichage en notation scientifique
options(scipen = 999, dplyr.summarise.inform = FALSE)


# --- 1. PATHS, DOSSIERS, PARAMETRES ------------------------------------------

PATH_INPUT  <- "data"
PATH_OUTPUT <- "outputs/datasets"

# Création des dossiers de sortie
dirs_out <- c("outputs", "outputs/datasets", "outputs/graphiques",
              "outputs/tableaux", "outputs/cartes", "outputs/rapport")
walk(dirs_out, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))

# Nom des fichiers de sortie
FILENAME_MONITORING         <- "monitoring"
FILENAME_LISTING            <- "liste_menage"
FILENAME_ENUMERATION        <- "denombrement"
FILENAME_HOUSEHOLD_MAIN     <- "menage_main"
FILENAME_HOUSEHOLD_BACKCHECK<- "menage_backchek"
FILENAME_CHILD_MAIN         <- "enfant_main"
FILENAME_CHILD_BACKCHECK    <- "enfant_backchek"
FILENAME_SUPERVISION        <- "supervision"

# Dates / bloc / users à inclure
START_DATE  <- ymd("2026-04-26")
END_DATE    <- START_DATE + 15
BLOC_LIST   <- c(1)
USERNAMES   <- c(
  "interviewer_tr01","interviewer_tr02","interviewer_tr03",
  "monitor_tr01","monitor_tr02","monitor_tr03",
  "supervisor_tr01","supervisor_tr02","supervisor_tr03"
)
HH_TO_EXCLUDE    <- c(180050020210018)
CHILD_TO_EXCLUDE <- c()
ZD_TO_EXCLUDE    <- c()

# Liste des suppressions manuelles (données soupçonnées de falsification)
FRAUD_FILTERS <- list(
  list(aire = 14001001061, date_max = ymd("2026-05-05")),
  list(aire = 18005001138, date_max = ymd("2026-05-13"))
)


# --- 2. MODE ONLINE (SurveyCTO) ou OFFLINE (.dta) ----------------------------

USE_SCTO <- FALSE
SCTO_AUTH <- NULL

if (has_scto && nzchar(Sys.getenv("SCTO_SERVER"))) {
  USE_SCTO <- tryCatch({
    SCTO_AUTH <- rsurveycto::scto_auth(
      server   = Sys.getenv("SCTO_SERVER"),
      username = Sys.getenv("SCTO_USERNAME"),
      password = Sys.getenv("SCTO_PASSWORD")
    )
    cli_alert_success("Connexion SurveyCTO établie - mode ONLINE.")
    if (file.exists("label_xlsform.R")) source("label_xlsform.R")
    TRUE
  }, error = function(e) {
    cli_alert_warning("Echec connexion SurveyCTO ({e$message}) - bascule en mode OFFLINE.")
    FALSE
  })
} else {
  cli_alert_info("rsurveycto non disponible ou variables d'env manquantes - mode OFFLINE.")
}

# Fallback : si label_xlsform n'est pas dispo, on définit une version no-op
if (!exists("label_xlsform", mode = "function")) {
  label_xlsform <- function(data, xlsform = NULL) data
}


# --- 3. HELPERS DE LECTURE ---------------------------------------------------

# Mapping dataset -> XLSForm (utilise pour normaliser les codes vers XLSForm)
FORM_PATHS <- list(
  denombrement         = "data/forms/01_tr_listing.xlsx",
  menage_main          = "data/forms/02_tr_screening.xlsx",
  menage_backcheck     = "data/forms/04_tr_bcscreening.xlsx",
  enfant_main          = "data/forms/03_tr_survey.xlsx",
  enfant_backcheck     = "data/forms/05_tr_bcsurvey.xlsx",
  supervision          = "data/forms/06_tr_qualityCheck.xlsx"
)

# -----------------------------------------------------------------------------
# HELPERS DE NORMALISATION DES CODES VERS LE STANDARD XLSFORM
# -----------------------------------------------------------------------------
# Objectif : quelle que soit la source (API SurveyCTO ou .dta existants),
# les fichiers .dta exportés utilisent des CODES uniformes alignés sur le
# XLSForm de référence. Pour les variables oui/non :
#   0 = Non,  1 = Oui,  -99 = Ne sait pas
# Pour les autres select_one : codes définis dans l'onglet `choices` du XLSForm.
#
# .lbl_to_num(x, var_name, xlsform_path) :
#   - Pour une variable haven_labelled, retourne un nouveau labelled où chaque
#     valeur a été ré-encodée selon les codes du XLSForm, en s'appuyant sur le
#     TEXTE des labels (insensible casse/accents, supporte FR + EN).
#   - Si la variable n'est pas labellisée OU si pas de correspondance XLSForm,
#     la variable est renvoyée telle quelle.
#
# .normalize_codes(df, xlsform_path) :
#   - Applique .lbl_to_num() à toutes les variables select_one du XLSForm
#     présentes dans le data.frame.
#   - Idempotent : appliquer plusieurs fois ne change rien si les codes sont
#     déjà alignés sur le XLSForm.
# -----------------------------------------------------------------------------

.lbl_to_num <- function(x, var_name, xlsform_path, verbose = FALSE) {
  if (is.null(x)) return(x)
  if (!inherits(x, c("haven_labelled", "haven_labelled_spss"))) return(x)
  if (!file.exists(xlsform_path)) return(x)

  # Lecture XLSForm (cache simple : on relit à chaque appel mais c'est rapide)
  survey <- tryCatch(
    suppressMessages(readxl::read_excel(xlsform_path, sheet = "survey",
                                        col_types = "text", trim_ws = TRUE)),
    error = function(e) NULL)
  choices <- tryCatch(
    suppressMessages(readxl::read_excel(xlsform_path, sheet = "choices",
                                        col_types = "text", trim_ws = TRUE)),
    error = function(e) NULL)
  if (is.null(survey) || is.null(choices)) return(x)
  names(survey)  <- trimws(names(survey))
  names(choices) <- trimws(names(choices))

  # Identifier le list_name de la variable
  row <- survey[!is.na(survey$type) & !is.na(survey$name) &
                survey$name == var_name, , drop = FALSE]
  if (nrow(row) == 0) return(x)
  type_str <- gsub("\\s+", " ", trimws(row$type[1]))
  if (!grepl("^select_one\\s", type_str)) return(x)
  list_name <- trimws(sub("^select_one\\s+", "", type_str))

  ch_sub <- choices[!is.na(choices$list_name) &
                    trimws(choices$list_name) == list_name, , drop = FALSE]
  if (nrow(ch_sub) == 0) return(x)

  # Choisir la colonne label : préférer label::French (fr) puis label puis 1ère
  lbl_cols <- grep("^label", names(ch_sub), value = TRUE)
  if (length(lbl_cols) == 0) return(x)
  fr_cols <- grep("french|fr\\)", lbl_cols, ignore.case = TRUE, value = TRUE)
  lbl_col_pref <- if (length(fr_cols) > 0) fr_cols[1]
                  else if ("label" %in% lbl_cols) "label"
                  else lbl_cols[1]
  # Aussi récupérer les autres langues pour matcher
  lbl_cols_all <- lbl_cols

  target_codes <- suppressWarnings(as.numeric(ch_sub$name))
  if (all(is.na(target_codes))) return(x)  # liste non numérique : on n'y touche pas
  target_labels_main <- ch_sub[[lbl_col_pref]]

  # Construire un dictionnaire {label_norm -> code} sur TOUTES les colonnes labels
  build_map <- function() {
    map_norm <- character(0)
    map_code <- numeric(0)
    for (col in lbl_cols_all) {
      lbl <- ch_sub[[col]]
      lbl_norm <- iconv(tolower(trimws(lbl)), to = "ASCII//TRANSLIT")
      ok <- !is.na(lbl_norm) & lbl_norm != ""
      map_norm <- c(map_norm, lbl_norm[ok])
      map_code <- c(map_code, target_codes[ok])
    }
    # Dédoublonner en conservant la première occurrence
    dup <- duplicated(map_norm)
    setNames(map_code[!dup], map_norm[!dup])
  }
  lbl2code <- build_map()

  # Convertir les valeurs actuelles -> texte via les labels présents
  text_vals <- suppressWarnings(as.character(haven::as_factor(x)))
  text_norm <- iconv(tolower(trimws(text_vals)), to = "ASCII//TRANSLIT")
  out_num <- unname(lbl2code[text_norm])

  # Compter les valeurs perdues (cas non mappés)
  n_orig <- sum(!is.na(x))
  n_mapped <- sum(!is.na(out_num))
  if (verbose && n_orig > n_mapped) {
    cli::cli_alert_warning(
      "{var_name} : {n_orig - n_mapped} valeur(s) sans correspondance label."
    )
  }

  # Construire le nouveau vecteur labelled (labels XLSForm, codes XLSForm)
  val_lbls <- setNames(target_codes, target_labels_main)
  # Supprimer les NA dans les labels (cas dégénéré)
  val_lbls <- val_lbls[!is.na(val_lbls) & !is.na(names(val_lbls))]
  result <- tryCatch(
    labelled::labelled(out_num, labels = val_lbls),
    error = function(e) out_num
  )

  # Préserver l'étiquette de variable
  var_lbl <- tryCatch(labelled::var_label(x), error = function(e) NULL)
  if (!is.null(var_lbl) && is.character(var_lbl) && length(var_lbl) == 1 &&
      !is.na(var_lbl) && nchar(var_lbl) > 0 && inherits(result, "haven_labelled")) {
    labelled::var_label(result) <- var_lbl
  }

  result
}

.normalize_codes <- function(df, xlsform_path, verbose = TRUE) {
  if (is.null(df) || !file.exists(xlsform_path)) return(df)
  survey <- tryCatch(
    suppressMessages(readxl::read_excel(xlsform_path, sheet = "survey",
                                        col_types = "text", trim_ws = TRUE)),
    error = function(e) NULL)
  if (is.null(survey)) return(df)
  names(survey) <- trimws(names(survey))

  sv <- survey[!is.na(survey$type) & !is.na(survey$name), , drop = FALSE]
  sv$type_base <- sub("\\s.*$", "", trimws(sv$type))
  vars <- sv$name[sv$type_base == "select_one" & sv$name %in% names(df)]

  if (verbose && length(vars) > 0) {
    cli::cli_alert_info("Normalisation des codes XLSForm : {length(vars)} variable(s) select_one ({basename(xlsform_path)})")
  }
  for (v in vars) df[[v]] <- .lbl_to_num(df[[v]], v, xlsform_path, verbose = verbose)
  df
}

# Lecture .dta locale (mode OFFLINE)
read_local_dta <- function(filename) {
  path <- file.path(PATH_INPUT, paste0(filename, ".dta"))
  if (!file.exists(path)) {
    cli_alert_warning("Fichier introuvable : {.path {path}}")
    return(NULL)
  }
  tryCatch({
    d <- haven::read_dta(path)
    cli_alert_success("Lecture {.path {path}} : {nrow(d)} lignes")
    d
  }, error = function(e) {
    cli_alert_danger("Erreur lecture {.path {path}} : {e$message}")
    NULL
  })
}

# Lecture SurveyCTO (mode ONLINE)
read_scto <- function(form_name, xlsform_path = NULL) {
  if (!USE_SCTO) return(NULL)
  tryCatch({
    d <- rsurveycto::scto_read(SCTO_AUTH, form_name)
    if (!is.null(xlsform_path) && file.exists(xlsform_path)) {
      d <- label_xlsform(d, xlsform = xlsform_path)
    }
    cli_alert_success("SurveyCTO : << {form_name} >> ({nrow(d)} lignes)")
    d
  }, error = function(e) {
    cli_alert_danger("Echec lecture SurveyCTO << {form_name} >> : {e$message}")
    NULL
  })
}

# Filtre temporel + utilisateurs + bloc + suppressions manuelles + déduplication KEY
.apply_standard_filters <- function(df, date_col = "interview_date") {
  if (is.null(df)) return(NULL)
  if (!date_col %in% names(df)) {
    # Tolérance : interviewdate ou interview_date
    if ("interviewdate" %in% names(df)) date_col <- "interviewdate"
  }
  if (!date_col %in% names(df)) {
    cli_alert_warning("Pas de colonne de date trouvée - filtre temporel ignoré.")
    return(df)
  }

  df <- df %>%
    mutate(.date_check = as.Date(.data[[date_col]]))

  if ("username" %in% names(df))
    df <- df %>% filter(username %in% USERNAMES | is.na(username))
  if ("bloc" %in% names(df))
    df <- df %>% filter(bloc %in% BLOC_LIST | is.na(bloc))

  df <- df %>% filter(.date_check >= START_DATE & .date_check <= END_DATE | is.na(.date_check))

  # Suppressions manuelles
  if ("aire_sante_id" %in% names(df)) {
    for (f in FRAUD_FILTERS) {
      df <- df %>% filter(!(aire_sante_id == f$aire & .date_check <= f$date_max))
    }
  }

  # Dédoublonnage sur KEY (le cas échéant)
  if ("KEY" %in% names(df)) {
    sort_col <- if ("submissionDate" %in% names(df)) "submissionDate" else date_col
    df <- df %>%
      group_by(KEY) %>%
      arrange(desc(.data[[sort_col]]), .by_group = TRUE) %>%
      slice_head(n = 1) %>%
      ungroup()
  }
  df %>% select(-.date_check)
}


# --- 4. CHARGEMENT DES JEUX DE DONNEES ---------------------------------------

cli_h1("Chargement des jeux de données")

# Monitoring (pas de XLSForm specifique, on ne normalise pas)
monitoring <- if (USE_SCTO) {
  m <- read_scto("tr_monitoring")
  if (!is.null(m)) m <- m %>% mutate(ref_date = as_date(ref_date))
  # Correction des variables inutiles
  m %>% 
    rename(
      total_bc_hh_int_eligible  = total_bc_hh_interviewed_eligible,
      total_bc_pop_int_eligible = total_bc_pop_interviewed_eligible
    )
} else {
  read_local_dta(FILENAME_MONITORING)
}

# Liste menage
liste_menage <- if (USE_SCTO) {
  read_scto("tr_hh_listing")
} else {
  read_local_dta(FILENAME_LISTING)
}

# Dénombrement
denombrement <- if (USE_SCTO) {
  d <- read_scto("tr_listing", FORM_PATHS$denombrement)
  d %>% .apply_standard_filters(date_col = "interview_date")
} else {
  read_local_dta(FILENAME_ENUMERATION) %>%
    .normalize_codes(FORM_PATHS$denombrement) %>%
    .apply_standard_filters("interview_date")
}

# Menage main + backcheck
household_main <- if (USE_SCTO) {
  d <- read_scto("tr_screening", FORM_PATHS$menage_main)
  d %>% .apply_standard_filters("interviewdate")
} else {
  read_local_dta(FILENAME_HOUSEHOLD_MAIN) %>%
    .normalize_codes(FORM_PATHS$menage_main) %>%
    .apply_standard_filters("interviewdate")
}

household_backcheck <- if (USE_SCTO) {
  d <- read_scto("tr_bcscreening", FORM_PATHS$menage_backcheck)
  d %>% .apply_standard_filters("interviewdate")
} else {
  read_local_dta(FILENAME_HOUSEHOLD_BACKCHECK) %>%
    .normalize_codes(FORM_PATHS$menage_backcheck) %>%
    .apply_standard_filters("interviewdate")
}

# Enfant main + backcheck
child_main <- if (USE_SCTO) {
  d <- read_scto("tr_survey", FORM_PATHS$enfant_main)
  d %>% .apply_standard_filters("interviewdate")
} else {
  read_local_dta(FILENAME_CHILD_MAIN) %>%
    .normalize_codes(FORM_PATHS$enfant_main) %>%
    .apply_standard_filters("interviewdate")
}

# --- Création de hhid : Identifiant ménage---
if (!is.null(child_main)) {
  # Utiliser la colonne caseid (ou child_caseid si elle existe)
  id_col <- if ("child_caseid" %in% names(child_main)) "child_caseid" else "caseid"
  child_main <- child_main %>%
    mutate(hhid = str_sub(.data[[id_col]], 1, nchar(.data[[id_col]]) - 2))
}

child_backcheck <- if (USE_SCTO) {
  d <- read_scto("tr_bcsurvey", FORM_PATHS$enfant_backcheck)
  d %>% .apply_standard_filters("interviewdate")
} else {
  read_local_dta(FILENAME_CHILD_BACKCHECK) %>%
    .normalize_codes(FORM_PATHS$enfant_backcheck) %>%
    .apply_standard_filters("interviewdate")
}

# --- Création de hhid : Identifiant ménage---
if (!is.null(child_backcheck)) {
  id_col <- if ("child_caseid" %in% names(child_backcheck)) "child_caseid" else "caseid"
  child_backcheck <- child_backcheck %>%
    mutate(hhid = str_sub(.data[[id_col]], 1, nchar(.data[[id_col]]) - 2))
}

# Supervision
supervision <- if (USE_SCTO) {
  d <- read_scto("tr_qualityCheck", FORM_PATHS$supervision)
  d %>% .apply_standard_filters("interview_date")
} else {
  read_local_dta(FILENAME_SUPERVISION) %>%
    .normalize_codes(FORM_PATHS$supervision) %>%
    .apply_standard_filters("interviewdate")
}


# --- 5. EXPORT (CSV + STATA) -------------------------------------------------

cli_h1("Export des jeux nettoyés")

# Helper d'export sécurisé
.export_dataset <- function(df, basename) {
  if (is.null(df)) {
    cli_alert_warning("Jeu non exporté (vide) : {basename}")
    return(invisible(NULL))
  }
  # CSV (sans labels haven, en chaînes)
  csv_path <- file.path(PATH_OUTPUT, paste0(basename, ".csv"))
  tryCatch({
    write_csv(labelled::to_character(df), csv_path, na = "")
    cli_alert_success("Export CSV : {.path {csv_path}}")
  }, error = function(e) cli_alert_danger("CSV {basename} : {e$message}"))

  # STATA -> outputs/datasets
  dta_path <- file.path(PATH_OUTPUT, paste0(basename, ".dta"))
  tryCatch({
    write_dta(df, dta_path)
    cli_alert_success("Export DTA : {.path {dta_path}}")
  }, error = function(e) cli_alert_danger("DTA {basename} : {e$message}"))

  # STATA -> data (copie pour réutilisation par les scripts d'analyse)
  dta_path_data <- file.path(PATH_INPUT, paste0(basename, ".dta"))
  tryCatch({
    write_dta(df, dta_path_data)
    cli_alert_success("Export DTA (data) : {.path {dta_path_data}}")
  }, error = function(e) cli_alert_danger("DTA data {basename} : {e$message}"))
}

.export_dataset(monitoring,         FILENAME_MONITORING)
.export_dataset(liste_menage,       FILENAME_LISTING)
.export_dataset(denombrement,       FILENAME_ENUMERATION)
.export_dataset(household_main,     FILENAME_HOUSEHOLD_MAIN)
.export_dataset(household_backcheck,FILENAME_HOUSEHOLD_BACKCHECK)
.export_dataset(child_main,         FILENAME_CHILD_MAIN)
.export_dataset(child_backcheck,    FILENAME_CHILD_BACKCHECK)
.export_dataset(supervision,        FILENAME_SUPERVISION)


# --- 6. COMMENTAIRES (EXCEL UNIQUE) ------------------------------------------

cli_h1("Export Excel des commentaires")

select_safe <- function(df, cols) {
  if (is.null(df)) return(NULL)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(NULL)
  df %>% select(all_of(cols)) %>%
    { if ("commentaires" %in% names(.)) filter(., !is.na(commentaires)) else . } %>%
    distinct()
}

db_comment <- list(
  denombrement     = select_safe(denombrement,
                                 c("aire_sante_id","aire_sante_name","commentaires")),
  menage_main      = select_safe(household_main,
                                 c("aire_sante_id","aire_sante_name","caseid",
                                   "case_label","commentaires")),
  enfant_main      = select_safe(child_main,
                                 c("aire_sante_id","aire_sante_name","caseid",
                                   "case_label","hhid","commentaires")),
  menage_backcheck = select_safe(household_backcheck,
                                 c("aire_sante_id","aire_sante_name","caseid",
                                   "case_label","commentaires")),
  enfant_backcheck = select_safe(child_backcheck,
                                 c("aire_sante_id","aire_sante_name","caseid",
                                   "case_label","hhid","commentaires")),
  supervision      = select_safe(supervision,
                                 c("aire_sante_id","aire_sante_name",
                                   "operation_name","description_difficultes",
                                   "solutions_apportees","commentaires"))
)
db_comment <- Filter(Negate(is.null), db_comment)

if (length(db_comment) > 0) {
  tryCatch({
    rio::export(db_comment, file.path(PATH_OUTPUT, "commentaires.xlsx"))
    cli_alert_success("Export Excel commentaires : {.path {file.path(PATH_OUTPUT, 'commentaires.xlsx')}}")
  }, error = function(e) cli_alert_danger("Export commentaires.xlsx : {e$message}"))
}


# --- 7. STATISTIQUES DE PILOTAGE (TAUX COUVERTURE/REFUS/NON ENQUETE) --------

cli_h1("Statistiques de pilotage")

# Helper : ligne de total
.add_totals <- function(df, group_col = "zone_sante_name",
                        subgroup_col = "aire_sante_name",
                        sum_cols, prop_calc = NULL) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Convertir la colonne de sous-groupe en caractère (évite l’incompatibilité)
  df <- df %>% mutate(!!subgroup_col := as.character(.data[[subgroup_col]]))
  
  tot_zone <- df %>%
    group_by(.data[[group_col]]) %>%
    summarise(across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(!!subgroup_col := "TOTAL") %>%
    select(all_of(c(group_col, subgroup_col, sum_cols)))
  tot_glob <- df %>%
    summarise(across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(!!group_col := "TOTAL", !!subgroup_col := "TOTAL") %>%
    select(all_of(c(group_col, subgroup_col, sum_cols)))
  bind_rows(df, tot_zone, tot_glob) %>%
    arrange(.data[[group_col]] == "TOTAL", .data[[group_col]],
            .data[[subgroup_col]] == "TOTAL", .data[[subgroup_col]])
}

# Helper : formatte les pourcentages
.fmt_rates <- function(df) {
  df %>%
    mutate(across(starts_with("tx_"),
                  ~ scales::percent(.x, accuracy = 1, na.rm = TRUE)))
}

# Dénombrement / Eligibilité
tab_denom <- NULL
tab_elig  <- NULL
if (!is.null(denombrement)) {
  occ <- if ("occupation" %in% names(denombrement)) {
    as.numeric(haven::zap_labels(denombrement$occupation))
  } else rep(1, nrow(denombrement))
  enu <- if ("enumerated_hh" %in% names(denombrement)) {
    as.numeric(haven::zap_labels(denombrement$enumerated_hh))
  } else rep(1, nrow(denombrement))
  he <- if ("total_hh_eligible" %in% names(denombrement)) {
    as.numeric(haven::zap_labels(denombrement$total_hh_eligible))
  } else NA
  denom_clean <- denombrement %>%
    mutate(.occ = occ, .enu = enu, .he = he) %>%
    filter(.occ == 1 & .enu == 1)

  tab_denom <- denom_clean %>%
    count(zone_sante_name, aire_sante_name, name = "menage_denombre")
  tab_elig <- denom_clean %>%
    filter(.he == 1) %>%
    count(zone_sante_name, aire_sante_name, name = "menage_eligible")

  tab_denom <- tab_denom %>%
    left_join(tab_elig, by = c("zone_sante_name","aire_sante_name")) %>%
    mutate(menage_eligible = replace_na(menage_eligible, 0L),
           tx_eligibilite  = menage_eligible / menage_denombre)
  tab_denom <- .add_totals(tab_denom, sum_cols = c("menage_denombre","menage_eligible")) %>%
    mutate(tx_eligibilite = menage_eligible / menage_denombre) %>%
    .fmt_rates()
}

# Enquête ménage principale
calc_enq_table <- function(df, nb_attendu_par_aire = 10) {
  if (is.null(df)) return(NULL)
  ps <- if ("part_status" %in% names(df)) as.numeric(haven::zap_labels(df$part_status)) else NA
  co <- if ("consent" %in% names(df)) as.numeric(haven::zap_labels(df$consent)) else NA
  iv <- if ("interviewed" %in% names(df)) as.numeric(haven::zap_labels(df$interviewed)) else NA
  df %>%
    mutate(
      .realise = as.integer(!is.na(ps) & ps > 0 & co == 1 & iv == 1),
      .refus   = as.integer((ps == 0) | (co == 0))
    ) %>%
    group_by(zone_sante_name, aire_sante_name) %>%
    summarise(nb_questionnaire = nb_attendu_par_aire,
              realise = sum(.realise, na.rm = TRUE),
              refus   = sum(.refus, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(non_enquete       = pmax(0, nb_questionnaire - (realise + refus)),
           tx_couverture     = (realise + refus) / nb_questionnaire,
           tx_participation  = realise / nb_questionnaire,
           tx_refus          = refus / nb_questionnaire,
           tx_non_enquete    = non_enquete / nb_questionnaire) %>%
    .add_totals(sum_cols = c("nb_questionnaire","realise","refus","non_enquete")) %>%
    mutate(tx_couverture    = (realise + refus) / nb_questionnaire,
           tx_participation = realise / nb_questionnaire,
           tx_refus         = refus / nb_questionnaire,
           tx_non_enquete   = non_enquete / nb_questionnaire) %>%
    .fmt_rates()
}

tab_menage_main <- calc_enq_table(household_main,     nb_attendu_par_aire = 10)
tab_menage_bc   <- calc_enq_table(household_backcheck, nb_attendu_par_aire = 4)


# --- 8. EXPORT EXCEL DES STATISTIQUES DE PILOTAGE ----------------------------

db_result <- list(
  `Dénombrement`                = tab_denom,
  `Enquête principale (ménage)` = tab_menage_main,
  `Enquête backcheck (ménage)`  = tab_menage_bc
)
db_result <- Filter(Negate(is.null), db_result)

if (length(db_result) > 0) {
  tryCatch({
    rio::export(db_result, file.path(PATH_OUTPUT, "resultats_pilotage.xlsx"))
    cli_alert_success("Export Excel pilotage : {.path {file.path(PATH_OUTPUT, 'resultats_pilotage.xlsx')}}")
  }, error = function(e) cli_alert_danger("Export pilotage : {e$message}"))
}

cli_h1("Traitement terminé")
