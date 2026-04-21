
#' Traitement complet des données IECMAR
#'
#' Cette fonction applique l'ensemble des traitements sur un jeu de données de mares :
#' - Traitements géométriques via `process_traitement_geom`
#' - Transformation et canonisation des réponses Kobo
#' - Calculs particuliers et compilation finale
#'
#' Elle prépare un tableau final avec les colonnes `X_index`, `CAN_name`, `CAN_choice` et `cor_iecmar`.
#'
#' @param df Un data.frame contenant les données initiales des mares et des réponses.
#' @param version Entier correspondant à la version du formulaire kobotoolbox utilisée (default : 5).
#' @param departement Entier utilisée pour sélectionner les bons jeux de données pour les traitements
#' géométriques (soucis de performances).
#' @param use_OS_for_reseaux Un booléen indiquant si la couche d'Occupation du Sol (ici, CLC18 lvl1), doit être
#' utilisée pour framgmenter les réseaux (default: TRUE).
#' @param use_RD_for_reseaux Un booléen indiquant si les réseaux doivent être fragmentées par la routes départementales
#' ou non (dans tous les cas : Type autoroutier, routes nationales, Ligne à Grande Vitesse).
#' @param buffer_size_for_reseaux Numeric, distance en metres appliquee comme tampon autour des pièces d'eau
#' (mares + plans d'eau) pour calculer les connexions au sein d'un réseau (default: 1000).
#' réseaux.
#'
#' @return Un data.frame avec les colonnes `X_index`, `CAN_name`, `CAN_choice` et `cor_iecmar`,
#'         prêt à être utilisé pour les analyses IECMAR.
#'
#' @importFrom dplyr bind_rows mutate select
#'
#' @export
process_all <- function(df, version = 5, departement = NULL,
                        use_OS_for_reseaux = TRUE, use_RD_for_reseaux = FALSE,
                        buffer_size_for_reseaux
) {
  if (!inherits(df, "sf")) {stop("Le jdd fourni n'est pas au format sf")}


  message("### Creation des reseaux")

  reseaux <- compute_reseaux_mares(
    df,
    eau = marecoPckg::eau_max1ha[[as.character(departement)]],
    routes = marecoPckg::routes_RnAu[[as.character(departement)]],
    lgv = marecoPckg::lgv,
    clc = marecoPckg::clc,
    use_OS = use_OS_for_reseaux,
    use_RD = use_RD_for_reseaux,
    buffer_size = buffer_size_for_reseaux
  )
  res_reseaux <- assign_id_reseau_to_mares(df, reseaux)

  message("### Traitements geometriques...")

  res_geom <- process_traitement_geom(
    df,
    eau = marecoPckg::eau[[as.character(departement)]],
    forets = marecoPckg::forets,
    routes = marecoPckg::routes[[as.character(departement)]]
  )

  message("### Correspondances des reponses kobo <-> iecmar")
  kobo_l <- kobo_wide_to_long(df)
  canonique <- process_kobo_canonised(kobo_l, corresp_v4, cor_canonique) # fait les jointures kobo -> canonique -> iecmar
  res_forms <- calculs_particuliers(canonique, version) # genre la surface ellipsoidale...

  message("### Compilation des traitements")
  compil <- bind_rows(res_forms, res_geom) %>%
    mutate(CAN_name = ifelse(is.na(CAN_name), colname, CAN_name),
           CAN_choice = ifelse(is.na(CAN_choice), value, CAN_choice)) %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  message("Calcul de la note iecmar")
  notes_detail <- calcul_iecmar(compil)
  note_simple_long <- output_note_only_l(notes_detail)

  res_sf <- left_join(res_reseaux, note_simple_long, by = "X_index") %>%
    calcul_mediane_iecmar_reseaux()

  res_sf_enhanced <- left_join(res_sf, df %>% select(X_index, photographie_URL, mare_existe) %>% st_drop_geometry(), by = "X_index")

  message("Creation des outputs")
  res <- list(
    resultat = res_sf,
    resultat_photo = res_sf_enhanced,
    notes_details = notes_detail,
    reseaux = reseaux
  )

  return(res)
}

#' Calcul des scores IECMAR
#'
#' Calcule les notes IECMAR à partir d’un jeu de données de réponses.
#' La fonction joint les points associés, applique un traitement des doublons
#' (en conservant la pire note par critère), puis calcule une note brute et
#' une note pondérée sur 20 par individu (`X_index`).
#'
#' @param df Un data.frame contenant au minimum les colonnes `X_index` et `cor_iecmar`.
#'
#' @return Un data.frame enrichi avec les colonnes `points`, `n` et`note`.
#'
#' @importFrom dplyr mutate left_join group_by
#'
calcul_iecmar <- function(df) {
  df_with_pts <- df %>%
    mutate(cor_iecmar = as.integer(cor_iecmar)) %>%
    left_join(iecmar, by = c("cor_iecmar" = "id_iecmar"))

  res <- ajustements_doublons(df_with_pts) %>%
    group_by(X_index) %>%
    mutate(n_critere = sum(!is.na(cor_iecmar)),
           note = sum(points, na.rm = T))

  return(res)
}

#' Ajustement des doublons IECMAR
#'
#' Gère les doublons par critère en conservant, pour chaque combinaison
#' `X_index` et `critere`, la pire note (valeur minimale de `points`).
#' Les lignes avec `critere` manquant (`NA`) sont conservées sans modification.
#'
#' @param df Un data.frame contenant les colonnes `X_index`, `critere` et `points`.
#'
#' @return Un data.frame sans doublons sur les critères non manquants.
#'
#' @importFrom dplyr filter group_by slice_min ungroup bind_rows
#'
ajustements_doublons <- function(df) {
  res_non_na <- df %>%
    filter(!is.na(critere)) %>%
    group_by(X_index, critere) %>%
    slice_min(points, n = 1, with_ties = FALSE) %>%
    ungroup()

  res_na <- df %>%
    filter(is.na(critere))

  bind_rows(res_non_na, res_na)
}

#' Extraire la première note par mare
#'
#' Sélectionne les colonnes clés liées à la notation IECMAR et conserve
#' uniquement la première ligne pour chaque mare (`X_index`).
#'
#' @param df `data.frame`, contenant au moins les colonnes :
#' \code{X_index}, \code{n_critere}, \code{note}
#'
#' @return Un `data.frame` réduit à une ligne par mare avec les colonnes :
#' \code{X_index}, \code{n_critere}, \code{note}
#'
#' @importFrom dplyr select group_by slice
#'
output_note_only_l <- function(df) {
  res <- df %>%
    select(X_index, n_critere, note) %>%
    group_by(X_index) %>%
    slice(1)
}

#' Calcul de la médiane IECMAR par réseau
#'
#' Pour chaque réseau identifié par `id_reseau`, calcule la médiane
#' des notes brutes et pondérées, et indique la position de chaque mare
#' par rapport à cette médiane.
#'
#' @param df `data.frame`, contenant au moins les colonnes :
#' \code{id_reseau}, \code{note}
#'
#' @return Un `data.frame` enrichi avec :
#' \describe{
#'   \item{median_iecmar_reseau}{Médiane des notes brutes par réseau}
#'   \item{median_iecmar_reseau_pon}{Médiane des notes pondérées par réseau}
#'   \item{position_mediane}{Indique si la note est au-dessus ou en dessous de la médiane}
#' }
#'
#' @importFrom dplyr group_by mutate if_else
#'
calcul_mediane_iecmar_reseaux <- function(df) {
  res <- df %>%
    group_by(id_reseau) %>%
    mutate(median_iecmar_reseau = median(note),
           position_mediane = ifelse(note < median_iecmar_reseau, "en dessous", "au dessus")
    )
}

#' Build GeoPackage filename from processing parameters
#'
#' Constructs a standardized GeoPackage (.gpkg) file name based on
#' the parameters used in the `process_all()` workflow. This ensures
#' reproducible and traceable output file naming.
#'
#' @param version Integer. Processing version used in `process_all()`.
#' @param departement Integer or character. Department identifier.
#' @param use_OS_for_reseaux Logical. Whether OS networks are used.
#' @param use_RD_for_reseaux Logical. Whether RD networks are used.
#' @param buffer_size_for_reseaux Numeric. Buffer size used for networks (in meters).
#' @param prefix Character. File name prefix. Default is `"resultats"`.
#' @param dir Character. Output directory path. Default is `"./output"`.
#'
#' @return Character string. Full path to the generated GeoPackage file.
#'
#' @details
#' The file name is constructed using a standardized pattern:
#' `prefix_v{version}_{departement}_{OS/RD flags}_{buffer}m.gpkg`.
#'
#' OS and RD flags are encoded as:
#' \itemize{
#'   \item `"OS"` / `"noOS"` depending on `use_OS_for_reseaux`
#'   \item `"RD"` / `"noRD"` depending on `use_RD_for_reseaux`
#' }
#'
#' This function does not check file existence or create directories.
#'
#' @examples
#' build_gpkg_name(
#'   version = 4,
#'   departement = 25,
#'   use_OS_for_reseaux = FALSE,
#'   use_RD_for_reseaux = FALSE,
#'   buffer_size_for_reseaux = 750
#' )
#'
#' @export
build_gpkg_name <- function(version,
                            departement,
                            use_OS_for_reseaux,
                            use_RD_for_reseaux,
                            buffer_size_for_reseaux,
                            prefix = "resultats",
                            dir = "./output") {

  os <- ifelse(use_OS_for_reseaux, "OS", "noOS")
  rd <- ifelse(use_RD_for_reseaux, "RD", "noRD")

  file_name <- paste0(
    prefix, "_v", version, "_",
    departement, "_",
    os, "_", rd, "_",
    buffer_size_for_reseaux, "m.gpkg"
  )

  file.path(dir, file_name)
}
