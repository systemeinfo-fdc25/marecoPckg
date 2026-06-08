
#' Calcul des scores IECMAR
#'
#' Calcule les notes IECMAR à partir d’un jeu de données de réponses.
#' La fonction joint les points associés, applique un traitement des doublons,
#' puis calcule une note brute et une note pondérée sur 20 par individu (`X_uuid`).
#'
#' @param df Un data.frame contenant au minimum les colonnes `X_uuid` et `cor_iecmar`.
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
    group_by(X_uuid) %>%
    mutate(n_critere = sum(!is.na(cor_iecmar)),
           note = sum(points, na.rm = T))

  return(res)
}

#' Ajustement des doublons IECMAR
#'
#' Gère les doublons par critère en conservant, pour chaque combinaison
#' `X_uuid` et `critere`, la pire note (valeur minimale de `points`).
#' Les lignes avec `critere` manquant (`NA`) sont conservées sans modification.
#'
#' @param df Un data.frame contenant les colonnes `X_uuid`, `critere` et `points`.
#'
#' @return Un data.frame sans doublons sur les critères non manquants.
#'
#' @importFrom dplyr filter group_by slice_min ungroup bind_rows
#'
ajustements_doublons <- function(df) {
  res_non_na <- df %>%
    filter(!is.na(critere)) %>%
    group_by(X_uuid, critere) %>%
    slice_min(points, n = 1, with_ties = FALSE) %>%
    ungroup()

  res_na <- df %>%
    filter(is.na(critere))

  bind_rows(res_non_na, res_na)
}

#' Extraire la première note par mare
#'
#' Sélectionne les colonnes clés liées à la notation IECMAR et conserve
#' uniquement la première ligne pour chaque mare (`X_uuid`).
#'
#' @param df `data.frame`, contenant au moins les colonnes :
#' \code{X_uuid}, \code{n_critere}, \code{note}
#'
#' @return Un `data.frame` réduit à une ligne par mare avec les colonnes :
#' \code{X_uuid}, \code{n_critere}, \code{note}
#'
#' @importFrom dplyr select group_by slice
#'
output_note_only_l <- function(df) {
  res <- df %>%
    select(X_uuid, n_critere, note) %>%
    group_by(X_uuid) %>%
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
    mutate(median_iecmar_reseau = as.integer(median(note)),
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

#' Build GeoPackage filename from processing parameters with date/time
#'
#' @param version Integer. Processing version used in `process_all()`.
#' @param departement Integer or character. Department identifier.
#' @param prefix Character. File name prefix. Default is `"resultats"`.
#'
#' @return Character string. Generated GeoPackage filename.
#'
#' @export
build_gpkg_name2 <- function(version,
                            departement,
                            prefix = "resultats") {
  file_name <- paste0(
    prefix, "_",
    "dep", departement, "_",
    "v", version, "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".gpkg"
  )
}
