#' Creation d'un reseau brut de plans d'eau
#'
#' Genere une couche polygonale continue en fusionnant deux couches d'entrees
#' (mares et plans d'eau), apres application d'un tampon. Cette etape permet
#' de constituer un reseau initial avant tout decoupage ou fragmentation ulterieure.
#'
#' @param mares_kobo `sf`, couche spatiale des mares (ex: issue de Kobo)
#' @param eau_surfacique_ign `sf`, couche spatiale des surfaces en eau (ex: IGN)
#' @param buffer Numeric, distance en metres appliquee comme tampon autour des geometries
#' avant fusion (default: 1000)
#'
#' @return Une couche `sf` de polygones representant le reseau brut fusionne
#'
#' @importFrom dplyr bind_rows
#' @importFrom sf st_buffer st_union st_cast st_as_sf
#'
create_raw_reseau <- function(mares_kobo, eau_surfacique_ign, buffer = 1000) {

  res <- bind_rows(mares_kobo, eau_surfacique_ign) %>%
    st_buffer(buffer) %>%
    st_union() %>%
    st_cast("POLYGON") %>%
    st_as_sf()

  return(res)
}

#' Decoupage d'un reseau par une couche de fragmentation
#'
#' Coupe un reseau polygonal (ex: mares et plans d'eau) a l'aide d'une couche
#' de polygones representant des elements fragmentants (prealablement buffes).
#' Cette operation permet de segmenter le reseau en entites distinctes.
#'
#' @param reseau `sf`, couche polygonale representant le reseau a decouper
#' @param by `sf`, couche polygonale utilisee pour le decoupage (ex: routes bufferisees)
#'
#' @return Une couche `sf` de polygones correspondant au reseau decoupe
#'
#' @importFrom sf st_make_valid st_cast st_as_sf
#' @importFrom rmapshaper ms_erase
#'
cut_reseau_by <- function(reseau, by) {
  res <- reseau %>%
      st_make_valid() %>%
      ms_erase(by) %>%
      st_cast('MULTIPOLYGON') %>%
      st_cast("POLYGON") %>%
      st_as_sf()

  return(res)
}


#' Génération et fragmentation du réseau de mares
#'
#' Crée un réseau polygonal en fusionnant les mares et les pièces d'eau, puis
#' fragmente ce réseau à l'aide des infrastructures linéaires de transport
#' (routes, LGV) et, optionnellement, d'une couche d'occupation du sol.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param eau `sf`, couche polygonale représentant les pièces d'eau
#' @param routes `sf`, couche linéaire représentant les routes
#' @param lgv `sf`, couche linéaire représentant les lignes à grande vitesse
#' @param clc `sf` ou NULL, couche polygonale d'occupation du sol (optionnel)
#' @param use_OS Logical, si TRUE et si `clc` est fourni, applique la couche
#' d'occupation du sol pour fragmenter le réseau (default: TRUE)
#' @param use_RD Logical, si
#' @param buffer_size_for_reseaux Numeric, distance en metres appliquee comme tampon autour des pièces d'eau
#' (mares + plans d'eau) pour calculer les connexions au sein d'un réseau (default: 1000).
#'
#' @return Une couche `sf` de polygones représentant les réseaux de mares,
#' avec les colonnes suivantes :
#' \describe{
#'   \item{id_reseau}{Identifiant du réseau (rmapshaperid)}
#'   \item{nb_mares_reseau}{Nombre de mares intersectant chaque polygone du réseau}
#'   \item{geometry}{Géométrie des polygones du réseau}
#' }
#'
#' @details
#' - Les mares sont fusionnées avec les pièces d'eau via `create_raw_reseau()`.
#' - Le réseau est découpé avec les couches linéaires `routes` et `lgv`.
#' - Si `use_OS = TRUE` et que `clc` est fourni, la couche d'occupation du sol
#'   est utilisée pour fragmenter davantage le réseau.
#' - Chaque polygone du réseau contient le nombre de mares qu'il contient.
#'
#' @importFrom dplyr select group_by slice mutate rename
#' @importFrom sf st_intersects
#'
compute_reseaux_mares <- function(kobo, eau, routes, lgv, clc = NULL, use_OS = TRUE, use_RD = FALSE, buffer_size) {

  kobo <- kobo %>%
    # garde seulement les mares existantes (ou sans info)
    filter(is.na(mare_existe) | mare_existe %in% c('oui', 'peut_etre', 'existe')) %>%
    # garde seulement les mares < 1ha (ou sans info)
    filter(
      is.na(mare_longueur) |
      is.na(mare_largeur) |
      (pi/4 * (as.integer(mare_longueur) * as.integer(mare_largeur))) < 10000 # Garde si inf a 1Ha
    )

  if (use_RD) {
    routes <- routes %>%
      prepare_negatif_layer(.03)
  } else {
    routes <- routes %>%
      filter(grepl("Autoroute", cpx_classe) | grepl("Nationale", cpx_classe)) %>% # pour virer RD
      prepare_negatif_layer(.03)
  }

  tampon_eau <- kobo %>%
    select(X_index) %>%
    create_raw_reseau(., eau, buffer = buffer_size)

  mares_sans_doublons <- kobo %>%
    group_by(geometry) %>%
    slice(1)

  res <- cut_reseau_by(tampon_eau, routes) %>%
         cut_reseau_by(., lgv)

  if (use_OS & !is.null(clc)) {
    res <- cut_reseau_by(res, clc)
  }

  res <- res %>%
    # Actuellement, compte seulement les mares. Ajouter un comptage pour les plans d'eau ?
    mutate(nb_mares_reseau = lengths(st_intersects(., mares_sans_doublons))) %>%
    mutate(nb_eau_reseau = lengths(st_intersects(., eau))) %>%
    mutate(reseau_valide = ifelse(
      (nb_mares_reseau + nb_eau_reseau >= 5) & (nb_mares_reseau >= 3),
      TRUE,
      FALSE)) %>%
    rename(id_reseau = "rmapshaperid")

  return(res)
}

#' Associer les mares à l'identifiant de leur réseau
#'
#' Pour chaque mare, ajoute l'identifiant du réseau polygonal auquel elle appartient
#' ainsi que le nombre de mares dans ce réseau.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param reseau `sf`, couche de polygones représentant les réseaux de mares
#'
#' @return Un objet `sf` avec les identifiants des reseaux de mares.
#'
#' @importFrom dplyr mutate select
#'
assign_id_reseau_to_mares <- function(kobo, reseau) {
  kobo_enhanced <- st_join_nearest(kobo, reseau) %>%
    # mutate(fid = row.names(.)) %>%
    select(X_index, id_reseau, nb_mares_reseau)

  return(kobo_enhanced)
}
