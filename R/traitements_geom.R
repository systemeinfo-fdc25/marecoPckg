
#' Find the nearest features and extract distances and attributes
#'
#' For each feature in object `a`, this function finds the nearest feature in object `b`,
#' computes the distance to it, and optionally extracts specified attribute columns from `b`.
#'
#' @param a An `sf` object (typically point or line geometry) for which to find nearest features.
#' @param b An `sf` object (point, line or polygon) representing the target features to search from.
#' @param b_cols Optional character vector of column names from `b` to extract and add to the output.
#'
#' @return An `sf` object identical to `a` with an added `dist` column (units in meters, if CRS is projected)
#'         and, if requested, additional columns from `b` corresponding to the nearest features.
#'
#' @examples
#' \dontrun{
#'   # Find distance only
#'   result <- minimal_distance(a, b)
#'
#'   # Find distance and add 'id' and 'type' columns from nearest feature in b
#'   result <- minimal_distance(a, b, b_cols = c("id", "type"))
#' }
#'
#' @importFrom sf st_nearest_feature st_distance
#'
find_nearest <- function(a, b, b_cols = NULL) {
  nearest_features <- st_nearest_feature(a, b)
  dist <- st_distance(a, b[nearest_features, ], by_element = TRUE)

  out <- a
  out$dist <- dist

  if (!is.null(b_cols)) {
    if (any(!b_cols %in% colnames(b))) {
      stop("Some columns in 'b_cols' were not found in 'b'")
    }
    for (col in b_cols) {
      out[[col]] <- b[[col]][nearest_features]
    }
  }

  return(out)
}

#' Jointure spatiale sur le plus proche voisin
#'
#' Associe a chaque geometrie de `x` les attributs de la geometrie la plus proche
#' dans `y`. Une distance maximale peut etre definie pour limiter les correspondances.
#' Si aucune geometrie de `y` n'est trouvee dans cette distance, les attributs ajoutes
#' sont renseignes a `NA`.
#'
#' @param x `sf`, couche spatiale a laquelle on souhaite ajouter les attributs (ex: points)
#' @param y `sf`, couche spatiale de reference utilisee pour la jointure (ex: polygones)
#' @param max_distance Numeric ou objet `units`, distance maximale en metres pour accepter
#' une correspondance (default: `Inf`)
#'
#' @return Une couche `sf` contenant les geometries de `x` avec les attributs de `y`
#' correspondant a la geometrie la plus proche
#'
#' @importFrom sf st_nearest_feature st_distance st_drop_geometry
#' @importFrom dplyr bind_cols
#' @importFrom units set_units
#'
st_join_nearest <- function(x, y, max_distance = Inf) {
  # x : objet sf à joindre (ex: points)
  # y : objet sf de référence (ex: polygones)
  # max_distance : distance max pour considérer une correspondance

  max_distance <- set_units(max_distance, "m")

  # Trouver l'indice du plus proche voisin dans y pour chaque géométrie de x
  nearest_idx <- st_nearest_feature(x, y)

  # Calculer la distance pour filtrer si nécessaire
  dist <- st_distance(x, y[nearest_idx, ], by_element = TRUE)

  # Appliquer la distance max
  nearest_idx[dist > max_distance] <- NA

  result <- bind_cols(
    x,
    y[nearest_idx, ] %>% st_drop_geometry()
  )

  return(result)
}


#' Préparation de la couche forêt en excluant les zones autour des mares
#'
#' @description
#' En réponse aux contraintes de la C13 du IECMAr : "Distance avec un site terrestre hivernal (pas pris
#' en compte à moins de 100m)", supprime des surfaces forestières les zones situées à proximité des
#' mares en appliquant un tampon autour de celles-ci.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param forets `sf`, couche de polygones des forêts > 1ha provenant de l'OCS-GE, simplifié
#' à 95%
#' @param tampon Numeric, distance en mètres utilisée pour créer un tampon autour
#' des mares (default: 100)
#'
#' @return Une couche `sf` correspondant aux surfaces forestières excluant les
#' zones tampon autour des mares
#'
#' @importFrom sf st_buffer
#' @importFrom rmapshaper ms_erase
#'
prepare_data_foret <- function(kobo, forets, tampon = 100) {

  forets_erased <- ms_erase(forets, st_buffer(kobo, tampon))

  return(forets_erased)
}

#' Préparation de la couche hydrographique en excluant les zones proches des mares
#'
#' Filtre les surfaces en eau pour exclure celles situées à proximité des mares
#' déjà étudiées, en appliquant un tampon autour des points Kobo.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param hydro `sf`, couche polygonale des surfaces en eau
#' @param tampon Numeric, distance en mètres utilisée pour exclure les surfaces
#' en eau proches des mares (default: 10)
#'
#' @return Une couche `sf` filtrée des surfaces en eau
#'
#' @importFrom dplyr filter
#' @importFrom sf st_intersects st_buffer
#'
prepare_data_hydro <- function(kobo, hydro, tampon = 10) {
  hydro_erased <- hydro %>%
    filter(st_intersects(geometry, st_buffer(kobo, tampon)) %>% lengths == 0)

  return(hydro_erased)
}


#' Ajouter un identifiant basé sur la géométrie
#'
#' Génère un identifiant pour chaque géométrie d'un objet `sf` afin de repérer
#' les géométries dupliquées (géométries strictement identiques).
#'
#' @param x `sf`, objet spatial
#' @param col_name Character, nom de la colonne à créer contenant l'identifiant
#' (default: "geom_id")
#'
#' @return Un objet `sf` avec une colonne supplémentaire identifiant les groupes
#' de géométries identiques
#'
#' @importFrom sf st_as_text st_geometry
#' @importFrom dplyr dense_rank
#'
add_geom_group_id <- function(x, col_name = "geom_id") {
  stopifnot(inherits(x, "sf"))

  wkt <- sf::st_as_text(sf::st_geometry(x))
  id <- dplyr::dense_rank(wkt)

  x[[col_name]] <- id
  x
}


#' Calcul de la distance minimale entre entités en excluant les doublons géométriques
#'
#' Calcule, pour chaque entité d'un objet `sf`, la distance minimale à une autre
#' entité en ignorant celles partageant le même identifiant de géométrie (`geom_id`).
#'
#' @param sf_object `sf`, objet spatial contenant une colonne `geom_id`
#'
#' @return Un objet `sf` avec une colonne supplémentaire `dist_mare`
#' correspondant à la distance minimale à une autre entité distincte
#'
#' @importFrom sf st_distance
#'
self_minimal_distance <- function(sf_object) {

  dist_matrix <- as.matrix(sf::st_distance(sf_object))

  # supprimer autodistance
  diag(dist_matrix) <- NA

  # matrice des mêmes identifiants
  same_id <- outer(sf_object$geom_id, sf_object$geom_id, "==")

  # ignorer les distances entre mêmes id
  dist_matrix[same_id] <- NA

  nearest_dist <- apply(dist_matrix, 1, min, na.rm = TRUE)

  sf_object$dist_mare <- nearest_dist

  return(sf_object)
}



#' Calcul de la distance à l'entité la plus proche
#'
#' Calcule, pour chaque entité d'un objet `sf`, la distance à l'entité la plus
#' proche dans une seconde couche spatiale.
#'
#' @param a `sf`, couche spatiale de référence (ex: points)
#' @param b `sf`, couche spatiale cible pour le calcul de distance (ex: polygones)
#'
#' @return Un objet `sf` identique à `a` avec une colonne supplémentaire
#' `dist_surfacique` correspondant à la distance à l'entité la plus proche de `b`
#'
#' @importFrom sf st_nearest_feature st_distance
#'
minimal_distance <- function(a, b) {
  nearest_features <- sf::st_nearest_feature(a, b)
  dist <- sf::st_distance(a, b[nearest_features,], by_element = TRUE)
  out <- a
  out$dist_surfacique <- dist
  return(out)
}

#' Calcul du nombre d'entités dans un rayon donné autour de chaque point
#'
#' Pour chaque entité d'une couche de points, compte le nombre d'autres entités
#' (points ou centroïdes de polygones) situées dans un rayon spécifié.
#'
#' @param a `sf`, couche de points de référence
#' @param b `sf`, couche de polygones dont les centroïdes seront considérés
#' @param rayon Numeric, distance en mètres pour définir le voisinage (default: 500)
#'
#' @return Un objet `sf` identique à `a` avec une colonne supplémentaire
#' `n_within_500m` indiquant le nombre d'entités dans le rayon autour de chaque point
#'
#' @importFrom sf st_centroid st_geometry st_is_within_distance st_sf
#'
nb_entites_dans_rayon_centroids <- function(a, b, rayon = 500) {

  # 2. Extraire les centroïdes des polygones
  b_centroids <- st_centroid(b)

  # 3. Combiner points et polygones (centroïdes)
  all_entities <- rbind(
    st_sf(type = "point", geometry = st_geometry(a)),
    st_sf(type = "polygon", geometry = st_geometry(b_centroids))
  )

  # 4. Identifier les entités dans le rayon
  within_mat <- st_is_within_distance(a, all_entities, dist = rayon)

  # 5. Nombre d'entités dans le rayon pour chaque point
  n_entities <- sapply(within_mat, length)

  # 6. Ajouter au dataset
  a$n_within_500m <- n_entities - 1

  return(a)
}

#' Distance à la pièce d’eau la plus proche (C11) : Calcul de la distance minimale aux mares et plans d'eau
#'
#' Pour chaque mare, calcule la distance minimale soit à une autre mare,
#' soit à une pièce d'eau de référence, puis attribue la note IECMAr (C11)
#' en fonction de cette distance.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param surface_eau `sf`, couche polygonale représentant les pièces d'eau
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{X_index}{Identifiant de la mare}
#'   \item{c11}{Code C11 attribué selon la distance minimale aux autres mares ou
#'   aux plans d'eau (31, 32 ou 33)}
#' }
#'
#' @importFrom dplyr mutate case_when select left_join
#' @importFrom sf st_drop_geometry
#'
iecmar_c11 <- function(kobo, surface_eau) {

  # Entres mares
  v4_geom_id <- add_geom_group_id(kobo, "geom_id")
  res_mares <- self_minimal_distance(v4_geom_id)

  # Avec les plans d'eau
  hydro_surfacique <- prepare_data_hydro(kobo, surface_eau)


  res_surfacique <- minimal_distance(kobo, hydro_surfacique) %>%
    st_drop_geometry() %>%
    select(X_index, dist_surfacique)

  res <- left_join(res_mares, res_surfacique, by = "X_index") %>%
    mutate(dist_mare = as.numeric(dist_mare),
           dist_surfacique = as.numeric(dist_surfacique)) %>%
    mutate(distance_retenu = ifelse(dist_mare < dist_surfacique, dist_mare, dist_surfacique)) %>%
    mutate(CAN_name = "Distance_eau",
           CAN_choice = as.character(round(distance_retenu, 0)),
           cor_iecmar = as.character(
             case_when(
               distance_retenu <= 250 ~ 31,
               distance_retenu <= 500 ~ 32,
               distance_retenu > 500 ~ 33
             )
           ),
    ) %>%
    st_drop_geometry() %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  return(res)
}

#' Nb de pièces d’eau à moins de 500 m (C12) : Calcul du nombre de mares + pièces d'eau à proximité
#'
#' Pour chaque mare, calcule le nombre de pièces d'eau situées dans un rayon
#' de 500 mètres (en utilisant les centroïdes) puis attribue la note IECMAr (C12)
#' en fonction de ce nombre.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param surface_eau `sf`, couche polygonale représentant les pièces d'eau
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{X_index}{Identifiant de la mare}
#'   \item{c12}{Code C12 attribué selon le nombre de pièces d'eau à proximité
#'   (34, 35 ou 36)}
#' }
#'
#' @importFrom dplyr mutate case_when select
#' @importFrom sf st_make_valid st_drop_geometry
#'
iecmar_c12 <- function(kobo, surface_eau) {

  hydro_surfacique <- prepare_data_hydro(kobo, surface_eau)

  buffed <- nb_entites_dans_rayon_centroids(kobo, st_make_valid(hydro_surfacique)) %>%
    mutate(CAN_name = "nb_piece_eau",
           CAN_choice = as.character(n_within_500m),
           cor_iecmar = as.character(
      case_when(n_within_500m >= 2 ~ 34,
                n_within_500m == 1 ~ 35,
                n_within_500m == 0 ~ 36
      )
    )) %>%
    st_drop_geometry() %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  return(buffed)
}

#' Distance avec un site terrestre hivernal (C13) : Calcul de la distance aux forêts
#'
#' Pour chaque mare, calcule la distance à un site terrestre hivernal et
#' attribue la note IECMAr (C13) selon la distance (en mètres).
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param site_hiver `sf`, couche de points ou polygones représentant les sites
#' terrestres hivernaux
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{X_index}{Identifiant de la mare}
#'   \item{c13}{Code C13 attribué selon la distance au site hivernal (37, 38 ou 39)}
#' }
#'
#' @importFrom dplyr mutate case_when select
#' @importFrom sf st_drop_geometry
#'
iecmar_c13 <- function(kobo, site_hiver) {
  res <- minimal_distance(kobo, site_hiver) %>%
    mutate(dist_surfacique = as.numeric(dist_surfacique)) %>%
    mutate(CAN_name = "site_hiver",
           CAN_choice = as.character(round(dist_surfacique, 0)),
           cor_iecmar = as.character(
      case_when(dist_surfacique <  250 ~ 37,
                dist_surfacique <  500 ~ 38,
                dist_surfacique >= 500 ~ 39
      )
    )) %>%
    st_drop_geometry() %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  return(res)

}

#' Zone d’écrasement d’Amphibiens (C15) : Calcul de la distance aux routes
#'
#' Pour chaque mare, évalue la proximité des routes et attribue la note IECMAr (C15)
#' selon la classe et la nature de la route.
#'
#' @param kobo `sf`, couche de points représentant les mares
#' @param routes `sf`, couche linéaire représentant les routes avec colonnes
#' `cpx_classe` et `nature`
#' @param buffer Numeric, distance en mètres pour définir le voisinage autour des mares (default: 250)
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{X_index}{Identifiant de la mare}
#'   \item{c15}{Code C15 attribué selon la proximité et le type de route (43, 44 ou 45)}
#' }
#'
#' @importFrom dplyr select mutate group_by arrange summarise left_join
#' @importFrom sf st_buffer st_intersection st_drop_geometry
#'
iecmar_c15 <- function(kobo, routes, buffer = 250) {

  routes_mares <- kobo %>%
    # Step 1
    select(X_index) %>%
    st_buffer(buffer) %>%
    # Step 2
    st_intersection(routes) %>%
    st_drop_geometry() %>%
    # Step 3
    mutate(c15 = case_when(
      !is.na(cpx_classe)                                   ~ 45, # 0 pts (Départementale/Nationale)
      is.na(cpx_classe) & nature == "Route a 1 chaussee"   ~ 44  # 2 pts (Route bitumée communale)
    )) %>%
    # Step 4
    group_by(X_index) %>%
    arrange(c15) %>%
    summarise(c15 = max(c15))

    # Step 5
  kobo_routes_mares <- kobo %>%
    left_join(routes_mares, by = "X_index") %>%
    mutate(c15 = ifelse(is.na(c15), 43, c15))  # 5 pts si absence de jointure

  res <-  kobo_routes_mares %>%
    st_drop_geometry() %>%
    mutate(CAN_name = "zone_ecrasement",
           CAN_choice = case_when(
             c15 == "43" ~ "Pas de route ou non bitumee",
             c15 == "44" ~ "Route bitumee communale",
             c15 == "45" ~ "Route departementale, nationale ou autoroute"
           ),
           cor_iecmar = as.character(c15)) %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  return(res)
}

#' Traitements géométriques IECMAR
#'
#' Applique une série de traitements géométriques sur des données de mares afin de
#' calculer plusieurs indicateurs IECMAR basés sur les distances et les voisinages spatiaux.
#'
#' @param sf_kobo `sf`, données géographiques des mares issues de KoboToolbox
#' @param eau `sf`, données des surfaces en eau (par défaut `marecoPckg::eau_25`)
#' @param forets `sf`, données des zones forestières (par défaut `marecoPckg::forets`)
#' @param routes `sf`, données du réseau routier (par défaut `marecoPckg::routes_25`)
#'
#' @return Un `data.frame` contenant les résultats des indicateurs IECMAR
#'
#' @importFrom dplyr bind_rows
#'
process_traitement_geom <- function(sf_kobo, eau, forets, routes) {

  message("# Calcul de la distance a la piece d'eau la plus proche")
  c11 <- iecmar_c11(sf_kobo, eau)
  message("# Compte le nombre de piece d'eau dans les 500 metres")
  c12 <- iecmar_c12(sf_kobo, eau)
  message("# Distance du site terrestre hivernal le plus proche")
  c13 <- iecmar_c13(sf_kobo, forets)
  message("# Detection d'une zone d'ecrasement d'amphibiens")
  c15 <- iecmar_c15(sf_kobo, routes)

  res <- bind_rows(c11, c12, c13, c15)

  return(res)
}
