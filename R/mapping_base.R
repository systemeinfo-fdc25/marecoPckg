#' Transformation des données Kobo du format large au format long
#'
#' Convertit un jeu de données Kobo du format large au format long,
#' en conservant un identifiant par ligne et en standardisant les types.
#'
#' @param kobo `data.frame`, données Kobo au format large
#'
#' @return Un `data.frame` au format long avec les colonnes :
#' \describe{
#'   \item{X_index}{Identifiant de l'observation}
#'   \item{colname}{Nom de la variable}
#'   \item{value}{Valeur associée}
#'   \item{id_temp}{Identifiant technique de ligne}
#' }
#'
#' @importFrom dplyr mutate across everything
#' @importFrom tidyr pivot_longer
#' @importFrom sf st_drop_geometry
#'
kobo_wide_to_long <- function(kobo) {

  long <- st_drop_geometry(kobo) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(-X_index, names_to = "colname", values_to = "value") %>%
    mutate(id_temp = row.names(.))
  return(long)
}

#' Jointure des questions à choix multiples – réponses multiples
#'
#' Filtre les variables correspondant à des questions à choix multiples avec
#' réponses multiples, transforme les modalités sélectionnées et réalise la
#' jointure avec les tables de correspondance et canonique.
#'
#' @param kobo `data.frame`, données Kobo au format long
#' @param cor `data.frame`, table de correspondance entre variables Kobo et choix
#' @param canonique `data.frame`, table de correspondance canonique
#'
#' @return Un `data.frame` enrichi avec les informations de correspondance
#'
#' @importFrom dplyr filter mutate left_join
#'
join_q_multi_r_multi <- function(kobo, cor, canonique) {
  res <- kobo %>%
    # Filtrer toutes les colonnes qui proposent des choix (question1.reponse1)
    filter(grepl("\\.", colname) & colname != "meta.rootUuid") %>%
    mutate(value = ifelse(value == 1, sub(".*\\.", "", .$colname), as.integer(0)),
           colname = sub("\\..*", "", colname)) %>%
    filter(value != 0) %>%
    mutate(value = as.character(value)) %>%
    left_join(cor, by = c("colname" = "name_survey",  "value" = "name_choice")) %>%
    left_join(canonique, by = c("id_can" = "CAN_id"))

  return(res)
}

#' Préparation des questions à choix multiples – réponses uniques
#'
#' Sélectionne les variables correspondant à des questions à choix multiples
#' avec réponse unique et applique les jointures de correspondance.
#'
#' @param kobo `data.frame`, données Kobo au format long
#' @param cor `data.frame`, table de correspondance
#' @param canonique `data.frame`, table canonique
#'
#' @return Un `data.frame` avec les jointures appliquées
#'
#' @importFrom dplyr filter left_join
#'
prepare_q_multi_r_uni <- function(kobo, cor, canonique) {
  res <- kobo %>%
    filter(!(grepl("\\.", colname) & colname != "meta.rootUuid")) %>%
    left_join(cor, by = c("colname" = "name_survey", "value" = "name_choice")) %>%
    left_join(canonique, by = c("id_can" = "CAN_id"))

  res
}

#' Traitement des questions sans choix
#'
#' Identifie les questions sans choix (sans correspondance canonique),
#' sépare les cas avec et sans valeur, puis recombine les résultats.
#'
#' @param q_multi_r_uni_before_filter `data.frame`, données préparées en amont
#' @param cor `data.frame`, table de correspondance
#' @param canonique `data.frame`, table canonique
#'
#' @return Un `data.frame` des questions sans choix
#'
#' @importFrom dplyr filter select left_join arrange bind_rows
#'
compute_q_uni <- function(q_multi_r_uni_before_filter, cor, canonique) {

  r_avec_value <- q_multi_r_uni_before_filter %>%
    filter(is.na(id_can), !is.na(value)) %>%
    select(X_index, colname, value, id_temp) %>%
    left_join(cor, by = c("colname" = "name_survey")) %>%
    left_join(canonique, by = c("id_can" = "CAN_id"))

  r_sans_value <- q_multi_r_uni_before_filter %>%
    filter(is.na(id_can), is.na(value))

  q_uni <- bind_rows(r_avec_value, r_sans_value) %>%
    arrange(as.integer(id_temp))

  q_uni
}


#' Finalisation des questions à choix multiples – réponses uniques
#'
#' Exclut les observations correspondant aux questions sans choix afin de ne
#' conserver que les questions à choix multiples avec réponse unique.
#'
#' @param q_multi_r_uni_before_filter `data.frame`, données préparées
#' @param q_uni `data.frame`, questions sans choix
#'
#' @return Un `data.frame` filtré
#'
#' @importFrom dplyr filter
#'
finalize_q_multi_r_uni <- function(q_multi_r_uni_before_filter, q_uni) {
  res <- q_multi_r_uni_before_filter %>%
    filter(!id_temp %in% unique(q_uni$id_temp))

  res
}

#' Assemblage des données canonisées
#'
#' Combine les différentes catégories de questions (choix multiples, uniques,
#' sans choix) en un seul jeu de données harmonisé.
#'
#' @param q_multi_r_multi `data.frame`, questions à choix multiples réponses multiples
#' @param q_multi_r_uni `data.frame`, questions à choix multiples réponses uniques
#' @param q_uni `data.frame`, questions sans choix
#'
#' @return Un `data.frame` final canonisé
#'
#' @importFrom dplyr select arrange bind_rows
#'
assemble_q_canonised <- function(q_multi_r_multi, q_multi_r_uni, q_uni) {
  res <- bind_rows(q_multi_r_multi, q_multi_r_uni, q_uni) %>%
    select(-name_choice) %>%
    arrange(as.integer(id_temp))

  res
}

#' Vérification des lignes orphelines après traitement
#'
#' Identifie les observations du jeu de données initial qui n'ont pas été
#' retrouvées dans les jeux de données extraits. Permet de détecter d'éventuelles
#' pertes d'information lors des transformations.
#'
#' @param total `data.frame`, jeu de données initial au format long
#' @param extract1 `data.frame`, premier jeu extrait
#' @param extract2 `data.frame`, deuxième jeu extrait
#' @param extract3 `data.frame`, troisième jeu extrait
#'
#' @return Un `data.frame` contenant les lignes orphelines (peut être vide)
#'
#' @importFrom dplyr filter bind_rows
#'
try_find_orphelin <- function(total, extract1, extract2, extract3) {
  all_joined <- bind_rows(extract1, extract2, extract3)
  v4_orphelin <- total %>%
    filter(id_temp %not_in% unique(all_joined$id_temp)) %>%
    filter(value != 0)

  if (nrow(v4_orphelin) == 0) {
    message("Il n'y a pas d'orphelins, c'est tout bon")
  } else {
    warning("Des orphelins ont été trouvé")
  }
  return(v4_orphelin)
}

#' Ajouter un défaut de mesures de protection (C20) pour les formulaires v4
#'
#' Pour chaque mare, ajoute une ligne indiquant l’absence de mesures de protection
#' si aucune information n’est déjà présente. Le code IECMAR C20 est fixé à 58.
#'
#' @param df `data.frame`, contenant au moins la colonne `X_index`
#'
#' @return Un `data.frame` enrichi avec une ligne par mare (si nécessaire) :
#' \describe{
#'   \item{CAN_name}{Nom de la variable : "mesures_protection"}
#'   \item{CAN_choice}{Valeur : "aucune"}
#'   \item{cor_iecmar}{Code IECMAR C20 (58)}
#' }
#'
#' @importFrom dplyr select distinct mutate bind_rows
#'
defaut_proprietaire_favorable_v4 <- function(df) {
  c20 <- df %>%
    select(X_index) %>%
    distinct(X_index) %>%
    mutate(CAN_name = "mesures_protection",
           CAN_choice = "aucune",
           cor_iecmar = as.character(58)
    )

  res <- bind_rows(df, c20)

  return(res)
}

#' Pipeline de canonisation des données Kobo
#'
#' Applique l'ensemble des étapes de transformation et de jointure pour produire
#' un jeu de données Kobo canonisé.
#'
#' @param kobo `data.frame`, données Kobo au format long
#' @param cor `data.frame`, table de correspondance
#' @param cor_canonique `data.frame`, table canonique
#'
#' @return Un `data.frame` final prêt à être exploité
#'
process_kobo_canonised <- function(kobo, cor, cor_canonique) {

  q_multi_r_multi <- join_q_multi_r_multi(kobo, cor, cor_canonique)

  q_multi_r_uni_before <- prepare_q_multi_r_uni(kobo, cor, cor_canonique)

  q_uni <- compute_q_uni(q_multi_r_uni_before, cor, cor_canonique)

  q_multi_r_uni <- finalize_q_multi_r_uni(q_multi_r_uni_before, q_uni)

  # option debug
  # orpehlin <- try_find_orphelin(kobo, q_multi_r_multi, q_multi_r_uni, q_uni)

  res <- assemble_q_canonised(q_multi_r_multi, q_multi_r_uni, q_uni)

  return(res)
}

#' Calculs particuliers IECMAR (superficie, végétation, déchets, habitats)
#'
#' Enchaîne une série de traitements spécifiques sur les données canonisées
#' d’observations de mares pour calculer différents indicateurs IECMAR.
#'
#' @param canonique `data.frame`, jeu de données canonisé contenant les colonnes
#' nécessaires aux fonctions internes (X_index, CAN_name, CAN_choice, etc.)
#' @param version `integer`, version du calcul (par ex. 4 pour inclure déchets,
#' habitats et mesures de protection)
#'
#' @return Un `data.frame` enrichi avec les résultats des calculs :
#' \describe{
#'   \item{Superficie}{via \code{\link{calculs_superficie}}}
#'   \item{Helophytes}{via \code{\link{calculs_helophytes}}}
#'   \item{Hydrophytes}{via \code{\link{calculs_hydrophytes}}}
#'   \item{Déchets et pollution}{si \code{version = 4}, via \code{\link{calculs_dechets}}}
#'   \item{Habitats}{si \code{version = 4}, via \code{\link{calculs_habitats_v4}}}
#'   \item{Mesures de protection}{si \code{version = 4}, via \code{\link{defaut_proprietaire_favorable_v4}}}
#' }
#'
#' @details
#' La fonction applique successivement les fonctions internes pour produire un
#' jeu de données final prêt pour l’analyse IECMAR.
#'
#' @importFrom dplyr mutate
#'
calculs_particuliers <- function(canonique, version) {
  res_s     <- calculs_superficie(canonique)
  res_h     <- calculs_helophytes(res_s)
  res_h2    <- calculs_hydrophytes(res_h)
  res_tinw  <- turbidite_if_no_water(res_h2)
  if (version == 4) {
    res_d   <- calculs_dechets(res_tinw)
    res_t   <- calculs_habitats_v4(res_d)
    res_p   <- defaut_proprietaire_favorable_v4(res_t)
    res_c   <- default_corridor_v4(res_p)
  } else {
    res_c   <- res_tinw
  }

  res <- res_c %>%
    mutate(X_index = as.integer(X_index))

  return(res)
}

