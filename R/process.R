#' Traitement complet des données IECMAR
#'
#' Exécute l'ensemble de la chaîne de traitement IECMAR :
#' préparation des données, calcul des réseaux, traitements
#' géométriques, correspondances Kobo vers IECMAR et
#' construction des sorties finales.
#'
#' @param df Objet sf contenant les données de mares.
#' @param version Version du formulaire Kobo utilisée (4L ou 5L).
#' @param departement Code du département utilisé pour sélectionner
#' les couches géographiques nécessaires aux traitements.
#' @param use_OS_for_reseaux Booléen indiquant si l'occupation du sol
#' doit être utilisée pour fragmenter les réseaux.
#' @param use_RD_for_reseaux Booléen indiquant si les routes
#' départementales doivent être utilisées pour fragmenter les réseaux.
#' @param buffer_size_for_reseaux Distance tampon (en mètres) utilisée
#' pour calculer les connexions entre mares.
#' @param use_history_if_exist Booléen indiquant si les données
#' historiques doivent être recherchées et utilisées lorsqu'elles sont disponibles.
#' @param jdd_v4 Liste des jeux de données issus du formulaire V4.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item resultat : objet sf enrichi des résultats IECMAR ;
#'   \item resultat_photo : objet sf enrichi des URLs de photographies ;
#'   \item notes_details : détail des notes IECMAR ;
#'   \item reseaux : couche des réseaux calculés.
#' }
#'
#' @export
process_all <- function(
  df,
  version = 5L,
  departement,
  use_OS_for_reseaux = TRUE,
  use_RD_for_reseaux = FALSE,
  buffer_size_for_reseaux = 1000L,
  use_history_if_exist = FALSE,
  jdd_v4 = NULL
) {

  # Juste un check et stop() si pas ok
  validate_inputs(
    df,
    version,
    departement,
    buffer_size_for_reseaux,
    use_OS_for_reseaux,
    use_RD_for_reseaux,
    use_history_if_exist
  )

  # Garde seulement les mares existes et ajoute de l'historique
  prep <- prepare_input_data(
    df,
    version,
    departement,
    use_history_if_exist,
    jdd_v4
  )

  # Cree les reseaux et ajoute l'ID reseaux aux mares (prep$df_all_v)
  reseaux_res <- process_reseaux(
    prep$df_all_v,
    departement,
    use_OS_for_reseaux,
    use_RD_for_reseaux,
    buffer_size_for_reseaux
  )

  # Calcul les indicateurs geometrique iecmar sur coordonnees des mares (c11, c12, c13, c15)
  res_geom <- process_iecmar_geom(
    prep$df_all_v,
    departement
  )

  # Wide -> long
  # Kobo -> canno -> iecmar
  # Calculs particulies (v4, v5 ou v4 + v5)
  res_forms <- process_iecmar_forms(
    prep$df_all_v,
    version,
    prep$multiple_form_v
  )

  # Rend les resultats iecmar
  compil <- compile_iecmar_inputs(
    res_forms,
    res_geom
  )

  # Cree les formats de sorties
  build_outputs(
    compil = compil,
    res_reseaux = reseaux_res$res_reseaux,
    cor_uuid_form_v = prep$cor_uuid_form_v,
    df_existe = prep$df_all_v,
    reseaux = reseaux_res$reseaux
  )
}

#' Validation des paramètres d'entrée
#'
#' Vérifie la cohérence des paramètres transmis à
#' \code{process_all()} avant exécution des traitements.
#'
#' @param df Objet sf contenant les données de mares.
#' @param version Version du formulaire Kobo utilisée.
#' @param departement Code du département.
#' @param buffer_size_for_reseaux Distance tampon utilisée pour le calcul des réseaux.
#' @param use_OS_for_reseaux Booléen d'utilisation de l'occupation du sol.
#' @param use_RD_for_reseaux Booléen d'utilisation des routes départementales.
#' @param use_history_if_exist Booléen d'utilisation de l'historique.
#' @param jdd_v4 Liste des jeux de données V4.
#'
#' @return Invisiblement TRUE si les vérifications sont validées.
#'
#' @keywords internal
validate_inputs <- function(
    df,
    version,
    departement,
    buffer_size_for_reseaux,
    use_OS_for_reseaux,
    use_RD_for_reseaux,
    use_history_if_exist,
    jdd_v4 = NULL
) {

  if (!inherits(df, "sf")) {
    stop("`df` doit être un objet sf")
  }

  if (!is.logical(use_OS_for_reseaux)) {
    stop("`use_OS_for_reseaux` doit etre logique (TRUE or FALSE)")
  }

  if (!is.logical(use_RD_for_reseaux)) {
    stop("`use_RD_for_reseaux` doit etre logique (TRUE or FALSE)")
  }

  if (!is.logical(use_history_if_exist)) {
    stop("`use_history_if_exist` doit etre logique (TRUE or FALSE)")
  }

  if (!version %in% c(4L, 5L)) {
    stop("`version` doit être 4L ou 5L")
  }

  if (is.null(departement)) {
    stop("`departement` est obligatoire (format nombre entier)")
  }

  if (!is.numeric(buffer_size_for_reseaux) ||
      length(buffer_size_for_reseaux) != 1 ||
      buffer_size_for_reseaux <= 0) {
    stop("`buffer_size_for_reseaux` doit etre un numerique strictement positif")
  }

  required_cols <- c("X_uuid", "mare_existe")

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Colonnes manquantes : %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' Préparation des données d'entrée
#'
#' Filtre les mares existantes et ajoute les données
#' historiques lorsque celles-ci sont disponibles.
#'
#' @param df Objet sf contenant les données de mares.
#' @param version Version du formulaire Kobo utilisée.
#' @param departement Code du département.
#' @param use_history_if_exist Booléen d'utilisation de l'historique.
#' @param jdd_v4 Liste des jeux de données V4.
#'
#' @return Une liste contenant les jeux de données préparés et les
#' informations de versionnement.
#'
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom sf st_drop_geometry
#'
#' @keywords internal
prepare_input_data <- function(
    df,
    version,
    departement,
    use_history_if_exist,
    jdd_v4
) {

  message("### Préparation des données")

  df_existe <- df %>%
    filter(is.na(mare_existe) | mare_existe %in% c('oui', 'peut_etre', 'existe')) %>%
    mutate(form_v = version)

  # Logical : dit si il faut utilise l'historique et s'il existe
  multiple_form_v <- ifelse(use_history_if_exist | is.null(jdd_v4), departement %in% names(jdd_v4), FALSE)


  # Ajoute le v4 du departement au JDD si il est connu
  if (multiple_form_v) {
    message("### Ajout des donnees historiques")
    df_all_v <- bind_rows(df_existe, jdd_v4[[as.character(departement)]])

    cor_uuid_form_v <- df_all_v %>%
      st_drop_geometry() %>%
      select(X_uuid, form_v) %>%
      unique()

  } else {
    df_all_v <- df_existe

    cor_uuid_form_v <- df_all_v %>%
      st_drop_geometry() %>%
      select(X_uuid) %>%
      unique() %>%
      mutate(form_v = version)
  }

  list(
    df_all_v = df_all_v,
    df_existe = df_existe,
    cor_uuid_form_v = cor_uuid_form_v,
    multiple_form_v = multiple_form_v
  )
}

#' Calcul des réseaux de mares
#'
#' Construit les réseaux de mares et attribue un identifiant
#' de réseau à chaque mare.
#'
#' @param df Objet sf contenant les données de mares.
#' @param departement Code du département.
#' @param use_OS_for_reseaux Booléen d'utilisation de l'occupation du sol.
#' @param use_RD_for_reseaux Booléen d'utilisation des routes départementales.
#' @param buffer_size_for_reseaux Distance tampon utilisée pour les connexions.
#'
#' @return Une liste contenant les réseaux calculés et les mares
#' enrichies de leur identifiant réseau.
#'
#' @keywords internal
process_reseaux <- function(
    df,
    departement,
    use_OS_for_reseaux,
    use_RD_for_reseaux,
    buffer_size_for_reseaux
) {

  message("### Création des réseaux")

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

  list(
    reseaux = reseaux,
    res_reseaux = res_reseaux
  )
}

#' Traitements géométriques IECMAR
#'
#' Réalise les calculs géométriques nécessaires à la
#' production des indicateurs IECMAR.
#'
#' @param df Objet sf contenant les données de mares.
#' @param departement Code du département.
#'
#' @return Un tableau long contenant les résultats des traitements géométriques.
#'
#' @keywords internal
process_iecmar_geom <- function(df, departement) {

  message("### Traitements géométriques")

  res_geom <- process_traitement_geom(
    df,
    eau = marecoPckg::eau[[as.character(departement)]],
    forets = marecoPckg::forets,
    routes = marecoPckg::routes[[as.character(departement)]]
  )

}

#' Traitement des formulaires Kobo
#'
#' Convertit les réponses Kobo vers le format canonique
#' IECMAR et applique les calculs spécifiques aux versions
#' de formulaire.
#'
#' @param df Objet sf contenant les données de mares.
#' @param version Version du formulaire Kobo utilisée.
#' @param multiple_form_v Booléen indiquant la présence de plusieurs versions.
#'
#' @return Un tableau long contenant les variables IECMAR issues des formulaires.
#'
#' @importFrom dplyr bind_rows filter group_by ungroup
#'
#' @keywords internal
process_iecmar_forms <- function(
    df,
    version,
    multiple_form_v
) {

  message("### Correspondances Kobo → IECMAR")

  kobo_l <- kobo_wide_to_long(df)

  if (!multiple_form_v) {

    corresp <- switch(
      as.character(version),
      "4" = corresp_v4,
      "5" = corresp_v5
    )

    canonique <- process_kobo_canonised(
      kobo_l,
      corresp,
      cor_canonique
    )

    return(
      calculs_particuliers(
        canonique,
        version
      )
    )
  } else {
    kobo_l_v4 <- kobo_l %>% group_by(X_uuid) %>% filter(any(colname == "form_v" & value == 4L)) %>% ungroup()
    kobo_l_v5 <- kobo_l %>% group_by(X_uuid) %>% filter(any(colname == "form_v" & value == 5L)) %>% ungroup()

    canonique_v4 <- process_kobo_canonised(kobo_l_v4, corresp_v4, cor_canonique)
    canonique_v5 <- process_kobo_canonised(kobo_l_v5, corresp_v5, cor_canonique)
    res_forms_v4 <- calculs_particuliers(canonique_v4, 4L)
    res_forms_v5 <- calculs_particuliers(canonique_v5, 5L)
    return(
      bind_rows(res_forms_v4, res_forms_v5)
    )
  }
}

#' Compilation des données IECMAR
#'
#' Fusionne les résultats issus des traitements géométriques
#' et des formulaires Kobo dans un format commun.
#'
#' @param res_forms Résultats issus des formulaires Kobo.
#' @param res_geom Résultats issus des traitements géométriques.
#'
#' @return Un tableau long prêt pour le calcul des notes IECMAR.
#'
#' @importFrom dplyr bind_rows coalesce mutate select
#'
#' @keywords internal
compile_iecmar_inputs <- function(
    res_forms,
    res_geom
) {

  message("### Compilation IECMAR")

  bind_rows(res_forms, res_geom) %>%
    mutate(
      CAN_name = coalesce(CAN_name, colname),
      CAN_choice = coalesce(CAN_choice, value)
    ) %>%
    select(
      X_uuid,
      CAN_name,
      CAN_choice,
      cor_iecmar
    )
}

#' Construction des sorties IECMAR
#'
#' Calcule les notes IECMAR et construit les objets de sortie
#' finaux du traitement.
#'
#' @param compil Tableau compilé des variables IECMAR.
#' @param res_reseaux Objet sf contenant les mares et leurs réseaux.
#' @param cor_uuid_form_v Correspondance entre identifiants et versions de formulaire.
#' @param df_existe Jeu de données filtré sur les mares existantes.
#' @param reseaux Réseaux calculés.
#'
#' @return Une liste contenant les différents objets de sortie IECMAR.
#'
#' @importFrom dplyr left_join mutate select
#' @importFrom sf st_drop_geometry
#'
#' @keywords internal
build_outputs <- function(
    compil,
    res_reseaux,
    cor_uuid_form_v,
    df_existe,
    reseaux
) {

  message("### Calcul IECMAR")

  notes_detail <- calcul_iecmar(compil)
  note_simple_long <- output_note_only_l(notes_detail)

  res_sf <- left_join(res_reseaux, note_simple_long, by = "X_uuid") %>%
    calcul_mediane_iecmar_reseaux() %>%
    left_join(cor_uuid_form_v, by = "X_uuid") %>%
    mutate(form_v = as.integer(form_v))

  res_sf_enhanced <-  left_join(res_sf, df_existe %>%
                                          select(X_uuid, photographie_URL) %>%
                                          st_drop_geometry(),
                         by = "X_uuid")

  #### Build big output
    new_origine_for_wide_output <- notes_detail %>%
    select(X_uuid, CAN_name, CAN_choice) %>%
    tidyr::pivot_wider(names_from = "CAN_name",
                       values_from = "CAN_choice")

  iecmar_for_wide_output <- notes_detail %>%
    select(X_uuid, critere_label, points) %>%
    filter(!is.na(critere_label)) %>%
    tidyr::pivot_wider(names_from = "critere_label",
                       values_from = "points")

  summary_for_wide_output <- res_sf %>%
    select(X_uuid, id_reseau, nb_mares_reseau, reseau_valide, note, median_iecmar_reseau, position_mediane)


  table_out <- summary_for_wide_output %>%
    left_join(iecmar_for_wide_output, by = "X_uuid") %>%
    left_join(new_origine_for_wide_output, by = "X_uuid") %>%
    select(# Donnees generales
           X_uuid, id_cen, id_reseau, nb_mares_reseau, reseau_valide, X_submission_time, form_v,
           # IECMAr
           note, median_iecmar_reseau, position_mediane,
           # Notes des criteres IECMAr
           8:24,
           # Variables renseignees Kobo
           type_mare, mare_superficie, profondeur_max, turbidite, fond_mare, berges_pentes_douce, rec_helophytes,
           rec_hydrophytes, corridor_lineaire_5m, presence_poissons, dechets, quantite_dechets, mesures_protection,
           # Variables calculees automatique pour IECMAr
           Distance_eau, nb_piece_eau, site_hiver, zone_ecrasement
    ) %>%
    # Repare la sortie car sinon il y a des list dans les colonnes
    mutate(
      across(
        where(is.list),
        ~ if (all(lengths(.x) == 1)) {
            type.convert(unlist(.x), as.is = TRUE)
          } else {
            purrr::map_chr(.x, ~ paste(.x, collapse = ";"))
          }
      )
    )

  list(
    resultat = res_sf,
    resultat_photo = res_sf_enhanced,
    notes_details = notes_detail,
    reseaux = reseaux,
    big_table = table_out
  )
}
