
#' Calcul de la superficie des mares et codage associé
#'
#' Calcule la superficie des mares à partir de deux dimensions (id_can 200 et 201),
#' puis attribue une classe de superficie selon des seuils définis.
#'
#' @param cano `data.frame`, données canonisées
#'
#' @return Un `data.frame` enrichi avec une ligne calculée de superficie et un code associé
#'
#' @importFrom dplyr filter group_by summarise ungroup mutate bind_rows arrange
#'
calculs_superficie <- function(cano) {
  q_surface <- cano %>%
    filter(id_can %in% c(200, 201)) %>%
    group_by(X_index) %>%
    summarise(id_temp = first(id_temp),
              colname = "mare_superficie",
              # value_simple = prod(as.integer(value)),
              value = pi/4 * (as.integer(value[1]) * as.integer(value[2]))) %>%
    ungroup() %>%
    mutate(cor_iecmar = as.character(
      case_when(value > 50                       ~ 4, # Sgrande (S > 50)
                value <= 50 & value >= 10        ~ 5, # Smoyen (50 ≥ S > 10)
                value < 10                       ~ 6  # Spetite (S < 10)
      )
    )) %>%
    mutate(value = as.character(value))

  res <- cano %>%
    filter(id_can %not_in% c(200, 201)) %>%
    bind_rows(q_surface) %>%
    arrange(as.integer(id_temp))

  return(res)
}

#' Calcul du recouvrement en hélophytes et codage associé
#'
#' Transforme les valeurs de recouvrement des hélophytes (id_can 210)
#' en classes selon des seuils définis.
#'
#' @param cano `data.frame`, données canonisées
#'
#' @return Un `data.frame` avec les valeurs transformées et codées
#'
#' @importFrom dplyr filter mutate bind_rows arrange
#'
calculs_helophytes <- function(cano) {

  q_helophyte <- cano %>%
    filter(id_can == 210) %>%
    mutate(value = as.integer(value),
           cor_iecmar = as.character(
      case_when(value > 50               ~ "19", # Helophytes abondant si > 50%
                value <= 50 & value > 0  ~ "20", # clairsemé si < 50%
                value == 0               ~ "21") # absent si 0
    )) %>%
    mutate(value = as.character(value))

  res <- cano %>%
    filter((id_can != 210) | is.na(id_can)) %>%
    bind_rows(q_helophyte) %>%
    arrange(as.integer(id_temp))

  return(res)
}

#' Calcul du recouvrement en hydrophytes et codage associé
#'
#' Agrège les valeurs de recouvrement des hydrophytes (id_can 208 et 209),
#' puis attribue une classe selon des seuils définis.
#'
#' @param cano `data.frame`, données canonisées
#'
#' @return Un `data.frame` avec les valeurs agrégées et codées
#'
#' @importFrom dplyr filter group_by summarise ungroup mutate bind_rows arrange
#'
calculs_hydrophytes <- function(cano) {

  q_hydrophye <- cano %>%
    filter(id_can %in% c(208, 209)) %>%
    group_by(X_index) %>%
    summarise(id_temp = first(id_temp),
              colname = "rec_hydrophytes",
              value = sum(as.integer(value))) %>%
    ungroup() %>%
    mutate(cor_iecmar = as.character(
      case_when(value > 50               ~ "22", # Hydro abondant si > 50
                value <= 50 & value > 0  ~ "23", # clairsemé si < 50
                value == 0               ~ "24") # absent si 0
    )) %>% mutate(value = as.character(value))

  res <- cano %>%
    filter(id_can %not_in% c(208, 209)) %>%
    bind_rows(q_hydrophye) %>%
    arrange(as.integer(id_temp))

  return(res)
}


#' Notation des déchets et pollution (codes C18 et C19) - V4 uniquement
#'
#' Pour chaque mare, évalue la présence de déchets et leur quantité, puis évalue la pollution.
#'
#' @param cano `data.frame`, données mares du kobotoolbox post-canonisation
#'
#' @return Le `data.frame` original avec les lignes correspondants aux dechets et quantites
#' dechêts modifiées pour correspondre à la notation iecmar (dechêts & pollution)
#'
#' @importFrom dplyr filter select mutate bind_rows case_when
#' @importFrom tidyr pivot_wider
#'
calculs_dechets <- function(cano) {
  evalued <- cano %>%
    filter(CAN_name %in% c("dechets", "quantite_dechets")) %>%
    select(X_index, CAN_name, CAN_choice) %>%
    tidyr::pivot_wider(names_from = CAN_name,
                  values_from = CAN_choice
    ) %>%
    # Eval dechets
    mutate(c18 = case_when(
      dechets == "aucun" ~ 51L,
      dechets != "aucun" & quantite_dechets == "faible" ~ 52L,
      dechets != "aucun" & quantite_dechets != "faible" ~ 53L
    )) %>%
    # Eval pollution
    mutate(c19 = case_when(
      grepl("dangereux", dechets) ~ 55L,
      TRUE ~ 54L
    ))

  # Add dechets resultats (C18)
  res <- cano %>%
    filter(CAN_name %not_in% c("dechets", "quantite_dechets")) %>%
    bind_rows(
      evalued %>%
        mutate(
          CAN_name = "dechets",
          CAN_choice = as.character(quantite_dechets),
          cor_iecmar = as.character(c18)) %>%
        select(X_index, CAN_name, CAN_choice, cor_iecmar)
    ) %>%
    # Add pollution resultats (C19)
    bind_rows(
      evalued %>%
        mutate(
          CAN_name = "pollution",
          CAN_choice = as.character(dechets),
          cor_iecmar = as.character(c19)) %>%
        select(X_index, CAN_name, CAN_choice, cor_iecmar)
    )

  return(res)
}


#' Calcul des codes IECMAR pour le type de mare et les habitats (V4)
#'
#' Pour chaque mare, détermine le type de mare et les habitats associés, puis
#' attribue le code IECMAR correspondant selon les règles définies.
#'
#' @param canonique `data.frame`, jeu de données canonisé avec colonnes
#' `X_index`, `CAN_name` et `CAN_choice`
#'
#' @return Un `data.frame` contenant toutes les lignes originales ainsi que les lignes
#' calculées pour `type_mare` avec la colonne `cor_iecmar` indiquant le code
#'
#'
#' @importFrom dplyr group_by summarise mutate select bind_rows filter
#'
calculs_habitats_v4 <- function(canonique) {
  res_code <- canonique %>%
    group_by(X_index) %>%
    summarise(
      type_mare = CAN_choice[CAN_name == "type_mare" & !is.na(CAN_choice)],  # réponse exacte à type_mare
      habitats = list(CAN_choice[CAN_name == "habitat" & !is.na(CAN_choice)])
    ) %>%
    mutate(
      CAN_name = "type_mare",
      cor_iecmar = as.character(case_when(
        type_mare == "foret" & grepl("bois_feuillus", habitats) & grepl("bois_resineux", habitats) ~ 2L,
        type_mare == "foret" & grepl("bois_feuillus", habitats) ~ 1L,
        type_mare == "foret" & grepl("bois_resineux", habitats) ~ 2L,
        type_mare == "foret" & grepl("bois_mixte",    habitats) ~ 2L,
        type_mare == "foret"                                           ~ 2L,
        type_mare == "prairie"                                         ~ 1L,
        type_mare == "culture"                                         ~ 2L,
        type_mare == "marais"                                          ~ 1L,
        type_mare == "carriere"                                        ~ 2L,
        type_mare == "bassin_routier"                                  ~ 3L,
        type_mare == "village"                                         ~ 3L,
        type_mare == "NSP"                                             ~ 2L,
        type_mare == "autre"                                           ~ 2L
      )
    )) %>%
    mutate(CAN_choice = paste0(type_mare, "(", as.character(habitats), ")")) %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  res <- canonique %>%
    filter(CAN_name %not_in% c("type_mare_autre", "habitat", "type_mare")) %>%
    bind_rows(res_code)

  return(res)
}

#' Compléter la turbidité en absence de donnée (C11)
#'
#' Assure qu’une information de turbidité est présente pour chaque mare.
#' Si aucune valeur n’est renseignée (absence d’eau ou donnée manquante),
#' une valeur par défaut est attribuée avec le code IECMAR correspondant.
#'
#' @param canonique `data.frame`, jeu de données canonisé contenant au moins
#' les colonnes `X_index`, `colname`, `CAN_name`, `CAN_choice`, `id_can`,
#' et `cor_iecmar`
#'
#' @return Un `data.frame` où les lignes liées à la turbidité sont complétées :
#' \describe{
#'   \item{CAN_name}{"turbidite" si absent}
#'   \item{CAN_choice}{"Inconnu" si valeur manquante}
#'   \item{cor_iecmar}{Code IECMAR 11 si valeur manquante}
#' }
#'
#' @details
#' La détection des valeurs manquantes se fait via `id_can`.
#' Si celui-ci est `NA`, la turbidité est considérée comme non renseignée
#' et une valeur par défaut est attribuée.
#'
#' @importFrom dplyr filter group_by mutate select bind_rows
#'
turbidite_if_no_water <- function(canonique) {
  res_code <- canonique %>%
    filter(colname == "turbidite") %>% # Usage du colname et non du CAN_name car si turbidite = NULL, le CAN_name est NA
    group_by(X_index) %>%
    mutate(CAN_name = ifelse(is.na(id_can), "turbidite", CAN_name)) %>%
    mutate(CAN_choice = ifelse(is.na(id_can), "Inconnu", CAN_choice)) %>%
    mutate(cor_iecmar = ifelse(is.na(id_can), as.character(11), cor_iecmar)) %>%
    select(X_index, CAN_name, CAN_choice, cor_iecmar)

  res <- canonique %>%
    filter(colname != "turbidite") %>%
    bind_rows(res_code)

  return(res)
}
