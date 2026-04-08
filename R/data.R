#' Surfaces d'eau de la BD TOPO de l'IGN ≤ 1000 m²
#'
#' Ce jeu de données contient les polygones représentant les petites surfaces
#' d'eau (mares, plans d'eau) filtrées à 1000 m² maximum.
#' Les geometries ont ete simplifees.
#' Il est utilisé dans le cadre du calcul des réseaux de mares.
#'
#'
#' @source Données originales provenant de la BD TOPO IGN (2025).
#'
"eau_max1ha"

#' Surfaces d'eau de la BD TOPO de l'IGN
#'
#' Ce jeu de données contient les polygones représentant les petites surfaces
#' d'eau (mares, plans d'eau). Les geometries ont ete simplifees.
#' Il est utilisé dans le cadre des traitements géométriques pour le calcul du IECMAr.
#'
#' @source Données originales provenant de la BD TOPO IGN (2025).
#'
"eau"


#' Linéaire de la Ligne a Grande Vitesse (LGV) de la BD TOPO de l'IGN
#'
#' Jeu de donnees de la LGV au format `sf` sur la region Bourgogne-Franche-Comte.
#' Il est utilisé dans le cadre du calcul des réseaux de mares.
#'
#' @source Données originales provenant de la BD TOPO IGN (2025).
#'
"lgv"

#' Linéaire routier d'apres la BD TOPO de l'IGN
#'
#' Jeu de donnees des routes format `sf` sur la region Bourgogne-Franche-Comte.
#' Les geometries ont ete simplifees.
#' Il est utilisé dans le cadre des traitements géométriques pour le calcul du IECMAr.
#'
#' @source Données originales provenant de la BD TOPO IGN (2025).
#'
"routes"

#' Linéaire routier d'apres la BD TOPO de l'IGN, filtré sur les RD, RN et autoroutes
#'
#' Jeu de donnees des routes format `sf` sur la region Bourgogne-Franche-Comte, filtré sur les RD, RN et autoroutes.
#' Les geometries ont ete simplifees.
#' Il est utilisé dans le cadre du calcul des réseaux de mares.
#'
#' @source Données originales provenant de la BD TOPO IGN (2025).
#'
"routes_RnAu"

#' Occupation du sol Corine Land Cover 2018 niveau 1
#'
#' Jeu de donnees du CLC au format `sf` sur la region Bourgogne-Franche-Comte.
#' Les geometries ont ete simplifees.
#' Il est utilisé (facultativement) dans le cadre du calcul des réseaux de mares.
#'
#' @source Données originales provenant du Corine Land Cover 2018.
#'
"clc"

#' Forêts (entitées ligneuses de l'OCS GE) > 1 ha
#'
#' Couche de l'OCS-GE, filtré pour ne garder que les forêts (au sens large) de plus de 1 hectare.
#' Les geometries ont ete simplifees.
#' Il est utilisé dans le cadre des traitements géométriques pour le calcul du IECMAr.
#'
#' @source Données originales provenant de l'OCS-GE 2023
#'
"forets"