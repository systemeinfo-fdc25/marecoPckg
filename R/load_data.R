#' Charger un fichier CSV Kobo et le convertir en objet spatial
#'
#' Lit un fichier CSV exporté depuis KoboToolbox contenant des coordonnées
#' latitude/longitude, filtre les lignes sans coordonnées, puis convertit
#' les données en objet `sf` projeté en Lambert-93 (EPSG:2154).
#'
#' @param path Character, chemin vers le fichier CSV à importer
#'
#' @return Un objet `sf` (points) contenant les réponses du formulaire (mares) avec géométrie
#'
#' @details
#' - Le fichier CSV doit contenir les colonnes `X_geometry_latitude` et
#'   `X_geometry_longitude`.
#' - Les valeurs vides ou "NA" sont considérées comme manquantes et exclues.
#' - Le CRS initial est EPSG:4326 (WGS84) puis transformé en EPSG:2154 (Lambert-93).
#'
#' @importFrom sf st_as_sf st_transform
#' @importFrom dplyr filter
#'
#' @export
load_kobo_from_csv <- function(path) {
  df <- read.csv2(path,
                  na.strings = c("", " ", "NA"),
                  fileEncoding = "UTF-8-BOM") %>%
  filter(is.na(X_geometry_latitude) == F | is.na(X_geometry_longitude) == F) %>%
  st_as_sf(coords = c("X_geometry_longitude", "X_geometry_latitude"), crs = 4326) %>%
  st_transform(2154)

  return(df)
}


#' Sépare les entités d'un shapefile par valeur de la colonne département.
#'
#' Utilisée pour la création des JDD. En amont du package (de data-raw -> data.rda).
#'
#' @param shp_path Lit un fichier shapefile (sf).
#' @param dep_col Colonne où sont disciminer les départements
#'
#' @return Retourne une liste nommée par département, avec chaque élément étant un objet sf.
#'
split_shp_by_departement <- function(shp_path, dep_col) {
  # Vérifie que le fichier existe
  if (!file.exists(shp_path)) {
    stop("Le fichier spécifié n'existe pas : ", shp_path)
  }

  # Lecture du shapefile
  df <- st_read(shp_path, quiet = TRUE)

  # Vérifie que la colonne existe
  if (!dep_col %in% colnames(df)) {
    stop("La colonne département spécifiée n'existe pas dans le shapefile : ", dep_col)
  }

  # Récupération des valeurs uniques de départements
  dep_vals <- unique(df[[dep_col]])

  # Création de la liste par département
  df_list <- lapply(dep_vals, function(dep) {
    df[df[[dep_col]] == dep, ]
  })

  # Nommer chaque élément de la liste avec le département
  names(df_list) <- as.character(dep_vals)

  return(df_list)
}


#' Preparation d'une couche lineaire pour decoupage de reseau (utilise dans data-raw)
#'
#' Transforme une couche lineaire en polygones via l'application de tampons etroits,
#' afin de permettre le decoupage ulterieur de reseaux polygonaux. Cette fonction est
#' typiquement utilisee pour representer des elements fragmentants (ex: routes, voies ferrees).
#'
#' @param layer `sf`, couche spatiale lineaire a transformer
#' @param buffer Numeric, distance en metres pour la creation des tampons
#' @param nQuadSegs Integer, nombre de segments utilises pour approximer les courbes
#' lors du buffer (default: 5)
#'
#' @return Une couche `sf` de polygones valides issue de la couche lineaire d'entree
#'
#' @importFrom sf st_buffer st_union st_make_valid st_cast st_as_sf
#'
prepare_negatif_layer <- function(layer, buffer, nQuadSegs = 5) {

  res <- layer %>%
    st_buffer(buffer, nQuadSegs = nQuadSegs) %>%
    st_union() %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    st_as_sf() %>%
    st_make_valid()

  return(res)
}
