# JDD utilise pour les traitements geoms
routes <- split_shp_by_departement("./data-raw/shp/routes/k15/routes.shp", dep_col = "dept")

# # JDD utilise pour les reseaux
# routes_RnAu <- lapply(routes, function(df) {
#   df %>% dplyr::filter(!is.na(cpx_classe)) %>% # pour garder seulement RD, RN et autoroutes
#   dplyr::filter(grepl("Autoroute", cpx_classe) | grepl("Nationale", cpx_classe)) %>% # pour virer RD
#     prepare_negatif_layer(.03)
# })

# JDD utilise pour les reseaux
routes_RnAu <- lapply(routes, function(df) {
  df %>% dplyr::filter(!is.na(cpx_classe)) # pour garder seulement RD, RN et autoroutes
})

usethis::use_data(routes, overwrite = TRUE)
usethis::use_data(routes_RnAu, overwrite = TRUE)
