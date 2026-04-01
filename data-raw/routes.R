# JDD utilise pour les traitements geoms
routes <- split_shp_by_departement("./data-raw/shp/routes/k15/routes.shp", dep_col = "dept")

# JDD utilise pour les reseaux
routes_RdRn <- lapply(routes, function(df) {
  df %>% dplyr::filter(!is.na(cpx_classe)) %>% # pour garder seulement RD, RN et autoroutes
    prepare_negatif_layer(.03)
})

usethis::use_data(routes, overwrite = TRUE)
usethis::use_data(routes_RdRn, overwrite = TRUE)
