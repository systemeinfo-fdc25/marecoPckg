
# JDD utilise pour les traitement geom (en dropant les plans d'eau deja present dans le kobo)
eau <- split_shp_by_departement("./data-raw/shp/eau/k25/eau.shp", dep_col = "dept")

# JDD utilise pour les reseaux
eau_max1ha <- lapply(eau, function(df) {
  df %>% dplyr::filter(st_area(geometry) <= units::set_units(10000, "m2"))
})

usethis::use_data(eau, overwrite = TRUE)
usethis::use_data(eau_max1ha, overwrite = TRUE)
