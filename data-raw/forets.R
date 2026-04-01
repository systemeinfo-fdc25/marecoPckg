# JDD utilise pour les traitements geom

forets <- st_read("data-raw/shp/forets/k05/ocs_ge_ligneux_sup_1ha.shp")

usethis::use_data(forets, overwrite = TRUE)
