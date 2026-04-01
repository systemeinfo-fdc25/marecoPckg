# JDD utilise pour les reseaux

lgv <- st_read("./data-raw/shp/lgv/LGV_bfc.shp") %>%
  prepare_negatif_layer( .05)


usethis::use_data(lgv, overwrite = TRUE)
