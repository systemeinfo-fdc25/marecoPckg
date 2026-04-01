# JDD utilise pour les reseaux

clc <- st_read("./data-raw/shp/clc/k50/clc_25.shp") %>%
  rmapshaper::ms_innerlines() %>%
  prepare_negatif_layer(.10)

usethis::use_data(clc, overwrite = TRUE)
