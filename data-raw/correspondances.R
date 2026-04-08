
load_cannonique <- function(path) {
  canonique <- read.csv2(path, fileEncoding = "UTF-8-BOM") %>%
    select(-id_v4, -id_v5, -rmq, -verif_critere)
}

corresp_v4 <- read.csv2("./data-raw/mapping/mapping_form_v4.csv") %>%
    select(id_can, name_survey, name_choice)
corresp_v5 <- read.csv2("./data-raw/mapping/mapping_form_v5.csv") %>%
    select(id_can, name_survey, name_choice)

cor_canonique <- load_cannonique("./data-raw/mapping/canonnique.csv")

iecmar <- read.csv2("./data-raw/mapping/referentiel_iecmar.csv", fileEncoding = "UTF-8-BOM")


usethis::use_data(corresp_v4, overwrite = TRUE)
usethis::use_data(corresp_v5, overwrite = TRUE)
usethis::use_data(cor_canonique, overwrite = TRUE)
usethis::use_data(iecmar, overwrite = TRUE)
