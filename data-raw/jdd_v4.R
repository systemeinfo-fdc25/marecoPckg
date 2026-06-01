
jdd_v4 <- list(
  "25" = load_kobo_from_csv("./data-raw/jdd_v4/export_kobo/fdc25_v4.csv"),
  "90" = load_kobo_from_csv("./data-raw/jdd_v4/export_kobo/fdc90_v4.csv"),
  "70" = load_kobo_from_csv("./data-raw/jdd_v4/export_kobo/fdc70_v4.csv")
)

jdd_v4 <- lapply(jdd_v4, function(df) {
  df %>% mutate(form_v = 4L) %>%
    filter(is.na(mare_existe) | mare_existe %in% c('oui', 'existe'))
})

usethis::use_data(jdd_v4, overwrite = TRUE)
