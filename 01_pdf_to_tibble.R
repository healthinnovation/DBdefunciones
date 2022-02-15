library(pdftools)
library(tidyverse)

lista_pdf <- list.files(path = "sources/", pattern = ".pdf$") %>% 
  sprintf("sources/%s",.)

pdf_to_tibble <- function(x) {
  year <- gsub("\\D", "", x) %>%
    substr(., 4, 7) %>%
    sprintf("year_%s", .)
  table <- pdf_text(x) %>%
    read_fwf() %>%
    mutate(n_defunciones = gsub("\\D", "", X1) %>% as.integer()) %>%
    mutate(distritos = gsub("[^a-zA-Z]", "", X1)) %>%
    mutate(n_defunciones = ifelse(is.na(n_defunciones), X2, n_defunciones)) %>%
    select(distritos, n_defunciones) %>%
    `[`(-c(1:9),)

  final_table <- table %>%
    mutate(
      anio = substring(year, 6, 10) %>% as.numeric()
    )
  return(final_table)
}


newdata <- lapply(lista_pdf, pdf_to_tibble) %>%
  map_df(.f = as.tibble) %>%
  filter(distritos != "")

write_csv(newdata,"basedata/BDdefun_2017_2022.csv")