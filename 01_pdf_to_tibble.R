library(pdftools)
library(tidyverse)

# 1. Reading to pdf  ------------------------------------------------------

lista_pdf <- list.files(path = "sources/", pattern = ".pdf$") %>% 
  sprintf("sources/%s",.)

# 2. PDF to tibble --------------------------------------------------------
pdf_to_tibble <- function(x) {
  year <- gsub("\\D", "", x) %>%
    substr(., 4, 7) %>%
    sprintf("year_%s", .)
  table1<- pdf_text(x) %>%
    read_fwf() %>%
    mutate(n_defunciones = gsub("\\D", "", X1) %>% as.integer()) %>%
    mutate(distritos = gsub("[^a-zA-Z]", "", X1)) 
  table2 <- table1 %>%
    mutate(n_defunciones = ifelse(is.na(n_defunciones), X2, n_defunciones)) %>%
    select(distritos, n_defunciones) %>%
    `[`(-c(1:9),)

  final_table <- table2 %>%
    mutate(
      anio = substring(year, 6, 10) %>% as.numeric()
    )
  return(final_table)
}

newdata <- lapply(lista_pdf, pdf_to_tibble) %>%
  map_df(.f = as_tibble) %>%
  filter(distritos != "") %>% 
  drop_na(n_defunciones)

# 3.Exporting a new database in a csv format ------------------------------
write_csv(newdata,"basedata/BDdefun_2017_2022.csv")