library(purrr)
library(dplyr)
library(vroom)

download.file(
  url = "https://drive.minsa.gob.pe/s/XJoxGPW2wBmkSAD/download",
  destfile = "./datos_abiertos/data/defunciones.csv",
  method = 'curl'
)


defunciones <- vroom("datos_abiertos/data/defunciones.csv",
  col_types = cols(
    Nº = col_integer(),
    `TIPO SEGURO` = col_factor(),
    SEXO = col_factor(),
    EDAD = col_integer(),
    `TIEMPO EDAD` = col_character(),
    `ESTADO CIVIL` = col_factor(),
    `NIVEL DE INSTRUCCIÓN` = col_factor(),
    ETNIA = col_factor() , 
    `COD# UBIGEO DOMICILIO` = col_character(), 
    `PAIS DOMICILIO` = col_character(),
    `DEPARTAMENTO DOMICILIO` = col_factor(),
    `PROVINCIA DOMICILIO` = col_factor(),
    `DISTRITO DOMICILIO` = col_character(), 
    FECHA = col_date(format = "%Y-%m-%d"),
    AÑO = col_integer(),
    MES = col_integer(),
    `TIPO LUGAR` = col_character(),
    INSTITUCION = col_character(),
    `MUERTE VIOLENTA` = col_character(),
    NECROPSIA = col_factor(),
    `DEBIDO A (CAUSA A)` = col_character(),
    `CAUSA A (CIE-X)`  = col_character(),
    `DEBIDO A (CAUSA B)` = col_character(), 
    `CAUSA B (CIE-X)` = col_character(), 
    `DEBIDO A (CAUSA C)` = col_character(), 
    `CAUSA C (CIE-X)`  = col_character(), 
    `DEBIDO A (CAUSA D)` = col_character(), 
    `CAUSA D (CIE-X)` = col_character(), 
    `DEBIDO A (CAUSA E)` = col_character(),
    `CAUSA E (CIE-X)` = col_character(),
    `DEBIDO A (CAUSA F)` = col_character(), 
    `CAUSA F (CIE-X)` = col_character(),
  )
)

names(defunciones) <- c(
  "ID","TIPO_SEGURO","SEXO","EDAD","TIEMPO_EDAD",
  "ESTADO_CIVIL","NIVEL_DE_INSTRUCCION","ETNIA",
  "UBIGEO","PAIS","DEPARTAMENTO","PROVINCIA",
  "DISTRITO","FECHA","ANIO","MES","TIPO_LUGAR",
  "INSTITUCION","MUERTE_VIOLENTA","NECROPSIA",
  "DEBIDO_A(CAUSA_A)","CAUSA_A(CIE_X)","DEBIDO_A(CAUSA_B)",
  "CAUSA_B(CAUSA_C)","DEBIDO_A(CAUSA_C)","CAUSA_C(CIE_X)",
  "DEBIDO_A(CAUSA_D)","DEBIDO_A(CAUSA_E)","CAUSA_E(CIE_X)",
  "CAUSA_E(CIE_X)","DEBIDO_A(CAUSA_F)","CAUSA_F(CIE-X)"
  )
study_area <- c(
    "MADRE DE DIOS ","UCAYALI","JUNIN","HUANCAVELICA",
    "ICA","AYACUCHO","APURIMAC","CUSCO","PASCO","AREQUIPA"
    "PUNO")

ubern_db <- defunciones %>%
        filter(DEPARTAMENTO %in% study_area)

saveRDS(
  ubern_db,
  file = paste0("./Ubern/data/","ubern_db.rds")
)

file.remove('./Ubern/data/defunciones.csv')