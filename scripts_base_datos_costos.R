library(DBI)
library(odbc)
library(dplyr)
library(tidyverse)


###############################################################################################
# Levantamos la base de legales                                                               #
###############################################################################################


conn <- dbConnect(odbc::odbc(), driver = "sql server", 
                  server = "dwdatamart.osde.ar", database = "DB_CIENCIADEDATOS", 
                  trusted_connection = "yes")


amparos <- dbGetQuery(conn = conn, "WITH amparosDiscaMac AS (
  SELECT *
  FROM [DB_CIENCIADEDATOS].[dbo].[CDD_SOCIOS_BASE_LEGALES]
  
)
SELECT d.DDES_MOTIVO_BAJA, sf.*
  FROM DWDATAMART.dbo.DSOCIO AS d
RIGHT JOIN amparosDiscaMac AS sf
ON d.DNUM_IC = sf.DNUM_IC;
")

amparos$DDES_MOTIVO_BAJA <- trimws(amparos$DDES_MOTIVO_BAJA)

motivo_baja_fallec <- amparos %>%
  dplyr::filter(grepl("allec", DDES_MOTIVO_BAJA, ignore.case = TRUE)) %>% 
  dplyr::select(DNUM_IC, DDES_MOTIVO_BAJA) %>% distinct()

unique(motivo_baja_fallec$DDES_MOTIVO_BAJA)

motivo_baja_fallec$DDES_MOTIVO_BAJA <- "Fallecimiento"

amparos <- amparos %>% 
  rename("GRUPO_AMPAROS"=`GRUPO AMPAROS`)

amparos_mac_disca <- amparos %>% 
  filter((PRODUCTO_PRINCIPAL == "Medicamentos de Alta Complejidad- MAC" |
            GRUPO_AMPAROS == "Discapacidad") & FECHA_INICIO>="2023-01-01" & CARTERA_ACTIVA_SN=="S") %>% 
  left_join(motivo_baja_fallec) %>% 
  filter(DDES_MOTIVO_BAJA != "Fallecimiento" & !is.na(DNUM_IC)) %>% 
  select(-DDES_MOTIVO_BAJA,-CARATULA, -TIPO_DE_PROCESO) %>% 
  distinct() %>% 
  as.data.frame()

unique(amparos_mac_disca$CARTERA_ACTIVA_SN)

amparos_mac_disca %>% 
  group_by(PRODUCTO_PRINCIPAL) %>% 
  summarise(conteo=n())


amparos_con_mas_de_un_asunto <- amparos_mac_disca %>% 
  group_by(ASUNTO) %>% 
  summarise(conteo=n()) %>% 
  filter(conteo>1)

base_amparos_con_mas_de_un_asunto <- amparos_mac_disca %>% 
  inner_join(amparos_con_mas_de_un_asunto %>% select(ASUNTO))

sum(amparos_con_mas_de_un_asunto$conteo)

amparos_mac_disca_univoco <- amparos_mac_disca %>% 
  anti_join(amparos_con_mas_de_un_asunto)


amparos_mac_disca_univoco %>% 
  group_by(ASUNTO) %>% 
  summarise(conteo=n())


amparos_mac_disca_univoco %>% 
  group_by(PRODUCTO_PRINCIPAL) %>% 
  summarise(conteo=n())

write.csv(
  amparos_mac_disca_univoco,
  file = "base_amparos_disca_mac.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



