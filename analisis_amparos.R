library(dplyr)
library(odbc)
library(DBI)
library(tidyverse)



###############################################################################################
# Levantamos la base de legales                                                               #
###############################################################################################



conn <- dbConnect(odbc::odbc(), driver = "sql server", 
                  server = "dwdatamart.osde.ar", database = "DB_CIENCIADEDATOS", 
                  trusted_connection = "yes")



amparos <- dbGetQuery(conn = conn, "SELECT * FROM [DB_CIENCIADEDATOS].[dbo].[CDD_SOCIOS_BASE_LEGALES]")

amparos_mac <- amparos %>% 
  filter(PRODUCTO_PRINCIPAL == "Medicamentos de Alta Complejidad- MAC")

sum(is.na(amparos_mac$DNUM_IC))

###############################################################################################
# Sacamos los casos que no tenemos IC                                                         #
###############################################################################################

amparos_mac_sin_ic <- amparos_mac %>% 
  filter(is.na(DNUM_IC))

amparos_mac_con_ic <- amparos_mac %>% 
  filter(!is.na(DNUM_IC))

# ctrl 

nrow(amparos_mac)-nrow(amparos_mac_con_ic)-nrow(amparos_mac_sin_ic)


###############################################################################################
# Obtenemos los consumos de medicamentos de esos IC en situacion actual                       #
###############################################################################################

ic_buscar <- paste0("'",amparos_mac_con_ic$DNUM_IC,"'", collapse = ", ")

# CONSUMOS DESDE ABRIL 2023 A SEPTIEMBRE 2025 ACTUALIZADOS

consumos_medicamentos_ic_amparos <- dbGetQuery(conn = conn, paste("SELECT B.DNUM_IC
      ,[DID_AGRUPAMIENTO]
      ,[DNUM_ANOMES_PRESTACION]
      ,[DNUM_FILIAL_CONSUMO]
      ,[DNUM_FILIAL_SERVICIO]
      ,[DID_PLAN]
      ,A.[DID_NOMENCLADOR]
      ,D.DDES_CORTA_PRACTICA
      ,[DID_ENTPRESTADORA_CONT]
      ,[DID_ENTPRESTADORA_EFECTOR]
      ,[NUM_CANTIDAD]
      ,[NUM_VALOR_PAGADO_HISTORICO]
      ,[NUM_VALOR_ACTUALIZADO]
      ,[DID_CLASECOSTO]
      ,[DID_TIPOPRESTACION]
      ,[DID_ORIGENGASTO]
      ,[NUM_LOTE_CONTRATACION]
      ,B.[DID_SOCIO]
      ,[DID_TIPODEUDOR]
      ,[DID_MEDICAMENTO]
      ,[DID_TIPO_FACTURACION]
      ,[DID_INSTPRESTACION]
      ,[ORIGEN]
      ,A.[DID_GRUPO_PRESUPUESTO]
      ,[DID_DISCAP_NOVAL]
      ,[DID_EDAD]
      ,[DID_MOSTRADOR]
      ,[DID_ENTPRESTADORA_PRESCRIPTOR]
      ,[DID_FECHA_PRESTACION]
      ,[DID_COBERTURA]
      ,[CONTEXTO]
      ,[DID_PMI]
  FROM [DBPresupuestos].[dbo].[DWCONS_CONSUMOS_SITUACION_ACTUAL_202509] A
  LEFT JOIN DWDATAMART.dbo.DSOCIO B
  ON A.DID_SOCIO = B.DID_SOCIO
  LEFT JOIN DWDATAMART.dbo.DGRUPO_PRESUPUESTO C
  ON A.DID_GRUPO_PRESUPUESTO = C.DID_GRUPO_PRESUPUESTO
  LEFT JOIN DWDATAMART.dbo.DNOMENCLADOR D 
  ON A.DID_NOMENCLADOR = D.DID_NOMENCLADOR
  WHERE B.DNUM_IC in (" ,ic_buscar,")
  AND C.GRUPO = '16 - MEDICAMENTOS'"))


agrup_medicamento <- consumos_medicamentos_ic_amparos %>% 
  group_by(DDES_CORTA_PRACTICA) %>% 
  summarise(NUM_VALOR_ACTUALIZADO_EN_MILL = sum(NUM_VALOR_ACTUALIZADO)/1000000) %>% 
  arrange(desc(NUM_VALOR_ACTUALIZADO_EN_MILL))



agrup_medicamento$decil <- cut(agrup_medicamento$NUM_VALOR_ACTUALIZADO_EN_MILL,
                breaks = quantile(agrup_medicamento$NUM_VALOR_ACTUALIZADO_EN_MILL,
                                  probs = seq(0, 1, 0.1), na.rm = TRUE),
                include.lowest = TRUE,
                labels = FALSE)


#Agrupar por decil y calcular cantidad y promedio
resumen <- aggregate(NUM_VALOR_ACTUALIZADO_EN_MILL ~ decil, data = agrup_medicamento, 
                     FUN = function(x) c(cantidad = length(x), promedio = mean(x)))



# Convertir a data.frame
resumen_df <- data.frame(
  decil = resumen$decil,
  cantidad = resumen$NUM_VALOR_ACTUALIZADO_EN_MILL[, "cantidad"],
  promedio = resumen$NUM_VALOR_ACTUALIZADO_EN_MILL[, "promedio"]
)


resumen_df %>% 
  ggplot(aes(x=factor(decil),y=promedio))+
  geom_col()+
  geom_text(aes(label = round(promedio, 1)), 
            vjust = -0.5, size = 4) +
  labs(title = "Promedio por decil",
       x = "Decil",
       y = "Promedio") +
  theme_minimal()


###############################################################################################
# Graficamos el decil 9 y 10                                                                  #
###############################################################################################


agrup_medicamento %>% 
  ggplot(aes(NUM_VALOR_ACTUALIZADO_EN_MILL)) +
  geom_boxplot()

agrup_medicamento %>% 
  ggplot(aes(NUM_VALOR_ACTUALIZADO_EN_MILL)) +
  geom_histogram()



consumos_medicamentos_ic_amparos %>% 
  group_by(DNUM_IC) %>% 
  summarise(NUM_VALOR_ACTUALIZADO_EN_MILL = sum(NUM_VALOR_ACTUALIZADO)/1000000) %>% 
  arrange(desc(NUM_VALOR_ACTUALIZADO_EN_MILL)) %>% View()



consumos_medicamentos_ic_amparos %>% 
  group_by(DNUM_IC,DID_NOMENCLADOR,DDES_CORTA_PRACTICA) %>% 
  summarise(NUM_VALOR_ACTUALIZADO_EN_MILL = sum(NUM_VALOR_ACTUALIZADO)/1000000) %>% 
  arrange(desc(NUM_VALOR_ACTUALIZADO_EN_MILL)) %>% View()
  
