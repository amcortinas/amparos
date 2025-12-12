library(DBI)
library(odbc)
library(dplyr)


###############################################################################################
# Levantamos la base de legales                                                               #
###############################################################################################


conn <- dbConnect(odbc::odbc(), driver = "sql server", 
                  server = "dwdatamart.osde.ar", database = "DB_CIENCIADEDATOS", 
                  trusted_connection = "yes")



amparos <- dbGetQuery(conn = conn, "SELECT * FROM [DB_CIENCIADEDATOS].[dbo].[CDD_SOCIOS_BASE_LEGALES]")


amparos_mac <- amparos %>%
  filter(PRODUCTO_PRINCIPAL == "Medicamentos de Alta Complejidad- MAC")

#sum(is.na(amparos_mac$DNUM_IC))

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
# Levantamos los consumos                                                                     #
###############################################################################################


ic_buscar <- paste0("'",amparos_mac_con_ic$DNUM_IC,"'", collapse = ", ")

# CONSUMOS DESDE ABRIL 2023 A SEPTIEMBRE 2025 ACTUALIZADOS

consumos_medicamentos_ic_amparos <- dbGetQuery(conn = conn, paste("SELECT B.DNUM_IC
      ,[DCOD_SEXO]
      ,[DID_AGRUPAMIENTO]
      ,[DNUM_ANOMES_PRESTACION]
      ,[DNUM_FILIAL_CONSUMO]
      ,[DNUM_FILIAL_SERVICIO]
      ,A.[DID_PLAN]
      ,B.[DCOD_PLAN]
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
      ,A.[DID_MEDICAMENTO]
      ,E.[DDES_MEDICAMENTO]
      ,E.[DDES_MONODROGA]
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
      ,B.DDES_MOTIVO_BAJA
  FROM [DBPresupuestos].[dbo].[DWCONS_CONSUMOS_SITUACION_ACTUAL_202509] A
  LEFT JOIN DWDATAMART.dbo.DSOCIO B
  ON A.DID_SOCIO = B.DID_SOCIO
  LEFT JOIN DWDATAMART.dbo.DGRUPO_PRESUPUESTO C
  ON A.DID_GRUPO_PRESUPUESTO = C.DID_GRUPO_PRESUPUESTO
  LEFT JOIN DWDATAMART.dbo.DNOMENCLADOR D 
  ON A.DID_NOMENCLADOR = D.DID_NOMENCLADOR
  LEFT JOIN [DWDATAMART].[dbo].[DMEDICAMENTO] E
  ON A.DID_MEDICAMENTO = E.DID_MEDICAMENTO
  LEFT JOIN [DWDATAMART].[dbo].[DPLAN] F
  ON A.DID_PLAN = F.DID_PLAN
  WHERE B.DNUM_IC in (" ,ic_buscar,")
  AND C.GRUPO = '16 - MEDICAMENTOS'"))

save(amparos, consumos_medicamentos_ic_amparos, file = "amparos_consumos.RData")
