library(stringdist)

matriz_similitud <- stringdistmatrix(
  unique(consumos_medicamentos_ic_amparos$DDES_CORTA_PRACTICA),
  unique(amparos_mac$SUB_PRODUCTO_PRINCIPAL),
  method = "jw"  # Jaro-Winkler, muy buena para textos cortos
)

matriz_similitud2 <- unique(consumos_medicamentos_ic_amparos$DDES_CORTA_PRACTICA) %>% 
  cbind(as.data.frame(matriz_similitud))

names(matriz_similitud2) <- c("DDES_CORTA_PRACTICA",unique(amparos_mac$SUB_PRODUCTO_PRINCIPAL))

matriz_similitud_longer <- matriz_similitud2 %>% 
  pivot_longer(cols = 2:93,names_to = "SUB_PRODUCTO_PRINCIPAL",values_to = "VALORES")

matriz_similitud_longer %>% 
  group_by(DDES_CORTA_PRACTICA) %>% 
  summarise(VALORES = min(VALORES)) %>% 
  inner_join(matriz_similitud_longer) %>%
  filter(VALORES<0.3) %>% 
  View()
