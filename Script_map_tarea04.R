### CONFIGURACIONES INCIALES ###

#limpiamos memoria
rm(list = ls())
library(tidyverse)  
library(sf)    
library(ggrepel)  

#### Datos ####
# Definimos una estructura de directorios, inputs y outputs


wd <- list()

wd$root <- "C:/Users/luis_/OneDrive/Escritorio/Tarea_clase04/Tarea_clase04/"
wd$inputs <- paste0(wd$root, "01_inputs/01_inputs/")
wd$shapef <- paste0(wd$inputs, "shapefiles/")
wd$outputs <- paste0(wd$root, "02_outputs/")


# Carguemos en memoria la informacion espacial 
peru_sf <- st_read(paste0(wd$shapef, "INEI_LIMITE_DEPARTAMENTAL.shp"))



#### Calculo de centroides ####
peru_sf <- peru_sf %>% mutate(centroid = map(geometry, st_centroid), #funcion map() sirve para aplicar la funcion "st_centroid" a cada elemento de la lista, devuelve otra lista como resultado
                              coords = map(centroid, st_coordinates),
                              coords_x = map_dbl(coords, 1), ##funcion map_dbl() sirve para aplicar una funcion a cada elemento de la lista, devuelve un vector numerico como resultado
                              coords_y = map_dbl(coords, 2)
                              )


#Carguemos la data Fallecidos_Sinadef
falle_sinadef <- read_delim(paste0(wd$root, "fallecidos_sinadef.csv"), delim = "|")


#Generamos la tabla de la cantidad de personas fallecidad en via publica por departamento
falle_sinadef_lugar_via_publi <- falle_sinadef %>% filter(`TIPO LUGAR` == "VIA PUBLICA")
depart_via_publica <- table(falle_sinadef_lugar_via_publi$`DEPARTAMENTO DOMICILIO`)
df_via_publica <- as.data.frame(depart_via_publica)
colnames(df_via_publica) <- c("NOMBDEP","conteo")



# Juntemos nuestra bd
peru_datos <- peru_sf %>% 
  left_join(df_via_publica)


#Generamos el grafico
ggplot(peru_datos)+
  geom_sf(aes(fill = conteo))+
  labs(title = "Fallecidos en Vias Publicas",
       caption = "Fuente de datos : SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Taza de fallecidos")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
                  size = 2,
                  max.overlaps = Inf
                  )
ggsave(paste0(wd$outputs, "MapaFallViaPublicaDpto.png"),
       width = 8.5, height = 11)





#Generamos la tabla de la cantidad de personas fallecidad por muerte violenta (suicidio)
falle_sinadef_suicidio <- falle_sinadef %>% filter(`MUERTE VIOLENTA` == "SUICIDIO")
depart_suicido <- table(falle_sinadef_suicidio$`DEPARTAMENTO DOMICILIO`)
df_depart_suicido <- as.data.frame(depart_suicido)
colnames(df_depart_suicido) <- c("NOMBDEP","conteo")



# Juntemos nuestra bd
peru_datos_2 <- peru_sf %>% 
  left_join(df_depart_suicido)


#Generamos el grafico
ggplot(peru_datos_2)+
  geom_sf(aes(fill = conteo))+
  labs(title = "Fallecidos por suicidio",
       caption = "Fuente de datos : SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Taza de fallecidos")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
                  size = 2,
                  max.overlaps = Inf
                  )


ggsave(paste0(wd$outputs, "MapaFallSuicidioDpto.png"),
       width = 8.5, height = 11)

  


#Generamos la tabla de la cantidad de mujeres fallecidas por homicidio
falle_sinadef_homici_mujer <- falle_sinadef %>% filter(`SEXO` == "FEMENINO") %>% filter(`MUERTE VIOLENTA` == "HOMICIDIO")
depart_hom_muj <- table(falle_sinadef_homici_mujer$`DEPARTAMENTO DOMICILIO`)
df_depart_hom_muj <- as.data.frame(depart_hom_muj)
colnames(df_depart_hom_muj) <- c("NOMBDEP","conteo")



# Juntemos nuestra bd
peru_datos_3 <- peru_sf %>% 
  left_join(df_depart_hom_muj)


#Generamos el grafico
ggplot(peru_datos_3)+
  geom_sf(aes(fill = conteo))+
  labs(title = "Mujeres Fallecidas por homicidio",
       caption = "Fuente de datos : SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Taza de fallecidos")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
                  size = 2,
                  max.overlaps = Inf
  )


ggsave(paste0(wd$outputs, "MapaFallMujerHomicidio.png"),
       width = 8.5, height = 11)




#Generamos el cuadro de la cantidad de personas fallecidad por suicidio y en via publica
colnames(df_depart_suicido) <- c("NOMBDEP","conteo_sui")
colnames(df_via_publica) <- c("NOMBDEP","conteo_via_pu")

# Juntemos nuestra bd
peru_datos_4 <- peru_sf %>% 
  left_join(df_depart_suicido)%>% 
  left_join(df_via_publica)


#Generamos el grafico
ggplot(peru_datos_4)+
  geom_sf(aes(fill = conteo_sui))+
  geom_point(aes(x = coords_x , y = coords_y, size = conteo_via_pu), color = "darkseagreen")+
  labs(title = "Fallecidos por suicidio en via publica",
       caption = "Fuente de datos : SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Taza de suicidios",
       size = "Muerte en via publica")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
                  size = 2,
                  max.overlaps = Inf
  )


ggsave(paste0(wd$outputs, "MapaFallSuicidioViaPublicaDpto.png"),
       width = 8.5, height = 11)


