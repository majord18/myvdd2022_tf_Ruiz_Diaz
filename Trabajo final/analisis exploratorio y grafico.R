#para el analisis y exploracion de datos vamos a trabajar con los siguientes paquetes.

library("readxl") # Para importar los archivos
library(readr) # Para importar los archivos
library("ggplot2") # Para trabajar con los gráficos en ggplot2
library("tidyr") # Para transformar los datos del dataset
library("dplyr") # Para manipular los datos
library("plyr") # Para manipular datos
library("reshape2") # Sirve para transformar datos.
library("sp") # Para tratar datos espaciales, clases y metodos de visualización.
library(raster) # Para trabajar capas de imagenesen los mapas.
library(rworldxtra) # Para cargar el dataset de mapas en alta resolusión.
library(sf) # Para datos espaciales y esta compuesta principalmente por puntos, lineas y polígonos.
library(tidyverse) # Contiene alrededor de 30 paquetes de libreria,
                   # lo descargo por si me olvido de algún paquete.
library(plotly) # Para trabajar con gráficos interactivos.
library(htmltools) # Para Manipular y escribir HTML
library(htmlwidgets) # un marco para crear windgets HTML
library(spData) # para trabajar con mapas
library(tmap) #para hacer interactivos los mapas
library(geodata) # Para trabajar con mapas.

####################################################################################
#                                                                                  #
#                  Exploración analitica y gráfica de datos                        #
#                                                                                  #
####################################################################################

#Para una mejor organizacion voy a trabajar con la tabla limpia.

Argentina_limpio <- read_csv("Argentina_limpio.xlsx")

#comienzo a realizar diferentes graficas de analisis.

#realizamos analisis exploratorio con la gráfica ggploT

#-------------------------------------------------------------------------------
# Análisis de habitantes según su edad, sexo y estado civil.
#-------------------------------------------------------------------------------

point <- ggplot(data= Argentina_limpio) +
  geom_point(mapping = aes(x=años_cumplidos, y = REGION, color=estado_civil))+
  facet_wrap(~sexo, nrow = 2,ncol = 1) +
  theme(plot.background = element_rect(fill = "lightblue")) +
  labs(x="Edad", y="Región",
       title = 'Estado civil',
       subtitle = 'Según edad y sexo',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares. 
       Buenos Aires, 2020')
ggplotly(point)



# Utilizo Geom_point para realizar un gráfico de puntos, con facet_wrap voy a separar los
# gráficos dependiendo de lo que quiera, en este ejemplo los separe por sexo, con ncol elijo
#en cuantas columnas quiero que se visualice, con nrow en cuantas filas.
#theme en este caso me permite elgir el color del fondo "celeste".
#En labs voy poniendo las etiquetas que quiero, por ejemplo:
#title = título, Subtitle = subtítulo, Caption = informacion de los datos, x e y
# información sobre los ejes.
# Por último con ggplotly podemos hacer interactivos los datos.

#Podemos observar que las mujeres tienden a estar solteras hasta antes de los 25 años,
#viendose un incremente de esdad de solteria en la zona Pampeana y una disminucion de la edad
#de solteria en la zona del NOA, donde tambien se puede observar una mayor cantidad de mujeres unidas.
#También se puede observar que a partir de los 75 muchas mujeres respondieron ser viudas.
#Mientras que en los hombres las solteria se extiende un poco más de 25 años, coincidiendo que en el NOA 
#hay más casos de hombres unidos y con respecto a la viudes hay menos hombres viudos y esto sucede con más 
#frecuencia a partid¿r de los 87 años.
#en el caso de los hobres se nota que entre los 62 y los 87 hay mas cantidad de hombres casados lo que
#difiere con las mujeres que es mucho mas variada su situacion en ese periodo de vida.



#-------------------------------------------------------------------------------
# Análisis de habitantes segun su edad y scolarización 
#-------------------------------------------------------------------------------
# Gráfica de Barras de nivel de cursado, segun la provincia
bar <- ggplot(data= Argentina_limpio) +
  geom_bar(mapping = aes(x=años_cumplidos, fill=escolarizado),
           show.legend = FALSE, width = 0.9)+
  labs(x="Edad", y="Cantidad de individuos",
       title = 'Escolarización',
       subtitle = '¿Asistio?',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares. 
       Buenos Aires, 2020') +
  coord_flip() +
  facet_wrap(~escolarizado)
ggplotly(bar)


#En este caso se visualizan gráficos de barra con la función geom_bar, al escribir 
#Show.lengend = FALSE le quito la leyenda de las referencias.
# Con coord_flip invierto los ejes.

#-------------------------------------------------------------------------------
# Análisis del nivel de estudio cursado segun el sexo
#-------------------------------------------------------------------------------

bar_1 <- ggplot(data= Argentina_limpio) +
  aes(x=nivel_de_cursado, fill=sexo) +
  geom_bar(show.legend = FALSE, width = 0.75, position = "dodge")+
  theme(aspect.ratio = 1)+
  labs(x="Nivel de estudio", y="Cantidad de individuos")+
  labs(title = 'Nivel de estudio de los individuos',
       subtitle = 'Discriminado por sexo',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares. 
       Buenos Aires, 2020') +
  scale_y_continuous("cantidad de individuaos", breaks= seq(0, 10000, 2000)) +
  coord_flip()
ggplotly(bar_1)


# width = 0.75 para cambiar el ancho de las barras. Position = "dodge" para que 
#se visualicen una barra al lado de la otra y no una sobre la otra.
# scale_y_continuous sirve para modificar la escla del eje y, breaks= seq(0, 10000, 2000)
# en este caso escribimos que va del 0 al 10000, de 2000 en 2000.


#-------------------------------------------------------------------------------
# Análisis del tipo de establecimineto al que asisten
#-------------------------------------------------------------------------------

ggplot(data= Argentina_limpio) +
  geom_bar(mapping = aes(x= tipo_de_establecimiento, fill=tipo_de_establecimiento),
           show.legend = FALSE, width = 1)+
  theme(aspect.ratio = 1,panel.grid.major = element_line(color = "red",
                                                         size = 0.5,
                                                         linetype = 2))+
  labs(x=NULL, y=NULL)+
  labs(title = 'Tipos de establecimientos',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares. 
       Buenos Aires, 2020') +
  coord_polar()

# coord_polar es para visualizar en coordenadas polares.
# labs(x=NULL, y=NULL) con esta sentencia hago que no aparezcan las leyendas de las
# coordenadas x e y.
# panel.grid.major = element_line(color = "red", size = 0.5, linetype = 2) sirve para
#cambiar el color de las lineas, el ancho y el grosor.


#-------------------------------------------------------------------------------
# Analisis 
#-------------------------------------------------------------------------------
ggplot(Argentina_limpio) +
  aes(x =nivel_de_cursado , y = provincia, fill = años_cumplidos) +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "gray", high = "green") +
  labs(title = 'Nivel educativos',
       subtitle = 'Según la provincia y la edad',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares.
       Buenos Aires, 2020')

# scale_fill_gradient(low = "gray", high = "green") con esta sentencia le cambio 
# los colores. Para cambiarle el color de las lineas que separan los rectángulos 
# utilizo geom_tile(colour = "white") 

#-------------------------------------------------------------------------------
#Analisis de cantidad de encuestados según sus edades.
#-------------------------------------------------------------------------------


#Histograma 

hist <- ggplot(Argentina_limpio) + 
  geom_histogram( aes(x = años_cumplidos),
                  colour = 5, fill = "red") +
  labs(x="Años Cumplidos", y = "Cantidad de personas", 
       title = 'Edad de los encuestados.',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares.
       Buenos Aires, 2020') +
  scale_y_continuous("cantidad de individuaos", breaks= seq(0, 4000, 500)) +
  scale_x_continuous("Edad", breaks= seq(0, 100, 10))
ggplotly(hist)

# scale_x_continuous("Edad", breaks= seq(0, 100, 10)) lo utilizo para cambiar las ecalas del eje x

# Histograma con curva de densidad

hist_1 <- ggplot(Argentina_limpio, aes(x = años_cumplidos)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 4, fill = "lightgreen") +
  geom_density() + 
  labs(x="Años Cumplidos", y = "Densidad de Personas", 
       title = 'Edad de los encuestados.',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares.
       Buenos Aires, 2020')
  ggplotly(hist_1)
  
# Para cambiar el color de las lineas y el reyeno de las barras utilizo
# colour = 4, fill = "lightgreen". 
# geom_density() si no pongo nada adentro solo fhace la curva de densidad, sin el área bajo la curva

 
# Histagrama con gráfica de densidad.

hist_2<- ggplot(Argentina_limpio, aes(x = años_cumplidos)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 8, fill = "yellow") +
  labs(x="Años Cumplidos", y = "Cantidad de personas", 
       title = 'Edad de los encuestados.',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares.
       Buenos Aires, 2020') +
geom_density(lwd = 1, colour = 6,
             fill = 10, alpha = 0.50)
ggplotly(hist_2)

#geom_density(lwd = 2, colour = 5, fill = 3, alpha = 0.50) con lwd cambio el grasor de
# la curva, con colour cambio el color de la curva, con fill cambio el tipo de
# transparencia que quiero usa en la densidad
#boxplot

boxp <- ggplot(Argentina_limpio, aes(x=as.factor(lee_y_escribe), y=años_cumplidos)) + 
  geom_boxplot(fill="green", alpha=0.5) + 
  xlab("lee_y_escribe")+
  labs(title = 'Sabe leer y escribir.',
       caption = 'Esta información es de tal fuente:Encuesta permanente de hogares.
       Buenos Aires, 2020')+
  theme(plot.background = element_rect(fill = "lightpink"),panel.grid.major = 
          element_line(color = "pink"))
ggplotly(boxp)

# Con geom_boxplot(fill="green", alpha=0.2) fill cambio el color de la caja y con 
# Alpha la transparencia.
# Con plot.background = element_rect(fill = "lightgreen" cambio el color del fondo.
# panel.grid.major = cambia el color y estilo de la cuadrícula.


#-------------------------------------------------------------------------------
#
#                                  Mapas                                       
#
#-------------------------------------------------------------------------------


data("countriesHigh")

Mundo <- countriesHigh %>% 
  st_as_sf()

#para ver cómo se llaman los codigos de paises
getData

#si quiero un código de una país
df <- getData('ISO3')


# descargar datos vectoriales 
Argentina <- getData(name = "GADM", country ="ARG", level=1)


# transformar a SF

Argentina_sf <- Argentina%>%
  st_as_sf()

# me muestra el mapa de Argentina con división política

ggplot()+
  geom_sf(data = Argentina_sf)

# Veo cuales son las variables

colnames(Argentina_sf)

# observo cuales son los nombres de la columna NAME_1

unique(Argentina_sf$NAME_1)


# armo un grupo con las provincias y los datos que quiero que se plasmen en el mapa

grupo <- Argentina_limpio %>%
  group_by(`provincia`) %>% 
  summarise(total = n())


#unir datos. Agrupo las variables de name_1 con las provincias de mi base de datos

dhWorld <-  Argentina_sf %>% 
  rename(provincia=NAME_1) %>% 
  full_join(grupo)

#filtrar solo lo de interes, selecciono las variables que quiero usar.

mapa <- dhWorld %>% 
  dplyr::select(provincia,geometry,total)

#gráfico por color segun una escala

ggplot()+
  geom_sf(data = mapa, aes(fill=total))+
  theme_bw()+
  scale_fill_viridis_c()
 
# mapa con datos

map_plot <- mapa%>%
  ggplot(aes(fill=total))+
  geom_sf()+
  geom_sf_text(aes(label=total,size =0.1, 
                     color='affffff')) 

map_plot


#activamos visión interactiva del packege tmap

tmap_mode("view")


#mapa interactivo

dhWorld_Plotly <- mapa %>% 
  mutate(text=paste0(provincia, "\n", total)) %>% 
  ggplot(aes(fill=total, text=text
  ))+
  geom_sf(color="white")+
  theme_void()

ggplotly(dhWorld_Plotly, tooltip = c("text")) 
