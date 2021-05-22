# Librerías -----
library(tidyverse)
library(gt)
library(scales)
library(extrafont)
library(readxl)


# Configuraciones generales ----------

# Colores
verde <- "#01B3B6"
negro <- "#333132"
gris <- "#AEB6BF"

color3 <- c(verde, gris, negro)
color2 <- c(verde, negro)

# Opciones de visualización --------
options(scipen = 999)   # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Ubuntu Mono"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#ecf0f1"),
                 text = element_text(family = "Ubuntu Mono"))

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#ecf0f1"),
                 text = element_text(family = "Ubuntu Mono"))


# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Datos Ficticios\nClub de R para RRHH"

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_per <- scale_x_continuous(labels = scales::percent_format(accuracy = 1))

eje_y_per <- scale_y_continuous(labels = scales::percent_format(accuracy = 1))



# Carga de Datos -----
encuesta <- read_excel("data/encuesta.xlsx")
plantel  <- read_excel("data/plantel.xlsx")




# Preparación de datos -----------

# Pivotea el dataset a un formato largo
enc <- encuesta %>% 
  pivot_longer(cols = c(7:11),
               names_to = "pregunta", 
               values_to = "valor")

# Cambia nombres y Organiza variables ordinales 

enc <- enc %>% 
  rename(id = "ID",
         genero = `¿Cómo definirías tu identidad de género?`,
         unidad = "Unidad de Negocio",
         pais = "País",
         sector = "Sector",
         cargo = "Tu cargo/nivel:") %>% 
  mutate(cargo = factor(cargo,
                        levels = c("Management", "Líder", "Contribuidor individual")))

# Crea categorías de resultados

enc <- enc %>% 
  mutate(resultado = if_else(valor == "Totalmente de acuerdo", "Positivo", 
                             if_else(valor == "De acuerdo", "Positivo", 
                                     if_else(valor == "Ni de acuerdo ni en desacuerdo",
                                             "Neutral", "Negativo"
      )
    )
  ),
         resultado = factor(resultado, 
                            levels = c("Positivo", "Neutral", "Negativo")))



## ----grafico1--------------------------------------------------------------------------------
ggplot(enc, aes(x = pais, fill = resultado)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(color3)) +
  estiloh +
  eje_y_per +
  labs(title = "Resultados por país",
       fill = "Resultado",
       x = "", y = "",
       caption = fuente)



## ----etiq-largas-----------------------------------------------------------------------------
enc %>% 
  group_by(pregunta, resultado) %>% 
  summarise(cant = n()) %>% 
  mutate(prop = cant/sum(cant)) %>% 
  filter(resultado == "Positivo") %>% 
  ggplot(aes(x = prop, y = pregunta)) +
  geom_col(fill = verde) +
  estilov +
  eje_x_per +
  labs(title = "Ranking de Respuestas Positivas",
       x = "", y = "",
       caption = fuente)


## ----etiq-largas1----------------------------------------------------------------------------
# Divide el largo de 'función' en varias líneas
enc$preg2 <- str_wrap(enc$pregunta, width = 40)

# Veamos como queda esto en el df
head(enc$preg2,5)



## ----etiq-largas2----------------------------------------------------------------------------
enc %>% 
  group_by(preg2, resultado) %>% 
  summarise(cant = n()) %>% 
  mutate(prop = cant/sum(cant)) %>% 
  filter(resultado == "Positivo") %>% 
  ggplot(aes(x = prop, y = reorder(preg2, prop))) +
  geom_col(fill = verde) +
  estilov +
  eje_x_per +
  labs(title = "Ranking de Respuestas Positivas",
       x = "", y = "",
       caption = fuente)



## ----etiq-largas3, fig.height=8--------------------------------------------------------------
ranking <- enc %>% 
  group_by(preg2, resultado) %>% 
  summarise(cant = n()) %>% 
  mutate(prop = cant/sum(cant)) %>% 
  filter(resultado == "Positivo") %>% 
  ggplot(aes(x = prop, y = reorder(preg2, prop))) +
  geom_col(fill = verde) +
  estilov +
  eje_x_per +
  labs(title = "Ranking de Respuestas Positivas",
       x = "", y = "",
       caption = fuente)

ranking


## ----texto1----------------------------------------------------------------------------------
ranking +
  geom_text(aes(label = percent(prop, # Muestra los resultados como porcentaje
                                accuracy = 1)), # Indica la cantidad de decimales
            size = 3,                           # Cambia el tamaño de la letra
            hjust = 1.2)                        # Mueve la etiqueta para la izquierda
            

            
            
# Funciones -------
str(enc)

cant_prop_gen <- function(df){
  df %>% 
    group_by(genero,resultado) %>% 
    summarise(cant = n()) %>% 
    mutate(prop = cant / sum(cant)) 

}


enc %>% 
  cant_prop_gen() %>% 
  filter(resultado == "Positivo")


#sucundun <- function(df, ){
#  df %>% 
#    group_by() %>% 
#    summarise(cant = n()) %>% 
#    mutate(prop = cant / sum(cant))
#}

# sucundun(enc, genero, resultado)

# Loops -----------

enc %>% 
  group_by(sector, genero, resultado) %>%
  summarise(cant = n()) %>% 
  mutate(prop = cant / sum(cant)) %>% 
  ggplot(aes(y = sector, x = cant, fill = genero)) +
  geom_col(position = "dodge")





graficos <- enc %>% 
  group_by(sector, genero, resultado) %>% 
  summarise(cant = n()) %>% 
  mutate(prop = cant / sum(cant)) %>% 
  split(.$sector) %>% 
  map(~ggplot(.x, aes(y = sector, x = cant, fill = genero)) +
        geom_col(position = "dodge") + estilov) 

paths <- stringr::str_c(names(graficos), ".png")

pwalk(list(paths, graficos), ggsave, path = "files/")


# Trust the Tidyverse ------

# Cuento la cantidad de líderes por sector y géenero
plantel <- plantel %>% 
  rename(division = `Unidad de Negocio`, 
         lider = Líder, 
         sexo = Género, 
         sector = Sector, 
         pais = País) %>% 
  filter(lider == "true") %>% 
  group_by(pais, division, sector, lider, sexo) %>% 
  tally() %>% 
  ungroup()

# Pivoteo el dataset a un dataset ancho
plantel <- plantel %>% 
  pivot_wider(.,
              names_from = sexo,
              values_from = n)

# Reemplaza los NA con un 0
plantel[is.na(plantel)] <- 0


# Calculo porcentaje de líderes hombres
plantel %>% 
  mutate(prop_lider_hombre = if_else(Femenino == 0, 1, Masculino / (Masculino +Femenino))) %>% 
  select(-lider)

