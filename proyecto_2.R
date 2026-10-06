
# Proyecto 2 
# Grupo 25

###### Cargar datos ######

datos_completos <- read.csv("grupo_5_clientes.csv")


dim(datos_completos)
head(datos_completos)
str(datos_completos)

### Muestra de 800 clientes ##

set.seed(25)

indices_muestra <- sample(
  1:nrow(datos_completos),
  size = 800,
  replace = FALSE
)

# Base definitiva para todo el análisis
datos <- datos_completos[indices_muestra, ]

# Comprobaciones COMENTAR DESPUES 
dim(datos)
nrow(datos)
ncol(datos)
length(unique(datos$id_cliente))

# Asegurarase de que no hay valores perdidos 
# esto es para ver que no hay que eliminar observaciones por datos faltantes 
colSums(is.na(datos))
sum(is.na(datos))

