
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

# Asegurarase de que no hay datos nulos (no me lo piden pero buena practica)
colSums(is.na(datos))
sum(is.na(datos))

#### Ceros estructurales ####

# clientes con ticket promedio igual a 0
sum(datos$ticket_promedio == 0)

# proporcion
mean(datos$ticket_promedio == 0)

# porcentaje
mean(datos$ticket_promedio == 0) * 100

# Verificar relación con estado_t1
table(
  Ticket_cero = datos$ticket_promedio == 0,
  Estado_T1 = datos$estado_t1
)