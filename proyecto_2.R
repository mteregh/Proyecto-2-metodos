
# Proyecto 2 
# Grupo 25

####### Cargar datos #######

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

####### Ceros estructurales #######

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

###### Truncamiento de dias ultima sesion ######

# valor máximo observado
max(datos$dias_ultima_sesion)

# cantidad de clientes con valor igual a 180
sum(datos$dias_ultima_sesion == 180)

# proporción
mean(datos$dias_ultima_sesion == 180)

# porcentaje
mean(datos$dias_ultima_sesion == 180) * 100

###### Estadisticos descriptivos ######

# cuatro variables continuas
variables_continuas <- datos[, c(
  "dias_ultima_sesion",
  "pedidos_12m",
  "ticket_promedio",
  "pct_restaurantes"
)]

# mínimo, cuartiles, mediana, media y máximo
summary(variables_continuas)

# Tabla de descriptivos
descriptivos <- data.frame(
  Media = sapply(variables_continuas, mean),
  Mediana = sapply(variables_continuas, median),
  Desv_Estandar = sapply(variables_continuas, sd),
  Minimo = sapply(variables_continuas, min),
  Maximo = sapply(variables_continuas, max)
)

# tabla de descriptivos redondeada a 2 decimales
# media, mediana, desviación estándar, mínimo y máximo 
round(descriptivos, 2)

###### Distribucion estados ######

# Frecuencias absolutas
table(datos$estado_t1)
table(datos$estado_t2)

# Porcentajes
round(prop.table(table(datos$estado_t1)) * 100, 2)
round(prop.table(table(datos$estado_t2)) * 100, 2)


###### Graficos descriptivos ######

par(mfrow = c(2, 2))

hist(datos$dias_ultima_sesion,
     main = "Última sesión",
     xlab = "Días",
     ylab = "Frecuencia")

hist(datos$pedidos_12m,
     main = "Pedidos 12 meses",
     xlab = "Número de pedidos",
     ylab = "Frecuencia")

hist(datos$ticket_promedio,
     main = "Ticket promedio",
     xlab = "Miles de pesos",
     ylab = "Frecuencia")

hist(datos$pct_restaurantes,
     main = "Gasto en restaurantes",
     xlab = "Proporción",
     ylab = "Frecuencia")

par(mfrow = c(1, 1))

