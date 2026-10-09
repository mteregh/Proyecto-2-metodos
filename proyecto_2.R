
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
write.csv(datos, "datos_muestra_800.csv", row.names = FALSE)

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

###### Matriz de correlacion ######

matriz_cor <- cor(variables_continuas)

round(matriz_cor, 2)

########### Parte 1 ###########

# estandarización de las variables continuas
datos_norm <- scale(variables_continuas)

colMeans(datos_norm)
apply(datos_norm, 2, sd)

# matriz de distancias euclideas sobre variables estandarizadas
dist_matriz <- dist(datos_norm, method = "euclidean")


# COMENTAR DESPUES 
# revisar una parte de la matriz
# round(as.matrix(dist_matriz)[1:6, 1:6], 2)

library(cluster)

# Jerárquico: dos o más enlaces
hc_ward <- hclust(dist_matriz, method = "ward.D2")
hc_comp <- hclust(dist_matriz, method = "complete")
hc_aver <- hclust(dist_matriz, method = "average")

# Correlación cofenética (qué tan bien cada enlace respeta las distancias)
cor(dist_matriz, cophenetic(hc_ward))
cor(dist_matriz, cophenetic(hc_comp))
cor(dist_matriz, cophenetic(hc_aver))

# Número de conglomerados en k-medias
k_max <- 8

# Codo
wss <- sapply(1:k_max, function(k) {
  set.seed(25)
  kmeans(datos_norm, centers = k, nstart = 25)$tot.withinss
})
plot(1:k_max, wss, type = "b",
     xlab = "Número de conglomerados (k)",
     ylab = "Suma de cuadrados intra-grupo")

# Silueta
sil <- sapply(2:k_max, function(k) {
  set.seed(25)
  km <- kmeans(datos_norm, centers = k, nstart = 25)
  mean(silhouette(km$cluster, dist_matriz)[, 3])
})
plot(2:k_max, sil, type = "b",
     xlab = "Número de conglomerados (k)",
     ylab = "Silueta promedio")
round(sil, 3)

# Estadístico gap
set.seed(25)
gap <- clusGap(datos_norm, FUN = kmeans, nstart = 25,
               K.max = k_max, B = 50)
plot(gap, main = "Estadístico gap")
print(gap, method = "firstSEmax")

# ---- Elige k mirando lo anterior y cámbialo aquí ----
k_final <- 4

# Dendrograma con el corte elegido
plot(hc_ward, labels = FALSE, hang = -1,
     main = "Dendrograma (Ward)", xlab = "", sub = "")
rect.hclust(hc_ward, k = k_final, border = 2:(k_final + 1))

h <- rev(hc_ward$height)
abline(h = mean(c(h[3], h[4])), lty = 2)

grupo_hc <- cutree(hc_ward, k = k_final)

# K-medias final
set.seed(25)
km <- kmeans(datos_norm, centers = k_final, nstart = 25)
datos$cluster_km <- km$cluster

# Comparación jerárquico vs k-medias
table(Ward = grupo_hc, Kmedias = km$cluster)

# Centroides en unidades originales
centroides <- aggregate(variables_continuas,
                        by = list(Cluster = km$cluster), FUN = mean)
centroides$n <- as.numeric(table(km$cluster))
centroides[, 2:5] <- round(centroides[, 2:5], 2)
centroides


















########### Parte 2 ###########

library(MASS)
library(e1071)
library(caret)
library(biotools)

niveles <- c("Alto Gasto", "Medio Gasto", "Bajo Gasto", "Inactivo")
vars <- c("dias_ultima_sesion", "pedidos_12m",
          "ticket_promedio", "pct_restaurantes")

df <- datos[, vars]
df$estado_t2 <- factor(datos$estado_t2, levels = niveles)

# Distribución de la clase y clasificador trivial
tab_t2 <- table(df$estado_t2)
round(prop.table(tab_t2) * 100, 2)
clase_mayoritaria <- names(which.max(tab_t2))

# Partición 70/30 estratificada
set.seed(25)
idx <- createDataPartition(df$estado_t2, p = 0.7, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]

pred_trivial <- factor(rep(clase_mayoritaria, nrow(test)), levels = niveles)
confusionMatrix(pred_trivial, test$estado_t2)$overall["Accuracy"]

# Supuestos: homogeneidad de covarianzas (Box's M)
boxM(train[, vars], train$estado_t2)

# Supuestos: normalidad multivariante por clase
# (distancia de Mahalanobis vs chi-cuadrado + Shapiro univariado)
par(mfrow = c(2, 2))
for (cl in niveles) {
  x <- train[train$estado_t2 == cl, vars]
  d2 <- mahalanobis(x, colMeans(x), cov(x))
  qqplot(qchisq(ppoints(nrow(x)), df = length(vars)), d2,
         main = cl, xlab = "Cuantiles chi-cuadrado", ylab = "Distancia^2")
  abline(0, 1)
  print(cl)
  print(round(sapply(x, function(v) shapiro.test(v)$p.value), 4))
}
par(mfrow = c(1, 1))

# Correlaciones dentro de cada clase (supuesto de Naive Bayes)
for (cl in niveles) {
  print(cl)
  print(round(cor(train[train$estado_t2 == cl, vars]), 2))
}

# Ajuste de modelos
mod_lda <- lda(estado_t2 ~ ., data = train)
mod_qda <- qda(estado_t2 ~ ., data = train)
mod_nb  <- naiveBayes(estado_t2 ~ ., data = train)

pred_lda <- predict(mod_lda, test)$class
pred_qda <- predict(mod_qda, test)$class
pred_nb  <- predict(mod_nb, test)

# Evaluación en validación
evaluar <- function(pred, real) {
  cm <- confusionMatrix(pred, real)
  print(cm$table)
  print(round(cm$byClass[, c("Sensitivity", "Pos Pred Value")], 3))
  cat("Accuracy:", round(cm$overall["Accuracy"], 3), "\n")
}

evaluar(pred_lda, test$estado_t2)
evaluar(pred_qda, test$estado_t2)
evaluar(pred_nb,  test$estado_t2)

# Variables que más discriminan
round(mod_lda$means, 2)
round(mod_lda$scaling, 3)
round(mod_lda$svd^2 / sum(mod_lda$svd^2), 3)

# Pregunta obligatoria: cambio de probabilidades a priori
# (ajusta los valores; el orden sigue a "niveles")
priors_actual  <- as.numeric(prop.table(table(train$estado_t2)))
priors_nuevos  <- c(0.15, 0.25, 0.25, 0.35)

mod_lda_p <- lda(estado_t2 ~ ., data = train, prior = priors_nuevos)
pred_lda_p <- predict(mod_lda_p, test)$class
evaluar(pred_lda_p, test$estado_t2)

mod_nb_p <- mod_nb
mod_nb_p$apriori[] <- priors_nuevos
pred_nb_p <- predict(mod_nb_p, test)
evaluar(pred_nb_p, test$estado_t2)

mod_qda_p <- qda(estado_t2 ~ ., data = train, prior = priors_nuevos)
evaluar(predict(mod_qda_p, test)$class, test$estado_t2)

priors_2 <- c(0.20, 0.25, 0.25, 0.30)
mod_qda_p2 <- qda(estado_t2 ~ ., data = train, prior = priors_2)
evaluar(predict(mod_qda_p2, test)$class, test$estado_t2)

resumen <- function(pred, real) {
  cm <- confusionMatrix(pred, real)
  c(Sens_Inactivo = unname(cm$byClass["Class: Inactivo", "Sensitivity"]),
    Prec_Inactivo = unname(cm$byClass["Class: Inactivo", "Pos Pred Value"]),
    Sens_Bajo     = unname(cm$byClass["Class: Bajo Gasto", "Sensitivity"]),
    Accuracy      = unname(cm$overall["Accuracy"]))
}

tabla_priors <- rbind(
  Originales = resumen(pred_qda, test$estado_t2),
  Priors_2   = resumen(predict(mod_qda_p2, test)$class, test$estado_t2),
  Priors_1   = resumen(predict(mod_qda_p, test)$class, test$estado_t2)
)
round(tabla_priors, 3)


















########### Parte 3 ###########

# Cluster vs estado_t2
datos$estado_t2 <- factor(datos$estado_t2, levels = niveles)

tab_cluster <- table(Cluster = datos$cluster_km, Estado_T2 = datos$estado_t2)
tab_cluster
round(prop.table(tab_cluster, 1) * 100, 1)

chi <- chisq.test(tab_cluster)
chi
sqrt(chi$statistic / (sum(tab_cluster) * (min(dim(tab_cluster)) - 1)))  # V de Cramér

# Cluster como predictor adicional (misma partición de la Parte 2)
df2 <- df
df2$cluster <- factor(datos$cluster_km)
train2 <- df2[idx, ]
test2  <- df2[-idx, ]

mod_nb2  <- naiveBayes(estado_t2 ~ ., data = train2)
pred_nb2 <- predict(mod_nb2, test2)

evaluar(pred_nb,  test$estado_t2)    # sin cluster
evaluar(pred_nb2, test2$estado_t2)   # con cluster