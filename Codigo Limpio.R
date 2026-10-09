# Proyecto Computacional 2 - Métodos Estadísticos para la Gestión
# Grupo 25 - Base asignada: grupo_5_clientes.csv

####### Librerías y semilla #######

library(cluster)
library(MASS)
library(e1071)
library(caret)
library(biotools)

set.seed(25)

####### Datos y muestra de 800 clientes #######

datos_completos <- read.csv("grupo_5_clientes.csv")

indices_muestra <- sample(1:nrow(datos_completos), size = 800, replace = FALSE)
datos <- datos_completos[indices_muestra, ]

# Base utilizada en el análisis (se entrega junto al informe)
write.csv(datos, "datos_muestra_800.csv", row.names = FALSE)

####### Ceros estructurales #######

sum(datos$ticket_promedio == 0)
mean(datos$ticket_promedio == 0) * 100

# Los ceros coinciden con los clientes Inactivos del T1
table(Ticket_cero = datos$ticket_promedio == 0, Estado_T1 = datos$estado_t1)

####### Truncamiento de dias_ultima_sesion #######

max(datos$dias_ultima_sesion)
sum(datos$dias_ultima_sesion == 180)

####### Estadísticos descriptivos #######

variables_continuas <- datos[, c("dias_ultima_sesion", "pedidos_12m",
                                 "ticket_promedio", "pct_restaurantes")]

summary(variables_continuas)

descriptivos <- data.frame(
  Media = sapply(variables_continuas, mean),
  Mediana = sapply(variables_continuas, median),
  Desv_Estandar = sapply(variables_continuas, sd),
  Minimo = sapply(variables_continuas, min),
  Maximo = sapply(variables_continuas, max)
)
round(descriptivos, 2)

# Distribución de los estados
table(datos$estado_t1)
table(datos$estado_t2)
round(prop.table(table(datos$estado_t1)) * 100, 2)
round(prop.table(table(datos$estado_t2)) * 100, 2)

####### Gráficos descriptivos #######

par(mfrow = c(2, 2))
hist(datos$dias_ultima_sesion, main = "Última sesión",
     xlab = "Días", ylab = "Frecuencia")
hist(datos$pedidos_12m, main = "Pedidos 12 meses",
     xlab = "Número de pedidos", ylab = "Frecuencia")
hist(datos$ticket_promedio, main = "Ticket promedio",
     xlab = "Miles de pesos", ylab = "Frecuencia")
hist(datos$pct_restaurantes, main = "Gasto en restaurantes",
     xlab = "Proporción", ylab = "Frecuencia")
par(mfrow = c(1, 1))

####### Matriz de correlación #######

round(cor(variables_continuas), 2)

########### PARTE 1: Análisis de conglomerados ###########

# Estandarización y distancia euclidiana
datos_norm <- scale(variables_continuas)
dist_matriz <- dist(datos_norm, method = "euclidean")

# Clustering jerárquico con tres enlaces
hc_ward <- hclust(dist_matriz, method = "ward.D2")
hc_comp <- hclust(dist_matriz, method = "complete")
hc_aver <- hclust(dist_matriz, method = "average")

# Correlación cofenética de cada enlace
cor(dist_matriz, cophenetic(hc_ward))
cor(dist_matriz, cophenetic(hc_comp))
cor(dist_matriz, cophenetic(hc_aver))

# Criterios para el número de conglomerados
k_max <- 8

# Codo
wss <- sapply(1:k_max, function(k) {
  set.seed(25)
  kmeans(datos_norm, centers = k, nstart = 25)$tot.withinss
})

# Silueta
sil <- sapply(2:k_max, function(k) {
  set.seed(25)
  km_k <- kmeans(datos_norm, centers = k, nstart = 25)
  mean(silhouette(km_k$cluster, dist_matriz)[, 3])
})
round(sil, 3)

# Estadístico gap
set.seed(25)
gap <- clusGap(datos_norm, FUN = kmeans, nstart = 25, K.max = k_max, B = 50)
print(gap, method = "firstSEmax")

# Los tres criterios en una sola figura
par(mfrow = c(1, 3))
plot(1:k_max, wss, type = "b", main = "Método del codo",
     xlab = "Número de conglomerados (k)",
     ylab = "Suma de cuadrados intra-grupo")
plot(2:k_max, sil, type = "b", main = "Silueta promedio",
     xlab = "Número de conglomerados (k)", ylab = "Silueta promedio")
plot(gap, main = "Estadístico gap",
     xlab = "Número de conglomerados (k)", ylab = "Estadístico gap")
par(mfrow = c(1, 1))

# Número de conglomerados elegido
k_final <- 4

# Dendrograma con el corte elegido
plot(hc_ward, labels = FALSE, hang = -1,
     main = "Dendrograma (Ward)", xlab = "Clientes (n = 800)",
     ylab = "Altura (distancia de Ward)", sub = "")
rect.hclust(hc_ward, k = k_final, border = 2:(k_final + 1))
h <- rev(hc_ward$height)
abline(h = mean(c(h[k_final - 1], h[k_final])), lty = 2)

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

# Nombres de los segmentos (según el orden de km$cluster; revisar si cambia)
nombres_cluster <- c("Regulares", "Dormidos", "Ocasionales", "Élite")

plot(datos$pedidos_12m, datos$ticket_promedio, col = km$cluster, pch = 19,
     main = "Segmentos de clientes",
     xlab = "Pedidos en 12 meses", ylab = "Ticket promedio (miles de pesos)")
legend("topleft", legend = nombres_cluster, col = 1:4, pch = 19, bty = "n")

########### PARTE 2: Clasificación supervisada ###########

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

# Supuesto: homogeneidad de covarianzas (Box's M)
boxM(train[, vars], train$estado_t2)

# Supuesto: normalidad multivariante por clase
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

# Verificación manual de la exactitud (QDA)
tabla_qda <- table(Predicho = pred_qda, Real = test$estado_t2)
sum(diag(tabla_qda)) / sum(tabla_qda)

# Variables que más discriminan (medias por clase en unidades originales)
round(mod_lda$means, 2)

# Pregunta obligatoria: cambio de probabilidades a priori
# (el orden sigue a "niveles": Alto, Medio, Bajo, Inactivo)
priors_actual <- as.numeric(prop.table(table(train$estado_t2)))
priors_nuevos <- c(0.15, 0.25, 0.25, 0.35)
priors_2      <- c(0.20, 0.25, 0.25, 0.30)

mod_lda_p <- lda(estado_t2 ~ ., data = train, prior = priors_nuevos)
evaluar(predict(mod_lda_p, test)$class, test$estado_t2)

mod_nb_p <- mod_nb
mod_nb_p$apriori[] <- priors_nuevos
evaluar(predict(mod_nb_p, test), test$estado_t2)

mod_qda_p <- qda(estado_t2 ~ ., data = train, prior = priors_nuevos)
evaluar(predict(mod_qda_p, test)$class, test$estado_t2)

mod_qda_p2 <- qda(estado_t2 ~ ., data = train, prior = priors_2)
evaluar(predict(mod_qda_p2, test)$class, test$estado_t2)

# Resumen comparativo de priors (QDA)
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

########### PARTE 3: Integración ###########

datos$estado_t2 <- factor(datos$estado_t2, levels = niveles)

# Conglomerado vs estado_t2
tab_cluster <- table(Cluster = datos$cluster_km, Estado_T2 = datos$estado_t2)
tab_cluster
round(prop.table(tab_cluster, 1) * 100, 1)

chi <- chisq.test(tab_cluster)
chi
sqrt(chi$statistic / (sum(tab_cluster) * (min(dim(tab_cluster)) - 1)))  # V de Cramér

barplot(t(prop.table(tab_cluster, 1) * 100),
        names.arg = nombres_cluster,
        col = c("#2c7fb8", "#7fcdbb", "#fdae61", "#d7191c"),
        xlab = "Conglomerado", ylab = "% de clientes",
        xlim = c(0, 6.5), legend.text = TRUE,
        args.legend = list(x = "right", bty = "n"))

# Exactitud de asignar a cada cluster su clase más frecuente
sum(apply(tab_cluster, 1, max)) / sum(tab_cluster)

# Conglomerado como predictor adicional (misma partición de la Parte 2)
df2 <- df
df2$cluster <- factor(datos$cluster_km)
train2 <- df2[idx, ]
test2  <- df2[-idx, ]

mod_nb2  <- naiveBayes(estado_t2 ~ ., data = train2)
pred_nb2 <- predict(mod_nb2, test2)

evaluar(pred_nb,  test$estado_t2)    # sin conglomerado
evaluar(pred_nb2, test2$estado_t2)   # con conglomerado

mean(pred_nb  == test$estado_t2)
mean(pred_nb2 == test2$estado_t2)

####### Información de la sesión (reproducibilidad) #######
sessionInfo()