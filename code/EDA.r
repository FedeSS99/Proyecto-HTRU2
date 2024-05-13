#################
# Configuracion #
#################

# Librerias
library(MASS)
library(psych)
library(gridExtra)
library(ggplot2)
library(CCA)
library(caret)

##################
# Carga de datos #
##################
data_htru2 <- read.csv("./data/HTRU_2.csv", header = FALSE)
colnames(data_htru2) <- c("Mean IP", "Standard deviation IP",
                          "Excess kurtosis IP", "Skewness IP",
                          "Mean DM-SNR", "Standard deviation DM-SNR",
                          "Excess kurtosis DM-SNR", "Skewness DM-SNR",
                          "Class")

# Estandarizacion de variables (inputs)
data_htru2[, 1:8] <- scale(data_htru2[, 1:8])
head(data_htru2)

# Separamos las propiedades y las clases de todas las observaciones (Variable objetivo)
# Se define como factor para que los modelos sepan que es una variable categorica
class_htru2 <- as.factor(data_htru2[, 9])
data_htru2[, 9] <- as.factor(data_htru2[, 9])

# Generamos un vector de colores asignados dependiendo de si
# 0 - No es un pulsar
# 1 - Si es un pulsar
colors <- character(nrow(data_htru2))
colors[class_htru2 == 0] <- "blue"
colors[class_htru2 == 1] <- "red"

#########################
# Analisis exploratorio #
#########################

# Graficamos por pares
png("./images/HTRU2_pairplots.png", width = 1600, height = 1600)
pairs.panels(data_htru2[, 1:8],
  ellipses = FALSE,
  smooth = FALSE,
  method = "pearson",
  hist.col = "#00AFBB",
  density = TRUE,
  bg = colors,
  pch = 21
)
dev.off()

# Histogramas: comparacion visual de distribuciones
histogramas<-list()
for (i in 1:8){
  x = data_htru2[,i]
  histogramas[[i]] <- ggplot(data.frame(x),
  aes(x = x, fill = colors, colour = colors)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  guides(fill = guide_legend(title = "Pulsar"), colour = guide_legend(title = "Pulsar")) +
  scale_fill_manual(values=c("blue", "red"), labels = c("0", "1")) + scale_color_manual(values=c("blue", "red"), labels = c("0", "1")) +
  ggtitle(colnames(data_htru2[i])) +
  xlab(colnames(data_htru2[i])) +
  ylab("Frecuencia")
}

png("./images/HTRU2_histogramas.png", width = 1600, height = 1600)
grid.arrange(histogramas[[1]], histogramas[[2]], histogramas[[3]], histogramas[[4]],
             histogramas[[5]], histogramas[[6]], histogramas[[7]], histogramas[[8]],
             ncol=3)
dev.off()

# Graficas de caja: deteccion de outliers
gcajas<-list()
for (i in 1:8){
  x = data_htru2[,i]
  gcajas[[i]] <- ggplot(data.frame(x),
  aes(y = x, x = data_htru2[,9], fill = colors, colour = colors)) +
  geom_boxplot(alpha = 0.5, position = "identity") +
  scale_fill_manual(values=c("blue", "red"), labels = c("0", "1")) + scale_color_manual(values=c("blue", "red"), labels = c("0", "1")) +
  ggtitle(colnames(data_htru2[i])) +
  ylab(colnames(data_htru2[i])) +
  xlab('¿Es pulsar?')
}

png("./images/HTRU2_gcajas.png", width = 1600, height = 1600)
grid.arrange(gcajas[[1]], gcajas[[2]], gcajas[[3]], gcajas[[4]],
             gcajas[[5]], gcajas[[6]], gcajas[[7]], gcajas[[8]],
             ncol=3)
dev.off()

######################
# Particion de datos #
######################
train.index <- createDataPartition(data_htru2$Class, p = .7, list = FALSE) # ya realiza particion estratificada
htru2_train <- data_htru2[ train.index,]
colors_train <- character(nrow(htru2_train))
colors_train[htru2_train$Class == 0] <- "blue"
colors_train[htru2_train$Class == 1] <- "red"

htru2_test  <- data_htru2[-train.index,]
colors_test <- character(nrow(htru2_test))
colors_test[htru2_test$Class == 0] <- "blue"
colors_test[htru2_test$Class == 1] <- "red"

###############
# Modelo Base #
###############

# Entrenamiento del modelo
mb <- train(Class ~ ., data = htru2_train, method = "glm", family = "binomial")

# Obtencion de las prediciones
mb_pred <- predict(mb, newdata = htru2_test)

# Obtencion de las metricas de evaluacion
mb_res <- confusionMatrix(mb_pred, htru2_test$Class, positive = "1")
print(mb_res)

############################
# Reduccion de dimensiones #
############################

# Prueba de correlacion de Spearman: correlacion no lineal
sp_13 <- cor.test(data_htru2[ , 1], data_htru2[ , 3],  method = "spearman")
sp_34 <- cor.test(data_htru2[ , 3], data_htru2[ , 4],  method = "spearman")
sp_56 <- cor.test(data_htru2[ , 5], data_htru2[ , 6],  method = "spearman")
sp_57 <- cor.test(data_htru2[ , 5], data_htru2[ , 7],  method = "spearman")
sp_78 <- cor.test(data_htru2[ , 7], data_htru2[ , 8],  method = "spearman")

# PCA
pca_htru2 <- prcomp(htru2_train[, 1:8])
print(summary(pca_htru2))

# Proporcion de varianza explicada
aux = pca_htru2$sdev^2 / sum(pca_htru2$sdev^2)
aux = data.frame(aux, c(1:8))
colnames(aux)[2] ="NO_COMP"

png("./images/HTRU2_PCA_Scree.png", width = 800, height = 800)
print(ggplot(aux, aes(x = NO_COMP, y = aux)) +
  geom_line() +
  xlab("Número de componentes principales") +
  ylab("Varianza explicada") +
  ggtitle("Gráfica de 'codo' (Scree plot)") +
  ylim(0, 1))
dev.off()

# Visualizacion de los datos en las primeras dos componentes principales
png("./images/HTRU2_PCA.png", width = 800, height = 800)
plot(pca_htru2$x[, 1:2], col = colors_train, pch = 16, main = "PCA")
dev.off()

# PCA de los datos de entrenamiento
pca_htru2_train <- data.frame(pca_htru2$x[, 1:3],htru2_train$Class)
colnames(pca_htru2_train)[4] ="Class"


# Se obtienen las proyecciones en PCA de los datos de prueba
pca_htru2_test <- predict(pca_htru2, newdata = htru2_test[1:8])
pca_htru2_test <- data.frame(pca_htru2_test[, 1:3],htru2_test$Class)
colnames(pca_htru2_test)[4] ="Class"

# FA
fa_htru2 <- factanal(data_htru2[, 1:8], factors = 2, scores = "Bartlett")
summary(fa_htru2)

png("HTRU2_FA.png", width = 800, height = 800)
plot(fa_htru2$scores[, 1:2], col = colors, pch = 16, main = "Factor analysis")
dev.off()

########################
# Modelos alternativos #
########################

# LDA y QDA?

# Modelo base con PCA #

# Se entrena el modelo con los datos transformados por PCA
mb_pca <- train(Class ~ ., data = pca_htru2_train, method = "glm", family = "binomial")

# Se obtienen las predicciones de los datos de prueba
mb_pca_pred <- predict(mb_pca, newdata = pca_htru2_test[, 1:3])

# Obtencion de las metricas de evaluacion
mb_pca_res <- confusionMatrix(mb_pca_pred, pca_htru2_test$Class, positive = "1")
print(mb_pca_res)


# Ponerse creativos
