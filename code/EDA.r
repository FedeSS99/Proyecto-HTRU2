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
class_htru2 <- data_htru2[, 9]

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
png("HTRU2_pairplots.png", width = 1600, height = 1600)
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

png("HTRU2_histogramas.png", width = 1600, height = 1600)
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

png("HTRU2_gcajas.png", width = 1600, height = 1600)
grid.arrange(gcajas[[1]], gcajas[[2]], gcajas[[3]], gcajas[[4]],
             gcajas[[5]], gcajas[[6]], gcajas[[7]], gcajas[[8]],
             ncol=3)
dev.off()

######################
# Particion de datos #
######################
train.index <- createDataPartition(data_htru2$class, p = .7, list = FALSE) # ya realiza particion estratificada
htru2_train <- data_htru2[ train.index,]
htru2_test  <- data_htru2[-train.index,]

###############
# Modelo Base #
###############

# LDA ? comprobar normalidad? (tambien para QDA?)

# Regresion logistica? (no hace falta comprobar supuestos de normalidad)

############################
# Reduccion de dimensiones #
############################

# PCA
pca_htru2 <- prcomp(data_htru2[, 1:8])
summary(pca_htru2)

png("./images/HTRU2_PCA.png", width = 800, height = 800)
plot(pca_htru2$x[, 1:2], col = colors, pch = 16, main = "PCA")
dev.off()

# FA
fa_htru2 <- factanal(data_htru2[, 1:8], factors = 2, scores = "Bartlett")
summary(fa_htru2)

png("./images/HTRU2_FA.png", width = 800, height = 800)
plot(fa_htru2$scores[, 1:2], col = colors, pch = 16, main = "Factor analysis")
dev.off()
