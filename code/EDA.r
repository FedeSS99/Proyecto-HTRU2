library(MASS)
library(scatterplot3d)
library(psych)


data_htru2 <- read.csv("./data/HTRU_2.csv", header = FALSE)
colnames(data_htru2) <- c("Mean IP", "Standard deviation IP",
                          "Excess kurtosis IP", "Skewness IP",
                          "Mean DM-SNR", "Standard deviation DM-SNR",
                          "Excess kurtosis DM-SNR", "Skewness DM-SNR",
                          "Class")
data_htru2[, 1:8] <- scale(data_htru2[, 1:8])
head(data_htru2)

# Separamos las propiedades y las clases de todas las observaciones
class_htru2 <- data_htru2[, 9]

# Generamos un vector de colores asignados dependiendo de si
# 0 - No es un pulsar
# 1 - Si es un pulsar
colors <- character(nrow(data_htru2))
colors[class_htru2 == 0] <- "blue"
colors[class_htru2 == 1] <- "red"

# Graficamos por pares
png("./images/HTRU2_pairplots.png", width = 1600, height = 800)
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

# PCA
pca_htru2 <- prcomp(data_htru2[, 1:8])
summary(pca_htru2)

plot(pca_htru2$x[, 1:2], col = colors)
scatterplot3d(pca_htru2$x[, 1:3], color = colors, pch = 16)

# FA
fa_htru2 <- factanal(data_htru2[, 1:8], factors = 3, scores = "Bartlett")
summary(fa_htru2)

plot(fa_htru2$scores[, 1:2], col = colors)
scatterplot3d(fa_htru2$scores[, 1:3], color = colors, pch = 16)
