data_htru2 <- read.csv("./htru2/HTRU_2.csv", header = FALSE)
colnames(data_htru2) <- c("Mean IP", "Standard deviation IP",
                          "Excess kurtosis IP", "Skewness IP",
                          "Mean DM-SNR", "Standard deviation DM-SNR",
                          "Excess kurtosis DM-SNR", "Skewness DM-SNR",
                          "Class")
head(data_htru2)

# Separamos las propiedades y las clases de todas las observaciones
Class_htru2 <- data_htru2[,9]


# Generamos un vector de colores asignados dependiendo de si
# 0 - No es un pulsar
# 1 - Si es un pulsar
colors <- character(nrow(data_htru2))
colors[Class_htru2 == 0] <- "blue" 
colors[Class_htru2 == 1] <- "red" 

# Graficamos por pares
library(psych)
pairs.panels(data_htru2[,1:8],
  ellipses = FALSE,
  smooth = FALSE,
  method = "pearson",
  hist.col = "#00AFBB",
  density = TRUE,
  bg = colors,
  pch = 21
)
