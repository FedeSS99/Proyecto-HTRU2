#################
# Configuracion #
#################

# Librerias
library(MASS)
library(caret)
source("./code/ConfusionMatrixMetrics.R")

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

######################
# Particion de datos #
######################
train.index <- createDataPartition(data_htru2$Class, p = .7, list = FALSE)
htru2_train <- data_htru2[train.index, ]
colors_train <- character(nrow(htru2_train))
colors_train[htru2_train$Class == 0] <- "blue"
colors_train[htru2_train$Class == 1] <- "red"

htru2_test  <- data_htru2[-train.index,]
colors_test <- character(nrow(htru2_test))
colors_test[htru2_test$Class == 0] <- "blue"
colors_test[htru2_test$Class == 1] <- "red"

######################
# LDA #
#####################$
lda_htru_original <- lda(x = data_htru2[, 1:8], 
                         prior = c(1, 1) / 2.0,
                         subset = train.index,
                         grouping = data_htru2$Class)
lda_predict_train <- predict(object = lda_htru_original, newdata = htru2_test[, 1:8])
confusionmatrix_lda <- table(htru2_test$Class, lda_predict_train$class, dnn = c("Clase real", "Clase predicha"))
getmetrics_confmatrix(confusionmatrix_lda)

partimat(data_htru2[, 1:8], grouping = data_htru2$Class, methods = "lda", plot.matrix = TRUE)

######################
# QDA #
#####################$
qda_htru_original <- qda(x = data_htru2[, 1:8], 
                         prior = c(1, 1) / 2.0,
                         subset = train.index,
                         grouping = data_htru2$Class)
qda_predict_train <- predict(object = qda_htru_original, newdata = htru2_test[, 1:8])
confusionmatrix_qda <- table(htru2_test$Class, qda_predict_train$class, dnn = c("Clase real", "Clase predicha"))
getmetrics_confmatrix(confusionmatrix_qda)

partimat(data_htru2[, 1:8], grouping = data_htru2$Class, methods = "qda", plot.matrix = TRUE)
