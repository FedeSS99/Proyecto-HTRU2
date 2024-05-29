#################
# Configuracion #
#################

# Librerias
library(MASS)
library(caret)
library(klaR)
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
# Particion de datos#
######################
train.index <- createDataPartition(data_htru2$Class, p = .7, list = FALSE)
htru2_test  <- data_htru2[-train.index,]

######################
# Analisis de factores #
#####################$
fa_htru <- factanal(data_htru2[, 1:8], factors = 2, scores = "Bartlett")
fa_htru_scores <- fa_htru$scores

fa_htru_train <- fa_htru_scores[train.index, ]
fa_htru_test <- fa_htru_scores[-train.index, ]
######################
# LDA #
#####################$
lda_htru_factors <- lda(x = fa_htru_scores,
                         prior = c(1, 1) / 2.0,
                         subset = train.index,
                         grouping = class_htru2)

lda_predict_train <- predict(object = lda_htru_factors, newdata = fa_htru_test)
confusionmatrix_lda <- table(htru2_test$Class, lda_predict_train$class, dnn = c("Clase real", "Clase predicha"))
metrics_lda <- getmetrics_confmatrix(confusionmatrix_lda)
print(confusionmatrix_lda)
print_metrics(metrics_lda)

partimat(fa_htru_scores, grouping = class_htru2, methods = "lda")

######################
# QDA #
#####################$
qda_htru_factors <- qda(x = fa_htru_scores,
                         prior = c(1, 1) / 2.0,
                         subset = train.index,
                         grouping = class_htru2)

qda_predict_train <- predict(object = qda_htru_factors, newdata = fa_htru_test)
confusionmatrix_qda <- table(htru2_test$Class, qda_predict_train$class, dnn = c("Clase real", "Clase predicha"))
metrics_qda <- getmetrics_confmatrix(confusionmatrix_qda)
print(confusionmatrix_qda)
print_metrics(metrics_qda)

partimat(fa_htru_scores, grouping = class_htru2, methods = "qda")
