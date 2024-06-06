##################
# Carga de datos #
##################
data_htru2 <- read.csv(paste0(rute_data, "HTRU_2.csv"), header = FALSE)
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
train.index <- createDataPartition(data_htru2$Class, p = .7, list = FALSE) # ya realiza particion estratificada
htru2_train <- data_htru2[ train.index,]
colors_train <- character(nrow(htru2_train))
colors_train[htru2_train$Class == 0] <- "blue"
colors_train[htru2_train$Class == 1] <- "red"

htru2_test  <- data_htru2[-train.index,]
colors_test <- character(nrow(htru2_test))
colors_test[htru2_test$Class == 0] <- "blue"
colors_test[htru2_test$Class == 1] <- "red"
