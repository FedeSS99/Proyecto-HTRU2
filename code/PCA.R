#######
# PCA #
#######
pca_htru2 <- prcomp(htru2_train[, 1:8])
print(summary(pca_htru2))

# Proporcion de varianza explicada
aux = pca_htru2$sdev^2 / sum(pca_htru2$sdev^2)
aux = data.frame(aux, c(1:8))
colnames(aux)[2] ="NO_COMP"

png(paste0(rute_img, "HTRU2_PCA_Scree.png"), width = 800, height = 800)
print(ggplot(aux, aes(x = NO_COMP, y = aux)) +
        geom_line() +
        xlab("Número de componentes principales") +
        ylab("Varianza explicada") +
        ggtitle("Gráfica de 'codo' (Scree plot)") +
        ylim(0, 1))
dev.off()

# Visualizacion de los datos en las primeras dos componentes principales
png(paste0(rute_img, "HTRU2_PCA.png"), width = 800, height = 800)
plot(pca_htru2$x[, 1:2], col = colors_train, pch = 16, main = "PCA")
dev.off()

# PCA de los datos de entrenamiento
pca_htru2_train <- data.frame(pca_htru2$x[, 1:3],htru2_train$Class)
colnames(pca_htru2_train)[4] ="Class"


# Se obtienen las proyecciones en PCA de los datos de prueba
pca_htru2_test <- predict(pca_htru2, newdata = htru2_test[1:8])
pca_htru2_test <- data.frame(pca_htru2_test[, 1:3],htru2_test$Class)
colnames(pca_htru2_test)[4] ="Class"

# Modelo base con PCA #

# Se entrena el modelo con los datos transformados por PCA
mb_pca <- train(Class ~ ., data = pca_htru2_train, method = "glm", family = "binomial")

# Se obtienen las predicciones de los datos de prueba
mb_pca_pred <- predict(mb_pca, newdata = pca_htru2_test[, 1:3])

# Obtencion de las metricas de evaluacion
mb_pca_res <- confusionMatrix(mb_pca_pred, pca_htru2_test$Class, positive = "1")
print(mb_pca_res)

######################
# LDA #
######################
lda_htru_factors <- lda(x = pca_htru2_train[1:3],
                        prior = c(1, 1) / 2.0,

                        grouping = pca_htru2_train$Class)

lda_predict_train <- predict(object = lda_htru_factors, newdata = pca_htru2_test[, 1:3])
lda_pca_res <- confusionMatrix(lda_predict_train$class, pca_htru2_test$Class, positive = "1")
print(lda_pca_res)

png(paste0(rute_img, "HTRU2_PCA_LDA.png"), width = 800, height = 800)
partimat(pca_htru2_train[1:3], grouping = class_htru2[train.index], nplots.hor = 2, main = 'LDA', methods = "lda")
dev.off()


######################
# QDA #
######################
qda_htru_factors <- qda(x = pca_htru2_train[1:3],
                        prior = c(1, 1) / 2.0,

                        grouping =pca_htru2_train$Class)

qda_predict_train <- predict(object = qda_htru_factors, newdata = pca_htru2_test[, 1:3])
qda_pca_res <- confusionMatrix(qda_predict_train$class, pca_htru2_test$Class, positive = "1")
print(qda_pca_res)

png(paste0(rute_img, "HTRU2_PCA_QDA.png"), width = 800, height = 800)
partimat(pca_htru2_train[1:3], grouping = class_htru2[train.index], nplots.hor = 2, main = 'QDA', methods = "qda")
dev.off()
