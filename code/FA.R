########################
# Analisis de factores #
########################

fa_htru <- factanal(data_htru2[train.index, 1:8], factors = 4, scores = "regression")
fa_htru_train <- fa_htru$scores

explained_var <- cumsum(colSums(fa_htru$loadings^2)/sum(diag(fa_htru$correlation)))

# Visualizacion de la varianza explicada acumulada por las componentes
png(paste0(rute_img, "HTRU2_FA_ExpVar.png"), width = 800, height = 800)
plot(explained_var, type = "b", xlab = "Component", ylab = "Eigenvalue", main = "Varianza explicada por las componentes")
dev.off()

# Seleccionamos tres factores dada la curva de varianza explicada
fa_htru <- factanal(data_htru2[train.index, 1:8], factors = 3, scores = "Bartlett", rotation = "varimax")
fa_htru_train <- fa_htru$scores

fa_htru_train <- data.frame(fa_htru_train[, 1:3], htru2_train$Class)
colnames(fa_htru_train)[4] ="Class"


fa_htru_test <- (as.matrix(htru2_test[,1:8]) %*% t(solve(fa_htru$correlation))) %*% fa_htru$loadings[,]

# Visualizacion de las dos primeras dimensiones dadas por los factores
png(paste0(rute_img, "HTRU2_FA.png"), width = 800, height = 800)
plot(x = fa_htru_train[, 1], y = fa_htru_train[, 2], col = colors_train, pch = 16, 
     xlab = 'Primer Factor', ylab = 'Segundo Factor', main = "Análisis de Factores (FA)")
dev.off()

# Cargas en los factores a emplear
print(fa_htru$loadings)

######
# LR #
######
fa_mb <- train(Class ~ ., data = fa_htru_train, method = "glm", family = "binomial")

fa_mb_pred <- predict(fa_mb, newdata = fa_htru_test)
fa_mb_res <- confusionMatrix(fa_mb_pred, htru2_test$Class, positive = "1")
print(fa_mb_res)

######################
# LDA #
######################
lda_htru_factors <- lda(x = fa_htru_train[1:3],
                        prior = c(1, 1) / 2.0,
                        grouping = class_htru2[train.index])

lda_predict_train <- predict(object = lda_htru_factors, newdata = fa_htru_test)
lda_fa_res <- confusionMatrix(lda_predict_train$class, htru2_test$Class, positive = "1")
print(lda_fa_res)

png(paste0(rute_img, "HTRU2_FA_LDA.png"), width = 800, height = 800)
partimat(fa_htru_train[1:3], grouping = class_htru2[train.index], nplots.hor = 2, main = 'LDA', methods = "lda")
dev.off()



######################
# QDA #
######################
qda_htru_factors <- qda(x = fa_htru_train[1:3],
                        prior = c(1, 1) / 2.0,
                        grouping = class_htru2[train.index])

qda_predict_train <- predict(object = qda_htru_factors, newdata = fa_htru_test)
qda_fa_res <- confusionMatrix(qda_predict_train$class, htru2_test$Class, positive = "1")
print(qda_fa_res)

png(paste0(rute_img, "HTRU2_FA_QDA.png"), width = 800, height = 800)
partimat(fa_htru_train[1:3], grouping = class_htru2[train.index],nplots.hor = 2, main = 'QDA', methods = "qda")
dev.off()

