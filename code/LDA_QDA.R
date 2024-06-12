######################
# LDA #
#####################$
lda_htru_original <- lda(x = htru2_train[, 1:8],
                         prior = c(1, 1) / 2.0,

                         grouping = htru2_train$Class)
lda_predict_train <- predict(object = lda_htru_original, newdata = htru2_test[, 1:8])
lda_res <- confusionMatrix(lda_predict_train$class, htru2_test$Class, positive = "1")
print(lda_res)

png(paste0(rute_img, "HTRU2_LDA.png"), width = 800, height = 800)
partimat(htru2_train[1:8], grouping = class_htru2[train.index], nplots.hor = 2, nplots.vert = 2, main = 'LDA', methods = "lda")
dev.off()


######################
# QDA #
#####################$
qda_htru_original <- qda(x = htru2_train[, 1:8],
                         prior = c(1, 1) / 2.0,

                         grouping = htru2_train$Class)
qda_predict_train <- predict(object = qda_htru_original, newdata = htru2_test[, 1:8])
qda_res <- confusionMatrix(qda_predict_train$class, htru2_test$Class, positive = "1")
print(qda_res)

png(paste0(rute_img, "HTRU2_QDA.png"), width = 800, height = 800)
partimat(htru2_train[1:8], grouping = class_htru2[train.index], nplots.hor = 2, nplots.vert = 2, main = 'QDA', methods = "qda")
dev.off()
