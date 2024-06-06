########################################
# Resultados de metricas de evaluacion #
########################################

# Modelos base
base_results = data.frame('LR' = round(mb_res$byClass, 4), "LDA" = round(lda_res$byClass, 4), "QDA" = round(qda_res$byClass, 4))
print(base_results)

# Aplicando PCA
pca_results = data.frame('LR_PCA' = round(mb_pca_res$byClass, 4), "LDA_PCA" = round(lda_pca_res$byClass, 4), "QDA_PCA" = round(qda_pca_res$byClass, 4))
print(pca_results)

# Aplicando FA
fa_results = data.frame('LR_FA' = round(fa_mb_res$byClass, 4), "LDA_FA" = round(lda_fa_res$byClass, 4), "QDA_FA" = round(qda_fa_res$byClass, 4))
print(fa_results)

#####################################
# Resultados en tiempo de ejecucion #
#####################################

# Base
time_mb <- microbenchmark(predict(mb, newdata = htru2_test))
time_lda <- microbenchmark(predict(object = lda_htru_original, newdata = htru2_test[, 1:8]))
time_qda <- microbenchmark(predict(object = qda_htru_original, newdata = htru2_test[, 1:8]))

# PCA
time_mb_pca <- microbenchmark(predict(mb_pca, newdata = pca_htru2_test[, 1:3]))
time_lda_pca <- microbenchmark(predict(object = lda_htru_factors, newdata = pca_htru2_test[, 1:3]))
time_qda_pca <- microbenchmark(predict(object = qda_htru_factors, newdata = pca_htru2_test[, 1:3]))

# FA
time_mb_fa <- microbenchmark(predict(fa_mb, newdata = fa_htru_test))
time_lda_fa <- microbenchmark(predict(object = lda_htru_factors, newdata = fa_htru_test))
time_qda_fa <- microbenchmark(predict(object = qda_htru_factors, newdata = fa_htru_test))


time_results <- rbind(t(data.frame('LR' = mean(time_mb$time))), t(data.frame('LDA' = mean(time_lda$time))), t(data.frame('QDA' = mean(time_qda$time))), 
                      t(data.frame('LR_PCA' = mean(time_mb_pca$time))), t(data.frame('LDA_PCA' = mean(time_lda_pca$time))), t(data.frame('QDA_PCA' = mean(time_qda_pca$time))),
                      t(data.frame('LR_FA' = mean(time_mb_fa$time))), t(data.frame('LDA_FA' = mean(time_lda_fa$time))), t(data.frame('QDA_FA' = mean(time_qda_fa$time))))/1000000
colnames(time_results) <- c('Tiempo(milisegundos)')
print(time_results)