###################
# Modelo Base: LR #
###################

# Entrenamiento del modelo
mb <- train(Class ~ ., data = htru2_train, method = "glm", family = "binomial")

# Obtencion de las prediciones
mb_pred <- predict(mb, newdata = htru2_test)

# Obtencion de las metricas de evaluacion
mb_res <- confusionMatrix(mb_pred, htru2_test$Class, positive = "1")
print(mb_res)
