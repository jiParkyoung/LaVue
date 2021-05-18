library(data.table)
library(GGally)

res_eval <- function(cm){
  TPR = Recall = cm[2,2]/sum(cm[2,])
  Precision = cm[2,2]/sum(cm[,2])
  TNR = cm[1,1]/sum(cm[1,])
  ACC = sum(diag(cm)) / sum(cm)
  BCR = sqrt(TPR*TNR)
  F1 = 2 * Recall * Precision / (Recall + Precision)
  res <- data.frame(TPR = TPR,
                   Precision = Precision,
                   TNR = TNR,
                   ACC = ACC,
                   BCR = BCR,
                   F1 = F1)
  return(res)
}

print("Train Data : ", str(nrow(train)))
print("Test Data : ", str(nrow(test)))

# 변수들 간의 다중공선성으로 인해 경고메시지 발생
ggcorr(train, name="corr", label=T)
log_model <- glm(heatWave~., train, family=binomial())
summary(log_model)

# 모형 예측 수행
pred_prob <- predict(log_model, test, type="response")
pred_class <- rep(0, nrow(test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test$heatWave)
res_eval(cm)

# forward selection
log_model_fwd <- step(glm(heatWave~1, train, family=binomial()), direction="forward", trace=0, scope=formula(log_model))
pred_prob <- predict(log_model_fwd, test, type="response")
pred_class <- rep(0, nrow(test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test$heatWave)
res_eval(cm)

#backward selection
log_model_bwd <- step(glm(heatWave~., train, family=binomial()), direction="backward", trace=0, scope=list(lower=heatWave~1, upper=formula(log_model)))
pred_prob <- predict(log_model_bwd, test, type="response")
pred_class <- rep(0, nrow(test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test$heatWave)
res_eval(cm)

#stepwise selection
log_model_step <- step(glm(heatWave~., train, family=binomial()), direction="both", trace=0, scope=list(lower=heatWave~1, upper=formula(log_model)))
pred_prob <- predict(log_model_step, test, type="response")
pred_class <- rep(0, nrow(test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test$heatWave)
res_eval(cm)