library(data.table)
library(GGally)
library(car)
library(HSAUR)
library(pROC)

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

# 정규성
# 등분산성

# 데이터 불러오기
dat_for_mice <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\mice_df_robust.csv")
dat_for_mice_original <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\dat_mice.csv")
# 42016행의 tempHigh가 33, robust 결과는 0.77248677
dat_for_mice$date <- as.Date(dat_for_mice$date)

# 다음날 폭염 여부 변수 추가
dat_for_mice[, "next_heatWave"] = 0
dat_for_mice[(dat_for_mice$to_tempHigh>=0.77248677)&(shift(dat_for_mice$to_tempHigh, fill = dat_for_mice$to_tempHigh[1])>=0.77248677), "next_heatWave"] = 1

# 종속변수 그래프
train_log$y <- exp(train_log$next_heatWave)/(1+exp(train_log$next_heatWave))
with(train_log,plot(next_heatWave,y,type = "l"))

# 쓸모없는 변수 제거
dat_for_mice <- dat_for_mice[,-which(names(dat_for_mice) %in% c("X"))]

# 데이터 슬라이싱
train <- subset(dat_for_mice, format(date,'%Y') < 2017)
test <- subset(dat_for_mice, format(date,'%Y') >= 2017)

# 쓸모없는 변수 제거
train <- train[,-which(names(train) %in% c("date"))]
test <- test[,-which(names(test) %in% c("date"))]

# 모델 정확도 시각화를 위한 다른 변수 제거
train <- train[,-which(names(train) %in% c("tempHigh", "windMaxDir", "airDXSum", "RHAvg", "VPAvg", "LocalAPAvg", "seaAPAvg", "warCloudAvg", "groundTempAvg", 
                                        "grassTempMin", "temp10Avg", "temp20Avg", "temp30Avg", "temp_5", "temp1", "temp1_5", "evapnSmallSum", 
                                        "heatWave", "to_tempHigh"))]
test <- test[,-which(names(test) %in% c("tempHigh", "windMaxDir", "airDXSum", "RHAvg", "VPAvg", "LocalAPAvg", "seaAPAvg", "warCloudAvg", "groundTempAvg", 
                                        "grassTempMin", "temp10Avg", "temp20Avg", "temp30Avg", "temp_5", "temp1", "temp1_5", "evapnSmallSum", 
                                        "heatWave", "to_tempHigh"))]

# 다중공선성 시각화화
ggcorr(train, name="corr", label=T)

# PCA에서 선택한 변수로 로지스틱 회귀 모델
log_model1 <- glm(next_heatWave~., train, family=binomial())
summary(log_model1)
vif(log_model1)

# 모형 예측 수행
pred_prob <- predict(log_model1, test, type="response")
pred_class <- rep(0, nrow(test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test$next_heatWave)
res_eval(cm)

# 그래프 그리기
# plot(test_log[2:32,2], type = 'o', col = 'red')#실제 최고기온 41805-41803=2 2+41803:32+41803=41835
# lines(predtemphigh[2:32], type = 'o', col = 'blue')#다음날 최고기온
predHeatWave <- predict(log_model1, test[,- which(names(test) %in% c("next_heatWave"))])
plot(test[182:213,"next_heatWave"], type='o', col='red') # 2017/7/1~2017/8/1 41984:42015, 41802로 뻄
lines(predHeatWave[182:213], type='o', col='blue')
plot(test[182:244,"next_heatWave"], type='o', col='red') # 2017/7/1~2017/9/1 41984:42046, 41802로 뻄
lines(predHeatWave[182:244], type='o', col='blue')

# log transformation
train_log <- log(train+24)
test_log <- log(test+24)
unique(train_log$next_heatWave)
train_log$next_heatWave <- ifelse(train_log$next_heatWave<3.18,0,1)
test_log$next_heatWave <- ifelse(test_log$next_heatWave<3.18,0,1)

# log_transformation한 변수로 로지스틱 회귀 모델 -> PCA로 선택한 변수인데, "glm.fit:적합된 확률값들이 0 또는 1입니다."라는 오류가 뜸
log_model2 <- glm(next_heatWave~., train_log, family=binomial())
summary(log_model2)
vif(log_model2)

# 모형 예측 수행
pred_prob <- predict(log_model2, test_log, type="response")
pred_class <- rep(0, nrow(test_log))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test_log$next_heatWave)
res_eval(cm)

# 그래프 그리기
# plot(test_log[2:32,2], type = 'o', col = 'red')#실제 최고기온 41805-41803=2 2+41803:32+41803=41835
# lines(predtemphigh[2:32], type = 'o', col = 'blue')#다음날 최고기온
predHeatWave <- predict(log_model2, test_log[,- which(names(test_log) %in% c("next_heatWave"))])
plot(test_log[182:213,"next_heatWave"], type='o', col='red') # 2017/7/1~2017/8/1 41984:42015, 41802로 뻄
lines(predHeatWave[182:213], type='o', col='blue')
plot(test_log[182:244,"next_heatWave"], type='o', col='red') # 2017/7/1~2017/9/1 41984:42046, 41802로 뻄
lines(predHeatWave[182:244], type='o', col='blue')

# 모델 정확도 시각화 : ROC Curve -> 오류
predict1 <- predict(log_model2, newdata = test_log, type = "response")
roc(predict1,test_log$next_heatWave, plot="ROC", AUC=T, main="logistic regression")

# 모형 진단 그래프
par(mfrow=c(2,2))
plot(log_model2)
plot(log_model2, which=1)
# 실제(Y)와 예측(predicted y)값 사이의 residual(잔차)
# 수는 우리 모델에서 예측한 것보다 훨씬 좋다
plot(log_model2, which=2)
# 잔차가 정규분포를 따르는지 확인
# 수는 정규성에 잘 맞지 않음
plot(log_model2, which=3)
# 선형모델이 예측한 Y와 잔차 간의 관계 비교
# X축은 예측된 Y 값, Y축은 표준화 잔차
# 이상치가 있는지 여부 파악 가능
plot(log_model2, which=4)
# 개별 관측치가 선형 모델의 파라미터에 끼친 영향도
# 수가 큰 비율로 영향을 줌
plot(log_model2, which=5)
#독립변수가 극단적으로 치우쳐 있는지 확인
# X축이 Leverage, Y축이 잔차
# Outlier 데이터/극단값을 가진 데이터를 걸러내는데 유의미

# forward selection - 76%의 정확도, 0.74의 F1 
log_model_fwd <- step(glm(next_heatWave~., train_log, family=binomial()), direction="forward", trace=0, scope=formula(log_model1))
pred_prob <- predict(log_model_fwd, test_log, type="response")
pred_class <- rep(0, nrow(test_log))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test_log$next_heatWave)
res_eval(cm)

#backward selection - 76%의 정확도, 0.74의 F1
log_model_bwd <- step(glm(next_heatWave~., train_log, family=binomial()), direction="backward", trace=0, scope=list(lower=next_heatWave~1, upper=formula(log_model1)))
pred_prob <- predict(log_model_bwd, test_log, type="response")
pred_class <- rep(0, nrow(test_log))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test_log$next_heatWave)
res_eval(cm)

#stepwise selection - 76%의 정확도, 0.74의 F1
log_model_step <- step(glm(next_heatWave~., train_log, family=binomial()), direction="both", trace=0, scope=list(lower=next_heatWave~1, upper=formula(log_model1)))
pred_prob <- predict(log_model_step, test_log, type="response")
pred_class <- rep(0, nrow(test_log))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=test_log$next_heatWave)
res_eval(cm)

summary(log_model_fwd)
summary(log_model_bwd)
summary(log_model_step)