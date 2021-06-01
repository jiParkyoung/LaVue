# 데이터 불러오기
data_mice <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\mice_df_robust.csv")
str(data_mice)
dat_mice<- data_mice[,-c(1)] 
dat_mice$date <- as.Date(dat_mice$date)

#다음날 최고기온뽑는과정
#to_tempHigh <- dat_mice$tempHigh
#str(to_tempHigh)
#to_tempHigh <- to_tempHigh[-1]
#to_tempHigh[43383]<-0
#mice_df <- data.frame(dat_mice, to_tempHigh)
#str(mice_df)

#데이더 나누기
mice_df_train <- subset(dat_mice, format(date,'%Y') < 2017)
mice_df_test <- subset(dat_mice, format(date,'%Y') >= 2017)
str(mice_df_test)

train <- mice_df_train[,c('to_tempHigh','tempHigh','tempAvg','tempLow','windMaxInstantDir','windMax','windAvg','RHMin','sunlightTimeSum',
                          'temp5Avg')]
test <- mice_df_test[,c('to_tempHigh','tempHigh','tempAvg','tempLow','windMaxInstantDir','windMax','windAvg','RHMin','sunlightTimeSum',
                        'temp5Avg')]
rlogtrain <- mice_df_train[,c('to_tempHigh','tempHigh','tempAvg','tempLow','windMaxInstantDir','windMax','windAvg','RHMin','sunlightTimeSum',
                              'temp5Avg')]
rlogtest <- mice_df_test[,c('to_tempHigh','tempHigh','tempAvg','tempLow','windMaxInstantDir','windMax','windAvg','RHMin','sunlightTimeSum',
                            'temp5Avg')]

min(rlogtrain)
min(rlogtest)
str(rlogtrain)
str(rlogtest)


train_rlog <- log(rlogtrain+2)
test_rlog <- log(rlogtest+2)


#tempAvg+tempLow+windMaxInstantDir+windMax+windAvg+RHMin+sunlightTimeSum+temp5Avg
robustLog_model <- lm(to_tempHigh~.-tempLow-tempAvg, train_rlog)
summary(robustLog_model)

#모델평가 테스트
predtemphigh <- predict(robustLog_model,test_rlog[-1])
y<- plot(test_rlog[2:32,2], type = 'o', col = 'red') + lines(predtemphigh[2:32], type = 'o', col = 'blue')

library(plotly)
library(rmarkdown)
library(dplyr)

Orange %>% 
  plot_ly() %>% 
  add_trace(x=0:30, y=test_rlog[2:32,2], color = 'red', mode = "lines")
  add_trace(x=0:30, y=predtemphigh[2:32], color = 'blue', mode = "lines") %>% 
  layout(
    title = "선형회귀",
    yaxis = list(title = "최고기온")
  )

fig <- plot_ly() 
fig <- fig %>% layout(title='선형회귀모델', yaxis=list(title='최고기온'), sizes=c(380,150))
fig <- fig %>% add_trace(x=0:30, y=test_rlog[2:32,2], color='실제값', mode = "lines") 
fig <- fig %>% add_trace(x=0:30, y=predtemphigh[2:32], color='예측값', mode = "lines")
fig