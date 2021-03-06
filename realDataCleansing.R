# 라이브러리
library(ggplot2)
library(dplyr)
library(mice)
library(DMwR)
library(randomForest)
library(missForest)

# 데이터 불러오기
dat1 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1907_1909.csv")
dat2 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1910_1919.csv")
dat3 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1920_1929.csv")
dat4 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1930_1939.csv")
dat5 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1940_1949.csv")
dat6 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1950_1959.csv")
dat7 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1960_1969.csv")
dat8 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1970_1979.csv")
dat9 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1980_1984.csv")
dat10 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1985_1989.csv")
dat11 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1990_1994.csv")
dat12 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_1995_1999.csv")
dat13 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_2000_2004.csv")
dat14 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_2005_2009.csv")
dat15 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_2010_2014.csv")
dat16 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_2015_2021.csv")
dat17 <- read.csv("C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\data\\OBS_ASOS_DD_2021_6.csv")

# dat : 1907~2021년도의 종관기상 관측 데이터
dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11, dat12, dat13, dat14, dat15, dat16, dat17)
str(dat)

# 변수명 변경
# point : 지점, place : 지점명, date : 일시, tempAvg : 평균기온 C, tempLow : 최저기온 C, tempLowTime : 최저기온 시각 hhmi, 
# tempHigh : 최고기온 C, tempHighTime : 최고기온 시각 hhmi, precipTime : 강수 지속 시간 hr, precipHighMin : 10분 최다 강수량 mm, precipHighMinTime : 10분 최다 강수량 시각 hhmi, precipHighHour : 1시간 최다강수량 mm, 
# precipHighHourTime : 1시간 최다 강수량 시각 hhmi, percipDate : 일강수량 mm, windMaxInstant : 최대 순간 풍속 ms, windMaxInstantDir : 최대 순간 풍속 풍향 16방위, windMaxInstantTime : 최대 순간 풍속 시각 hhmi, windMax : 최대 풍속 ms,
# windMaxDir : 최대 풍속 풍향 16방위, windMaxTime : 최대 풍속 시각 hhmi, windAvg : 평균 풍속 ms, airDXSum : 풍정합 100m, windDir : 최다풍향 16방위, dewPointAvg : 평균 이슬점 온도 C,
# RHMin : 최소 상대 습도, RHMinTime : 최소 상대 습도 시각 hhmi, RHAvg : 평균 상대습도, VPAvg : 평균 증기압 hPa, localAPAvg : 평균 현지 기압 hPa, seaAPMax : 최고 해면 기압 hPa, 
# seaAPMaxTime : 최고 해면 기압 시각 hhmi, seaAPMin : 최저 해면 기압 hPa, seaAPMinTime : 최저 해면 기압 시각 hhmi, seaAPAvg : 평균 해면 기압 hPa, sunDuration : 가조시간, sunlightTimeSum : 합계 일조시각,
# SRMaxHourTime : 1시간 최 SRMaxHour : 1시간 최다 일사다 일사 시각,량, SRSum : 합계 일사량, snowDepthDaily : 일 최심신적설 cm, snowDepthDailyTime : 일 최심신적설 시각 hhmi, snowDepth :일 최심적설 cm, 
# snowDepthTime : 일 최심적설 시각 hhmi, snowDepthThreeSum : 3시간 신적설 합계 cm, warCloudAvg : 평균 전운량, cloudMidAvg : 평균 중하층운량, groundTempAvg : 평균 지면온도 C, grassTempMin : 최저 초상 온도 C, 
# temp5Avg : 평균 5cm 지중온도 C, temp10Avg : 평균 10cm 지중온도 C, temp20Avg : 평균 20cm 지중온도 C, temp30Avg : 평균 30cm 지중온도 C, temp_5 : 0.5m 지중온도 C, temp1 : 1.0m 지중온도 C,
# temp1_5 : 1.5m 지중온도 C, temp3 : 3.0m 지중온도 C, temp5 : 5.0m 지중온도 C, evapnLargeSum : 합계 대형 증발량 mm, evapnSmallSum : 합계 소형 증발량 mm, precip9_9 : 9.9 강수 mm, article : 기사, fogTime : 안개 계속 시간 hr
names(dat) <- c("point", "place", "date", "tempAvg", "tempLow", "tempLowTime",
                "tempHigh", "tempHighTime", "precipTime", "precipHighMin", "precipHighMinTime", "precipHighHour",
                "precipHighHourTime", "precipDate", "windMaxInstant", "windMaxInstantDir", "windMaxInstantTime", "windMax",
                "windMaxDir", "windMaxTime", "windAvg", "airDXSum", "windDir", "dewPointAvg",
                "RHMin", "RHMinTime", "RHAvg", "VPAvg", "LocalAPAvg", "seaAPMax", 
                "seaAPMaxTime", "seaAPMin", "seaAPMinTime", "seaAPAvg", "sunDuration", "sunlightTimeSum",
                "SRMaxHourTime", "SRMaxHour", "SRSum", "snowDepthDaily", "snowDepthDailyTime", "snowDepth",
                "snowDepthTime", "snowDepthThreeSum", "warCloudAvg", "cloudMidAvg", "groundTempAvg", "grassTempMin",
                "temp5Avg", "temp10Avg", "temp20Avg", "temp30Avg", "temp_5", "temp1",
                "temp1_5", "temp3", "temp5", "evapnLargeSum", "evapnSmallSum", "precip9_9", "article", "fogTime")

# chr 형태의 변수 date를 date로 형 변환
dat$date <- as.Date(dat$date)

# 결측치 확인
sapply(dat, function(x) sum(is.na(x)))
md.pattern(dat)

# 데이터 저장
save_dat_before_dataCleansing <- write.csv(dat, "C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\dat_before_dataCleansing.csv")

#결측치가 10000개 이상인 변수와 chr형태의 변수와 쓸모없는 변수 제거
dat <- dat[,-which(names(dat) %in% c("tempLowTime", "tempHighTime", "precipTime", "precipHighMin", "precipHighMinTime", "precipHighHour", "precipHighHourTime", "precipDate", "windMaxInstant", "windMaxInstantTime", "windMaxTime", "windDir", "dewPointAvg", "RHMinTime", "seaAPMax", "seaAPMaxTime", "seaAPMin", "seaAPMinTime", "sunDuration", "SRMaxHourTime", "SRMaxHour", "SRSum", "snowDepthDaily", "snowDepthDailyTime", "snowDepth", "snowDepthTime", "snowDepthThreeSum", "cloudMidAvg", "temp3", "temp5", "evapnLargeSum", "precip9_9", "article", "point", "place", "fogTime"))]

# 결측치를 mice로 대체
da_mice <- mice(dat, method="rf")
dat_mice <- complete(da_mice)
sapply(dat_mice, function(x) sum(is.na(x)))

# mice로 결측치 대체한 데이터 저장
save_dat_mice <- write.csv(dat_mice, "C:\\Users\\jessy\\Desktop\\팀플\\LaVue\\dat_mice.csv")
