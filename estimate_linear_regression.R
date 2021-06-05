coe1<-model_fwd3$coefficients
coe1<-as.data.frame(coe1)

coe2<-model_fwd3_d2$coefficients
coe2<-as.data.frame(coe2)

coe3<-model_fwd3_d3$coefficients
coe3<-as.data.frame(coe3)

coe4<-model_fwd3_d4$coefficients
coe4<-as.data.frame(coe4)

coe5<-model_fwd3_d5$coefficients
coe5<-as.data.frame(coe5)

coe6<-model_fwd3_d6$coefficients
coe6<-as.data.frame(coe6)

coe7<-model_fwd3_d7$coefficients
coe7<-as.data.frame(coe7)

var_name <- c("temp5Avg", "temp30Avg", "sunlightTimeSum", "windMaxInstantDir", "windMaxDir", "VPAvg", "seaAPAvg", "LocalAPAvg", "tempHigh", "RHAvg", "windAvg", "RHMin", "windMax")
coe1_est <- c(coe1['temp5Avg',], coe1['temp30Avg',], coe1['sunlightTimeSum',], coe1['windMaxInstantDir',], coe1['windMaxDir',], coe1['VPAvg',], coe1['seaAPAvg',], coe1['LocalAPAvg',], coe1['tempHigh',], coe1['RHAvg',], coe1['windAvg',], coe1['RHMin',], coe1['windMax',])
coe2_est <- c(coe2['temp5Avg',], coe2['temp30Avg',], coe2['sunlightTimeSum',], coe2['windMaxInstantDir',], coe2['windMaxDir',], coe2['VPAvg',], coe2['seaAPAvg',], coe2['LocalAPAvg',], coe2['tempHigh',], coe2['RHAvg',], coe2['windAvg',], coe2['RHMin',], coe2['windMax',])
coe3_est <- c(coe3['temp5Avg',], coe3['temp30Avg',], coe3['sunlightTimeSum',], coe3['windMaxInstantDir',], coe3['windMaxDir',], coe3['VPAvg',], coe3['seaAPAvg',], coe3['LocalAPAvg',], coe3['tempHigh',], coe3['RHAvg',], coe3['windAvg',], coe3['RHMin',], coe3['windMax',])
coe4_est <- c(coe4['temp5Avg',], coe4['temp30Avg',], coe4['sunlightTimeSum',], coe4['windMaxInstantDir',], coe4['windMaxDir',], coe4['VPAvg',], coe4['seaAPAvg',], coe4['LocalAPAvg',], coe4['tempHigh',], coe4['RHAvg',], coe4['windAvg',], coe4['RHMin',], coe4['windMax',])
coe5_est <- c(coe5['temp5Avg',], coe5['temp30Avg',], coe5['sunlightTimeSum',], coe5['windMaxInstantDir',], coe5['windMaxDir',], coe5['VPAvg',], coe5['seaAPAvg',], coe5['LocalAPAvg',], coe5['tempHigh',], coe5['RHAvg',], coe5['windAvg',], coe5['RHMin',], coe5['windMax',])
coe6_est <- c(coe6['temp5Avg',], coe6['temp30Avg',], coe6['sunlightTimeSum',], coe6['windMaxInstantDir',], coe6['windMaxDir',], coe6['VPAvg',], coe6['seaAPAvg',], coe6['LocalAPAvg',], coe6['tempHigh',], coe6['RHAvg',], coe6['windAvg',], coe6['RHMin',], coe6['windMax',])
coe7_est <- c(coe7['temp5Avg',], coe7['temp30Avg',], coe7['sunlightTimeSum',], coe7['windMaxInstantDir',], coe7['windMaxDir',], coe7['VPAvg',], coe7['seaAPAvg',], coe7['LocalAPAvg',], coe7['tempHigh',], coe7['RHAvg',], coe7['windAvg',], coe7['RHMin',], coe7['windMax',])
estimate_linear <- data.frame(var_name, coe1_est, coe2_est, coe3_est, coe4_est, coe5_est, coe6_est, coe7_est)
save_estimate <- write.csv(estimate_linear, "C:\\Users\\jessy\\Desktop\\ÆÀÇÃ\\LaVue\\estimates_linear.csv")
