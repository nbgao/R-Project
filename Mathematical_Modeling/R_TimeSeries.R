library(xlsx)

input_file1 <- "����3���ƽ��ֵ.csv"
input_file1_m <- "../ͼ��/��2��/����_3��_��ƽ��.xlsx"
input_file2_m <- "../ͼ��/��2��/������_3��_��ƽ��.xlsx"
input_file3_m <- "../ͼ��/��2��/�Ϻ�_3��_��ƽ��.xlsx"
input_file4_m <- "../ͼ��/��2��/̨�庣Ͽ_3��_��ƽ��.xlsx"

Data <- read.csv(input_file1, header=T)

data1 <- Data$C1
data2 <- Data$C2
data3 <- Data$C3

data1_ts <- ts(data1, frequency = 365, start = c(2002,4,1))
plot.ts(data1_ts, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n") 
axis(1, at=c((2002-90/365):(2011-90/365)), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

## Holt-Wintersָ��ƽ����(����������)
# ȡ�������ټ��������Ӱ�죬���������
# data1_ts_log <- log(data1_ts)
# plot.ts(data1_ts_log, col='blue', xlab="", ylab="", xaxt="n")
# axis(1, at=c(2002:2011), labels = c(2002:2011))

# Holt-Wintersָ��ƽ������
data1_ts_HW <- HoltWinters(data1_ts)
plot(data1_ts_HW, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n", main="HoltWintersָ��ƽ������")
axis(1, at=c((2002-90/365):(2011-90/365)), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-WintersԤ��
data1_ts_forecast <- foercast(data1_ts_HW, h=365*3)
plot(data1_ts_forecast, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n")
axis(1, at=c((2002-90/365):(2014-90/365)), labels = c(2002:2014))
# ���ɫ����Ϊ80%���������䣬ǳ��ɫΪ95����������

# Ԥ�����ACF�����Ljung-Box����
acf(data1_ts_forecast$residuals[!is.na(data1_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(data1_ts_forecast$residuals, lag=20, type="Ljung-Box")



Data1 <- read.xlsx(input_file1_m, sheetIndex = 1, header = F)
Data1_1 <- Data1[1]
Data1_2 <- Data1[2]
Data1_3 <- Data1[3]

Data1_1_ts <- ts(Data1_1, frequency = 12, start = c(2002,4))
plot.ts(Data1_1_ts, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n") 
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-Wintersָ��ƽ������
Data1_1_ts_HW <- HoltWinters(Data1_1_ts)
plot(Data1_1_ts_HW, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n", main="HoltWintersָ��ƽ������")
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-WintersԤ��
Data1_1_ts_forecast <- forecast(Data1_1_ts_HW, h=12*3)
c(Data1_1_ts_forecast$model$alpha, Data1_1_ts_forecast$model$beta, Data1_1_ts_forecast$model$gamma)
plot(Data1_1_ts_forecast, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n")
axis(1, at=c(2002:2014), labels = c(2002:2014))
# ���ɫ����Ϊ80%���������䣬ǳ��ɫΪ95����������

# Ԥ�����ACF�����Ljung-Box����
acf(Data1_1_ts_forecast$residuals[!is.na(Data1_1_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(Data1_1_ts_forecast$residuals, lag=20, type="Ljung-Box")



Data1_2_ts <- ts(Data1_2, frequency = 12, start = c(2002,4))
plot.ts(Data1_2_ts, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n") 
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(12:30), labels = c(12:30))

# Holt-Wintersָ��ƽ������
Data1_2_ts_HW <- HoltWinters(Data1_2_ts)
plot(Data1_2_ts_HW, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n", main="HoltWintersָ��ƽ������")
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(12:30), labels = c(12:30))

# Holt-WintersԤ��
Data1_2_ts_forecast <- forecast(Data1_2_ts_HW, h=12*3)
plot(Data1_2_ts_forecast, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n")
axis(1, at=c(2002:2014), labels = c(2002:2014))
# ���ɫ����Ϊ80%���������䣬ǳ��ɫΪ95����������

# Ԥ�����ACF�����Ljung-Box����
acf(Data1_2_ts_forecast$residuals[!is.na(Data1_2_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(Data1_2_ts_forecast$residuals, lag=20, type="Ljung-Box")



Data1_3_ts <- ts(Data1_3, frequency = 12, start = c(2002,4))
plot.ts(Data1_3_ts, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n") 
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(19:30), labels = c(19:30))

# Holt-Wintersָ��ƽ������
Data1_3_ts_HW <- HoltWinters(Data1_3_ts)
plot(Data1_3_ts_HW, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n", yaxt="n", main="HoltWintersָ��ƽ������")
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(19:30), labels = c(19:30))

# Holt-WintersԤ��
Data1_3_ts_forecast <- forecast(Data1_3_ts_HW, h=12*3)
plot(Data1_3_ts_forecast, col='blue', xlab="", ylab="�����¶�(��)", xaxt="n")
axis(1, at=c(2002:2014), labels = c(2002:2014))
# ���ɫ����Ϊ80%���������䣬ǳ��ɫΪ95����������

# Ԥ�����ACF�����Ljung-Box����
acf(Data1_3_ts_forecast$residuals[!is.na(Data1_3_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(Data1_3_ts_forecast$residuals, lag=20, type="Ljung-Box")