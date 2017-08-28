library(Matrix)
library(lme4)
library(pbkrtest)
library(car)

# ��������
data <- read.csv("yeardata.csv")
head(data)


### ��϶�Ԫ���Իع�ģ��
# �鿴������������
cor(data)
cor(data[c("GDP", "Consumption", "M0")])
# ɢ�����ͼ
scatterplotMatrix(data, spread = FALSE, main = "Scatter Plot Matrix via car package")
# spread=FALSE ��ʾɾ���˲в�������������ƽ�������ϵ�չ���ͷǶԳ���Ϣ

# ���ģ��
GDP <- data[,1]
Consumption <- data[,2]
M0 <- data[,3]
EM <- data[,4]
Investment <- data[,5]
fit <- lm(GDP~Consumption + M0 + EM + Investment)
fit

fit <- lm(GDP~Consumption + M0 + EM + Investment, data = data)
fit
# �õ��Ķ�Ԫ�ع�ģ��
sprintf('��Ԫ�ع鷽��Ϊ:')
sprintf('GDP = %f + %f*Consumption + %f*M0 + %f*EM + %f*Investment', fit$coefficients[1], fit$coefficients[2], fit$coefficients[3], fit$coefficients[4], fit$coefficients[5])

# ��Ͻ������
summary(fit)
# Residuals   Min   1Q    Mean    3Q    Max
# Coefficients ����ֵEstimate   ��׼��Std.Error   tֵt-value    PֵPr(>|t|)
# ����Ŷ�R^2(�ɾ�ϵ�����ж�ϵ��)
# ģ������������Fͳ���� F-statistic   ���ɶ�DF    p-value



### ��Ԫ���Իع�ģ�͵����
par(mfrow=c(2,2)) # ��ͼ����2*2����ʽ����һ��ͼ�δ���
plot(fit)
# Residuals vs Fitted ͼ�����ϣ���Ҫ���ڼ���ٶ� 1�������ڲ�����ͼ����ʾ����в�ֵ�����ֵ����û�бȽ����ԵĹ�������˵���Ա����������֮�������Թ�ϵ������ٶ� 1��
# Normal Q-Q ͼ�����ϣ���Ҫ������������̬�ԣ���������̬�Եļ��裬��ͼ�ϵ�Բ��Ӧ���� 45 �Ƚǵ�ֱ���ϣ���֮����������̬�Եļ��衣��ͼ�н������������������̬�Եļ��衣
# Scale-Locationͼ�����£���Ҫ��������ͬ�����Եļ��裬��ˮƽ����Χ�ĵ�����ֲ���������ͬ������裻��֮�������㡣���۲�˴����������ͬ�����Եļ��衣
# Residuals vs Leverage ͼ�����£���Ҫ���ڹ۲������еĵ���ֵ�������ڼ�����顣


### ��Ԫ���Իع�ģ�͵�����ѡ��
# �����Ϣ׼��AIC
fit1 <- lm(GDP~Consumption + M0 + EM + Investment, data = data)
fit2 <- lm(GDP~Consumption + M0 + EM, data = data)
fit3 <- lm(GDP~Consumption + M0, data)
fit4 <- lm(GDP~Consumption, data = data)
AIC(fit1, fit2, fit3, fit4)   # �����Ϣ׼��
# AIC׼��������fit2 AICֵ��С����Ͻ����ü�ѡ�� Consumption��M0��EM ��������Ϊ�Ա����Ķ�Ԫ���Իع�ģ��Ϊ����ģ��


### ģ�Ͳ�������
summary(fit2)
# �ع鷽�̵Ľؾ����� -761.13
# ���ѣ�Consumption��ÿ�䶯һ����λ������ GDP Ϊ�����ľ������� 1.67 ����λ
# ���ң�M0��ÿ����һ����λ�Ĺ�Ӧ������ GDP ���� 1.38 
# ͬ���������ܶEM��ÿ����һ����λ������ GDP ���� 0.43
# Ҳ����˵��Ϊ�˴ٽ��ҹ����õĿ��ٷ�չ�����Բ�ȡ�̼��������ٽ���������
# Ҳ�����ʵ������ӻ��ҹ�Ӧ�������ܹ���Ͷ�ţ���Ϊ������ͨ������
# ������������⿪�ų̶������ӽ����ڴӶ��ٽ���������

### ģ��Ԥ��
new <- data.frame(Consumption=330000, M0=65000, EM=250000)   # �����Ա���
lm.pred <- predict(fit2, new, interval="prediction", level=0.95)   # ����Ԥ��
lm.pred