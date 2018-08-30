# 전체 해양 사고 데이터 시계열
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(caret)
library(car)
library(ROSE)
library(rpart)
library(MASS)
library(e1071)
library(glmnet)

setwd("C:/Users/Kim Seok Joon/Desktop/대외활동/공모전/기상청/최종/데이터")
df=read.csv("해양사고_전체.csv")

# 예측변수 생성
df$accident = ifelse(is.na(df$관할해경서),0,1) # 발생X:'0',발생O:'1'
df$accident=as.factor(df$accident)


# 변수 분리 및 제거
df$year=as.factor(substr(df$일시,1,4))
df$month=as.factor(substr(df$일시,6,7))
df$day=as.factor(substr(df$일시,9,10))
df$hour=substr(df$일시,12,13)
df$hour=as.factor(gsub(":","",df$hour))
df$일시=as.POSIXct(df$일시, format="%Y-%m-%d %H:%M")
df[1]=NULL
df[15:22]=NULL
df$week=strftime(df$일시, '%u') # 요일

# 연도별 사고 데이터 시계열
df1=subset(df, accident==1)

year=df1 %>%
  group_by(year) %>%
  summarize(count=n())

ggplot(data = year, 
       mapping = aes(x = year, y = count, col="red",group=1)) + theme_minimal() +
  geom_point(data = year,size=3) +
  geom_line(data = year,size=1) + 
  ggtitle("연도별 해양 사고 발생 추이")+theme(text = element_text(size = 20))

# 월별 사고 데이터 시계열
month=df1 %>%
  group_by(month) %>%
  summarize(count=n())

ggplot(data = month, 
       mapping = aes(x = month, y = count, col="red",group=1)) + theme_minimal() +
  geom_point(data = month,size=3) +
  geom_line(data = month,size=1) + 
  ggtitle("월별 해양 사고 발생 추이")+theme(text = element_text(size = 20))

# 일별 사고 데이터 시계열
day=df1 %>%
  group_by(day) %>%
  summarize(count=n())


ggplot(data = day, 
       mapping = aes(x = day, y = count, col="red",group=1)) + theme_minimal() +
  geom_point(data = day,size=3) +
  geom_line(data = day,size=1) + 
  ggtitle("일별 해별양 사고 발생 추이")+theme(text = element_text(size = 20))

# 요일별 사고 데이터 시계열
week=df1 %>%
  group_by(week) %>%
  summarize(count=n())

ggplot(data = week, 
       mapping = aes(x = week, y = count, col="red",group=1)) + theme_minimal() +
  geom_point(data = week,size=3) +
  geom_line(data = week,size=1) + 
  ggtitle("요일별 해별양 사고 발생 추이")+theme(text = element_text(size = 20))

# 시간별 사고 데이터 시계열
hour=df1 %>%
  group_by(hour) %>%
  summarize(count=n())
hour$hour=as.numeric(as.character(hour$hour))
hour=hour[order(hour$hour,decreasing = F),]

ggplot(data = hour, 
       mapping = aes(x = hour, y = count, col="red",group=1)) + theme_minimal() +
  geom_point(data = hour,size=3) +
  geom_line(data = hour,size=1) + 
  ggtitle("시간별 해별양 사고 발생 추이")+theme(text = element_text(size = 20))


####################################
##########서해 해양 사고############
####################################
setwd("C:/Users/Kim Seok Joon/Desktop/대외활동/공모전/기상청/최종/데이터/최종본")
df=read.csv("na제거_진짜최종.csv")

# 예측변수 생성
colnames(df)[15]="accident"
df$accident=as.factor(df$accident)

# 변수 분리 및 제거
df$year=as.factor(substr(df$관측시간,1,4))
df$month=as.factor(substr(df$관측시간,6,7))
df$day=as.factor(substr(df$관측시간,9,10))
df$hour=substr(df$관측시간,12,13)
df$hour=as.factor(gsub(":","",df$hour))
df$관측시간=as.POSIXct(df$관측시간, format="%Y-%m-%d %H:%M")
df$weekday=strftime(df$관측시간, '%u') # 요일

# 시간별 해양 사고 풍속(평균) 추이
wind=df %>%
  group_by(hour,accident) %>%
  summarize(wind=mean(풍속, na.rm=TRUE))
wind$hour=as.numeric(as.character(wind$hour))
wind=wind[order(wind$hour,decreasing = F),]

ggplot(data = wind, 
       mapping = aes(x = hour, y = wind, col=accident)) + theme_minimal() +
  geom_point(data = wind,aes(group=accident)) +
  geom_line(data = wind,aes(group=accident),size=1) + 
  ggtitle("요일별 해양 사고 풍속(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 풍향(평균) 추이
wind_direction=df %>%
  group_by(hour,accident) %>%
  summarize(wind_direction=mean(풍향, na.rm=TRUE))
wind_direction$hour=as.numeric(as.character(wind_direction$hour))
wind_direction=wind_direction[order(wind_direction$hour,decreasing = F),]

ggplot(data = wind_direction, 
       mapping = aes(x = hour, y = wind_direction, col=accident)) + theme_minimal() +
  geom_point(data = wind_direction,aes(group=accident)) +
  geom_line(data = wind_direction,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 풍향(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 기압(평균) 추이
atmosphere=df %>%
  group_by(hour,accident) %>%
  summarize(atmosphere=mean(기압, na.rm=TRUE))
atmosphere$hour=as.numeric(as.character(atmosphere$hour))
atmosphere=atmosphere[order(atmosphere$hour,decreasing = F),]

ggplot(data = atmosphere, 
       mapping = aes(x = hour, y = atmosphere, col=accident)) + theme_minimal() +
  geom_point(data = atmosphere,aes(group=accident)) +
  geom_line(data = atmosphere,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 기압(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 최대파고(평균) 추이
wave_max=df %>%
  group_by(hour,accident) %>%
  summarize(wave_max=mean(최대파고.m., na.rm=TRUE))
wave_max$hour=as.numeric(as.character(wave_max$hour))
wave_max=wave_max[order(wave_max$hour,decreasing = F),]

ggplot(data = wave_max, 
       mapping = aes(x = hour, y = wave_max, col=accident)) + theme_minimal() +
  geom_point(data = wave_max,aes(group=accident)) +
  geom_line(data = wave_max,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 최대파고(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 파주기(평균) 추이
wave_period=df %>%
  group_by(hour,accident) %>%
  summarize(wave_period=mean(파주기.sec., na.rm=TRUE))
wave_period$hour=as.numeric(as.character(wave_period$hour))
wave_period=wave_period[order(wave_period$hour,decreasing = F),]

ggplot(data = wave_period, 
       mapping = aes(x = hour, y = wave_period, col=accident)) + theme_minimal() +
  geom_point(data = wave_period,aes(group=accident)) +
  geom_line(data = wave_period,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 파주기(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 파향(평균) 추이
wave_direction=df %>%
  group_by(hour,accident) %>%
  summarize(wave_direction=mean(파향.deg., na.rm=TRUE))
wave_direction$hour=as.numeric(as.character(wave_direction$hour))
wave_direction=wave_direction[order(wave_direction$hour,decreasing = F),]

ggplot(data = wave_direction, 
       mapping = aes(x = hour, y = wave_direction, col=accident)) + theme_minimal() +
  geom_point(data = wave_direction,aes(group=accident)) +
  geom_line(data = wave_direction,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 파향(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 습도(평균) 추이
humidity=df %>%
  group_by(hour,accident) %>%
  summarize(humidity=mean(습도, na.rm=TRUE))
humidity$hour=as.numeric(as.character(humidity$hour))
humidity=humidity[order(humidity$hour,decreasing = F),]

ggplot(data = humidity, 
       mapping = aes(x = hour, y = humidity, col=accident)) + theme_minimal() +
  geom_point(data = humidity,aes(group=accident)) +
  geom_line(data = humidity,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 습도(평균) 추이")+theme(text = element_text(size = 20))

# 시간별 해양 사고 수온(평균) 추이
water=df %>%
  group_by(hour,accident) %>%
  summarize(water=mean(수온, na.rm=TRUE))
water$hour=as.numeric(as.character(water$hour))
water=water[order(water$hour,decreasing = F),]

ggplot(data = water, 
       mapping = aes(x = hour, y = water, col=accident)) + theme_minimal() +
  geom_point(data = water,aes(group=accident)) +
  geom_line(data = water,aes(group=accident),size=1) + 
  ggtitle("시간별 해양 사고 수온(평균) 추이")+theme(text = element_text(size = 20))

####################################
#############상관 분석##############
####################################

# na 제거
cor=subset(df, accident=='1')
cor=subset(cor,select=(-c(1,2,4,6,14:20))) # 풍향, 파향(범주형 변수) 제거
head(cor)
cor=cor[(complete.cases(cor)),]

corrplot <- cor(cor)
corrplot(corrplot, order = "hclust",
         col = brewer.pal(n = 9, name = "RdBu"))

####################################
###############모델링###############
####################################

## 로지스틱 회귀 모델
west <- read.csv("na제거_진짜최종_거리 이상치_제거.csv", header=T)
nbwest <- west[,c(3,5, 7:13,15)]
nbwest$check <- as.factor(nbwest$check)
nbwest2 <- nbwest
nbwest2 <- na.omit(nbwest2)
nbwest3 <- nbwest2

set.seed(123) 
intrain=createDataPartition(y=nbwest3$check, p=0.8, list=FALSE)
train=nbwest3[intrain,]
test=nbwest3[-intrain,]

prop.table(table(test$check))

data.rose <- ROSE(check ~ ., data = train, seed = 1)$data

m = glm(check~., family = "binomial", data=data.rose)
summary(m)

new.model <- step(m, direction = 'both') 
formula(new.model)

glm.prob=predict(new.model,test,type = "response")
glm.pred=rep("0", 50127)
glm.pred[glm.prob>0.5]="1"
glm.pred=as.factor(glm.pred)

confusionMatrix(glm.pred, test$check, mode = "prec_recall", positive="1")

## LDA
# scale을 활용하여 표준화 (종속변수는 카테고리형이라서, 해당 변수 제외해야 함)
nbwest3_std <- scale(nbwest3[,-10])
nbwest3_std %>% head()

# 표준화가 잘 되었는가?  표준화 한 컬럼의 분산이 1인지 확인
var(nbwest3_std[,4])

# nbwest3_std와 종속변수 merge
nbwest3<- cbind(nbwest3_std, nbwest3[10])

set.seed(102) # best set.seed
intrain=createDataPartition(y=nbwest3$check, p=0.8, list=FALSE)
train=nbwest3[intrain,]
test=nbwest3[-intrain,]

prop.table(table(test$check))

data.rose <- ROSE(check ~ ., data = train, seed = 1)$data

# lda modeling
m <- lda(check~., data = data.rose, prior = c(1/2,1/2))

# predict
testpred <- predict(m, test)


# misclass error 확인
misclass.error <- mean(test$check != testpred$class);misclass.error

confusionMatrix(testpred$class, test$check, mode = "prec_recall", positive="1")

## 정규화 선형회귀

west <- read.csv("na제거_진짜최종_거리 이상치_제거.csv", header=T)
new_west <- west[,c(3,5,7:13,15)]
new_west$check <- as.factor(new_west$check)
new_west <- na.omit(new_west)

set.seed(102) 

#데이터 분할(8:2)
intrain=createDataPartition(y=new_west$check, p=0.8, list=FALSE)
train=new_west[intrain,]
test=new_west[-intrain,]

#훈련용 데이터 오버샘플링
data.rose <- ROSE(check ~ ., data = train, seed = 102)$data
train <- data.rose


for (i in c(0,5,10)) {
  assign(paste("fit", i, sep=""), cv.glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), type.measure="mse", 
                                            alpha=i/10,family="binomial"))
}

par(mfrow=c(1,1))


###라쏘 모델
grid=10^seq(10, -2, length=100)
lasso.mod1=glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=1, lambda=grid)
cv.lasso=cv.glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=1, family="binomial")

#라쏘 모델의 최적의 람다 값 탐색
windows()
plot(lasso.mod1, xvar="lambda", main="LASSO LAMBDA Search",label=T)

plot(lasso.mod1, xvar="dev", main="LASSO DEV",label=T)

bestlam.lasso=cv.lasso$lambda.min;bestlam.lasso

#라쏘 모델의 MSE 그래프
plot(fit10, main="LASSO MSE")

#라쏘 모델로 테스트 데이터 예측
lasso.pred=predict(cv.lasso,s=bestlam.lasso ,newx=as.matrix(test[,-10]), type="response")
lasso.prob=rep("0", nrow(test))
lasso.prob[lasso.pred>0.5]="1"
lasso.prob <- as.factor(lasso.prob)

## 릿지 모델
ridge.mod1=glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=0, lambda=grid)
cv.ridge=cv.glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=0, family="binomial")

#릿지 모델의 최적의 람다 값 탐색
plot(ridge.mod1, xvar="lambda", main="RIDGE LAMBDA Search",label=T)

plot(ridge.mod1, xvar="dev", main="RIDGE DEV",label=T)

bestlam.ridge=cv.ridge$lambda.min;bestlam.ridge

#릿지 모델의 MSE 그래프
plot(fit0, main="RIDGE MSE")

#릿지 모델로 테스트 데이터 예측
ridge.pred=predict(cv.ridge,s=bestlam.ridge ,newx=as.matrix(test[,-10]), type="response")
ridge.prob=rep("0", nrow(test))
ridge.prob[ridge.pred>0.5]="1"
ridge.prob <- as.factor(ridge.prob)
# predict(cv.ridge,type="coefficients")

## 엘라스틱넷 모델
elastic.mod1=glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=.5, lambda=grid)
cv.elastic=cv.glmnet(as.matrix(train[,-10]), as.matrix(train[,10]), alpha=.5, family="binomial")

# 엘라스틱넷 모델의 최적의 람다 값 탐색
plot(elastic.mod1, xvar="lambda", main="ELASTICNET LAMBDA Search", label=T)

plot(elastic.mod1, xvar="dev", main="ELASTICNET DEV", label=T)

bestlam.elastic=cv.elastic$lambda.min;bestlam.elastic

#엘라스틱넷 모델의 MSE 그래프
plot(fit5, main="ELASTICNET MSE")

#엘라스틱넷 모델로 테스트 데이터 예측
elastic.pred=predict(cv.elastic,s=bestlam.elastic ,newx=as.matrix(test[,-10]), type="response")
elastic.prob=rep("0", nrow(test))
elastic.prob[elastic.pred>0.5]="1"
elastic.prob <- as.factor(elastic.prob)

##비교

#1. 라쏘 혼동행렬
confusionMatrix(lasso.prob, test$check)

#2. 릿지 혼동행렬
confusionMatrix(ridge.prob, test$check)

#3. 엘라스틱넷 혼동행렬
confusionMatrix(elastic.prob, test$check)
