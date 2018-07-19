rm(list=ls())
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(readr)) install.packages("readr"); library(readr)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(plyr)) install.packages("plyr"); library(plyr)
if(!require(reshape)) install.packages("reshape"); library(reshape)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(gam)) install.packages("gam"); library(gam)
if(!require(Metrics)) install.packages("Metrics"); library(Metrics)
if(!require(caTools)) install.packages("caTools"); library(caTools)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)
if(!require(psych)) install.packages("psych"); library(psych)

train<-read.csv("train.csv",stringsAsFactors = FALSE)
train$Hour <- as.factor(hour(ymd_hms(train$datetime)))
train.new <- train
colnames(train)[10:11]=c("정기권X","정기권O")
Rent_type_RentHour <- aggregate(train[,c("정기권X","정기권O")], by=list(train$Hour), "sum") #시간대별로 자전거 이용 시간 합산
Rent_type_RentHour
colnames(Rent_type_RentHour) <- c('시간','정기권X','정기권O') 

Rent_type_RentHour <- melt(Rent_type_RentHour[,c('시간','정기권X','정기권O')], id.vars = 1)
colnames(Rent_type_RentHour) <- c('시간','이용형태', '이용수') # colname 교체 

ggplot(data = Rent_type_RentHour, mapping = aes(x = 시간, y = 이용수)) +
  geom_bar(mapping = aes(fill = 이용형태), stat = "identity", position="dodge")+theme_minimal() 

# 정식으로 등록한 자전거 이용시간은 8시, 17~19시 가 가장 높았다 이는 정식으로 등록해서 사용하는 자전거 이용자는 주로 출퇴근 자 라는것을 알 수있다.

# _________________________________________________________________________________________________________________________________
#정식등록 자전거의 working day 여부에 따른 이용시간 추이

Workingday_date <- aggregate(train[,c("정기권X","정기권O")], by=list(train$Hour, train$workingday), "sum")
colnames(Workingday_date) <- c('시간', '평일', '정기권X','정기권O') #working day 와 non working day의 자전거 이용시간 군집화한 data frame

Workingday_date$정기권O_평일O <- Workingday_date$정기권O[Workingday_date$평일 == 1]
Workingday_date$정기권O_평일X <- Workingday_date$정기권O[Workingday_date$평일 == 0]

Workingday_date <- Workingday_date[1:24,c('시간','정기권O_평일O','정기권O_평일X')]

Workingday_date <- melt(Workingday_date[,c('시간','정기권O_평일O', '정기권O_평일X')], id.vars = 1)
colnames(Workingday_date) <- c('시간', '이용형태_평일', '이용수')

ggplot(data = Workingday_date, mapping = aes(x = 시간, y = 이용수)) +theme_minimal() +
  geom_bar(mapping = aes(fill = 이용형태_평일), stat = "identity", position="dodge")

# _____________________________________________________________________________________________________________________
#자전거 대여를 통한 이용자의 working day 여부에 따른 이용시간 추이
Workingday_date2 <- aggregate(train[,c("정기권X","정기권O")], by=list(train$Hour, train$workingday), "sum")
Workingday_date2
colnames(Workingday_date2) <- c('시간', '평일', '정기권X','정기권O') #working day 와 non working day의 자전거 이용시간 군집화한 data frame


Workingday_date2$정기권X_평일O <- Workingday_date2$정기권X[Workingday_date2$평일 == 1]
Workingday_date2$정기권X_평일X <- Workingday_date2$정기권X[Workingday_date2$평일 == 0]

Workingday_date2 <-Workingday_date2[1:24,c('시간','정기권X_평일O','정기권X_평일X')]

Workingday_date2 <- melt(Workingday_date2[,c('시간','정기권X_평일O', '정기권X_평일X')], id.vars = 1)
colnames(Workingday_date2) <- c('시간', '이용형태_평일', '이용수')

ggplot(data = Workingday_date2, mapping = aes(x = 시간, y = 이용수)) +theme_minimal() +
  geom_bar(mapping = aes(fill = 이용형태_평일), stat = "identity", position="dodge")
#따라서 우리는 두개의 그래프를 통해서 registered bike 이용자는 출퇴근을 위한 이용자가 주로 라는것을 알게 되었다.
# _________________________________________________________________________________________________________________________

#날씨에 따른 registered 이용자 추이 구하기 날씨의 여부에 따라서 자전거 이용행태가 바뀌는지에 대해서 알아본다.
weatherbike <- aggregate(train[,c("weather")], by=list(train$Hour, train$weather), "sum")
colnames(weatherbike) <- c('시간', '날씨', '총이용수') #날씨에 따른 출퇴근이용자 추이

weatherbike$날씨<-gsub("1","맑음/구름",weatherbike$날씨)
weatherbike$날씨<-gsub("2","안개",weatherbike$날씨)
weatherbike$날씨<-gsub("3","가벼운 비",weatherbike$날씨)
weatherbike$날씨<-gsub("4","폭풍우",weatherbike$날씨)

ggplot(data = weatherbike, 
       mapping = aes(x = 시간, y = 총이용수, color = 날씨)) + theme_minimal() +
  geom_point(data = weatherbike, aes(group = 날씨)) +
  geom_line(data = weatherbike, aes(group = 날씨),size=1)

# __________________________________________


weatherbike2 <- aggregate(train[,c("정기권X","정기권O")], by=list(train$Hour, train$weather), "sum")
colnames(weatherbike2) <- c('시간', '날씨', '정기권X','정기권O') #날씨에 따른 출퇴근이용자 추이

weatherbike2$날씨<-gsub("1","맑음/구름",weatherbike2$날씨)
weatherbike2$날씨<-gsub("2","안개",weatherbike2$날씨)
weatherbike2$날씨<-gsub("3","가벼운 비",weatherbike2$날씨)
weatherbike2$날씨<-gsub("4","폭풍우",weatherbike2$날씨)

#casual user 의 날씨에 따른 이용
ggplot(data = weatherbike2, 
       mapping = aes(x = 시간, y = 정기권X, color = 날씨)) +theme_minimal() + 
  geom_point(data = weatherbike2, aes(group = 날씨)) +
  geom_line(data = weatherbike2, aes(group = 날씨))



#registered user의 날씨에 따른 이용
ggplot(data = weatherbike2, 
       mapping = aes(x = 시간, y = 정기권O, color = 날씨)) + theme_minimal() +
  geom_point(data = weatherbike2, aes(group = 날씨)) +
  geom_line(data = weatherbike2, aes(group = 날씨))

# ____________________________________________________________
#total 의 날씨별 이용자수 차
weatherbike2$총이용수<-(weatherbike2$정기권X+weatherbike2$정기권O)
ggplot(weatherbike2,aes(x=날씨,y=총이용수,color = 날씨))+theme_minimal() +geom_boxplot()
summary(aov(총이용수~날씨,weatherbike2))

#____________________________________________________________________________
ggplot(weatherbike2,aes(x=날씨,y=정기권O,color = 날씨))+theme_minimal() +geom_boxplot()
#boxplot 을 통해서 날씨별 registered 이용자가 차이가 있다는것을 알아냈다.
summary(aov(정기권O~weather,train)) 
#실제로 aov를 돌려서 차이확인

ggplot(weatherbike2,aes(x=날씨,y=정기권X,color = 날씨))+theme_minimal() +geom_boxplot()
#날씨별 casual user 차이
summary(aov(정기권X~날씨,weatherbike2))
#________________________________________________________________

#___________________________________________________________

seasonbike <- aggregate(train[,c("정기권X","정기권O")], by=list(train$Hour, train$season), "sum")
colnames(seasonbike) <- c('시간', '계절', '정기권X','정기권O') #계절별
seasonbike$총이용수<-(seasonbike$정기권X+seasonbike$정기권O)


seasonbike$계절<-gsub("1","봄",seasonbike$계절)
seasonbike$계절<-gsub("2","여름",seasonbike$계절)
seasonbike$계절<-gsub("3","가을",seasonbike$계절)
seasonbike$계절<-gsub("4","겨울",seasonbike$계절)
#______________________________________________________________
ggplot(seasonbike,aes(x=계절,y=총이용수,color = 계절))+theme_minimal()+geom_boxplot()
summary(aov(총이용수~계절,seasonbike))
#total 의 차이

#________________________________________________________________

ggplot(seasonbike,aes(x=계절,y=정기권O,color = 계절))+theme_minimal() +geom_boxplot()
#boxplot 을 통해서 날씨별 registered 이용자가 차이가 있다는것을 알아냈다.
summary(aov(정기권O~계절,seasonbike)) 
#실제로 aov를 돌려서 차이확인

ggplot(seasonbike,aes(x=계절,y=정기권X,color = 계절))+theme_minimal() +geom_boxplot()
#날씨별 casual user 차이
summary(aov(정기권X~계절,seasonbike))
#________________________________________________________________












###########################################################################################################################















month <- as.integer(format(as.POSIXlt(train.new$datetime), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(train.new$datetime), format = "%u"))
hour <- as.integer(format(as.POSIXlt(train.new$datetime), format = "%H"))
train <- data.frame(train$season, month, weekday, hour, as.factor(train$workingday), as.factor(train$holiday), as.factor(train$weather), train$temp, train$hum, train$windspeed, train$count)
names(train) <- c("계절", "달", "평일", "시간", "평일여부", "공휴일여부", "날씨", "온도", "습도", "풍속", "이용수")



# train_factor$day <- weekdays(as.Date(train_factor$datetime))
# train_factor$day <- as.factor(train_factor$day)


train <- train[which(train$풍속 != 0.0000),]
count.summary <- ddply(train,.(계절, 달, 평일, 시간, 평일여부, 공휴일여부, 날씨), summarise, 온도 = mean(온도), 습도 = mean(습도), 풍속 = mean(풍속), 이용수 = mean(이용수))

train.select <- data.frame(train$계절, train$달, train$평일, train$시간, as.integer(train$평일여부), as.integer(train$공휴일여부), as.integer(train$날씨), train$온도, train$습도, train$풍속, train$이용수)
names(train.select) <- c("계절", "달", "평일", "시간", "평일여부", "공휴일여부", "날씨", "온도", "습도", "풍속", "이용수")



###### 사람들은 봄에 가장 적게 타고 가을에 가장 많이 탄다. 
###### 겨울이 가장 적게 탈 것 같은데 의외의 결과
season.summary <- ddply(train.select,.(계절,시간),
                        summarise, 이용수 = mean(이용수))

ggplot(train.select, aes(x = as.factor(시간), y = 이용수, colour = as.factor(계절))) +
  geom_point(data = season.summary, aes(group = as.factor(계절))) +
  geom_line(data = season.summary, aes(group = as.factor(계절))) +
  scale_x_discrete("시간") +
  scale_y_continuous("이용수") +
  theme_minimal() +
  ggtitle("사람들은 봄에 가장 적게 타고 가을에 가장 많이 탄다\n") + 
  scale_color_discrete("",labels=c("봄","여름","가을","겨울"))
theme(plot.title=element_text(size=18))


##### 왜
##### 주말에는 낮에 많이 탐. 평일에는 아침과 저녁. 확실히 통근하는 데에 이용하는 것으로 보임. 
day.summary <- ddply(train.select,.(평일=as.factor(평일),시간=as.factor(시간)),
                     summarise, 이용수 = mean(이용수))

ggplot(train.select, aes(x = as.factor(시간), y = 이용수, colour = as.factor(평일))) +
  geom_point(data = day.summary, aes(group=as.factor(평일))) +
  geom_line(data = day.summary, aes(group=as.factor(평일))) +
  scale_x_discrete("시간") +
  scale_y_continuous("이용수") +
  theme_minimal() +
  scale_color_discrete("요일",labels=c("월","화","수","목","금","토","일")) +
  ggtitle("주말에는 낮, 평일에는 아침과 저녁에 많이 탄다 \n")



###### 시간대 별로 날씨가 보통일 확률
weather.prob <- ddply(train.select,.(계절 = as.factor(계절), 시간 = as.factor(시간)),
                      summarise, 좋음 = mean(날씨 == "1"),
                      보통 = mean(날씨 == "2"),
                      나쁨 = mean(날씨 == "3"),
                      매우나쁨 = mean(날씨 == "4"))


ggplot(train.select, aes(x = 시간, y = 보통, colour = 계절)) +
  geom_point(data = weather.prob, aes(group = 계절)) +
  geom_line(data = weather.prob, aes(group = 계절)) +
  scale_x_discrete("시간") +
  scale_y_continuous("보통인 날의 비율") +
  theme_minimal() +
  ggtitle("아침시간에는 겨울이 날이 보통일 확률이 확실히 높다.  \n") + 
  scale_color_discrete("",labels=c("봄","여름","가을","겨울")) +
  theme(plot.title=element_text(size=18))


###### 사람들은 오전시간에 날이 보통인 날 더 많이 이용해. 그래서..... 겨울이 더 높다
weather.summary <- ddply(train.select,.(날씨,시간),
                         summarise, 이용수 = mean(이용수))

ggplot(train.select, aes(x = as.factor(시간), y = 이용수, colour = as.factor(날씨))) +
  geom_point(data = weather.summary, aes(group = as.factor(날씨))) +
  geom_line(data = weather.summary, aes(group = as.factor(날씨))) +
  scale_x_discrete("시간") +
  scale_y_continuous("이용수") +
  theme_minimal() +
  ggtitle("오전시간에는 날이 보통인 날 더 많이 이용한다. \n") + 
  scale_color_discrete( name = "날씨",
                        labels = c("맑음/구름", 
                                   "안개", 
                                   "가벼운비", 
                                   "폭풍우"))+
  theme(plot.title=element_text(size=18))















#######################################################################################################################










# 상관분석 및 다중 선형 회귀


train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
head(train)

# 전처리
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
train$datetime <-as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")

test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
test$datetime <-as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")

train$day <-  strftime(train$datetime, '%u') # 요일
train$day <- as.factor(train$day)
test$day <-  strftime(test$datetime, '%u')
test$day <- as.factor(test$day)

train$hour <- substring(train$datetime, 12,13)
train$hour <- as.factor(train$hour)
test$hour <- substring(test$datetime, 12,13)
test$hour <- as.factor(test$hour)

train=subset(train, select=-c(datetime,casual,registered)) 
# casual과 registered의 자전거 대여수의 합은 결국 count의 합과 같기 때문에 제거 했음
# date는 연속형 범주형이기 때문에 제거했음

# 상관분석 

train_subset=subset(train, select = c(temp,atemp,humidity,windspeed,count))
# 기상간에 상관관계와 기상과 count의 상관관계를 파악하기 위해 기상에 관련된 변수와 count만 뽑음 

train_subset$humidity <- as.numeric(train_subset$humidity)
train_subset$count <- as.numeric(train_subset$count)

train_cor <- cor(train_subset)
corrplot(train_cor, method = 'color', addCoef.col="black")
# temp와 atemp의 상관관계가 높은 것으로 나타남

# 산점도 행렬 (상관분석 방법2)
pairs.panels(train_subset)

# 데이터 분할 
set.seed(123)
split <- sample.split(train$count, SplitRatio = 0.7)
training <- subset(train, split == TRUE)
validation <- subset(train, split == FALSE)

# 선형회귀
bikerent <- lm(count~., data = training) 
summary(bikerent) 

# 변수선택

selection<-stepAIC(bikerent, direction="both") # 혼합선택법을 사용한 변수선택
summary(selection)
formula(selection) # 변수 선택 결과

# 검증셋 예측
predict_validation <- predict(selection, newdata = validation)

# rmse
validaion_rmse<-rmse(validation$count,predict_validation)
print(validaion_rmse)

# 실제값
summary(validation$count)

# 예측값
summary(predict_validation) # 예측값이 음수가 나온 것이 존재, 따라서 log화를 시켜서 진행

# 로그화 시킨후 선형회귀 
log=lm(log(count)~., data = training)
logselection <- stepAIC(log, direction="both") # 위와 동일하게 진행 
predict_validation_log <- predict(logselection,newdata=validation)

# 지수함수를 이용해 log -> non-log 값으로 변경
predict_validation_nonlog <- exp(predict_validation_log)
summary(predict_validation_nonlog) # 음수값이 존재X

# RMSLE
# 과대평가 된 항목보다는 과소평가 된 항목에 페널티를 준다.
#오차(Error)를 제곱(Square)해서 평균(Mean)한 값의 제곱근(Root)으로 값이 작을수록 정밀도가 높다.
# 0에 가까운 값이 나올수록 정밀도가 높은 값이다.

rmsle(validation$count,predict_validation_nonlog) # RMSLE 함수사용, 매우 유의하게 나옴

# test셋 예측
predict_test_log <- predict(logselection,newdata=test)
predict_test_nonlog <- exp(predict_test_log) # log -> non-log
predict <- cbind(as.data.frame(predict_test_nonlog), test$datetime)
colnames(predict) <- c("count", "datetime")
head(predict)

# 따릉이 수요 예측
rm(list=ls())
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(readr)) install.packages("readr"); library(readr)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(plyr)) install.packages("plyr"); library(plyr)
if(!require(reshape)) install.packages("reshape"); library(reshape)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(gam)) install.packages("gam"); library(gam)
if(!require(Metrics)) install.packages("Metrics"); library(Metrics)
if(!require(caTools)) install.packages("caTools"); library(caTools)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)
if(!require(psych)) install.packages("psych"); library(psych)

train <- read.csv("train.csv")
head(train)

korea=read.csv("usebike.csv")
head(korea)


train$season <- as.factor(train$season)
train$weather <- as.factor(train$weather)
train$datetime <-as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")

korea$season <- as.factor(korea$season)
korea$weather <- as.factor(korea$weather)
korea$datetime <-as.POSIXct(korea$datetime, format="%Y-%m-%d")

train$datetime=as.Date(train$datetime, format = "%Y-%m-%d")
korea$datetime=as.Date(korea$datetime, format = "%Y-%m-%d")
kor_date=korea$datetime
kor_count=korea$count
korea2=korea # 백업

train=subset(train, select=c(temp,humidity,weather,windspeed,atemp,season,count)) # 국내데이터 변수와 동일하게 해줌
korea=korea[,-1:-2]

# 데이터 분할 
set.seed(123)
split <- sample.split(train$count, SplitRatio = 0.7)
training <- subset(train, split == TRUE)
validation <- subset(train, split == FALSE)

# 선형회귀
bikerent <- lm(count~., data = training) 
summary(bikerent) 

# 변수선택

selection<-stepAIC(bikerent, direction="both") # 혼합선택법을 사용한 변수선택
summary(selection)
formula(selection) # 변수 선택 결과

# 검증셋 예측
predict_validation <- predict(selection, newdata = validation)

# rmse
validaion_rmse<-rmse(validation$count,predict_validation)
print(validaion_rmse)

# 실제값
summary(validation$count)

# 예측값
summary(predict_validation) # 예측값이 음수가 나온 것이 존재, 따라서 log화를 시켜서 진행

# 로그화 시킨후 선형회귀 
log=lm(log(count)~., data = training)
logselection <- stepAIC(log, direction="both") # 위와 동일하게 진행
formula(logselection)
predict_validation_log <- predict(logselection,newdata=validation)

# 지수함수를 이용해 log -> non-log 값으로 변경
predict_validation_nonlog <- exp(predict_validation_log)
summary(predict_validation_nonlog) # 음수값이 존재X

rmsle(validation$count,predict_validation_nonlog) 

# test셋 예측
predict_test_log <- predict(logselection,newdata=korea)
predict_test_nonlog <- exp(predict_test_log) # log -> non-log
predict <- cbind(as.data.frame(kor_date), predict_test_nonlog)
colnames(predict) <- c("datetime","count")

head(predict) # 예측값
head(korea2[,1:2]) # 실제값

rmsle(predict$count,korea2$count)
