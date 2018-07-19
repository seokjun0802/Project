library(cluster)
library(MASS)
library(PCAmixdata)
library(ggplot2)
library(gridExtra)
library(caret)
library(nnet)
library(pscl)

setwd("C:/Users/Kim Seok Joon/Desktop/전공/다변량통계분석/다변량 프로젝트")
automobile=read.csv("자동차.csv")
head(automobile)
str(automobile)

# 국산차 vs 외제차 판매가 비교 
ggplot(data = automobile, aes(x = 나라, y = 판매가)) +
  geom_boxplot(width = 0.8, outlier.size = 2 , outlier.color = "red") +
  stat_summary(fun.y="mean", geom="point", shape=21, size=3, fill="blue") 

# 수치형 변수 스케일
ind = sapply(automobile, is.numeric) 
df_scale=automobile
df_scale[ind] = lapply(automobile[ind], scale)

# 범주형 변수이랑 수치형 변수를 나눠줌 
head(df_scale)
x.quanti = df_scale[,c(4:7,11,13:21)] # 수치형
x.quali = df_scale[,c(8:10,12)] # 범주형 

# 2차원축으로 pca를 실행
pca=PCAmix(x.quanti,x.quali,ndim=2,graph=TRUE)
pca$eig
pca$ind$coord

# pca 좌표 플롯
par(mfcol=c(1,2))
plot(pca,choice="ind",main="numeric+factor") # ind : results for the individuals after rotation (coord)

# 첫 주성분을 사용해 Q-Q plot을 그리고 해석.
qqnorm(pca$ind$coord[,1])
qqline(pca$ind$coord[,1])
# Q-Q plot에서 대각선을 기준으로 산점도 점들이
# 가깝게 선형을 이루고 있으므로 정규성을 띈다고 평가할 수 있다.

shapiro.test(pca$ind$coord[,1])
# p-value>0.05 이므로 정규분포를 따른다고 할 수 있다.

# 수치형만
head(df_scale)
x.quanti = df_scale[,c(4:7,11,13:21)] # 수치형

# 2차원축으로 pca를 실행
pca2=PCAmix(x.quanti,ndim=2,graph=TRUE)
pca2$eig
pca2$ind$coord

# pca 좌표 플롯 
plot(pca2,choice="ind",main="numeric")
text(pca2$ind$coord[,1], pca2$ind$coord[,2], labels = rownames(automobile), cex=0.8, pos=4)

# 첫 주성분을 사용해 Q-Q plot을 그리고 해석.
qqnorm(pca2$ind$coord[,1])
qqline(pca2$ind$coord[,1])
# Q-Q plot에서 대각선을 기준으로 산점도 점들이
# 가깝게 선형을 이루고 있으므로 정규성을 띈다고 평가할 수 있다.

shapiro.test(pca2$ind$coord[,1])
# p-value>0.05 이므로 정규분포를 따른다고 할 수 있다.


# 데이터 프레임으전 전환 (수치형+범주형)
coords<-as.data.frame(pca$ind$coord)

# k means clustering
km <- kmeans(coords, centers = 5) # hyper-parameter
km

plot(coords[,1], coords[,2], col = km$cluster, pch = 20,main="numeric+factor")
text(coords[,1], coords[,2], labels = rownames(coords), cex=0.8, pos=4,col=km$cluster)
automobile$cluster<-as.factor(km$cluster)

# 데이터 프레임으전 전환 (범주형)
coords2<-as.data.frame(pca2$ind$coord)

# k means clustering
km2 <- kmeans(coords2, centers = 5) # hyper-parameter
km2

plot(coords2[,1], coords2[,2], col = km$cluster, pch = 20,main="numeric")
text(coords2[,1], coords2[,2], labels = rownames(coords), cex=0.8, pos=4,col=km$cluster)

# 국산차 vs 외제차 가성비 비교 
plot(coords[,1], automobile$판매가, col = km$cluster, pch = 20, cex=2)
text(coords[,1], automobile$판매가, labels = automobile$나라, cex=0.8, pos=4,col=km$cluster)
cor(coords[,1], automobile$판매가)
# 동가격대에서 국산차가 성능이 더 좋고 가격이 낮은 경우가 있다.
# 외제차의 가격은 대부분 높다. 즉, 거픔가가 껴있다!

######################################
##그렇다면 국산용 예측 모델
### 국산차로만 선형회귀모델을 만든 후 외제차 가격을 예측
a<-automobile
a2<-automobile[automobile$나라=="국산",]
head(a2)
dim(a2)
r<-a2[21,] # 예측할 차종은 임의적으로 선택
a2r<-a2[-21,]
b2r<-a2r[,!colnames(a) %in% c("차종","브랜드","엔진형식","나라","구동방식","미션형식1","미션형식2")] 
c2r<-b2r[,colnames(b2r) %in% c("공식연비","배기량","최대토크","폭","판매가")]
c2r
r
colnames(c2r)
pr0_2<-lm(판매가~.,data=b2r)
pr3_2<-lm(판매가~ .,data=c2r)

r$판매가 #3440
predict(pr0_2,r)
predict(pr3_2,r)

library(caret)
idx <- createDataPartition(a2$판매가, p=.7, list=F)
train <- a2[idx,]
test <- a2[-idx,]
train<-a2r[,!colnames(a2) %in% c("차종","브랜드","엔진형식","나라","구동방식","미션형식1","미션형식2")]
train<-b2r[,colnames(train) %in% c("공식연비","배기량","최대토크","폭","판매가")]
predict_train <- lm(판매가 ~., data=train)
a2[30,]
a[30,]
predict(predict_train,a2[30,])
predict(predict_train, a[30,])
#####################################

#이 모델로 외제차를 함 보자
a[17,] 
predict(pr0_2,a[17,]) 
# 국산차 훈련 모델로 외제차를 예측했을때 가격이 반 가격이 됨
# 즉, 외제차의 판매가가 거품이다!

# visualization

# 군집별 성능비교
auto_scale=scale(automobile[,c(4,5,6,7,13)])
auto_scale=as.data.frame(auto_scale)
auto_scale$cluster<-as.factor(km$cluster)

boxplot(auto_scale[auto_scale$cluster==1,], main = paste("Cluster",1), las=2)
boxplot(auto_scale[auto_scale$cluster==2,], main = paste("Cluster",2), las=2)
boxplot(auto_scale[auto_scale$cluster==3,], main = paste("Cluster",3), las=2)
boxplot(auto_scale[auto_scale$cluster==4,], main = paste("Cluster",4), las=2)
boxplot(auto_scale[auto_scale$cluster==5,], main = paste("Cluster",5), las=2)

# korea VS foreign

# 국산차 VS 외제차 클러스터 분포도 
ggplot(automobile, aes(나라,cluster,color=cluster))+ geom_jitter()

# 국산차 VS 외제차를 기준으로 모든 변수값 비교

a <- ggplot(automobile, aes(나라, 판매가,color=cluster))+ geom_boxplot()

b <- ggplot(automobile, aes(나라, 공식연비,color=cluster))+ geom_boxplot()

c <- ggplot(automobile, aes(나라, 배기량,color=cluster)) + geom_boxplot()
d <- ggplot(automobile, aes(나라, 최고출력,color=cluster)) + geom_boxplot()

e <- ggplot(automobile, aes(나라, 최대토크,color=cluster))+ geom_boxplot()

f <- ggplot(automobile, aes(나라, 연료탱크,color=cluster))+ geom_boxplot()

g <- ggplot(automobile, aes(나라, 공차중량,color=cluster))+ geom_boxplot()

h <- ggplot(automobile, aes(나라, 부피,color=cluster))+ geom_boxplot()

i <- ggplot(automobile, aes(나라, 면적,color=cluster))+ geom_boxplot()

j <- ggplot(automobile, aes(나라, 승차인원,color=cluster))+ geom_boxplot()

k <- ggplot(automobile, aes(나라, 도어수,color=cluster)) + geom_boxplot()

l <- ggplot(automobile, aes(나라, 타이어,color=cluster)) + geom_boxplot()

m <- ggplot(automobile, aes(나라,차종 ,color=cluster))+ geom_jitter()

n <- ggplot(automobile, aes(나라,브랜드 ,color=cluster))+ geom_jitter()

o <- ggplot(automobile, aes(나라,엔진형식,color=cluster))+ geom_jitter()

p <- ggplot(automobile, aes(나라,미션형식1 ,color=cluster))+ geom_jitter()

q <- ggplot(automobile, aes(나라 ,미션형식2,color=cluster))+ geom_jitter()

r <- ggplot(automobile, aes(나라 ,구동방식,color=cluster))+ geom_jitter()

grid.arrange(a,b,c,d,e,f,g,h,i,j, ncol=2,nrow=5)
grid.arrange(k,l,m,n,o,p,q,r, ncol=2,nrow=5)

##다항로지스틱회귀
# 차종 추전시스템 

car=automobile[,-1] # 차종 변수 삭제

set.seed(1004)
intrain=createDataPartition(y=car$cluster, p=0.7, list=FALSE)
train=car[intrain,]
test=car[-intrain,]

m <- multinom(cluster ~ ., data = train)
summary(m)

predict(m, newdata = test, type = "class") # 예측데이터가 속하는 군집
predict(m, newdata = test, type = "probs") # 예측데이터가 해당되는 군집에 속할 확률

predicted <- predict(m, newdata = test)
sum(predicted == test$cluster)/NROW(test) # 정확도 측정 

xtabs(~predicted + test$cluster) # 혼동행렬

# ex
# 가장 싸고, 평균적으로 성능을 내는, 그리고 브랜드는 기아(국산)꺼를 선호합니다 이에 맞는 차를 추천해주세요 
predict(m, newdata = data.frame(브랜드='기아',판매가=min(car$판매가),공식연비=mean(car$공식연비),배기량=mean(car$배기량),
                                   최고출력=mean(car$최고출력),최대토크=mean(car$최대토크),엔진형식='직렬4',미션형식1='자동',미션형식2='5단',
                                   연료탱크=mean(car$연료탱크),구동방식='가솔린',공차중량=mean(car$공차중량),길이=mean(car$길이),폭=mean(car$폭),높이=mean(car$높이), 부피=mean(car$부피),면적=mean(car$면적),
                                   승차인원=mean(car$승차인원),도어수=mean(car$도어수),
                                   타이어=20,나라='국산'), type = "class")
predict(m, newdata = data.frame(브랜드='기아',판매가=min(car$판매가),공식연비=mean(car$공식연비),배기량=mean(car$배기량),
                                   최고출력=mean(car$최고출력),최대토크=mean(car$최대토크),엔진형식='직렬4',미션형식1='자동',미션형식2='5단',
                                   연료탱크=mean(car$연료탱크),구동방식='가솔린',공차중량=mean(car$공차중량),길이=mean(car$길이),폭=mean(car$폭),높이=mean(car$높이), 부피=mean(car$부피),면적=mean(car$면적),
                                   승차인원=mean(car$승차인원),도어수=mean(car$도어수),
                                   타이어=20,나라='국산'), type = "class")

a=subset(automobile,automobile$cluster==2) 
b<- subset(a, 나라=="국산") # 해당되는 옵션에 근접하는 군집이므로 다른 차종까지 같이 나옴. 따라서 다시 해당 옵션을 필터링해줌
c<- subset(b, 브랜드=="기아")
c
