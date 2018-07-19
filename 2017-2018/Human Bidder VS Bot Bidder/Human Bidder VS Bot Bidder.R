setwd("C:/Users/Kim Seok Joon/Desktop/전공/탐색적데이터분석/탐데분플젝")

install.packages("doMC", repos="http://R-Forge.R-project.org")
library("doMC")
getDoParWorkers()
registerDoMC(cores = 2)

library(data.table)
bids=fread("bids.csv",verbose = T)
head(bids)

train=fread("train.csv", verbose=T)
bro=fread("human or bot_파생변수.csv",verbose=T)
bro$who=bro[,3]
bro=bro[,-3]
head(bro)

colnames(bro)[2:10]=c("총입찰건수","평균경매식별코드개수","평균URL개수","전화모델개수","경매식별코드개수","입력된국가개수",
                       "IP개수","URL개수","그룹")
head(bro)

final=merge(bids,train, by="bidder_id")


#인간과 로봇의 비율
library(dplyr)
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)
install.packages("forcats")
library(forcats)
library(scales)
bot=merge(distinct(train, bidder_id),train)[,-2:-3]

bot<-bot%>%
  group_by(outcome)%>%
  summarise(n())
bot$outcome<-as.factor(bot$outcome)
bot

bot %>%
  arrange(desc(`n()`)) %>%
  mutate(prop = percent(`n()` / sum(`n()`))) -> bot 

pie <- ggplot(bot, aes(x = "", y = `n()`, fill = fct_inorder(outcome))) +
  geom_bar(width = 1, stat = "identity") +
  ggtitle("인간과 로봇의 비율") +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))+
  labs(x="", y="비율")


windows() 
pie

#인간과 로봇의 총입찰건수


a<-final%>%
  group_by(outcome)%>%
  summarise(n())
a$outcome<-as.factor(a$outcome)

a %>%
  arrange(desc(`n()`)) %>%
  mutate(prop = percent(`n()` / sum(`n()`))) -> a 

pie <- ggplot(a, aes(x = "", y = `n()`, fill = fct_inorder(outcome))) +
  geom_bar(width = 1, stat = "identity") +
  ggtitle("인간과 로봇의 총입찰건수 비율") +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group")) +
  labs(x="", y="비율")

windows() 
pie


#그룹별 물품별 비율
library(ggplot2)
library(dplyr)
a<-final%>%
  group_by(outcome,merchandise)%>%
  summarise(n())

a$outcome<-as.factor(a$outcome)
windows()
ggplot(a, aes(x=a$merchandise,y=a$`n()`,fill=outcome ))+geom_bar(stat="identity",position="fill") +
  ggtitle("그룹별 물품별 비율") +
  geom_hline(yintercept = 0.138,
             color = "#25a1c4", size=1.5) +
  labs(x="상품", y="비율")



###########################################그룹별 물품별 구매횟수 +구매빈도 비율
m1<-final %>%
  group_by(outcome,merchandise)%>%
  summarise(m1=length(merchandise))
head(m1)

m2<-final %>%
  group_by(outcome,merchandise)%>%
  summarise(m2=length(merchandise))%>%
  summarise(sum(m2))
head(m2)

m3<-merge(m1,m2)
m3$mean=round(m3$m1/m3$`sum(m2)`,4)
m3<-m3[-3:-4]
m3$mean=round(m3$mean,4)
head(m3)

m3=xtabs(mean~outcome+merchandise,data=m3)
sort(m3)

v1<-final %>%
  group_by(outcome)%>%
  summarise(m1=length(merchandise))
head(v1)

v2<-final %>%
  group_by(outcome)%>%
  summarise(m2=length(merchandise))%>%
  summarise(m=sum(m2))
head(v2)

v3=merge(v1,v2)
v3$m=round(v3$m1/v3$m,4)
v3=v3[,-2]
v3$m=round(v3$m,2)
head(v3)

v3=xtabs(m~outcome,data=v3)
head(v3)
m3=cbind(v3,m3)
head(m3)

colnames(m3)[1]="Proportion"

par(mar=c(4,4,4,4))

windows()
barplot(m3,beside = TRUE,ylim=c(0,0.9),
        main="인간과 로봇의 물품별 구매빈도의 분포",col=color
        ,xlab="merchandise",ylab="frequency",args.legend=list(x='topright',inset=c(-0.2,0),cex=0.8))
?barplot
legend(locator(1),c("Human","Bot"), fill = color,cex=1.5, title = "Group")
m2=c(1:2,4:5,7:8,10:11,13:14,16:17,19:20,22:23,25:26,28:29,31:32)+0.5
text(m2,m3+0.02,m3)
abline(h=m3[,1],col=rainbow(c(1,3)))


#그룹별로 입찰건에 대한 나라 분포도
a<-final %>%
  group_by(country,outcome)%>%
  summarise(a=length(country))
head(a)
a=a[-1:-2,]

b<-final %>%
  group_by(country,outcome)%>%
  summarise(a=length(country))%>%
  summarise(sum(a))
b=b[-1,]
head(b)

c<-merge(a,b)
c$mean=c$a/c$`sum(a)`
c<-c[-3:-4]
head(c)

library(portfolio)

d=final %>%
  group_by(country,outcome)%>%
  summarise(frequency=length(country))

d=merge(c,d)
d=d[order(d$frequency, decreasing = T),]

windows()
map.market(id=d$outcome, area =d$frequency, group = d$country,
           lab=c(T,T), color = sqrt(d$mean)-sqrt(median(d$mean)),main="그룹별 입찰건에 대한 나라 분포도")
subset(d, country=="us") # us는 비율이 비슷해서 구별이 명확하게 안됨

head(c)


#그룹별 평균 구매횟수과 평균 입찰시간과의 이변량 분석 


a1<-final %>%
  group_by(outcome)%>%
  summarise(a1=length(bidder_id)/length(unique(bidder_id)))


head(a1)

library(dplyr)

sort(final$time,decreasing = T)

g=final[order(final$time,decreasing = F),]
head(g)
sort(final$time,decreasing = T)

midtime.bid <-g %>%
  group_by(outcome) %>%
  summarise(mean(diff(time)))

head(midtime.bid)

freqtime=merge(a1,midtime.bid)

head(freqtime)
sort(freqtime$`mean(diff(time))`)
freqtime$sqrt=sqrt(freqtime$`mean(diff(time))`)
freqtime$sqrtbuy=sqrt(freqtime$a1)
head(freqtime)

library(MASS)
density=kde2d(freqtime$sqrtbuy, freqtime$sqrt, n=500)
image(density, xlab="frequency", ylab="time",main="그룹별 평균 구매횟수와 평균 입찰시간")
contour(density)

library(rgl)
persp3d(density, back="lines", col="skyblue", 
        xlab="frequency", ylab="time")


#####유동IP를 사용한 우회 입찰자_버블차트 
 
head(bro)
radius=sqrt(bro$총입찰건수)
windows()
symbols(bro$IP개수,bro$입력된국가개수,circles = radius,inches = 0.5,
        fg="white",bg="#42bff4",xlab = "The Average Amount of IP",ylab="The Average Amount of Country",main = "유동IP를 사용한 우회 입찰자")
text(bro$IP개수,bro$입력된국가개수,bro$그룹,cex=1)

ab=bro[order(bro$IP개수,decreasing = T)]
head(ab)
radius2=sqrt(ab$총입찰건수)
ab[2:1984,]=NA
symbols(ab$IP개수,ab$입력된국가개수,circles = radius2,inches = 0.5,
        fg="white",bg="#2fd83d",xlab = "The Average Amount of IP",ylab="The Average Amount of Country",main = "유동IP를 사용한 우회 입찰자",add=TRUE)
text(ab$IP개수,ab$입력된국가개수,ab$그룹,cex=1)



#유동IP를 사용한 상위 10%의 우회 입찰자의 총입찰건수_평행좌표플롯
library(lattice)
parallel(bro[,2:9],horizontal.axis=FALSE,col=1)

color=as.numeric(bro$총입찰건수>quantile(bro$총입찰건수,.90))+1


windows()
parallelplot(bro[,2:9],horizontal.axis=FALSE, col=color, main="상위 10% 입찰자")



n=bro[bro$총입찰건수>quantile(bro$총입찰건수,.90)]

#######################
#그룹별 다변량 비교 시각
head(bro)
str(bro)
other= transform(bro,
                 그룹번호 = ifelse(그룹 == "0", 1,
                              ifelse(그룹 == "1",2,3)))

head(other)
head(other[,c("그룹","그룹번호")])
library(MASS)
windows()
parcoord(other[,2:9],
         lty=other$그룹번호,
         col=other$그룹번호,
         var.label = TRUE,      #var.label = TRUE 옵션을 설정하면 각 변수별로 minimum value, maximum value가 하단과 상단에 표기됨.
         main="그룹별 다변량 비교 시각화")

legend("topright",
       legend = c("Human","Bot"),
       lty =c(1:2),
       col =c(1:2),
       lwd =2,
       cex=0.7)


#spider

bro$구별=table(bro$그룹)
install.packages("doBy")
library(doBy)


mean_by_구별= summaryBy(총입찰건수+평균경매식별코드개수+평균URL개수+
                             전화모델개수+경매식별코드개수+입력된국가개수+
                             IP개수+URL개수~구별,
                      data=bro,
                      FUN = c(mean))
mean_by_구별

install.packages("fmsb")
library(fmsb)

df_radarchart <- function(df) { 
  df <- data.frame(df)
  dfmax <- apply(df, 2, max)  
  dfmin <- apply(df, 2, min)  
  as.data.frame(rbind(dfmax, dfmin, df))
  }

mean_by_Type_trans <- df_radarchart(scale(mean_by_구별[,c(2:9)]))
mean_by_Type_trans

windows()
radarchart(df = mean_by_Type_trans,
           seg = 6,
           pty = 16,
           pcol = c("#42bff4","#2fd83d"),
           plty = 1:6,
           plwd = 2,
           title = "그룹별 다변량 비교 시각화")

legend("topleft", legend = c("Human","Bot"), col=c("#42bff4","#2fd83d"),
       lty=c(1:2), lwd=2)
