library(dplyr)
library(PCAmixdata)
library(ggplot2)

setwd("C:/Users/Kim Seok Joon/Desktop/새 폴더")
df=read.csv("top_accident.csv")

df=df[,c(36:37,40,46:47)]
head(df)

df2=df %>% count(포장구분,평면선형,종단경사,방책시설..중분대.,방책시설..갓길.,
                     sort = TRUE)
df2=data.frame(df2)
colnames(df2)[6]="사고건수"


# PCA
x.quali= df2[,1:5]

pca=PCAmix(,x.quali,ndim=2,graph=TRUE)

#plot(pca$ind$coord[,1],df2$`사고 건수`)

df2$pca= pca$ind$coord[,1]


#p=ggplot(df2,aes(x=pca,y=사고건수))+geom_point(size=4)+ theme_bw()


p=ggplot(df2,aes(x=pca,y=사고건수,col=사고건수<20))+geom_point(size=5) + theme_bw()+ xlab("도로 환경적 요인")+ theme(text = element_text(size = 25))
p

#theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))
#p+theme

#p+stat_ellipse(type="norm")+ theme_bw() + theme(text = element_text(size = 25)) + xlab("도로 특성")

# result
df3=subset(df2,사고건수>20)
df3[-7]

a=subset(df2,사고건수<20)
a

#result=read.csv("pca_result.csv") # 엑셀작업 

result=read.csv("pca_result_final.csv")
sort(table(result$노선명))
