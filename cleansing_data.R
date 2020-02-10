library(tidyverse)
data <- read.csv("./data/seoulsi.csv")
View(data)

jja <- data.frame()
for (i in seq(1989, 2018, 1)) {
  june <- data[grep(paste0(i, ".06"),data$기간),]
  july <- data[grep(paste0(i, ".07"),data$기간),]
  august <- data[grep(paste0(i, ".08"),data$기간),]
  jja <- rbind(jja, june, july, august)
}

jja <- jja[,-c(2)]

colnames(jja) <- c("년도","평균기온","평균최고기온","극점최고기온",
                   "평균최저기온","극점최저기온","강수량",
                   "평균습도","최소습도","평균해면기압",
                   "이슬점온도","평균운량","일조시간",
                   "평균풍속","최대풍속")
jja$강수량 <- as.numeric(jja$강수량)
jja$이슬점온도 <- as.numeric(jja$이슬점온도)

View(jja)
sdf <- jja %>%
  group_by(substr(년도,1,4)) %>% 
  summarise(평균기온=mean(평균기온), 평균최고기온=mean(평균최고기온),
            평균극점최고기온=mean(극점최고기온),극점최고기온=max(극점최고기온),
            평균최저기온=mean(평균최저기온),평균극점최저기온=mean(극점최저기온),
            극점최저기온=min(극점최저기온),강수량=mean(강수량),
            평균습도=mean(평균습도),최소습도=mean(최소습도),
            평균해면기압=mean(평균해면기압),이슬점온도=mean(이슬점온도),
            평균운량=mean(평균운량),일조시간=mean(일조시간),
            평균풍속=mean(평균풍속),최대풍속=mean(최대풍속))
                
names(sdf)[names(sdf) == "substr(년도, 1, 4)"] <- c("년도")
names(sdf)
View(sdf)

heatdata <- read.csv("./data/dataheatindex.csv")

heat <- heatdata %>% 
  group_by(year) %>% 
  summarise(열지수=mean(heatindex),최대열지수=max(heatindex))
View(heat)

summer <- cbind(sdf,heat)
summer <- summer[,-c(18)]
View(summer)

djf <- data.frame()
for (i in seq(1989, 2019, 1)) {
  december <- data[grep(paste0(i, ".12"),data$기간),]
  january <- data[grep(paste0(i+1, ".01"),data$기간),]
  febrary <- data[grep(paste0(i+1, ".02"),data$기간),]
  group <- rbind(december,january,febrary)
  group$year <- i
  djf <- rbind(djf,group)
}

djf <- djf[,-c(2)]
View(djf)
colnames(djf) <- c("년도","평균기온","평균최고기온","극점최고기온",
                   "평균최저기온","극점최저기온","강수량",
                   "평균습도","최소습도","평균해면기압",
                   "이슬점온도","평균운량","일조시간",
                   "평균풍속","최대풍속","year")
View(djf)
djf <- djf %>%
  group_by(year) %>% 
  summarise(겨울평균기온=mean(평균기온), 겨울평균최고기온=mean(평균최고기온),
                겨울평균극점최고기온=mean(극점최고기온),겨울극점최고기온=max(극점최고기온),
                겨울평균최저기온=mean(평균최저기온),겨울평균극점최저기온=mean(극점최저기온),
                겨울극점최저기온=min(극점최저기온),겨울강수량=mean(강수량),
                겨울평균습도=mean(평균습도),겨울최소습도=mean(최소습도),
                겨울평균해면기압=mean(평균해면기압),겨울이슬점온도=mean(이슬점온도),
                겨울평균운량=mean(평균운량),겨울일조시간=mean(일조시간),
                겨울평균풍속=mean(평균풍속),겨울최대풍속=mean(최대풍속))
djf <- djf[,-c(1,9:17)]
View(djf)
summer_winter_df <- cbind(summer,djf)
View(summer_winter_df)

#데이터 정규화
scale_df <- as.data.frame(scale(summer_winter_df[2:26]))
View(scale_df)

#데이터 나누기
summer_winter_average <- scale_df[,-c(20:25)] #19
summer_winter_average_max <- scale_df[,-c(19,21:25)] #20
summer_winter_average_pole_max <- scale_df[,-c(19:20,22:25)] #21
summer_winter_pole_max <- scale_df[,-c(19:21,23:25)] #22
summer_winter_average_min <- scale_df[,-c(19:22,24:25)] #23
summer_winter_average_pole_min <- scale_df[,-c(19:23,25)] #24
summer_winter_pole_min <- scale_df[,-c(19:24)] #25

#데이터 구조확인
str(summer_winter_average)
summary(summer_winter_average)

str(summer_winter_average_max)
summary(summer_winter_average_max)

str(summer_winter_average_pole_max)
summary(summer_winter_average_pole_max)

str(summer_winter_pole_max)
summary(summer_winter_pole_max)

str(summer_winter_average_min)
summary(summer_winter_average_min)

str(summer_winter_average_pole_min)
summary(summer_winter_average_pole_min)

str(summer_winter_pole_min)
summary(summer_winter_pole_min)

#상관관계분석
library(PerformanceAnalytics)
chart.Correlation(summer_winter_average[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average")

chart.Correlation(summer_winter_average_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_max")

chart.Correlation(summer_winter_average_pole_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_pole_max")

chart.Correlation(summer_winter_pole_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_pole_max")

chart.Correlation(summer_winter_average_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_min")

chart.Correlation(summer_winter_average_pole_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_pole_min")

chart.Correlation(summer_winter_pole_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_pole_min")

#ggcorr plot 그리기
library(ggplot2)
library(GGally)

ggcorr(summer_winter_average[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_average_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_average_pole_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_pole_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_pole_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_pole_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_average_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_average_pole_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_pole_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(summer_winter_pole_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_pole_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#주성분분석
library(factoextra)
df_pca <- transform(summer_winter_df)
all_pca <- prcomp(df_pca[,-1],cor=TRUE,scale=TRUE)
summary(all_pca)

fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter All Variances - PCA",
       x = "Principal Components", y = "% of variances")

swam_pca <- transform(summer_winter_average_min)
swam_all_pca <- prcomp(swam_pca,cor=TRUE,scale=TRUE)
summary(swam_all_pca)

fviz_eig(swam_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average_min Variances - PCA",
       x = "Principal Components", y = "% of variances")

#중요 변수 확인하기
all_var <- get_pca_var(all_pca)
all_var
library(corrplot)
corrplot(all_var$cos2,is.corr = FALSE)

swam_var <- get_pca_var(swam_all_pca)
swam_var
corrplot(swam_var$cos2, is.corr=FALSE)

set.seed(218)
res.all <- kmeans(all_var$coord, centers = 5, nstart = 25)
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")

set.seed(218)
res.average_min <- kmeans(swam_var$coord, centers = 6, nstart = 25)
grp1 <- as.factor(res.average_min$cluster)

fviz_pca_var(swam_all_pca, col.var = grp1, 
             palette = "jco",
             legend.title = "Cluster")
