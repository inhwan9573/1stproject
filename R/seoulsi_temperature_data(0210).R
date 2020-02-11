#패키지 불러오기
library(tidyverse)

#데이터 불러오기
data <- read.csv("./data/seoulsi.csv")
View(data)

#여름철(6,7,8월) 데이터 만들기
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
#변수 수치화(reshape)
jja$강수량 <- as.numeric(jja$강수량)
jja$이슬점온도 <- as.numeric(jja$이슬점온도)
View(jja)
#분석을 위한 데이터 만들기
summer_df <- jja %>%
  group_by(substr(년도,1,4)) %>% 
  summarise(평균기온=mean(평균기온), 평균최고기온=mean(평균최고기온),
            평균극점최고기온=mean(극점최고기온),극점최고기온=max(극점최고기온),
            평균최저기온=mean(평균최저기온),평균극점최저기온=mean(극점최저기온),
            극점최저기온=min(극점최저기온),강수량=mean(강수량),
            평균습도=mean(평균습도),최소습도=mean(최소습도),
            평균해면기압=mean(평균해면기압),이슬점온도=mean(이슬점온도),
            평균운량=mean(평균운량),일조시간=mean(일조시간),
            평균풍속=mean(평균풍속),최대풍속=mean(최대풍속))
names(summer_df)[names(summer_df) == "substr(년도, 1, 4)"] <- c("년도")
names(summer_df)
View(summer_df)
#열지수 데이터 만들기
heatdata <- read.csv("./data/dataheatindex.csv")
heat_df <- heatdata %>% 
  group_by(year) %>% 
  summarise(열지수=mean(heatindex),최대열지수=max(heatindex))
View(heat_df)
#변수 삽입하기
summer <- cbind(summer_df,heat_df)
summer <- summer[,-c(18)]
View(summer)

#겨울철(12,1,2월) 데이터 만들기
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
winter_df <- djf %>%
  group_by(year) %>% 
  summarise(겨울평균기온=mean(평균기온), 겨울평균최고기온=mean(평균최고기온),
                겨울평균극점최고기온=mean(극점최고기온),겨울극점최고기온=max(극점최고기온),
                겨울평균최저기온=mean(평균최저기온),겨울평균극점최저기온=mean(극점최저기온),
                겨울극점최저기온=min(극점최저기온),겨울강수량=mean(강수량),
                겨울평균습도=mean(평균습도),겨울최소습도=mean(최소습도),
                겨울평균해면기압=mean(평균해면기압),겨울이슬점온도=mean(이슬점온도),
                겨울평균운량=mean(평균운량),겨울일조시간=mean(일조시간),
                겨울평균풍속=mean(평균풍속),겨울최대풍속=mean(최대풍속))
winter_df <- winter_df[,-c(1,9:17)]
View(winter_df)
summer_winter_df <- cbind(summer,winter_df)
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
#여름과 겨울평균기온
chart.Correlation(summer_winter_average[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average")
#여름과 겨울평균최고기온
chart.Correlation(summer_winter_average_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_max")
#여름과 겨울평균극점최고기온
chart.Correlation(summer_winter_average_pole_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_pole_max")
#여름과 겨울극점최고기온
chart.Correlation(summer_winter_pole_max[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_pole_max")
#여름과 겨울평균최저기온
chart.Correlation(summer_winter_average_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_min")
#여름과 겨울평균극점최저기온
chart.Correlation(summer_winter_average_pole_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_average_pole_min")
#여름과 겨울극점최저기온
chart.Correlation(summer_winter_pole_min[,c(1:19)], histogram = TRUE, col="grey10",
                  pch=1, main="summer - winter_pole_min")

#ggcorr plot 그리기
library(GGally)
#summer - winter_average
ggcorr(summer_winter_average[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_average_max
ggcorr(summer_winter_average_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_average_pole_max
ggcorr(summer_winter_average_pole_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_pole_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_pole_max
ggcorr(summer_winter_pole_max[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_pole_max")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_average_min
ggcorr(summer_winter_average_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_average_pole_min
ggcorr(summer_winter_average_pole_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_average_pole_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))
#summer - winter_pole_min
ggcorr(summer_winter_pole_min[,c(1:19)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="summer - winter_pole_min")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.1,size=14))

#주성분분석
#all
library(factoextra)
df_pca <- transform(summer_winter_df)
all_pca <- prcomp(df_pca[,-1],cor=TRUE,scale=TRUE)
summary(all_pca)

fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter All Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swa
swa_pca <- transform(summer_winter_average)
swa_all_pca <- prcomp(swa_pca,cor=TRUE,scale=TRUE)
summary(swa_all_pca)

fviz_eig(swa_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swam
swam_pca <- transform(summer_winter_average_max)
swam_all_pca <- prcomp(swam_pca,cor=TRUE,scale=TRUE)
summary(swam_all_pca)

fviz_eig(swam_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average_max Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swapm
swapm_pca <- transform(summer_winter_average_pole_max)
swapm_all_pca <- prcomp(swapm_pca,cor=TRUE,scale=TRUE)
summary(swapm_all_pca)

fviz_eig(swapm_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average_pole_max Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swpm
swpm_pca <- transform(summer_winter_pole_max)
swpm_all_pca <- prcomp(swpm_pca,cor=TRUE,scale=TRUE)
summary(swpm_all_pca)

fviz_eig(swpm_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_pole_max Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swami
swami_pca <- transform(summer_winter_average_min)
swami_all_pca <- prcomp(swami_pca,cor=TRUE,scale=TRUE)
summary(swami_all_pca)

fviz_eig(swami_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average_min Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swapmi
swapmi_pca <- transform(summer_winter_average_pole_min)
swapmi_all_pca <- prcomp(swapmi_pca,cor=TRUE,scale=TRUE)
summary(swapmi_all_pca)

fviz_eig(swapmi_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_average_pole_min Variances - PCA",
       x = "Principal Components", y = "% of variances")

#swpmi
swpmi_pca <- transform(summer_winter_pole_min)
swpmi_all_pca <- prcomp(swpmi_pca,cor=TRUE,scale=TRUE)
summary(swpmi_all_pca)

fviz_eig(swpmi_all_pca, addlabels=TRUE, ylim=c(0,50), geom = c("bar", "line"),
         barfill = "skyblue", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "summer-winter_pole_min Variances - PCA",
       x = "Principal Components", y = "% of variances")

#중요 변수 확인하기
#all
library(corrplot)
all_var <- get_pca_var(all_pca)
all_var
corrplot(all_var$cos2,is.corr = FALSE)

#swa
swa_var <- get_pca_var(swa_all_pca)
swa_var
corrplot(swa_var$cos2,is.corr = FALSE)

#swam
swam_var <- get_pca_var(swam_all_pca)
swam_var
corrplot(swam_var$cos2,is.corr = FALSE)

#swapm
swapm_var <- get_pca_var(swapm_all_pca)
swapm_var
corrplot(swapm_var$cos2,is.corr = FALSE)

#swpm
swpm_var <- get_pca_var(swpm_all_pca)
swpm_var
corrplot(swpm_var$cos2,is.corr = FALSE)

#swami
swami_var <- get_pca_var(swami_all_pca)
swami_var
corrplot(swami_var$cos2,is.corr = FALSE)

#swapmi
swapmi_var <- get_pca_var(swapm_all_pca)
swapm_var
corrplot(swapm_var$cos2,is.corr = FALSE)

#swpmi
swpmi_var <- get_pca_var(swpmi_all_pca)
swpmi_var
corrplot(swpmi_var$cos2,is.corr = FALSE)

#주성분분석 Plot
#all
set.seed(210)
res.all <- kmeans(all_var$coord, centers = 5, nstart = 25)
grp_all <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp_all, 
             palette = "jco",
             legend.title = "Cluster")

#swa
set.seed(210)
res.swa <- kmeans(swa_var$coord, centers = 6, nstart = 25)
grp_swa <- as.factor(res.swa$cluster)

fviz_pca_var(swa_all_pca, col.var = grp_swa, 
             palette = "jco",
             legend.title = "Cluster")

#swam
set.seed(210)
res.swam <- kmeans(swam_var$coord, centers = 6, nstart = 25)
grp_swam <- as.factor(res.swam$cluster)

fviz_pca_var(swam_all_pca, col.var = grp_swam, 
             palette = "jco",
             legend.title = "Cluster")

#swapm
set.seed(210)
res.swapm <- kmeans(swapm_var$coord, centers = 6, nstart = 25)
grp_swapm <- as.factor(res.swapm$cluster)

fviz_pca_var(swapm_all_pca, col.var = grp_swapm, 
             palette = "jco",
             legend.title = "Cluster")

#swpm
set.seed(210)
res.swpm <- kmeans(swpm_var$coord, centers = 7, nstart = 25)
grp_swpm <- as.factor(res.swpm$cluster)

fviz_pca_var(swpm_all_pca, col.var = grp_swpm, 
             palette = "jco",
             legend.title = "Cluster")

#swami
set.seed(210)
res.swami <- kmeans(swami_var$coord, centers = 6, nstart = 25)
grp_swami <- as.factor(res.swami$cluster)

fviz_pca_var(swami_all_pca, col.var = grp_swami, 
             palette = "jco",
             legend.title = "Cluster")

#swapmi
set.seed(210)
res.swapmi <- kmeans(swapmi_var$coord, centers = 6, nstart = 25)
grp_swapmi <- as.factor(res.swapmi$cluster)

fviz_pca_var(swapmi_all_pca, col.var = grp_swapmi, 
             palette = "jco",
             legend.title = "Cluster")

#swpmi
set.seed(210)
res.swpmi <- kmeans(swpmi_var$coord, centers = 6, nstart = 25)
grp_swpmi <- as.factor(res.swpmi$cluster)

fviz_pca_var(swpmi_all_pca, col.var = grp_swpmi, 
             palette = "jco",
             legend.title = "Cluster")

#다중회귀분석
#swa(회귀선이 유의하지도 않고 설명력이 현저히 떨어짐)
library(car) #vif함수를 사용하기 위함
model_swa <- lm(겨울평균기온 ~ . , data = summer_winter_average) #설명변수 다 넣어보기
vif(model_swa) #다중공선성확인 (보통 10이상이면 변수제거)
#다중공선성을 가진 변수를 제거한 후의 회귀식
model_swa <- lm(겨울평균기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 +
                    평균해면기압 + 평균운량 + 일조시간 + 평균풍속 + 최대풍속, data = summer_winter_average)
summary(model_swa) #유의미한 변수를 찾아보려 하였지만 나타나지 않음
step(model_swa, direction = "both" ) #both = 단계적 선택법으로 회귀식 추정
model_swa <- lm(겨울평균기온 ~ 평균운량 + 일조시간, data=summer_winter_average)
summary(model_swa)

#swam(회귀선은 유의하나 설명력이 낮은편이다. 그래도 이 회귀선이 가장 나음)
model_swam <- lm(겨울평균최고기온 ~ . , data = summer_winter_average_max)
vif(model_swam)
model_swam <- lm(겨울평균최고기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                           평균운량 + 일조시간 + 평균풍속 + 최대풍속, data = summer_winter_average_max)
summary(model_swam)
step(model_swam, direction = "both")
model_swam <- lm(겨울평균최고기온 ~ 강수량 + 최소습도 + 일조시간 + 최대풍속
                         , data = summer_winter_average_max)
summary(model_swam)

#swapm(회귀선이 유의하지도 않고 설명력이 현저히 떨어짐)
model_swapm <- lm(겨울평균극점최고기온 ~ . , data = summer_winter_average_pole_max)
vif(model_swapm)
model_swapm <- lm(겨울평균극점최고기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                              평균운량 + 일조시간 + 평균풍속 + 최대풍속 , data = summer_winter_average_pole_max)
summary(model_swapm)
step(model_swapm, direction = "both")
model_swapm <- lm(겨울평균극점최고기온 ~ 최소습도 + 평균운량 + 최대풍속 , data = summer_winter_average_pole_max)
summary(model_swapm)

#swpm(어떤 설명변수도 종속변수를 설명할 수 없음)
model_swpm <- lm(겨울극점최고기온 ~ . , data = summer_winter_pole_max)
vif(model_swpm)
model_swpm <- lm(겨울극점최고기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                           평균운량 + 일조시간 + 평균풍속 + 최대풍속 , data = summer_winter_pole_max)
summary(model_swpm)
step(model_swpm, direction = "both")
model_swpm <- lm(겨울극점최고기온 ~ 1 , data = summer_winter_pole_max)
summary(model_swpm)

#swami(종속변수에 대한 설명력이 부족함)
model_swami <- lm(겨울평균최저기온 ~ . , data = summer_winter_average_min)
vif(model_swami)
model_swami <- lm(겨울평균최저기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                            평균운량 + 일조시간 + 평균풍속 + 최대풍속 , data = summer_winter_average_min)
summary(model_swami)
step(model_swami, direction = "both")
model_swami <- lm(겨울평균최저기온 ~ 최소습도 + 평균운량 + 일조시간, data = summer_winter_average_min)
summary(model_swami)

#swapmi(회귀선이 가까스로 유의하나 설명력이 부족함)
model_swapmi <- lm(겨울평균극점최저기온 ~ . , data = summer_winter_average_pole_min)
vif(model_swapmi)
model_swapmi <- lm(겨울평균극점최저기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                            평균운량 + 일조시간 + 평균풍속 + 최대풍속 ,
                            data = summer_winter_average_pole_min)
summary(model_swapmi)
step(model_swapmi, direction = "both")
model_swapmi <- lm(겨울평균극점최저기온 ~ 평균풍속, data = summer_winter_average_pole_min)
summary(model_swapmi)

#swpmi(회귀선이 유의하지도 않고 설명력이 현저히 떨어짐)
model_swpmi <- lm(겨울극점최저기온 ~ . , data = summer_winter_pole_min)
vif(model_swpmi)
model_swpmi <- lm(겨울극점최저기온 ~ 평균극점최저기온 + 극점최저기온 + 강수량 + 최소습도 + 평균해면기압 +
                               평균운량 + 일조시간 + 평균풍속 + 최대풍속 ,
                             data = summer_winter_pole_min)
summary(model_swpmi)
step(model_swpmi, direction = "both")
model_swpmi <- lm(겨울극점최저기온 ~ 최소습도, data = summer_winter_pole_min)
summary(model_swpmi)

