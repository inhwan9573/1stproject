library(tidyverse)
library(ggplot2)
library(plotly)

data <- read.csv("./data/seoulsi.csv")
# View(data)
# 여름 데이터

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


sdf <- jja %>%
  group_by(substr(년도,1,4)) %>% 
  summarise(평균기온=mean(평균기온), 평균최고기온=mean(평균최고기온),
                평균극점최고기온=mean(극점최고기온))

names(sdf)[names(sdf) == "substr(년도, 1, 4)"] <- c("년도")
# names(sdf)
view(sdf)
# 여름 년도, 평균기온, 평균최고기온 뽑기
sdf <- sdf[,-c(5:17)]

#겨울 데이터

djf <- data.frame()
for (i in seq(1989, 2018, 1)) {
  december <- data[grep(paste0(i, ".12"),data$기간),]
  january <- data[grep(paste0(i+1, ".01"),data$기간),]
  febrary <- data[grep(paste0(i+1, ".02"),data$기간),]
  group <- rbind(december,january,febrary)
  group$year <- i
  djf <- rbind(djf,group)
}

djf <- djf[,-c(2)]
colnames(djf) <- c("년도","평균기온","평균최고기온","극점최고기온",
                   "평균최저기온","극점최저기온","강수량",
                   "평균습도","최소습도","평균해면기압",
                   "이슬점온도","평균운량","일조시간",
                   "평균풍속","최대풍속","year")
wdf <- djf %>%
  group_by(year) %>% 
  summarise(겨울평균기온=mean(평균기온), 겨울평균최고기온=mean(평균최고기온),
                  겨울평균극점최고기온=mean(극점최고기온),
                  겨울평균최저기온=mean(평균최저기온),겨울평균극점최저기온=mean(극점최저기온),
                  겨울극점최저기온=min(극점최저기온))

wdf <- wdf[,-c(1)]
view(wdf)
names(wdf)
summer_winter_df <- cbind(sdf,wdf)

summer_winter_df[summer_winter_df$평균극점최고기온 >= 34, "평균최고기온범주"] = "덥다"
summer_winter_df[summer_winter_df$평균극점최고기온 < 34, "평균최고기온범주"] = "안덥다"

view(summer_winter_df)

# 여름범주_겨울평균최저기온
b1 <- summer_winter_df %>% ggplot(aes(x=평균최고기온범주, y=겨울평균최저기온, fill=평균최고기온범주)) + geom_boxplot()
b1 <- ggplotly(b1)
b1

# 여름범주_겨울평균기온
b2 <- summer_winter_df %>% ggplot(aes(x=평균최고기온범주, y=겨울평균기온, fill=평균최고기온범주)) + geom_boxplot()
b2 <- ggplotly(b2)
b2
