install.packages('hrbrthemes')
install.packages('gridExtra')
install.packages('grid')
library(hrbrthemes)
library(readxl)
library(tidyverse)
library(gridExtra)
library(grid)
#열지수, 체감온도 데이터 불러오기
dataheatindex <- read.csv("./data/heatindex_S.csv")
view(dataheatindex)
datawindchill <- read.csv("data/windchill_NW.csv")
#열지수 데이터
summary(dataheatindex)
dataheatindex$date <- as.character(dataheatindex$date)
str(dataheatindex)
dataheatindex <- mutate(dataheatindex,'year'= str_sub(date,1,4))
heatindexdata <- dataheatindex %>% 
  select(year,heatindex) %>% 
  filter(heatindex >= 32) %>% 
  group_by(year) %>% 
  summarise(n = n())
names(heatindexdata)
heatindexdata <- rename(heatindexdata, year=year, countheat = n)

view(heatindexdata)
#체감온도 데이터
summary(datawindchill)
datawindchill$date <- as.character(datawindchill$date)
datawindchill <- mutate(datawindchill,'year'= str_sub(date,1,4))
windchilldata <- datawindchill %>%  
  select(year, windchill) %>% 
  filter (windchill < -10.5) %>% 
  group_by(year) %>% 
  summarise(n = n())
names(windchilldata)
windchilldata <- rename(windchilldata, year=year, countchill = n)


view(windchilldata)
#열지수, 체감온도 데이터 합치기
alldata <- merge(heatindexdata, windchilldata, by="year", all=T)
view(alldata)
#결측값 0으로 만들기
alldata$countheat[is.na(alldata$countheat)] <- 0
alldata$countchill[is.na(alldata$countchill)] <- 0
view(alldata)

# 시각화
p <- alldata %>% 
  ggplot() + 
  geom_bar(aes(x=year, y = countheat),stat="identity",fill="#FF3300", width = 0.2) + 
  geom_bar(aes(x=year, y = -countchill),stat="identity", fill="#3399FF",width = 0.2)+
labs(title = "열지수 32 이상, 체감온도 -10.5 이하인 날의 수 비교", x="연도", y="횟수")+
  theme_minimal() +
  theme(
    title = element_text(family= "Malgun Gothic", size=12), 
    plot.title = element_text(family= "Malgun Gothic", size=15, hjust=0.5)
  )
p


p1 <-alldata %>% ggplot() +
  geom_bar(aes(x=year, y = countheat),stat="identity",fill="#FF3300", width = 0.2) +
  xlab("연도") + ylab("횟수")+
labs(title = "열지수 32 이상인 날", x="연도", y="횟수")+
  theme_minimal() +
  theme(
    title = element_text(family= "Malgun Gothic",size=10), 
    plot.title = element_text(family= "Malgun Gothic",size=10,hjust=0.5)
  )
p1
# 한파 데이터
p2 <- alldata %>% ggplot() +
  geom_bar(aes(x=year, y = countchill),stat="identity", fill="#3399FF",width = 0.2) +
  xlab("연도") + ylab("횟수") + scale_y_reverse()+
  labs(title = "체감온도 -10.5 이하인 날")+
  theme_minimal() +
  theme(　　　　　　　　
    title = element_text(family= "Malgun Gothic",size=10), 
    plot.title = element_text(family= "Malgun Gothic",size=10, hjust=0.5)
  )
p2

hiwc <- grid.arrange(p1, p2,
             top = textGrob("연도별 열지수와 체감온도 비교",gp=gpar(fontsize=15)))
#열지수와 여름기온 변화 비교/체감온도와 겨울기온 변화 비교
#같은 모양이 아니면 왜 그럴지 생각..?