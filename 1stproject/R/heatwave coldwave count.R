library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readxl)
library(tidyverse)
library(gridExtra)
# 한파, 폭염 데이터 가져오기
heatdata <- read_csv("./data/heatwave.csv")
colddata <- read.csv("./data/coldwave_NSW.csv")
heatdata <- na.omit(heatdata)
colddata <- colddata[,-1]
write.csv(heatdata,'./data/heatwave.csv')
write.csv(colddata,'./data/coldwave_NSW.csv')
view(heatdata)
view(colddata)
# 폭염 6월 7월 8월 데이터 추출
heatwavedata <- heatdata %>% filter(str_sub(time, 6, 7) == '06' | str_sub(time, 6, 7) == '07' | str_sub(time, 6, 7) == '08')
view(heatwavedata)
#폭염 년도 묶음, 갯수 세기
heatwavedata1 <- heatwavedata %>% group_by(str_sub(time, 1, 4)) %>% summarise(count = n())
heatwavedata1 <- rename(heatwavedata1, year='str_sub(time, 1, 4)')
view(heatwavedata1)
#한파
coldwavedata <- colddata %>% group_by(str_sub(Year, 1, 4)) %>% summarise(count = n())
coldwavedata1 <- rename(coldwavedata, year='str_sub(Year, 1, 4)')
view(coldwavedata1)
# 폭염, 한파 데이터 합치기
heatcolddata <- merge(heatwavedata1, coldwavedata1, by = "year", all=T)
view(heatcolddata)
names(heatcolddata)
heatcolddata <- rename(heatcolddata, heatcount = count.x, coldcount = count.y)
view(heatcolddata)

cor(heatcolddata$heatcount,heatcolddata$coldcount)
cor(heatcolddata$heatcount,heat_chill$최대열지수)

#데이터 그리기
# 폭염 데이터
p1 <-heatcolddata %>% ggplot() +
  geom_bar(aes(x=year, y = heatcount),stat="identity",fill="#FF3300", width = 0.2) +
  xlab("연도") + ylab("횟수")
p1
# 한파 데이터
p2 <- heatcolddata %>% ggplot() +
  geom_bar(aes(x=year, y = coldcount),stat="identity", fill="#3399FF",width = 0.2) +
  xlab("연도") + ylab("횟수") + scale_y_reverse()
p2
grid.arrange(p1, p2)

write.csv(heatwavedata1,'./data/heatwavecount.csv')
write.csv(coldwavedata1,'./data/coldwavecount.csv')
