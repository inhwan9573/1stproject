library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)

# 한파, 폭염 데이터 가져오기
heatdata <- read_csv("./data/heatwave.csv")
colddata <- read.csv("./data/coldwave_NSW.csv")

view(heatdata)
view(colddata)


# 폭염 6월 7월 8월 데이터 추출
heatwavedata <- heatdata %>% filter(str_sub(time, 6, 7) == '06' | str_sub(time, 6, 7) == '07' | str_sub(time, 6, 7) == '08')

#폭염 년도 묶음, 갯수 세기
heatwavedata1 <- heatwavedata %>% group_by(str_sub(time, 1, 4)) %>% summarise(count = n())
heatwavedata1 <- rename(heatwavedata1, year='str_sub(time, 1, 4)')
view(heatwavedata1)

#폭염 중간값
meanheat <- heatwavedata1 %>% summarise(mean(count))
meanheat <- as.numeric(meanheat)

#한파 
coldwavedata <- colddata %>% group_by(str_sub(Year, 1, 4)) %>% summarise(count = n())

coldwavedata1 <- rename(coldwavedata, year='str_sub(Year, 1, 4)')
view(coldwavedata1)
# 한파 중간값
meancold <- coldwavedata1 %>% summarise(mean(count))
meancold <- as.numeric(meancold)

# 폭염, 한파 데이터 합치기
heatcolddata <- merge(heatwavedata1, coldwavedata1, by = "year", all=T)
view(heatcolddata)
names(heatcolddata)
heatcolddata <- rename(heatcolddata, heatcount = count.x, coldcount = count.y)
heatcolddata <- heatcolddata %>% mutate(sum = heatcount + coldcount)
view(heatcolddata)

#데이터 그리기

# 폭염 데이터
heatgplot <-heatcolddata %>% ggplot(aes(x=year, y = heatcount)) + 
  geom_bar(stat="identity",fill="#FF3300", width = 0.2) +
  xlab("연도") + ylab("횟수") + ggtitle("폭염")

# 한파 데이터
coldgplot <- heatcolddata %>% ggplot(aes(x=year, y = coldcount)) + 
  geom_bar(stat="identity", fill="#3399FF",width = 0.2) +
  xlab("연도") + ylab("횟수") + scale_y_reverse() + ggtitle("한파")

grid.arrange(heatgplot, coldgplot, top = "연도별 폭염,한파 발생 횟수")

