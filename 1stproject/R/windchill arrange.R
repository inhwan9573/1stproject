library(readxl)
library(tidyverse)

windchill <- read_excel('./data/Wind Chill 8906-1902.xlsx')

#연도/월 항목 만들기
windchill <- windchill %>% 
  mutate( 'Year'= str_sub(일자,1,4),'Month'=str_sub(일자,6,7))


#12,1,2월 필터링

windchill_W <- filter(windchill,Month=='01'|Month=='02'|Month=='12')

#숫자형으로 바꾸기-계산하려고
windchill_W$Year <- as.numeric(windchill_W$Year)
windchill_W$Month <- as.numeric(windchill_W$Month)
str(windchill_W)

#12월/1월 2월 행 빼기
windchill_12 <- filter(windchill_W,Month=='12')
windchill_W1_2 <- filter(windchill_W,Month=='1'|Month=='2')
#n년 12월 + n+1년 1월 2월 묶어서 연도 n년 겨울로 바꾸기

windchill_W1_2
windchill_12$Year <- paste(windchill_12$Year,'겨울', sep = ' ')

windchill_W1_2$Year <- as.numeric(windchill_W1_2$Year)
windchill_W1_2$Year <- paste(windchill_W1_2$Year-1,'겨울', sep = ' ')

str(windchill_W1_2)

#12월, 1/2월 데이터 하나로 묶기

windchill_NW <- arrange(rbind(windchill_12,windchill_W1_2),Year)

windchill_NW <- windchill_NW[c('일자','Year','Month','체감기온(°C)')]




#n년 겨울별로 한파 개수 세기

windchill_count <- windchill_NW %>% 
  group_by(Year) %>% 
  summarise(
    n=n())

str(coldwave_NSW)

#데이터셋 정리
coldwave_NSW <- coldwave_NSW[c("Year","Month",'Area','Title')]

#엑셀 저장
write.csv(windchill_NW,"./data/windchill_NW.csv")

cc <- ggplot(windchill_count, aes(x=Year, y=n))

barplot(windchill_count$n)

table(windchill_count$n)
