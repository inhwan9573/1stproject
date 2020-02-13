library(readxl)
library(tidyverse)

coldwave <- read_excel('./data/cold wave report 2007-2019.xlsx')


#서울만 필터링


coldwave_S <- filter(coldwave,Area=='서울·인천·경기도')


#연도/월 항목 만들기
coldwave_S <- coldwave_S %>% 
  mutate( 'Year'= str_sub(Time,1,4),'Month'=str_sub(Time,6,7))


#12,1,2월 필터링

coldwave_SW <- filter(coldwave_S,Month=='01'|Month=='02'|Month=='12')

coldwave_SW$Year <- as.numeric(coldwave_SW$Year)
coldwave_SW$Month <- as.numeric(coldwave_SW$Month)
str(coldwave_SW)

#12월/1월 2월 행 빼기
coldwave_SW12 <- filter(coldwave_S,Month=='12')
coldwave_SW1_2 <- filter(coldwave_S,Month=='01'|Month=='02')
#n년 12월 + n+1년 1월 2월 묶어서 연도 n년 겨울로 바꾸기

coldwave_SW12$Year <- paste(coldwave_SW12$Year,'겨울', sep = ' ')
               
coldwave_SW1_2$Year <- as.numeric(coldwave_SW1_2$Year)
coldwave_SW1_2$Year <- paste(coldwave_SW1_2$Year-1,'겨울', sep = ' ')

str(coldwave_SW1_2)

#12월, 1/2월 데이터 하나로 묶기

coldwave_NSW <- arrange(rbind(coldwave_SW12,coldwave_SW1_2),Year)

#n년 겨울별로 한파 개수 세기

coldwave_count <- coldwave_NSW %>% 
  group_by(Year) %>% 
  summarise(
    n=n())

str(coldwave_NSW)

#데이터셋 정리
coldwave_NSW <- coldwave_NSW[c("Year","Month",'Area','Title')]

#엑셀 저장
write.csv(coldwave_count,"./data/coldwave_count.csv")

cc <- ggplot(coldwave_count, aes(x=Year, y=n))

barplot(coldwave_count$n)

table(coldwave_count$n)
