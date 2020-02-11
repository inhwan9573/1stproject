library(tidyverse)
library(ggplot2)
library(plotly)

heatdata <- read.csv("./data/dataheatindex.csv")
#view(heatdata)
heat <- heatdata %>%
  group_by(year) %>%
  summarise(열지수=mean(heatindex),최대열지수=max(heatindex))
#view(heat)


chilldata <- read.csv("./data/datawindchill.csv")
#view(chilldata)
chill <- chilldata %>%
  group_by(Year) %>%
  summarise(체감온도 = mean(windchill), 최저체감온도 = min(windchill))
#View(chill)
colnames(chill) <- c("년도", "평균체감온도","최저체감온도")

heat_chill <- cbind(heat, chill)
heat_chill <- heat_chill[,-c(4)]
view(heat_chill)
names(heat_chill)

heat_chill[heat_chill$최대열지수 >= 33, "열지수범주"] = "덥다"
heat_chill[heat_chill$최대열지수 < 33, "열지수범주"] = "안덥다"

# 열지수범주_평균체감온도
hcb1 <- heat_chill %>% ggplot(aes(x=열지수범주, y=평균체감온도, fill=열지수범주)) + geom_boxplot()
hcb1 <-ggplotly(hcb1)
hcb1

#열지수범주_최저체감온도
hcb2 <- heat_chill %>% ggplot(aes(x=열지수범주, y=최저체감온도, fill=열지수범주)) + geom_boxplot()
hcb2 <- ggplotly(hcb2)
hcb2
