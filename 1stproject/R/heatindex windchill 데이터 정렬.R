library(tidyverse)
dataheatindex
str(datawindchill)

HIWC <- merge(dataheatindex, datawindchill, by="year", all=T)

dataheatindex <- mutate(dataheatindex,'date'= paste(str_sub(date,1,4),str_sub(date,5,6),
                        str_sub(date,7,8),sep = '-'))


write.csv(dataheatindex, "./data/dataheatindex.csv", row.names=FALSE)
write.csv(datawindchill, "./data/datawindchill.csv", row.names=FALSE)
