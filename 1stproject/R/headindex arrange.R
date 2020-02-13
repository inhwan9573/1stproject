heatindex <- read_excel('./data/Heat Index 8906-1902.xlsx')

heatindex <- heatindex %>% 
  mutate( 'Year'= str_sub(date,1,4),'Month'=str_sub(date,5,6))
heatindex_S <- filter(heatindex,Month=='06'|Month=='07'|Month=='08')
heatindex_S$Year <- paste(heatindex_S$Year,'여름', sep = ' ')
heatindex_S <- heatindex_S[c('date',"Year","Month",'heatindex')]
write.csv(heatindex_S,"./data/heatindex_S.csv")
