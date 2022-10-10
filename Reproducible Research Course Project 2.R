library(tidyr)
library(dplyr)
library(ggplot2)

storm <- read.csv("repdata_data_StormData.csv")
head(storm)
dim(storm)
names(storm)
unique(storm$EVTYPE)

sum_f_by_event <- storm %>% 
  group_by(EVTYPE) %>%
  summarise(
    sum_fatalities = sum(FATALITIES, na.rm = TRUE)) %>%
  arrange(desc(sum_fatalities))
mean_f_by_event <- storm %>% 
  group_by(EVTYPE) %>%
  summarise(
    mean_fatalities = mean(FATALITIES, na.rm = TRUE)) %>%
  arrange(desc(mean_fatalities))

sum_i_by_event <- storm %>%
  group_by(EVTYPE) %>%
  summarise(
  sum_injuries = sum(INJURIES, na.rm = TRUE)) %>%
  arrange(desc(sum_injuries)) 
head(sum_i_by_event)

mean_i_by_event <- storm %>%
  group_by(EVTYPE) %>%
  summarise(
    mean_injuries = mean(INJURIES, na.rm = TRUE)) %>%
  arrange(desc(mean_injuries)) 

head(storm$PROPDMGEXP)
unique(storm$PROPDMGEXP) %>%
  table()
#K: thousands, m: millions, B: billions    
unique(storm$CROPDMGEXP)
storm %>%
  filter(CROPDMGEXP == "") %>%
  select(CROPDMG)


storm$CROPDMGEXP[storm$CROPDMGEXP == "B"] <- 1e9
storm$CROPDMGEXP[storm$CROPDMGEXP == "M"] <- 1e6
storm$CROPDMGEXP[storm$CROPDMGEXP == "m"] <- 1e6
storm$CROPDMGEXP[storm$CROPDMGEXP == "K"] <- 1000
storm$CROPDMGEXP[storm$CROPDMGEXP == "k"] <- 1000
storm$CROPDMGEXP[storm$CROPDMGEXP == ""] <- 1
storm$CROPDMGEXP[storm$CROPDMGEXP == 2] <- 1
storm$CROPDMGEXP[storm$CROPDMGEXP == 0] <- 1
storm$CROPDMGEXP[storm$CROPDMGEXP == "?"] <- 1

storm <- storm %>% 
  mutate(
    total_crop_dmg = as.numeric(CROPDMG) * as.numeric(CROPDMGEXP)
  )

unique(storm$PROPDMGEXP)
storm %>% 
  group_by(PROPDMGEXP) %>%

getNumericRounding()
head(storm)
table(unique(storm$REMARKS))
