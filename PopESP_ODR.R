# ---------------------- 
# Population Development
# ---------------------- 

# set working directory
dir()
setwd("C:/Users/y4956294S/Documents/LONGPOP/LE Data Spain Diss/DISSLTESP/data")

set.seed(17952)

## use hmd/hfd package for load the data (Spain)

# LIBRARIES #
library(ggplot2)
library(gcookbook)
library(HMDHFDplus)
library(plyr)
library(reshape2)
library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(RColorBrewer)


POP.ESP <- readHMD("ESP_pop.txt", fixup = T)


### --------------
### Population 65+
### --------------

# Pop total
POP.tot <- aggregate(x=POP.ESP$Total1,by=list(POP.ESP$Year), FUN=sum)
names(POP.tot) <- c("Year","Tot_pop")
# Pop 65 +
POP.65plus <- POP.ESP %>% filter(Age>64)
POP.65plus <- aggregate(x=POP.65plus$Total1, by=list(POP.65plus$Year), FUN=sum)
names(POP.65plus) <- c("Year","65_pop")

# Combine and calculate the percentage
POP.ESP_65 <- left_join(POP.tot, POP.65plus, by="Year")
POP.ESP_65 <- POP.ESP_65 %>% mutate(Per65 = `65_pop`/Tot_pop)

### --- ###
### ODR ###
### --- ###

# Pop 65 +
POP.15_64 <- POP.ESP %>% filter(Age>15) %>% filter(Age<65)
POP.15_64 <- aggregate(x=POP.15_64$Total1, by=list(POP.15_64$Year), FUN=sum)
names(POP.15_64) <- c("Year","Pop15_64")
# Combine and calculate the percentage
POP.ODR <- left_join(POP.15_64, POP.65plus, by="Year")
POP.ODR <- POP.ODR %>% mutate(ODR = `65_pop`/`Pop15_64`)


### ------------------------------------------------------------------------------------------------- ###
# Graph Pop 65 +
per65_graph <- POP.ESP_65 %>% filter(Year>1970) %>% ggplot() + 
  geom_line(aes(x=Year, y=Per65)) +
  geom_point(aes(x = Year, y = Per65)) +
  scale_x_continuous(breaks = round(seq(1970, max(POP.ESP_65$Year), by = 10),1)) + 
  theme_bw()
per65_graph <- per65_graph + scale_y_continuous(labels = percent, name = "Proportion Population age 65+")
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
# ODR
ODR_plot <- POP.ODR %>% filter(Year>1970) %>% ggplot() + 
  geom_line(aes(x=Year, y=ODR)) +
  geom_point(aes(x = Year, y = ODR)) +
  scale_x_continuous(breaks = round(seq(1970, max(POP.ESP_65$Year), by = 10),1)) + 
  scale_y_continuous(name = "ODR (D)", breaks = round(seq(0.1, 0.35, by = 0.025),3)) +
  theme_bw()
ODR_plot
### ------------------------------------------------------------------------------------------------- ###

multiplot(per65_graph,ODR_plot)
