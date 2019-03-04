### Spanish Aggregated Data ###
### ------------------------------------------------------------------ ###
###  General Descriptive and aggregated measures for the Dissertation  ###
### ------------------------------------------------------------------ ###

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
library(MortalitySmooth)
# library(MortHump)

### -------------------------------------------------- ###
### South European Countries LE at Birth and at age 50 ###
### -        -        -         -        -         -   ###
### and later for the life span disparity              ###

# Spain
LT.ESP <- readHMD("ESP_LT.txt", fixup = T)

POP.ESP <- readHMD("ESP_pop.txt", fixup = T)
DEAD.ESP <- readHMD("ESP_dea.txt", fixup = T)

# Italy
LT.ITA <- readHMD("ITA_LT.txt", fixup = T)

# Portugal
LT.PRT <- readHMD("POR_LT.txt", fixup = T)

# Greece
LT.GRC <- readHMD("GRE_LT.txt", fixup = T)

# Japan
LT.JAP <- readHMD("JAP_LT.txt", fixup = T)


summary(LT.ESP)
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###

### 2. Binding the data together and show development for LE 0 and LE 65

#### Age 0
#### -----

## Spain

LT.ESP.0 <- LT.ESP %>% select(ex,Age,Year) %>% mutate(country = "Spain") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  #filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Italy

LT.ITA.0 <- LT.ITA %>% select(ex,Age,Year) %>% mutate(country = "Italy") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Greece

LT.GRC.0 <- LT.GRC %>% select(ex,Age,Year) %>% mutate(country = "Greece") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Portugal

LT.PRT.0 <- LT.PRT %>% select(ex,Age,Year) %>% mutate(country = "Portugal") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Japan

LT.JAP.0 <- LT.JAP %>% select(ex,Age,Year) %>% mutate(country = "Japan") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)



## Create a long dataset with all the countries

LE.all_0 <- bind_rows(LT.ESP.0,LT.ITA.0) %>% bind_rows(LT.GRC.0) %>% 
  bind_rows(LT.PRT.0) %>% bind_rows(LT.JAP.0) %>% 
  ## Cut to a uniform age range
  filter(Year>1910) %>% 
  # highlight Spanish values
  mutate(highlight_flag = ifelse(country=="Spain",T,F))

head(LE.all_0)
tail(LE.all_0)
summary(LE.all_0)

### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###


#### Age 65
#### ------

## Spain

LT.ESP.65 <- LT.ESP %>% select(ex,Age,Year) %>% mutate(country = "Spain") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 65) %>% 
  # to assure the same end year (2012)
  #filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Italy

LT.ITA.65 <- LT.ITA %>% select(ex,Age,Year) %>% mutate(country = "Italy") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 65) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Greece

LT.GRC.65 <- LT.GRC %>% select(ex,Age,Year) %>% mutate(country = "Greece") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 65) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Portugal

LT.PRT.65 <- LT.PRT %>% select(ex,Age,Year) %>% mutate(country = "Portugal") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 65) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Japan

LT.JAP.65 <- LT.JAP %>% select(ex,Age,Year) %>% mutate(country = "Japan") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 65) %>% 
  # to assure the same end year (2012)
  # filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Create a long dataset with all the countries

LE.all_65 <- bind_rows(LT.ESP.65,LT.ITA.65) %>% bind_rows(LT.GRC.65) %>% 
  bind_rows(LT.PRT.65) %>% bind_rows(LT.JAP.65) %>% 
  ## Cut to a uniform age range
  filter(Year>1910) %>% 
  # highlight the Spanish values
  mutate(highlight_flag = ifelse(country=="Spain",T,F))




rm(LT.ESP.0, LT.ITA.0, LT.GRC.0, LT.PRT.0, LT.JAP.0, LT.ESP.65, LT.ITA.65, LT.GRC.65, LT.PRT.65, LT.JAP.65)

### 3. Plot (using multiplot)

# Life Expectancy at age zero
plotLE_zero <- LE.all_0 %>% ggplot() +
  # line plot
  geom_line(aes(x = Year, y = ex, color = country, alpha = highlight_flag))  +
  geom_point(aes(x = Year, y = ex, color = country, alpha = highlight_flag )) +
  scale_y_continuous(name = "Life expectancy at birth") +
  scale_x_continuous(name = " ") +
  scale_colour_manual(values = c("grey20","grey35", "grey55","grey75", "black"), name="", guide=F) +
  scale_alpha_discrete(range = c(0.25, 0.85), name="", guide=F) +
  theme_bw()
plotLE_zero <- plotLE_zero + theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=12,face="bold"))

# ----------------------------------------------------------------------------------- #
# changing colors and legends
# scale_colour_manual(values = c("#FF6934", "#26B7FF","#26FF57", "#FFD846"), name="")
# theme(legend.position = c(0.85, 0.25))
#### -----------------------------------
# colors
#        blue  = #0D3BB2
#         red  = #D52513
# yellow/gold  = #D5AD21
#      purple  = #8D11B2
#       green  = #20D51A
#### ----------------------------------
# c("#0D3BB2","#D5AD21", "#8D11B2","#20D51A", "#D52513")
# ----------------------------------------------------------------------------------- #

# Life Expectancy at age 65
plotLE_65 <- LE.all_65 %>% ggplot() +
  # line plot
  geom_line(aes(x = Year, y = ex, color = country, alpha = highlight_flag))  +
  geom_point(aes(x = Year, y = ex, color = country, alpha = highlight_flag)) +
  scale_y_continuous(name = "Life expectancy at age 65") +
  scale_x_continuous(name = " ") +
  scale_colour_manual(values = c("grey20", "grey35", "grey55","grey75", "black"), name="") +
  scale_alpha_discrete(range = c(0.25, 0.85), name="", guide=F) +
  theme_bw()

plotLE_65 <- plotLE_65 + theme(legend.position = c(0.85, 0.25)) + 
  scale_shape_discrete(guide=FALSE) + theme(axis.text=element_text(size=12),
                                           axis.title=element_text(size=12,face="bold"))

# Combine Plots
multiplot(plotLE_zero,plotLE_65)




### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
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

# Graph
per65_graph <- POP.ESP_65 %>% filter(Year>1950) %>% ggplot() + 
  geom_line(aes(x=Year, y=Per65)) +
  geom_point(aes(x = Year, y = Per65)) +
  scale_x_continuous(breaks = round(seq(1950, max(POP.ESP_65$Year), by = 10),1)) + 
  theme_bw()
per65_graph <- per65_graph + scale_y_continuous(labels = percent, name = "Proportion Population age 65+")
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###






### ------------------------------------------------ ###
###    For the gender gap in LE the LT by sex        ###

# Spain
LT.ESP.fem <- readHMD("ESP_LT_fem.txt", fixup = T)
LT.ESP.mal <- readHMD("ESP_LT_mal.txt", fixup = T)

# Italy
#LT.ITA.fem <- readHMDweb(CNTRY="ITA", item="fltper_1x1", username=name.m, password=pw.m, fixup = T)
#LT.ITA.mal <- readHMDweb(CNTRY="ITA", item="mltper_1x1", username=name.m, password=pw.m, fixup = T)

# Espana
Gap.ESP <- as.data.frame(cbind(rep(NA,109), seq(1908,2016,1)))
names(Gap.ESP) <- c("gap","year")

for (i in min(LT.ESP.fem$Year):max(LT.ESP.fem$Year)) {
  Gap.ESP$gap[Gap.ESP$year==i] <- GAP(LT.ESP.fem$Female[ESP$Year==i],ESP$Male[ESP$Year==i])
}

### -------------------------------------------------- ###

## There are very different start years (Greece only starts from 1981 and Italy ends already at 2012)
# In the text I used the 50 years between 1964 and 2014 - best choice for now = 1982 - 2012!
year <- seq(1981,2012,1)
le.ita <- cbind(LT.ITA$ex[LT.ITA$Age==0 & LT.ITA$Year>1981], LT.ITA$ex[LT.ITA$Age==65 & LT.ITA$Year>1981])
le.esp <- cbind(LT.ESP$ex[LT.ESP$Age==0 & LT.ESP$Year>1981 & LT.ESP$Year<=2012],LT.ESP$ex[LT.ESP$Age==65 & LT.ESP$Year>1981 & LT.ESP$Year<=2012])
le.grc <- cbind(LT.GRC$ex[LT.GRC$Age==0 & LT.GRC$Year>1981 & LT.GRC$Year<=2012],LT.GRC$ex[LT.GRC$Age==65 & LT.GRC$Year>1981 & LT.GRC$Year<=2012])
le.prt <- cbind(LT.PRT$ex[LT.PRT$Age==0 & LT.PRT$Year>1981 & LT.PRT$Year<=2012],LT.PRT$ex[LT.PRT$Age==65 & LT.PRT$Year>1981 & LT.PRT$Year<=2012])

LE.all <- as.data.frame(cbind(le.ita,le.esp,le.grc,le.prt,year))
names(LE.all) <- c("ITA","ITA65","ESP","ESP65","GRC", "GRC65","PRT","PRT65","Year")

### Plot LE at birth ###
########################

LE.Birth <- ggplot(LE.all, aes(Year)) +  geom_line(aes(y = ITA, colour = "ITA")) + geom_line(aes(y = ESP, colour = "ESP")) + 
  geom_line(aes(y = GRC, colour = "GRC"))+ geom_line(aes(y = PRT, colour = "PRT")) + geom_point(aes(y = ITA, colour = "ITA")) +
  geom_point(aes(y = ESP, colour = "ESP")) + geom_point(aes(y = GRC, colour = "GRC")) + geom_point(aes(y = PRT, colour = "PRT"))

plot.LE.Birth <- LE.Birth  + ggtitle("") + scale_y_continuous(name="Life Expectancy at Birth in Years") + labs(colour=" ") + theme_bw() + 
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010)) + theme(legend.position="none")

### Plot LE at age 65 ###
########################
LE.65 <- ggplot(LE.all, aes(Year)) +  geom_line(aes(y = ITA65, colour = "ITA")) + geom_line(aes(y = ESP65, colour = "ESP")) + 
  geom_line(aes(y = GRC65, colour = "GRC"))+ geom_line(aes(y = PRT65, colour = "PRT")) + geom_point(aes(y = ITA65, colour = "ITA")) +
  geom_point(aes(y = ESP65, colour = "ESP")) + geom_point(aes(y = GRC65, colour = "GRC")) + geom_point(aes(y = PRT65, colour = "PRT"))

plot.LE.65 <- LE.65  + ggtitle("") + scale_y_continuous(name="Life Expectancy at age 65 in Years") + labs(colour=" ") + theme_bw() + 
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010)) + theme(legend.position = c(0.8, 0.2))

### Multiplot ###
# 1. load the function! - and change the layout matrix settings in the first row
# 2. use it for the two LE plots

multiplot(plot.LE.Birth,plot.LE.65,cols = 1)

#########################################################################################################
# Longer Time Series -  1910 - 2012 (means for Portugal and Greece we need two strings of placeholder)
#########################################################################################################

year <- seq(1910,2012,1)
le.ita <- cbind(LT.ITA$ex[LT.ITA$Age==0 & LT.ITA$Year>1909], LT.ITA$ex[LT.ITA$Age==65 & LT.ITA$Year>1909])
le.esp <- cbind(LT.ESP$ex[LT.ESP$Age==0 & LT.ESP$Year>1909 & LT.ESP$Year<=2012],LT.ESP$ex[LT.ESP$Age==65 & LT.ESP$Year>1909 & LT.ESP$Year<=2012])
le.grc.a <- matrix(data=NA,nrow = 72,ncol = 2)
le.grc.b <- cbind(LT.GRC$ex[LT.GRC$Age==0 & LT.GRC$Year>1981 & LT.GRC$Year<=2012],LT.GRC$ex[LT.GRC$Age==65 & LT.GRC$Year>1981 & LT.GRC$Year<=2012])
le.grc <- rbind(le.grc.a,le.grc.b)
le.prt.a <- matrix(data=NA,nrow = 30,ncol = 2)
le.prt.b <- cbind(LT.PRT$ex[LT.PRT$Age==0 & LT.PRT$Year<=2012],LT.PRT$ex[LT.PRT$Age==65 & LT.PRT$Year<=2012])
le.prt <- rbind(le.prt.a,le.prt.b)

## tie them together and transform them to a data frame for ggplot
LE.all <- as.data.frame(cbind(le.ita,le.esp,le.grc,le.prt,year))
names(LE.all) <- c("ITA","ITA65","ESP","ESP65","GRC", "GRC65","PRT","PRT65","Year")

### Plot LE at birth ###
########################

LE.Birth <- ggplot(LE.all, aes(Year)) +  geom_line(aes(y = ITA, colour = "ITA")) + geom_line(aes(y = ESP, colour = "ESP")) + 
  geom_line(aes(y = GRC, colour = "GRC"))+ geom_line(aes(y = PRT, colour = "PRT")) + geom_point(aes(y = ITA, colour = "ITA")) +
  geom_point(aes(y = ESP, colour = "ESP")) + geom_point(aes(y = GRC, colour = "GRC")) + geom_point(aes(y = PRT, colour = "PRT"))

plot.LE.Birth <- LE.Birth  + ggtitle("") + scale_y_continuous(name="Life Expectancy at Birth in Years") + labs(colour=" ") + theme_bw() + 
  scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)) + theme(legend.position="none")

### Plot LE at age 65 ###
########################
LE.65 <- ggplot(LE.all, aes(Year)) +  geom_line(aes(y = ITA65, colour = "ITA")) + geom_line(aes(y = ESP65, colour = "ESP")) + 
  geom_line(aes(y = GRC65, colour = "GRC"))+ geom_line(aes(y = PRT65, colour = "PRT")) + geom_point(aes(y = ITA65, colour = "ITA")) +
  geom_point(aes(y = ESP65, colour = "ESP")) + geom_point(aes(y = GRC65, colour = "GRC")) + geom_point(aes(y = PRT65, colour = "PRT"))

plot.LE.65 <- LE.65  + ggtitle("") + scale_y_continuous(name="Life Expectancy at age 65 in Years") + labs(colour=" ") + theme_bw() + 
  scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)) + theme(legend.position = c(0.8, 0.2))

### Multiplot ###
# 1. load the function! - and change the layout matrix settings in the first row
# 2. use it for the two LE plots

multiplot(plot.LE.Birth,plot.LE.65,cols = 1)

