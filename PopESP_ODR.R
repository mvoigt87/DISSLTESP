# ---------------------- 
# Population Development
# ----------------------

# Aging indexes

# 1. ODR - Old Age Dependency Ratio
# 2. Prospective Aging

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

# load life table for prospective aging

load("TLT_HMD.Rdata") # doublecheck


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
per65_graph <- POP.ESP_65 %>% filter(Year>1950) %>% ggplot() + 
  geom_line(aes(x=Year, y=Per65)) +
  geom_point(aes(x = Year, y = Per65)) +
  scale_x_continuous(breaks = round(seq(1950, max(POP.ESP_65$Year), by = 10),1)) + 
  theme_bw()
per65_graph <- per65_graph + scale_y_continuous(labels = percent, name = "Proportion Population age 65+")
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
# ODR
ODR_plot <- POP.ODR %>% filter(Year>1950) %>% ggplot() + 
  geom_line(aes(x=Year, y=ODR)) +
 # geom_point(aes(x = Year, y = ODR)) +
  scale_x_continuous(breaks = round(seq(1950, max(POP.ESP_65$Year), by = 10),1)) + 
  scale_y_continuous(name = "ODR (D)", breaks = round(seq(0.1, 0.35, by = 0.025),3)) +
  theme_bw()
ODR_plot <- ODR_plot + theme(axis.text=element_text(size=12),
                axis.title=element_text(size=12,face="bold"))


### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### Prospective Aging Measures
### ------------------------------------------------------------------------------------------------- ###

## Prospective ODR

# Step 1 - Extract remaining life expectancy at the index year 1950

# Cut at INDEX YEAR 1950
tot.smooth <- tot.smooth %>% filter(Year>=1950)

tot.smooth$ex[tot.smooth$Year==1950 & tot.smooth$Age==15]
tot.smooth$ex[tot.smooth$Year==1950 & tot.smooth$Age==65]
# e_15 = 54.02391
# e_65 = 13.18478

# Step 2 - Calculate the prospective age based on the remaining life exspectancy for the rest of the years

NEAR.fun <- function(x){
  which.min(abs(x$ex - 13.18478)) }

prosp.age65 <- by(data = tot.smooth, INDICES = tot.smooth$Year, FUN = NEAR.fun)
prosp.age65 <- as.data.frame(cbind(unique(tot.smooth$Year),prosp.age65))
colnames(prosp.age65)[1] <- "Year"

# Calculate the ODR (prospective)

# Pop Prospective ODR

### Population between 15 and Prospective Age

POP.15_PA <- POP.ESP %>% filter(Year>1949) %>%  filter(Age>15) %>% select(Total1, Year, Age) 

for (i in 1950:2016){
  POP.15_PA$Total1[POP.15_PA$Year==i] <- subset(POP.15_PA$Total1[POP.15_PA$Year==i],
                                                POP.15_PA$Age<prosp.age65$prosp.age65[prosp.age65$Year==i])
  }

# Now make the rows where the population == 0

POP.15_PA <- POP.15_PA %>% mutate(Total1 = ifelse(is.na(Total1), 0, Total1))

# Now aggregate the values by year

POP.15_PA <- aggregate(x=POP.15_PA$Total1, by=list(POP.15_PA$Year), FUN=sum)
names(POP.15_PA) <- c("Year","Pop15_PA")

### Population older than Prospective Age
POP.PA <- POP.ESP %>% filter(Year>1949) %>% select(Total1, Year, Age)
for (j in 1950:2016){
  POP.PA$Total1[POP.PA$Year==j] <- subset(POP.PA$Total1[POP.PA$Year==i],
                                                POP.PA$Age>=prosp.age65$prosp.age65[prosp.age65$Year==i])
}
# Now make the rows where the population == 0
POP.PA <- POP.PA %>% mutate(Total1 = ifelse(is.na(Total1), 0, Total1))

# Now aggregate the values by year

POP.PA <- aggregate(x=POP.PA$Total1, by=list(POP.PA$Year), FUN=sum)
names(POP.PA) <- c("Year","Pop_PA")


# Combine and calculate the percentage
PROP.ODR <- left_join(POP.15_PA, POP.PA, by="Year")
PROP.ODR <- PROP.ODR %>% mutate(PODR = `Pop_PA`/`Pop15_PA`)


### ------------------------------------------------------------------------------------------------- ###
# ODR + PROP ODR
PRODR_plot <- POP.ODR %>% filter(Year>1949) %>% ggplot() + 
  geom_line(aes(x=Year, y=ODR, linetype="Classic OADR")) +
  geom_line(aes(x=PROP.ODR$Year, y=PROP.ODR$PODR, linetype="Prospective OADR")) +
  # geom_point(aes(x = Year, y = ODR)) +
  scale_x_continuous(breaks = round(seq(1950, max(POP.ESP_65$Year), by = 10),1)) + 
  scale_y_continuous(name = "Dependency Ratios", breaks = round(seq(0.1, 0.35, by = 0.025),3)) +
  scale_linetype(name= " ") +
  theme_bw()
PRODR_plot <- PRODR_plot + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=12,face="bold")) + theme(legend.position = c(0.2, 0.85))


### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------- ###



### For comparison use the median age
POP.ESP2 <- POP.ESP %>% filter(Year>=1950)

# function to calculate the median age of the population

MEDIAN.fun <- function(x){
  x$Age[max(which(cumsum(x$Total1)/sum(x$Total1)<0.5)+1)] 
}


median.age <- by(data = POP.ESP2, INDICES = POP.ESP2$Year, FUN = MEDIAN.fun)

median.age <- as.data.frame(cbind(unique(tot.smooth$Year),median.age))
colnames(median.age)[1] <- "Year"

# Define index year (i.e. 1950) and "retrospective" age with desired remaining life expectancy

# Start at the median age at the INDEX YEAR with the time line of the prospective age measures

X <- median.age$median.age[median.age$Year==1950] 
X # 27

# The index value (life expectancy in the INDEX YEAR and the median age => to calculate the median prospective age)
tot.smooth$ex[tot.smooth$Year==1950 & tot.smooth$Age==27]
# 43.65843

# insert the number in the function that finds the nearest value 

NEAR.fun <- function(x){
  which.min(abs(x$ex- 43.65843)) }

prosp.age <- by(data = tot.smooth, INDICES = tot.smooth$Year, FUN = NEAR.fun)

prosp.age <- as.data.frame(cbind(unique(tot.smooth$Year),prosp.age))
colnames(prosp.age)[1] <- "Year"

summary(prosp.age$prosp.age)

# Prospective Aging
PA_plot <- prosp.age %>% ggplot() + 
  geom_line(aes(x=Year, y=prosp.age, linetype="Prospective Median Age")) +
  geom_line(aes(x=median.age$Year, y=median.age$median.age, linetype="Median Age")) +
  #geom_point(aes(x = median.age$Year, y = prosp.age)) +
  scale_x_continuous(breaks = round(seq(1950, 2016, by = 10),1)) + 
  scale_y_continuous(name = "Prospective Age", breaks = seq(25, 45, by = 2.5)) +
  scale_linetype(name= " ") +
  theme_bw()
PA_plot <- PA_plot + theme(legend.position = c(0.2, 0.80)) + theme(axis.text=element_text(size=12),
                                                                  axis.title=element_text(size=12,face="bold"))



### ------------------------------------------------------------------------------------------------- ###

multiplot(ODR_plot, PA_plot)
### ------------------------------------------------------------------------------------------------- ###



