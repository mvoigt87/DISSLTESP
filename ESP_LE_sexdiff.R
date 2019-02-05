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
plotLE_zero

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
  scale_shape_discrete(guide=FALSE)

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


  
  
  
  
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####  
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####  

  ### Rectangularization plot with smooth data
  
  ## 1. Smooth grid of age-specific death rates
  
  # Data manipulation
  
  y <- unique(DxS_M$Year)
  x <- unique(DxS_M$Age)
  m <- length(x)
  n <- length(y)
  
  # 1.1 Extract deaths and exposure
  
  # Dx
  DxS_M = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Male)
  DxS_F = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Female)
  DxS_T = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Total)
  
  
  # Nx
  NxS_M = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Male1)
  NxS_F = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Female1)
  NxS_T = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Total1)
  
  
  ## Deaths and exposures in a matrix format (age x year)
  
  # --- males
  D.Mal <- do.call(cbind,tapply(X = DxS_M$Male, INDEX = DxS_M$Year, FUN = identity))
  
  E.Mal <- do.call(cbind,tapply(X = NxS_M$Male1, INDEX = NxS_M$Year, FUN = identity))
  
  # --- females
  D.Fem <- do.call(cbind,tapply(X = DxS_F$Female, INDEX = DxS_F$Year, FUN = identity))
  
  E.Fem <- do.call(cbind,tapply(X = NxS_F$Female1, INDEX = NxS_F$Year, FUN = identity))
  
  # --- total
  D.Tot <- do.call(cbind,tapply(X = DxS_T$Total, INDEX = DxS_T$Year, FUN = identity))
  
  E.Tot <- do.call(cbind,tapply(X = NxS_T$Total1, INDEX = NxS_T$Year, FUN = identity))
  

  # some E are equal to zer0, -> weights are necessary  (Wheight matrix)
  W <- matrix(1, m, n)
  
  W[E.Mal==0] <- 0
  W[E.Fem==0] <- 0
  W[E.Tot==0] <- 0
  
  # Smoothing deaths using Mort2Dsmooth() for both sexes and combined mortality
  # ----------------------------------------------------------------------------
  
  fitDx_M = Mort2Dsmooth(x = x, y = y, Z = D.Mal, offset = log(E.Mal),W=W)
  fitDx_F = Mort2Dsmooth(x = ages, y = years, Z = D.Fem, offset = log(E.Fem),W=W)
  fitDx_T = Mort2Dsmooth(x = ages, y = years, Z = D.Tot, offset = log(E.Tot), W=W)
  
  # plot raw age-specific mortality rates vs. smoothed rates
  # --------------------------------------------------------
  plot(fitDx_M)
  plot(fitDx_F)
  plot(fitDx_T)
  
  # See the fit of log mortality (should be more or less on a straight line)    #### it is!
  # ------------------------------------------------------------------------
  plot(fitDx_M$logmortality, log(D.Mal/E.Mal))
  plot(fitDx_F$logmortality, log(D.Fem/E.Fem))
  plot(fitDx_T$logmortality, log(D.Tot/E.Tot))  
  
### ------------------------------------------------------------------------------------------------ ###   

#############
#### MALE ###
#############
  
  # survival function by year (beautiful ;) )
  delta <- 1
  Bxs.Mal <- fitDx_M$Bx
  ## over years are the same
  By.Mal <- fitDx_M$By
  ## fitted coefficients
  betas.Mal <- fitDx_M$coef
  
  ## log-mortality (linear predictor) over new ages and years (output: log hazard)
  ln.h.Mal <- MortSmooth_BcoefB(Bxs.Mal, By.Mal, betas.Mal)
  ## hazard
  h.Mal <- exp(ln.h.Mal)
  
  ## cumulative hazard using cumsum
  H.Mal <- matrix(0, m, n)
  for(i in 1:n){
    H.Mal[,i] <- cumsum(h.Mal[,i]*delta)
  }
  
  ## fitted survival functions
  S.Mal <- apply(H.Mal, 2, function(x){exp(-x)})
  ## fitted density functions
  f.Mal <- h.Mal * S.Mal
  
  image(t(f.Mal))
  
  #### building life tables from the fx ####
  
  dim(f.Mal)
  # radix of the table by year
  colSums(f.Mal)
  
### ------------------------------------------------------------------------------------------------ ### 
  
### ------------------------------------------------------------------------------------------------ ###   

###############
#### FEMALE ###
###############
  
    # survival function by year (beautiful ;) )
  delta <- 1
  Bxs.Fem <- fitDx_F$Bx
  ## over years are the same
  By.Fem <- fitDx_F$By
  ## fitted coefficients
  betas.Fem <- fitDx_F$coef
  
  ## log-mortality (linear predictor) over new ages and years (output: log hazard)
  ln.h.Fem <- MortSmooth_BcoefB(Bxs.Fem, By.Fem, betas.Fem)
  ## hazard
  h.Fem <- exp(ln.h.Fem)
  
  ## cumulative hazard using cumsum
  H.Fem <- matrix(0, m, n)
  for(i in 1:n){
    H.Fem[,i] <- cumsum(h.Fem[,i]*delta)
  }
  
  ## fitted survival functions
  S.Fem <- apply(H.Fem, 2, function(x){exp(-x)})
  ## fitted density functions
  f.Fem <- h.Fem * S.Fem
  
  image(t(f.Fem))
  
  #### building life tables from the fx ####
  
  dim(f.Fem)
  # radix of the table by year
  colSums(f.Fem)
  

cols <- grey(ncol(S.Fem):1/ncol(S.Fem))
  
### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###   

### ------------------------------------------------------------------------------------------------ ### 

### ------------------------------------------------------------------------------------------------ ###   

##############
#### TOTAL ###
##############

# survival function by year (beautiful ;) )
delta <- 1
Bxs.Tot <- fitDx_T$Bx
## over years are the same
By.Tot <- fitDx_T$By
## fitted coefficients
betas.Tot <- fitDx_T$coef

## log-mortality (linear predictor) over new ages and years (output: log hazard)
ln.h.Tot <- MortSmooth_BcoefB(Bxs.Tot, By.Tot, betas.Tot)
## hazard
h.Tot <- exp(ln.h.Tot)

## cumulative hazard using cumsum
H.Tot <- matrix(0, m, n)
for(i in 1:n){
  H.Tot[,i] <- cumsum(h.Tot[,i]*delta)
}

## fitted survival functions
S.Tot <- apply(H.Tot, 2, function(x){exp(-x)})
## fitted density functions
f.Tot <- h.Tot * S.Tot

image(t(f.Tot))

#### building life tables from the fx ####

dim(f.Tot)
# radix of the table by year
colSums(f.Tot)


### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###





  par(mfrow = c(1, 2))
  # survival function by year (beautiful ;) )
  matplot(S.Mal, type = "l", lty = 1, col = cols, xlab="Age", ylab="Probability of Survival", main="Male")
  legend("bottomleft", inset=.05, legend=c("1915", "2016"), lty=1, col=c("lightgrey", "black"), bty="n")
  matplot(S.Fem, type = "l", lty = 1, col = cols, xlab = "Age", ylab=" ", main="Female")                   
  par(mfrow = c(1,1))
 
   # different colors: heat.colors(n = ncol(S.Fem))

    
### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###   
  
  
#### Plotting with smooth survival curves for total population in ggplot
#### -------------------------------------------------------------------
  
  #ggplot needs a dataframe
  data <- as.data.frame(S.Tot)
  #id variable for position in matrix 
  data$id <- 1:nrow(S.Tot) 
  #reshape to long format
  plot_data <- melt(S.Tot,id.var="id")
  # to change Var2 back to years and ages for easier handling
  plot_data <- plot_data %>% mutate(years = Var2+1914) %>% mutate(ages = Var1)
  
  
  #plot
  ggplot_Surv <- plot_data %>% ggplot(aes(x=ages,y=value,group=years,colour=years)) +
                 geom_line() +
                 scale_y_continuous(name = "Probability of Survival") +
                 scale_x_continuous(name = "Age") +
                 scale_colour_gradient(name= " ",low = "white", high = "black") +
                 theme_bw()
  # move legend
  ggplot_Surv <-ggplot_Surv + theme(legend.position = c(0.1, 0.2))
    
  
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

##########################################################################################
##########################################################################################
 #### Disparity Measures of Mortality (Vaupels table) ####
  disp1 <- read.csv("C:/Users/y4956294S/Documents/LONGPOP/Data-Andalucia/Lifespan-disparity/disparity_R.csv", header = T, sep=";")
  head(disp1) # delete variables at the end
  disp1 <- subset(disp1,select = 1:8)
  #tbl_df(disp1)
  #disp1 %>% select(disp1,-X)
  
  disp.esp <- subset(disp1, Country=="ESP")
  disp.fem <- subset(disp.esp,Sex=="female")
  disp.mal <- subset(disp.esp,Sex=="male")
  
  ### Plot the measures for men and women in the same plot
  summary(disp.fem$Gini)   # between 0.071 and 0.519
  summary(disp.mal$Gini)   # between 0.091 and 0.521
  
  par(mfrow=c(1,3))
  gini.ESP <- plot (disp.fem$Year1, disp.fem$Gini, type="l", lwd=2, col="red", ylab="GINI coefficient", xlab = "Year", ylim=c(0,0.6))
  lines(disp.mal$Year1, disp.mal$Gini, lwd=2, col="blue")
  legend ("topright", c("females", "males"), col=c("red","blue"), lwd=c(2,2), bty="n")
  
  # Keyfitz entropy
  gini.ESP <- plot (disp.fem$Year1, disp.fem$H, type="l", lwd=2, col="red", ylab="Keyfitz' Entropy", xlab = "Year")
  lines(disp.mal$Year1, disp.mal$H, lwd=2, col="blue")
  legend ("topright", c("females", "males"), col=c("red","blue"), lwd=c(2,2), bty="n")
  
  # Coefficient Variation -  CV 
  gini.ESP <- plot (disp.fem$Year1, disp.fem$CV, type="l", lwd=2, col="red", ylab="CV", xlab = "Year")
  lines(disp.mal$Year1, disp.mal$CV, lwd=2, col="blue")
  legend ("topright", c("females", "males"), col=c("red","blue"), lwd=c(2,2), bty="n")
  par(mfrow=c(1,1))
  