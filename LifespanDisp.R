#### --------------------------- ####
#### Lifespan Disparity Measures ####
#### --------------------------- ####

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

library(latex2exp)

# load data

load("MLT_HMD.Rdata")
load("FLT_HMD.Rdata")
load("TLT_HMD.Rdata") # doublecheck

### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###





### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###


# ### Calculate the (late) Modal ages at death
# 
MDA.fun <- function(lt) {
  
  # fine if we work with integer ages above age 10
  x <- lt$Age[which.max(lt$dx)]
  M <- x + ((lt$dx[x]-lt$dx[x-1])/(lt$dx[x]-lt$dx[x-1])+(lt$dx[x]-lt$dx[x+1]))
  return(M)
}
# 
# tot.smooth <- tot.smooth %>% filter(Age<=100)
# Mode.1920 <- MDA.fun(tot.smooth[tot.smooth$Year==1920])


###########################################################################

tot.smooth_Mod <- tot.smooth %>% filter(Age>5) %>% filter(Age<=100)

# List of modal ages (after age 5)
Modes_tot <- by(data = tot.smooth_Mod, INDICES = tot.smooth_Mod$Year, FUN = MDA.fun)

Modes_tot <- as.data.frame(cbind(unique(tot.smooth$Year),Modes_tot))
colnames(Modes_tot)[1] <- "Year"

####### Add modal ages for random years to distribution of death plot (For now 1920 und 2010)



### Plot of distributions of deaths (life tables for both sexes)
################################################################

textmod1920 <- "Mode 1920"
textmod2010 <- "Mode 2010"

# ---- #
ggplot_dx <- tot.smooth %>% ggplot(aes(x=Age,y=dx, group=Year, colour=Year)) +
  geom_line() +
  scale_y_continuous(name = "Distributions of deaths", limit = c(0, 0.06)) +
  scale_x_continuous(name = "Age") +
  scale_colour_gradient(name= " ",low = "white", high = "black") +
  geom_vline(xintercept=c(75.00145, 89.00405), linetype='dashed', alpha=0.75) +
  geom_text(aes(x = 73, y = 0.05, label = textmod1920, angle=90)) +
  geom_text(aes(x = 87, y = 0.05, label = textmod2010, angle=90)) +
  theme_bw()

# move legend
ggplot_dx <-ggplot_dx + theme(legend.position = c(0.15, 0.80))



### Load Disparity Measures functions from the files
### ------------------------------------------------

class(mal.smooth)

# Cut at age 100 (to avoid the problem with the NAs)
mal.smooth <- mal.smooth %>% filter(Age<=100)
fem.smooth <- fem.smooth %>% filter(Age<=100)


# E-Dagger ### time series
# ------------------------ #

# males
edag_M <- by(data = mal.smooth, INDICES = mal.smooth$Year, FUN = EDAG.FUN)

# females
edag_F <- by(data = fem.smooth, INDICES = fem.smooth$Year, FUN = EDAG.FUN)


# Plot E dagger
# -------------

# data frame

edag_both <- as.data.frame(cbind(unique(mal.smooth$Year),edag_M, edag_F))

colnames(edag_both)[1] <- "Year"

edag_both_plot <- edag_both %>% ggplot(aes(x=Year)) +
                  geom_line(aes(y = edag_M, color="Male")) + 
                  geom_line(aes(y = edag_F, color="Female")) +
                  scale_y_continuous(name = TeX('$e^\\dagger$')) +
                  scale_color_manual(name=" ", values = c("black", "grey65"), guide=F) +
                  theme_bw()

# edag_both_plot <- edag_both_plot + theme(legend.position = c(0.8, 0.8))


# Function to estimate the standard deviation around the modal age at death
# ------------------------------------------------------------------------- #

# Literature Canudas Romo (2008)
      
sdfun <- function(x, trun = 0){
  
  if(is.numeric(trun)){
    
    x <- x[x$Age >= trun,]
    
  }else{
    
    mode <- MDA.fun(x)
    
    x <- x[na.omit(x$Age),]
    
  }
  
  # mean <- sum(x$Age * x$dx) / sum(x$dx)
  
  sdev <- sqrt(sum((x$Age - mode)^2 * x$dx) / sum(x$dx))
  
  return(sdev)
  
}
      # # To avoid a second mode at age zero
      # 
      # mal.smooth <- mal.smooth %>% filter(Age>=5)
      # fem.smooth <- fem.smooth %>% filter(Age>=5)
      # 


### Time Series ###
### ----------- ###

      # males
      sd_M <- by(data = mal.smooth, INDICES = mal.smooth$Year, FUN = sdfun)


      # females
      sd_F <- by(data = fem.smooth, INDICES = fem.smooth$Year, FUN = sdfun)
      
      
      # # Plot the SD+
      # # -----------

      sd_both <- as.data.frame(cbind(unique(mal.smooth$Year),sd_M, sd_F))

      colnames(sd_both)[1] <- "Year"

      sd_both_plot <- sd_both %>% ggplot(aes(x=Year)) +
        geom_line(aes(y = sd_M, color="Male")) +
        geom_line(aes(y = sd_F, color="Female")) +
        scale_y_continuous(name = "SDM") +
        scale_color_manual(name=" ", values = c("black", "grey65")) +
        theme_bw()

      sd_both_plot <- sd_both_plot + theme(legend.position = c(0.8, 0.8))+
        
        
#### Combine both measures in one graph
        
multiplot(edag_both_plot, sd_both_plot)
        


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
