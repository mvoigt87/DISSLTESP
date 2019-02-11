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

#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####  
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####  

  ### Rectangularization plot with smooth data
  
  ## 1. Smooth grid of age-specific death rates
  
  # 1.1 Extract deaths and exposure
  
  # Dx
  DxS_M = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Male)
  DxS_F = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Female)
  DxS_T = DEAD.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Total)
  
  
  # Nx
  NxS_M = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Male1)
  NxS_F = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Female1)
  NxS_T = POP.ESP %>% filter(Year>=1915 & Year<=2016) %>% select(Year, Age, Total1)
  
  
  # Data manipulation
  
  y <- unique(DxS_M$Year)
  x <- unique(DxS_M$Age)
  m <- length(x)
  n <- length(y)
  

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
  fitDx_F = Mort2Dsmooth(x = x, y = y, Z = D.Fem, offset = log(E.Fem),W=W)
  fitDx_T = Mort2Dsmooth(x = x, y = y, Z = D.Tot, offset = log(E.Tot), W=W)
  
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
  # rough estimate of the life expectancy at birth
  plot(y, colSums(S.Mal) / 10, type = "b", las = 1, main = "e0")
  
  
  ######################
  # Build a life table #
  ######################
  
  # ------------------------------- #  
    N <- length(x)
    Widths <- rep(delta, length(x))
  # ------------------------------- #
  
  mal.smooth <- as.data.frame(rep(x,n))
  
  mal.smooth <- mal.smooth %>% mutate(Year=rep(min(y):max(y), times=1, each=111)) %>% 
    # ax values (may be to be changed for the highest age groups)
    mutate(ax = rep(Widths / 2, times=n))
  # ------------------------------------------------------------
  # ax for first year of life
  
  # ------------------------------------------------------------
  # obtain the mx values from the smoothed hazard function
  dim(h.Mal)
  ## get the hx in the right format
  h.new.Mal <- as.data.frame(h.Mal)
  h.new.Mal <- data.frame(mx=unlist(h.new.Mal, use.names = FALSE))
  # ------------------------------------------------------------
  mal.smooth <- mal.smooth %>% bind_cols(h.new.Mal) %>% 
    # qx
    mutate(qx = (Widths * mx) / (1 + (Widths - ax) * mx)) 
  # ------------------------------------------------------------
  ## make the last qx=1 with a little trick which would not work with a data frame
  qx <- matrix(mal.smooth$qx)
  qx[1:(0+111)==(0+111)] <- 1
  # ------------------------------------------------------------      
  mal.smooth <- mal.smooth %>% select(-qx) %>%  bind_cols(as.data.frame(qx))
  colnames(mal.smooth)[5] <- "qx"
  mal.smooth <- mal.smooth %>% mutate(qx = ifelse(qx>1,1,qx)) %>% 
    ## add the px
    mutate(px = 1 - qx)
  # ------------------------------------------------------------   
  ## matrix operations: sum over the columns of the estimated f-values to obtain
  ## the base/radix for the life table
  radix.mat <- as.data.frame(matrix(data=colSums (f.Mal, na.rm = FALSE, dims = 1), nrow = 1)) %>% 
    ## now making filling dummie values in between to make it the same length as the data frame
    bind_rows(as.data.frame(matrix(data = 0,nrow = 110, ncol = 102))) 
  ## stack them in order and delete the extra variable
  radix.mat <- stack(radix.mat) %>% select(-ind)
  # ------------------------------------------------------------   
  mal.smooth <- mal.smooth %>% bind_cols(as.data.frame(radix.mat))
  colnames(mal.smooth)[7] <- "lx"
  ## use the dplyr group_by command to calculate the rest of the lx from the px
  mal.smooth <- mal.smooth %>% group_by(Year) %>% mutate(lx = c(lx[1],lx[1] * cumprod(px))[1:N]) %>% 
    ## dx values from the lx (alternatively from the smoothing algorithm)
    group_by(Year) %>%  mutate(dx = c(-diff(lx),lx[N])) %>% 
    ## Create the Lx from the lx and the dx
    group_by(Year) %>% mutate(Lx = c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * ax[N])) %>% 
    ## account for infinite Lx and NA
    mutate(Lx = ifelse(is.infinite(Lx),1,Lx)) %>% mutate(Lx = ifelse(is.na(Lx),0,Lx)) %>% 
    ## Calculate the Tx from the Lx
    group_by(Year) %>% mutate(Tx = rev(cumsum(rev(Lx)))) %>% 
    ## Finally obtain the life expectancy from the Tx and lx
    group_by(Year) %>% mutate(ex = Tx / lx)
  
  # Change names for ages
  colnames(mal.smooth)[1] <- "Age"
  
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
  
######################
# Build a life table #
######################

# ------------------------------- #  
N <- length(x)
Widths <- rep(delta, length(x))
# ------------------------------- #

fem.smooth <- as.data.frame(rep(x,n))

fem.smooth <- fem.smooth %>% mutate(Year=rep(min(y):max(y), times=1, each=111)) %>% 
  # ax values (may be to be changed for the highest age groups)
  mutate(ax = rep(Widths / 2, times=n))
# ------------------------------------------------------------
# ax for first year of life

# ------------------------------------------------------------
# obtain the mx values from the smoothed hazard function
dim(h.Fem)
## get the hx in the right format
h.new.Fem <- as.data.frame(h.Fem)
h.new.Fem <- data.frame(mx=unlist(h.new.Fem, use.names = FALSE))
# ------------------------------------------------------------
fem.smooth <- fem.smooth %>% bind_cols(h.new.Fem) %>% 
  # qx
  mutate(qx = (Widths * mx) / (1 + (Widths - ax) * mx)) 
# ------------------------------------------------------------
## make the last qx=1 with a little trick which would not work with a data frame
qx <- matrix(fem.smooth$qx)
qx[1:(0+111)==(0+111)] <- 1
# ------------------------------------------------------------      
fem.smooth <- fem.smooth %>% select(-qx) %>%  bind_cols(as.data.frame(qx))
colnames(fem.smooth)[5] <- "qx"
fem.smooth <- fem.smooth %>% mutate(qx = ifelse(qx>1,1,qx)) %>% 
  ## add the px
  mutate(px = 1 - qx)
# ------------------------------------------------------------   
## matrix operations: sum over the columns of the estimated f-values to obtain
## the base/radix for the life table
radix.mat <- as.data.frame(matrix(data=colSums (f.Fem, na.rm = FALSE, dims = 1), nrow = 1)) %>% 
  ## now making filling dummie values in between to make it the same length as the data frame
  bind_rows(as.data.frame(matrix(data = 0,nrow = 110, ncol = 102))) 
## stack them in order and delete the extra variable
radix.mat <- stack(radix.mat) %>% select(-ind)
# ------------------------------------------------------------   
fem.smooth <- fem.smooth %>% bind_cols(as.data.frame(radix.mat))
colnames(fem.smooth)[7] <- "lx"
## use the dplyr group_by command to calculate the rest of the lx from the px
fem.smooth <- fem.smooth %>% group_by(Year) %>% mutate(lx = c(lx[1],lx[1] * cumprod(px))[1:N]) %>% 
  ## dx values from the lx (alternatively from the smoothing algorithm)
  group_by(Year) %>%  mutate(dx = c(-diff(lx),lx[N])) %>% 
  ## Create the Lx from the lx and the dx
  group_by(Year) %>% mutate(Lx = c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * ax[N])) %>% 
  ## account for infinite Lx and NA
  mutate(Lx = ifelse(is.infinite(Lx),1,Lx)) %>% mutate(Lx = ifelse(is.na(Lx),0,Lx)) %>% 
  ## Calculate the Tx from the Lx
  group_by(Year) %>% mutate(Tx = rev(cumsum(rev(Lx)))) %>% 
  ## Finally obtain the life expectancy from the Tx and lx
  group_by(Year) %>% mutate(ex = Tx / lx)

# Change names for ages
colnames(fem.smooth)[1] <- "Age"

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

tot.smooth <- as.data.frame(rep(x,n))

# ------------------------------- #  
N <- length(x)
Widths <- rep(delta, length(x))
# ------------------------------- #

tot.smooth <- tot.smooth %>% mutate(Year=rep(min(y):max(y), times=1, each=111)) %>% 
  # ax values (may be to be changed for the highest age groups)
  mutate(ax = rep(Widths / 2, times=n))
# ------------------------------------------------------------
# ax for first year of life

# ------------------------------------------------------------
# obtain the mx values from the smoothed hazard function
dim(h.Tot)
## get the hx in the right format
h.new.Tot <- as.data.frame(h.Tot)
h.new.Tot <- data.frame(mx=unlist(h.new.Tot, use.names = FALSE))
# ------------------------------------------------------------
tot.smooth <- tot.smooth %>% bind_cols(h.new.Tot) %>% 
  # qx
  mutate(qx = (Widths * mx) / (1 + (Widths - ax) * mx)) 
# ------------------------------------------------------------
## make the last qx=1 with a little trick which would not work with a data frame
qx <- matrix(tot.smooth$qx)
qx[1:(0+111)==(0+111)] <- 1
# ------------------------------------------------------------      
tot.smooth <- tot.smooth %>% select(-qx) %>%  bind_cols(as.data.frame(qx))
colnames(tot.smooth)[5] <- "qx"
tot.smooth <- tot.smooth %>% mutate(qx = ifelse(qx>1,1,qx)) %>% 
  ## add the px
  mutate(px = 1 - qx)
# ------------------------------------------------------------   
## matrix operations: sum over the columns of the estimated f-values to obtain
## the base/radix for the life table
radix.mat <- as.data.frame(matrix(data=colSums (f.Tot, na.rm = FALSE, dims = 1), nrow = 1)) %>% 
  ## now making filling dummie values in between to make it the same length as the data frame
  bind_rows(as.data.frame(matrix(data = 0,nrow = 110, ncol = 102))) 
## stack them in order and delete the extra variable
radix.mat <- stack(radix.mat) %>% select(-ind)
# ------------------------------------------------------------   
tot.smooth <- tot.smooth %>% bind_cols(as.data.frame(radix.mat))
colnames(tot.smooth)[7] <- "lx"
## use the dplyr group_by command to calculate the rest of the lx from the px
tot.smooth <- tot.smooth %>% group_by(Year) %>% mutate(lx = c(lx[1],lx[1] * cumprod(px))[1:N]) %>% 
  ## dx values from the lx (alternatively from the smoothing algorithm)
  group_by(Year) %>%  mutate(dx = c(-diff(lx),lx[N])) %>% 
  ## Create the Lx from the lx and the dx
  group_by(Year) %>% mutate(Lx = c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * ax[N])) %>% 
  ## account for infinite Lx and NA
  mutate(Lx = ifelse(is.infinite(Lx),1,Lx)) %>% mutate(Lx = ifelse(is.na(Lx),0,Lx)) %>% 
  ## Calculate the Tx from the Lx
  group_by(Year) %>% mutate(Tx = rev(cumsum(rev(Lx)))) %>% 
  ## Finally obtain the life expectancy from the Tx and lx
  group_by(Year) %>% mutate(ex = Tx / lx)

  # Change names for ages
  colnames(tot.smooth)[1] <- "Age"
  
### ------------------------------------------------------------------------------------------------ ###   
### ------------------------------------------------------------------------------------------------ ###
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
  

  
  ### ------------------------------------------------------------------------------------------------ ###   
  ### ------------------------------------------------------------------------------------------------ ###
  
  ############################################
  ##### Save life tables as data frames! #####
  ############################################
  
  ## Female Life Table
  save(fem.smooth,file = "FLT_HMD.Rdata")
  
  ## Male Life Table
  save(mal.smooth,file = "MLT_HMD.Rdata")
  
  ## Total Life Table
  save(tot.smooth,file = "TLT_HMD.Rdata")
  