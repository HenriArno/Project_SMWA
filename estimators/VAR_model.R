################ Vector autoregressions #################



# loading required packages -----------------------------------------------

# Clean Workspace and setwd
rm(list=ls())

#sets working directory to file directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("../..")
setwd("~/R/SMWA/Assignment 2") ######################### !!!!!!!!!!!!!!!!!!!!!!!!! 

# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,vars)

#import basetable
basetable <- read.csv('./sources/raw/dataset.csv', header=F)

#how many variables to include?
basetable <- basetable %>% select("")

#How many lags to include?
VARselect(basetable[,1:2], lag.max=8,
          type="const")[["selection"]]

#Care should be taken when using the AIC as it tends to choose large numbers of lags. 
#Instead, for VAR models, we prefer to use the BIC

#modeling

var1 <- VAR(basetable[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(basetable[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")
var3 <- VAR(basetable[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")
#> 
#>  Portmanteau Test (asymptotic) to test if the residuals are uncorrelated
#> 
#> data:  Residuals of VAR object var3
#> Chi-squared = ??, df = ??, p-value = ??

#plot the generated forecast
forecast(var3) %>%
  autoplot() + xlab("Year")


#Source : https://otexts.com/fpp2/VAR.html




