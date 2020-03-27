################ Vector autoregressions #################



# loading required packages -----------------------------------------------

# Clean Workspace and setwd
rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,vars,car)

#import basetable
data <- read.csv('./sources/cleaned/basetable.csv', header=T)

#inspect basetable
#View(data)

#how many variables to include?
basetable <- data[,7:23]
basetable <- cbind(data$timestamp,basetable)
colnames(basetable)[1] <- "timestamp"

#Missing Values
apply(basetable, 2, function(x) any(is.na(x)))
#No missing values

#Adjust for multicol.
basetable$best_topic <- NULL
basetable$best_topic_gamma <- NULL
basetable$sentiment_dict_daily_avg <- NULL
basetable$sentimentr_2 <- NULL
basetable$topic_1_dummy <- NULL
basetable$topic_2_dummy <- NULL
basetable$topic_3_dummy <- NULL
basetable$topic_4_dummy <- NULL


#Data visualization
plot(basetable$cancellations~basetable$timestamp)

#groupby date , useless because only 10rows?
basetable_group <- basetable %>% group_by(basetable$timestamp) %>% summarise_all("mean")
colnames(basetable_group)[1] <- "timestamp"
basetable_group[,2] <- NULL

#Try small example: 

try <- basetable[,(c('sentiment_dict_daily_avg' ,'cancellations'))]
VARselect(try, lag.max=8,type="const")
var4 <- VAR(try, p=1, type="const")
serial.test(var4, lags.pt=16, type="PT.asymptotic")

summary(var4, equation = "cancellations")
var4

forecast(var4) %>%
  autoplot() + xlab("Day")







#How many lags to include?
VARselect(basetable[,2:10], lag.max=4,type="both")

#Care should be taken when using the AIC as it tends to choose large numbers of lags. 
#Instead, for VAR models, we prefer to use the BIC

#modeling


var2 <- VAR(basetable[,2:10], p=2, type="both")

summary(var1, equation = "cancellations")

#matrix is not invertible, vectors are probably colinear

var1


serial.test(var1, lags.pt=16, type="PT.asymptotic")

var2 <- VAR(basetable, p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(basetable, p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")
#> 
#>  Portmanteau Test (asymptotic) to test if the residuals are uncorrelated
#> 
#> data:  Residuals of VAR object var3
#> Chi-squared = ??, df = ??, p-value = ??

#plot the generated forecast
forecast(var4) %>%
  autoplot() + xlab("Year")


#Source : https://otexts.com/fpp2/VAR.html
#Source : https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf




