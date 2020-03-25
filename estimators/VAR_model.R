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
p_load(tidyverse,vars)

#import basetable
data <- read.csv('./sources/cleaned/basetable.csv', header=T)

#inspect basetable
View(data)

#how many variables to include?
basetable <- data[,7:23]
basetable <- cbind(data$timestamp,basetable)

#Missing Values
apply(basetable, 2, function(x) any(is.na(x)))
#NA value in cancellations


for(i in nrow(basetable)){
  if(is.na(basetable[i,18])){
    #take average
    
  }
}

apply(basetable, 2, function(x) any(is.na(x)))

#For now, delete missing values
basetable <- basetable %>% drop_na()

#Check for multicol.
basetable$best_topic <- NULL
basetable$best_topic_gamma <- NULL
basetable$sentiment_dict_daily_avg <- NULL
basetable$sentimentr_2 <- NULL
colnames(basetable)[1] <- "timestamp"
basetable$topic_4_gamma <- NULL

#Data visualization
plot(basetable$cancellations~basetable$timestamp)

#groupby date

basetable <- basetable %>% group_by(basetable$timestamp) %>% summarise_all("mean")
colnames(basetable)[1] <- "timestamp"
basetable[,2] <- NULL
str(basetable)

#How many lags to include?
VARselect(basetable[,2:13], lag.max=4,
          type="both")[["selection"]]

#Care should be taken when using the AIC as it tends to choose large numbers of lags. 
#Instead, for VAR models, we prefer to use the BIC

#modeling


#LM model

dep <- "cancellations"

indep <- basetable
indep$timestamp <- NULL
indep$cancellations <- NULL
indep <- colnames(indep)
lm_data <- basetable
lm_data$timestamp <- NULL


model <- lm(basetable$cancellations ~ basetable$sentiment_dict + basetable$sentimentr_1 +basetable$topic_1_dummy 
            +basetable$topic_2_dummy +basetable$topic_3_dummy +basetable$topic_4_dummy +basetable$topic_1_gamma 
            +basetable$topic_2_gamma +basetable$topic_3_gamma +basetable$sentimentr_1_wordc +basetable$sentimentr_2_wordc)

summary(model)





#VAR model

var1 <- VAR(basetable[,2:13], p=1, type="both")
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
forecast(var3) %>%
  autoplot() + xlab("Year")


#Source : https://otexts.com/fpp2/VAR.html
#Source : https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf




