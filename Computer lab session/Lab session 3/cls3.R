rm(list = ls())  # Clear environment

# Install the package "car", which provides the command "linearHypothesis"
install.packages('car')
library(car)

# Set working directory
setwd('C:\\Users\\dmtwong\\Desktop')  # Change this path appropriately

# Read the data set to the data frame "UIPdata"
UIPdata <- read.csv('UIP_dataset_5.csv', stringsAsFactors = F)
str(UIPdata)  # Display current data structure

# Convert observation dates from chr (character) to Date (R object) 
UIPdata <- transform(UIPdata, Date = as.Date(Date, "%m/%d/%Y"))
str(UIPdata)  # Display current data structure
#head(UIPdata); tail(UIPdata)

################################################ Model 1 ##################################################

# Estimate a (two explanatory variables) multiple regression model using OLS
M1 <- lm(s_ch_Aus ~ r_Aus+r_US, data=UIPdata) 
summary(M1)
M1summary <- summary(M1)

########################################## Confidence Intervals ###########################################

# Compute L_1, the left endpoint of a 95% confidence interval for the parameter of r_Aus
L_1 <- M1summary$coefficients[2,1] - 1.96*M1summary$coefficients[2,2]; L_1

# Compute R_1, the right endpoint of a 95% confidence interval for the parameter of r_Aus
R_1 <- M1summary$coefficients[2,1] + 1.96*M1summary$coefficients[2,2]; R_1

# Compute L_2, the left endpoint of a 95% confidence interval for the parameter of r_US
L_2 <- M1summary$coefficients[3,1] - 1.96*M1summary$coefficients[3,2]; L_2

# Compute R_2, the right endpoint of a 95% confidence interval for the parameter of r_US
R_2 <- M1summary$coefficients[3,1] + 1.96*M1summary$coefficients[3,2]; R_2

########################################### Hypothesis Testing ############################################

# Compute p_0, the p-value for the null hypothesis that the intercept is less than or equal to 0
t_0 <- M1summary$coefficients[1,3]  # The required t-statistic
p_0 <- 1 - pnorm(t_0); p_0

# Compute p_1, the p-value for the null hypothesis that the parameter of r_Aus is 1
t_1 <- (M1summary$coefficients[2,1] - 1) / M1summary$coefficients[2,2]  # Compute the required t-statistic
p_1 <- 2*pnorm(-abs(t_1)); p_1

# Test the null hypothesis that the sum of the parameters of r_Aus and r_US is 0
my_1st_H0 <- c("r_Aus+r_US=0")
linearHypothesis(M1, my_1st_H0)  
F_1st <- linearHypothesis(M1, my_1st_H0)[2, 5]  # Compute the required F-statistic
p_1st <- pf(F_1st, 1, Inf, lower.tail=FALSE); p_1st  # Compute the associated p-value

# Test the null hypothesis that the intercept is 0 and the parameters of r_Aus and r_US are -1 and 1, 
# respectively
my_2nd_H0 <- c("(Intercept)=0","r_Aus=-1","r_US=1")
linearHypothesis(M1, my_2nd_H0)  
F_2nd <- linearHypothesis(M1, my_2nd_H0)[2, 5]  # Compute the required F-statistic
p_2nd <- pf(F_2nd, 3, Inf, lower.tail=FALSE); p_2nd  # Compute the associated p-value

################################################ Model 2 ##################################################

UIPdata$s_ch_Aus_lag <- c(NA, UIPdata$s_ch_Aus[-nrow(UIPdata)])  # Add the one-period lagged variable 
head(UIPdata,3); tail(UIPdata,3)

# Estimate the 2nd (three explanatory variables) multiple regression model using OLS
M2 <- lm(s_ch_Aus ~ r_Aus+r_US+s_ch_Aus_lag, data=UIPdata) 
summary(M2)

################################################ Plots ####################################################

CC <- complete.cases(UIPdata)
UIPdataCC <- UIPdata[CC, ]  # Remove all missing values

# Plot the actual relative changes in the spot exchange rate
plot(UIPdataCC$Date, UIPdataCC$s_ch_Aus, type="l", lty=1, lwd=1.25, col="blue", xlab="", ylab="", 
     ylim = range(c(min(UIPdataCC$s_ch_Aus, na.rm=T)-2, max(UIPdataCC$s_ch_Aus, na.rm=T)+2)),
     xlim = range(min(UIPdataCC$Date), max(UIPdataCC$Date)))
par(new=T)  # Do not clean the frame before next plot
# Plot the by model 2 predicted relative changes in the spot exchange rate
plot(UIPdataCC$Date, predict(M2), type="l", lty=2, lwd=1.25, col="red", axes=F, xlab="", ylab="",
     ylim = range(c(min(UIPdataCC$s_ch_Aus, na.rm=T)-2, max(UIPdataCC$s_ch_Aus, na.rm=T)+2)),
     xlim = range(min(UIPdataCC$Date), max(UIPdataCC$Date)))
# Add a plot legend
legend("bottomleft", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1:2, cex=1.1)
# Add a plot title
title(main="Actual and predicted relative changes in the spot exchange rate", sub="", xlab="Date", ylab="")

SSP <- (UIPdataCC$Date > '1980-01-01' & UIPdataCC$Date < '1999-12-31')  # State the sub sample period
UIPdataSSP <- UIPdataCC[SSP, ]

# Plot the actual relative changes in the spot exchange rate over the sub sample period
plot(UIPdataSSP$Date, UIPdataSSP$s_ch_Aus, type="l", lty=1, lwd=1.25, col="blue", xlab="", ylab="", 
     ylim = range(c(min(UIPdataSSP$s_ch_Aus, na.rm=T)-2, max(UIPdataSSP$s_ch_Aus, na.rm=T)+2)),
     xlim = range(min(UIPdataSSP$Date), max(UIPdataSSP$Date)))
par(new=T)  # Do not clean the frame before next plot
# Plot the by model 2 predicted relative changes in the spot exchange rate over the sub sample period
plot(UIPdataSSP$Date, predict(M2)[SSP], type="l", lty=2, lwd=1.25, col="red", axes=F, xlab="", ylab="",
     ylim = range(c(min(UIPdataSSP$s_ch_Aus, na.rm=T)-2, max(UIPdataSSP$s_ch_Aus, na.rm=T)+2)),
     xlim = range(min(UIPdataSSP$Date), max(UIPdataSSP$Date)))
# Add a plot legend
legend("bottomleft", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1:2, cex=1.1)
# Add a plot title
title(main="Actual and predicted relative changes in the spot exchange rate (1980-1999)", sub="", 
      xlab="Date", ylab="")

# Plot the OLS residuals of model 2 
plot(UIPdataCC$Date, unname(resid(M2)), xlab="", ylab="")
lines(UIPdataCC$Date, unname(resid(M2)), col='blue')
title(main="OLS residuals of the extended model", sub="", xlab="Date", ylab="Residual")  # Add a plot title

