# Install the package "stargazer", which provides the command "stargazer"
install.packages('stargazer')
library(stargazer)

# Install the package "lmtest", which provides the command "coeftest"
install.packages('lmtest')
library(lmtest)

# Install the package "car", which provides the command "hccm"
install.packages('car')
library(car)

# Set working directory
setwd('/Users/pdapreve/Desktop/CLS 4')  # Change this path appropriately

# Read the data set to the data frame "LFdata"
LFdata <- read.csv('cls4_data.csv')
str(LFdata)  # Display data structure

# Estimate a linear probability model using OLS
LPM <- lm(inlf ~ educ+exper+age+kidslt6, data=LFdata)

# Regression output based on White's heteroskedasticity-robust standard errors
coeftest(LPM, vcov=hccm(LPM, type="hc0"))

# Estimate probit model using ML
PM <- glm(inlf ~ educ+exper+age+kidslt6, family = binomial(link=probit), data=LFdata) 

# Estimate logit model using ML
LM <- glm(inlf ~ educ+exper+age+kidslt6, family = binomial(link=logit), data=LFdata)

# Display probit & logit estimation results
stargazer(PM, LM, type = 'text', digits=4)

# Compute a certain predicted probability using the linear probability, probit, and logit models
LPMsummary <- summary(LPM)
PP_LPM <- LPMsummary$coefficients[1,1] + LPMsummary$coefficients[2,1]*17 + LPMsummary$coefficients[3,1]*20 + 
  LPMsummary$coefficients[4,1]*39 + LPMsummary$coefficients[5,1]*0  # linear probability model
PMsummary <- summary(PM)
PP_PM <- pnorm(PMsummary$coefficients[1,1] + PMsummary$coefficients[2,1]*17 + PMsummary$coefficients[3,1]*20 + 
  PMsummary$coefficients[4,1]*39 + PMsummary$coefficients[5,1]*0) # probit model
LMsummary <- summary(LM)
PP_LM <- 1 / (1 + exp(-(LMsummary$coefficients[1,1] + LMsummary$coefficients[2,1]*17 + LMsummary$coefficients[3,1]*20 + 
  LMsummary$coefficients[4,1]*39 + LMsummary$coefficients[5,1]*0)))  # logit model

# Display the predicted probabilities
A <- cbind(PP_LPM, PP_PM, PP_LM)
colnames(A) <- c("Linear probability model", "Probit model", "Logit model")
rownames(A) <- c("Predicted probability")
A



