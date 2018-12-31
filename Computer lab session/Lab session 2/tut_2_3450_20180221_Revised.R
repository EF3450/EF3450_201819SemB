rm(list = ls())
##'Computer Lab Session 2: Simple Regression'
## Example: Capital Asset Pricing Model

## Display the data 
setwd('C:\\Users\\dmtwong\\Desktop')
data_ex2 <- read.csv('CAPM_dataset_3.csv', 
                     stringsAsFactors = F)
str(data_ex2)

## Data processing 
## ?strptime
data_ex2 <- transform(data_ex2, 
                      Date = as.Date(Date, "%m/%d/%Y"))
str(data_ex2)

## Create a empty data frame  
#list_name <- c('p_IBM', 'p_SPX')
ret_name  <- c('r_IBM', 'r_SPX')
ret_name

ret_df <- data.frame(matrix(ncol = length(ret_name) + 1 , 
                            nrow = nrow(data_ex2) - 1 ))

## Then insert first column using 'Date' from 'data_ex2' 
ret_df[,1] <- data_ex2[-1,1] 
colnames(ret_df) <- c('Date', ret_name) #and set column names
colnames(ret_df)
# head(ret_df,1); tail(ret_df,1)

## Calculate return
ret_df[, -1] <- sapply(data_ex2[,-c(1,4)],
                       function(x){
                         diff(x)/x[-length(x)]*100
                       }
)
str(ret_df);head(ret_df,1); tail(ret_df,1)


## Check first return of IBM (optional)
head(data_ex2, 2)
head(ret_df, 1)
ret_df[1, 2] == diff(data_ex2[1:2, 2])/data_ex2[1,2]*100 # first return of IBM

## Check last return of IBM (optional)
tail(ret_df, 1)
tail(data_ex2, 2)
ret_df[nrow(ret_df), 2] == diff(data_ex2[198:199, 2])/ data_ex2[198,2]*100 # last return of IBM

##Note: These test cases are far from comprehensive, and it does not guarantee what we did is correct

## Now add the risk free rate to ret_df as well
ret_df$r_f <- data_ex2[-1, 'r_f']
#str(ret_df);head(ret_df, 1);tail(ret_df, 1)


## (Optional) Generate excess return
#ret_df$ex_r_IBM <- ret_df$r_IBM - ret_df$r_f
#ret_df$ex_r_SPX  <- ret_df$r_SPX  - ret_df$r_f
#str(ret_df)


## Regression 
reg_CAPM <- lm(I(r_IBM-r_f) ~ I(r_SPX-r_f), data = ret_df)
#reg_CAPM2 <- lm(ex_r_IBM ~ ex_r_SPX, data = ret_df) #Alternatively

## Summarizing Linear Model Fits
summary(reg_CAPM)
#summary(reg_CAPM2)

## (OPTIONAL) R Object Classes 101 (cover S3 only)
## generic functions: like summary(), mean(), print()
## methods: summary.data.frame(), summary.lm()
#methods(summary)
#methods(class = data.frame)
#methods(class = lm)
#summary(reg_CAPM)
#summary.lm(reg_CAPM)
#reg_CAPM <- unclass(reg_CAPM)
#class(reg_CAPM)
#summary(reg_CAPM)
#reg_CAPM$coefficients
#class(reg_CAPM) <- 'lm'
#summary(reg_CAPM)


###
### hypothesis testing using t-distribution (Small sample) 
###

##Hypothesis Testing 1: Can we describe IBM as an aggressive asset? (right tailed)
## H0: beta <= 1 against H1: beta > 1   
## Approach 1: Compare with critical value (using t distribution)  
##Step 1: Compute the test statistic first
reg_result <- summary(reg_CAPM)
beta_hat <- reg_result$coefficients[2,1]
se_beta <- reg_result$coefficients[2,2]
t_stat_HT1 <- ( beta_hat - 1 ) /  se_beta # the test statistic
#beta_hat;se_beta;t_stat_HT1 

##Step 2: Compare with critical value using t distribution
critical_val_t <- qt(1-0.05 , 196) 
# critical_val_normal <- qnorm(1-0.05) 
## As sample size grows, df is larger and t is closer to std norm
#c(critical_val_normal, critical_val_t)
t_stat_HT1
critical_val_t
t_stat_HT1 > critical_val_t 
# Cannot reject H0: beta <=1 at 5% level and hence cannnot conclude IBM as an aggressive asset at 5% sign level.

## Hypothesis Testing 2: Does IBM outperform the market? (right tailed)
## H0: alpha <= 0 against H1: alpha > 0  
## Approach 2: Compare p-value with significance level (say 5%)  
##Step 1: Again compute the test statistic first (Or directly use t-value reported in reg result as hypthesised value is 0)

alpha_hat <- reg_result$coefficients[1,1]
se_alpha <- reg_result$coefficients[1,2]
t_stat_HT2 <- ( alpha_hat - 0 ) /  se_alpha
alpha_hat;se_alpha;t_stat_HT2 ## the test statistic
t_stat_HT2 == reg_result$coefficients[1,3]

## Does IBM outperform the market? (cont)
##Step 2: Compute p(X > t-statistic) following t distribution

pt(t_stat_HT2, df=196, lower.tail = FALSE) # alternatively: 1 - pt(t_stat_HT2, df=196, lower.tail = TRUE)
sig_level_HT2 <- 0.05
pt(t_stat_HT2, df= 196, lower.tail = FALSE) < sig_level_HT2
## Cannot reject H0: alpha <=0 at 5% level and hence cannnot conclude IBM outperform mkt at 5% sign level.

## Hypothesis Testing 3: 
## Two-tailed test
## H0: beta <= 0 against H1: beta > 0  
## Step 1: t-stat
beta_hat <- reg_result$coefficients[2,1]
se_beta <- reg_result$coefficients[2,2]
t_stat_HT3 <- ( beta_hat - 0 ) /  se_beta
t_stat_HT3 ## the test statistic
t_stat_HT3 == reg_result$coefficients[2,3]

## Does IBM really related to business cycle?? (cont)
#critical_val_3_norm <- qnorm( 1- 0.05/2 ) #0.975 quantile
critical_val_3_t <- qt( 1-0.05/2 , 196) # 2 tail test, each side have 0.025
#c(critical_val_3_norm, critical_val_3_t) # very similar as mentioned

# Aprroach 1
abs(t_stat_HT3) > critical_val_3_t 
# Appriach 2
2 * ( pt(t_stat_HT3, df = 196, lower.tail = FALSE) )# alternatively: 2 * ( 1- pt(t_stat_HT3, df = 196, lower.tail = TRUE) )
sig_level_HT3 <- 0.05
2 * ( pt(t_stat_HT3, df = 196, lower.tail = FALSE) ) < sig_level_HT3
2 * ( pt(t_stat_HT3, df = 196, lower.tail = FALSE) ) == reg_result$coefficients[2,4]
# we can reject the H0: beta  = 0 at 5% level 

###
### hypothesis testing using normal-distribution (large sample)
###

##As sample size grows, coefficient estimates are asymptotically normal distributed
##Let's revisit the hypothesis testing using normal distribution

##Hypothesis Testing 1: Can we describe IBM as an aggressive asset?
## H0: beta <= 1 against H1: beta > 1   
## Approach 1: Compare with critical value (using normal distribution)  
## Step 1: Compute the test statistic first
reg_result <- summary(reg_CAPM)
beta_hat <- reg_result$coefficients[2,1]
se_beta <- reg_result$coefficients[2,2]
t_stat_HT1 <- ( beta_hat - 1 ) /  se_beta # the test statistic
beta_hat;se_beta;t_stat_HT1 

## Can we describe IBM as an aggressive asset? (cont)
## Step 2: Compare with critical value using normal distribution

# critical_val_t <- qt(1-0.05 , 196) 
critical_val_normal <- qnorm(1-0.05) 
## with large sample size, df is large and t is close to std normal
# c(critical_val_normal, critical_val_t)
t_stat_HT1
critical_val_normal
t_stat_HT1 > critical_val_normal 
# Cannot reject H0: beta <=1 at 5% level and hence cannnot conclude IBM as an aggressive asset at 5% sign level.


## Hypothesis Testing 2: Does IBM outperform the market? 
## H0: alpha <= 0 against H1: alpha > 0  
## Approach 2: Compare p-value with significance level (say 5%)  
## Step 1: Again compute the test statistic first
alpha_hat <- reg_result$coefficients[1,1]
se_alpha <- reg_result$coefficients[1,2]
t_stat_HT2 <- ( alpha_hat - 0 ) /  se_alpha
alpha_hat;se_alpha;t_stat_HT2 ## the test statistic

## Does IBM outperform the market? (cont)
##Step 2: Compute p(X > t statistic) following normal distribution

pnorm(t_stat_HT2, lower.tail = FALSE) # alternatively: 1 - pnorm(t_stat_HT2, df=196, lower.tail = TRUE)
sig_level_HT2 <- 0.05
pnorm(t_stat_HT2, lower.tail = FALSE) < sig_level_HT2

## Cannot reject H0: alpha <=0 at 5% level and hence cannnot conclude IBM outperform mkt at 5% sign level.

## Hypothesis Testing 3: 
## Two-tailed test
## H0: beta <= 0 against H1: beta > 0  
## Step 1: t-stat

beta_hat <- reg_result$coefficients[2,1]
se_beta <- reg_result$coefficients[2,2]
t_stat_HT3 <- ( beta_hat - 0 ) /  se_beta
t_stat_HT3 ## the test statistic

#Approach 1
critical_val_3_norm <- qnorm( 1- 0.05/2 ) #0.975 quantile
#critical_val_3_t <- qt( 1-0.05/2 , 196) # 2 tail test, each side have 0.025
#c(critical_val_3_norm, critical_val_3_t) # very similar as mentioned
abs(t_stat_HT3) > critical_val_3_norm 

# Approach 2: p-value 
2* (pnorm(t_stat_HT3, lower.tail = FALSE)) #Alternatively: 2* (1- pnorm(t_stat_HT3, lower.tail = TRUE))
sig_level_HT3 <- 0.05
2* (pnorm(t_stat_HT3, lower.tail = FALSE)) < sig_level_HT3
# we can reject the H0: beta  = 0 at 5% level 




## Optional: Rounding result
#round(reg_result$coefficients, 4)

## (Optional) 'stargazer' package: Well-formatted regression tables
#install.packages('stargazer')
#library(stargazer)
#set.seed(1234)
#ret_df$x <- rnorm(nrow(ret_df))
#reg_CAPM3 <- lm(I(r_IBM-r_f) ~ I(r_SPX-r_f) + x, data = ret_df)

#stargazer(reg_CAPM, reg_CAPM3, title="CAPM", type="text", f=FALSE, digits=5)

