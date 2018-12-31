rm(list=ls()) 

setwd('C:\\Users\\dmtwong\\Desktop' ) 
getwd()

data_set_1  <- read.csv('T1_Data.csv',  stringsAsFactors = F)
str(data_set_1 )
data_set_1 <- transform(data_set_1, Date = as.Date(Date, "%m/%d/%Y")) # Transform an element in data frame
                        #p_AAN = as.numeric(p_AAN) )

str(data_set_1)

ret_matrix <- as.data.frame(matrix(NA, 
                                   nrow = nrow(data_set_1) - 1, 
                                   ncol = ncol(data_set_1),
                                   dimnames = list(NULL, #alt you could provide date as row name 
                                                   colnames(data_set_1)) ) )

ret_matrix[, -1] <- sapply(data_set_1[,-1], 
                           function(x){
                             diff(x)/x[-length(x)]*100
                           }
)

ret_matrix[, 1] <- data_set_1[-1, 1]
mean(ret_matrix$p_AAN)
var(ret_matrix$p_AAN)
sd(ret_matrix$p_AAN)
mean(ret_matrix$p_IBN); sd(ret_matrix$p_IBN); var(ret_matrix$p_IBN)
mean(ret_matrix$p_IBN, na.rm = TRUE); sd(ret_matrix$p_IBN, na.rm = TRUE); var(ret_matrix$p_IBN, na.rm = TRUE)

r_summary <- function(x, remove_NA = TRUE){
  c(Mean = mean(x, na.rm = remove_NA), 
    Variance = var(x, na.rm = remove_NA), 
    Std_Dev = sd(x, na.rm = remove_NA) )
}

sapply(ret_matrix[, -1], r_summary, TRUE)

hist(ret_matrix[['p_IBN']])

hist(ret_matrix[['p_IBN']], 
     main = paste('return of', names(ret_matrix)[2], ' (Unbalanced) ', sep = ' '),
     freq = F, breaks = 25)

abline(v = mean(ret_matrix[['p_IBN']], na.rm = T), col ='red')

cov(ret_matrix[,-1]) # -1 exclude date
cov(ret_matrix[,-1], use = "complete.obs")
cor(ret_matrix[,-1], use = "complete.obs")
# Computer moving average

install.packages('zoo')
library(zoo) 

ma_aan <- rollapply(data_set_1[, 'p_AAN'], width = 20,
                    #FUN = function(x) mean(x, na.rm = T), 
                    FUN = mean,
                    by = 1,
                    align = 'right')

names(ma_aan) <- data_set_1$Date[-c(1:19)] # Assign date as names for each element in 'ma_aan'
head(ma_aan, 2); tail(ma_aan,2)

ma20 <- rollapply(data_set_1[, -1], width = 20, 
                  FUN = mean, by = 1, align = 'right')
ma20 <- as.data.frame(ma20)
ma20$Date <- data_set_1[-c(1:19), 'Date'] 
head(ma20, 2); tail(ma20,2)

ma20[] <- ma20[c(6,1:5)] #optional 
head(ma20, 2)
# Alternatively we can type out c('Date', 'p_AAN', 'p_IBN', .....
colnames(ma20) <- colnames(ma20)[c(6,1:5)] 
colnames(ma20)

# Moving Average starting from 2009
# create logical vector (TRUE, FALSE, or NA)
ix_p <- data_set_1$Date > '2009-01-01'
ix_ma20 <- ma20$Date > '2009-01-01'
head(ix_p,2); tail(ix_ma20,2)
head(data_set_1$Date,2); tail(ma20$Date,2)

ds1_2009 <- data_set_1[ix_p, ]
ma20_2009 <- ma20[ix_ma20, ]
head(ds1_2009,2); head(ma20_2009,2)
tail(ds1_2009,2); tail(ma20_2009,2)

# Plot the graph
par(mar = c(5,5,2,5)) # Setting margins for (bottom, left, top, right)

with(ds1_2009, 
     plot(Date, p_IBN, type="l", col="red",
          cex=0.2, ylab='Price',
          ylim = range(c(min(p_IBN)-0.5,
                         max(p_IBN)+0.5) ),
          main = 'daily price and moving average of p_IBN') )
par(new = T) #on same graphic device

plot(ma20_2009$Date, ma20_2009$p_IBN, type="l", col="blue", 
     axes=F, xlab="", ylab="", cex=0.2,
     ylim = range(c(min(ds1_2009$p_IBN)-0.5,
                    max(ds1_2009$p_IBN)+0.5) ) #On same scale
)

legend("topleft",
       legend=c('Daily', 'MA 20'),
       pch=c(16, 16), col=c("red", "blue"), cex = 0.6)

# Save the graph
dev.copy(png, file = "MA_IBN_2009.png", width = 1200, height = 300, 
         units = "px")
dev.off()
?dev.copy
dir()
