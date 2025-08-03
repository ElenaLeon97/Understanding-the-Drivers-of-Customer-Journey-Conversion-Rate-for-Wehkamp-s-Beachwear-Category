rm(list = ls())

# load all data sources
results <- read.csv("Results.csv") 
weather <- read.csv("weather.csv")
competition <- read.csv("competition.csv")

# Format the variables
results$session_date <- as.Date(results$session_date) # change into date
# dummys for season
results$winter <- ifelse(results$season=="WINTER",1,0)
results$spring <- ifelse(results$season=="SPRING",1,0)
results$avg_product_popularity <- as.numeric(results$avg_product_popularity) # change into numeric
results$avg_product_popularity[is.na(results$avg_product_popularity)] <- 0 # 0s for NAs
results$min <- as.numeric(results$min)
# session time measured in seconds
results$session_time <-  (as.numeric(as.POSIXct(strptime(results$session_time, format = "%H:%M:%S"))) -
                            as.numeric(as.POSIXct(strptime("0", format = "%S"))))
# competition measured in weeks so week_no created to merge tables
results$week_no <- strftime(results$session_date, format = '%V')
competition$week_no <- strftime(competition$Week, format = '%V')
results <- merge(results, competition, by = "week_no", all.x = TRUE)
# weather noted as YMD changed into Y-M-D to merge with results
weather$date <- as.character(weather$date)
weather$date <- format(as.Date(weather$date, format = "%Y%m%d", origin = "1970-01-01"), "%Y-%m-%d")
weather$date <- as.Date(weather$date)
colnames(weather)[2] <- "session_date" # column name has to match
results <- merge(results, weather, by = "session_date", all.x = TRUE)
colnames(results)[17:20] <- c("Zalando","Amazon","Wehcamp","Bol")
results$past_purchase_1 <- ifelse(results$past_purchase==1,1,0)
results$past_purchase_2 <- ifelse(results$past_purchase>1,1,0)
colnames(results)[10] <- "sex"


# choose only neccesary columns
data <- results[,c(6:12,14,15,17:20,22:26)]
rm(list = c("results","weather","competition"))

data1 <- data[,-5]

data$dist_data <- mahalanobis(data1,colMeans(data1),cov(data1))

## Calculate the p-value and locate the outliers
data$p<- pchisq(data$dist_data, df = ncol(data1), lower.tail = FALSE)
outliers <- data$p < 0.01

## We used a stricter CI (1% instead of 5%) to locate the outliers, so we can remove them without any serious concequences
#in our data set as they represent the 4.1% of the total data set.
data <- data[!(data$p < 0.01), ]
data1 <- data[,-5]
## Recalculate MD.
data$dist_data<- mahalanobis(data1,colMeans(data1),cov(data1))

## Plot Mahalanobis distance for each point
plot(density(data$dist_data,bw=0.5),
     main="Squared Mahalanobis Distances, n=78234,p=9",lwd=2)
install.packages('fitdistrplus')
library(fitdistrplus)
library(dplyr)
library(MASS)
## Plot Chi-square distribution with df = 18
xrange <- seq(0,max(data$dist_data),by=.001)
fit <- fitdist(data$dist_data, distr = "chisq", start = list(df=4))
lines(xrange,dchisq(xrange,fit$estimate),col="dodgerblue",lwd=2)

## Make Q-Q plot to assess Mahalanobis distance
qqplot(qchisq(ppoints(100), df = fit$estimate), data$dist_data,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[18]^2))
abline(0, 1, col = 'gray',lwd = 2)

# Deal with missings
# Load MICE library
library(mice)
data <- data[,-c(19,20)]
PM <- matrix()
mice_data <- mice(data, m=5, maxit = 50, method = 'pmm',  seed = 500)