
# Clean our workspace ----
rm(list = ls())

# Load necessary packages ----
library(MASS)
library(mice)
library(fitdistrplus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(car)
library(gmodels)
library(ggplot2)
library(RColorBrewer)

# Upload necessary data sources ----
results <- read.csv("Results.csv")
weather <- read.csv("weather.csv")
competition <- read.csv("competition.csv")

# Create our data set that will be used in our analysis. ----
## Change session_date format into date.
results$session_date <- as.Date(results$session_date)

## Change Week format into date.
competition$Week <- as.Date(competition$Week, format = "%d/%m/%Y")

## Create dummys for season
results$winter <- ifelse(results$season=="WINTER",1,0)
results$spring <- ifelse(results$season=="SPRING",1,0)
results$summer <- ifelse(results$season=="SUMMER",1,0)

## Change avg_product_popularity into numeric
results$avg_product_popularity <- as.numeric(results$avg_product_popularity)

# 0s for NAs
results$avg_product_popularity[is.na(results$avg_product_popularity)] <- 0

## Convert session_time in minutes.
results$session_time <- (as.numeric(as.POSIXct(strptime(results$session_time, format =
"%H:%M:%S"))) -
as.numeric(as.POSIXct(strptime("0", format = "%M"))))

## Week_no is created in "competition" and "results" so we can merge tables.
results$week_no <- strftime(results$session_date, format = '%V')
competition$week_no <- strftime(competition$Week, format = '%V')
results <- merge(results, competition, by = "week_no", all.x = TRUE)

## Weather was noted as YMD and changed into Y-M-D to merge with results.
weather$date <- as.character(weather$date)
weather$date <- format(as.Date(weather$date, format = "%Y%m%d", origin = "1970-01-01"),
"%Y-%m-%d")
weather$date <- as.Date(weather$date)
weather$avg_total_rainfall[weather$avg_total_rainfall<0] <- 0

# Renamed date variable in "weather" table as session_date, so we can merge the tables by it.
colnames(weather)[2] <- "session_date" # column name has to match
results <- merge(results, weather, by = "session_date", all.x = TRUE)

## Add competition variables.
colnames(results)[18:21] <- c("Zalando","Amazon","Wehcamp","Bol")

## Adjust past_purchase variable.
results$past_purchase <-
ifelse(results$past_purchase==0,0,ifelse(results$past_purchase==1,1,2))

## Change any NULL variables into NAs for sex
results$sex[results$sex=="NULL"] <- NA

## Create our final data set by choosing only necessary columns.
data <- results[,c(5:21,23:25)] #Kept Week to use for plots later
rm(list = c("results","weather","competition"))

## Convert the sex variable.
data$sex <- ifelse(data$sex==0,0,ifelse(data$sex==1,1,NA))

# Treate any outliers. ----

## Calculate the correlation between the variables.
# We remove the season variable since it is represented by the dummies.
data1 <- data[,-c(1,13)]
corre_data<- cor(data1)

## Sex variable has multiple NAs so we exclude it from Mahalanobis.
# Summer and Wehkamp variables is excluded because of high correlation (dummy variable
trap)
data1 <- data1[,-c(5,11,14)]

## Calculate the Mahalanobis distances.
data$dist_data <- mahalanobis(data1,colMeans(data1),cov(data1))

## Plot Mahalanobis distance for each point
plot(density(data$dist_data,bw=0.5),
main="Squared Mahalanobis Distances, n=81573,p=15",lwd=2, xlim=c(0,70))

## Plot Chi-square distribution with df = 15
xrange <- seq(0,max(data$dist_data) ,by=.002)
fit <- fitdist(data$dist_data, distr = "chisq", start = list(df=4))
lines(xrange,dchisq(xrange,fit$estimate),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance
qqplot(qchisq(ppoints(100), df = 15), data$dist_data ,
main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
" vs. quantiles of" * ~ chi[15]^2))
abline(0, 1, col = 'gray')

## Calculate the p-value and locate the outliers
data$p<- pchisq(data$dist_data, df = ncol(data1), lower.tail = FALSE)
# We used a stricter CI (1% instead of 5%) to locate the outliers, so we can remove them
without any serious consequences in our data set as they represent the 3.82% of the total data
set.
data <- data[!(data$p < 0.01), ]
data1 <- data[,-c(1,6,12,13,15)]

## Recalculate MD.
data$dist_data<- mahalanobis(data1,colMeans(data1),cov(data1))

## Plot Mahalanobis distance for each point
plot(density(data$dist_data,bw=0.5),
main="Squared Mahalanobis Distances, n=78486,p=15",lwd=2)

## Plot Chi-square distribution with df = 18
xrange <- seq(0,max(data$dist_data),by=.001)
fit <- fitdistr(data$dist_data,"chi-squared", start = list(df=4))
lines(xrange,dchisq(xrange,fit$estimate),col="dodgerblue",lwd=2)

## Make Q-Q plot to assess Mahalanobis distance
qqplot(qchisq(ppoints(100), df = fit$estimate), data$dist_data,
main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
" vs. quantiles of" * ~ chi[18]^2), ylab = "",xlab="")
abline(0, 1, col = 'gray',lwd = 2)

## Remove data1 data set and distance and p-value columns.
rm(data1)
rm(xrange)
rm(corre_data)
rm(fit)
data <- data[,-c(21,22)]

# Deal with missings ----
## Find which variable has missings
summary(data)

# Sex variable has 16646 missings.
## Inspect pattern of missings
par(cex = 0.8)
md.pattern(data)

## Indicate which variables can be used for predicting missings.
data2 <- data[,c(2:7,9)]

## Impute data.
mice_data <- mice(data2, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(mice_data)

# 5 imputations were done using predictive mean matching method for the sex variable.
## Clear unecessary data.
rm(list = c("data2"))

# Report the key figures. ----
summary(data)

# Histogram for Session time
ggplot(data, aes(x = session_time)) +
geom_histogram(binwidth = 100, fill = "orange", color = "black") +
labs(title = "Session Time Histogram",
x = "Session Time",
y = "Frequency") +
theme_minimal()

# Line plot for Temperature
ggplot(data, aes(x = Week, y = temp)) +
geom_point() +
labs(title = "Temperature Over Time",
x = "Week",
y = "Temperature (°C)")

# Bar plot for conversion counts.
ggplot(data, aes(x = factor(conversion), fill = factor(conversion))) +
geom_bar() +
labs(title = "Conversion Counts",
x = "Conversion",
y = "Count")

# Search Interest over time
ggplot(data, aes(x = Week)) +
geom_line(aes(y = Wehcamp, color = "Wehkamp")) +
geom_line(aes(y = Bol, color = "Bol")) +
geom_line(aes(y = Zalando, color = "Zalando")) +
geom_line(aes(y = Amazon, color = "Amazon")) +
labs(title = "Search Interest Over Time",
x = "Week",
y = "Search Interest",
color = "Platform") +
scale_color_manual(values = c("Wehkamp" = "blue", "Bol" = "green", "Zalando" = "red",
"Amazon" = "purple")) +
theme_minimal()
#or
ggplot(data, aes(x = Week)) +
geom_area(aes(y = Wehcamp, fill = "Wehcamp"), alpha = 0.7) +
geom_area(aes(y = Bol, fill = "Bol"), alpha = 0.7) +
geom_area(aes(y = Zalando, fill = "Zalando"), alpha = 0.7) +
geom_area(aes(y = Amazon, fill = "Amazon"), alpha = 0.7) +
labs(title = "Stacked Area Chart for Search Interest",
x = "Week",
y = "Search Interest") +
scale_fill_manual(values = c("Wehcamp" = "blue", "Bol" = "green", "Zalando" = "red",
"Amazon" = "purple")) +
theme_minimal()

# Rain Frequency
ggplot(data, aes(x = season, fill = factor(rain))) +
geom_bar(position = "dodge") +
labs(title = "Conversion by Season",
x = "Season",
y = "Count") +
scale_fill_manual(values = c("0" = "gray", "1" = "darkblue")) +
theme_minimal()
#or
ggplot(data, aes(x = avg_total_rainfall)) +
geom_histogram(binwidth = 2, fill = "darkblue", color = "black") +
labs(title = "Average Rainfall Histogram",
x = "AVG Total Rainfall",
y = "Frequency") +
theme_minimal()

# Load the package.
library(gmodels)

# Analysis and answering our hypotheses. ----
## Analyses

# H4c: Mornings have lower customer journey conversion rates than the afternoon/evening.
CrossTable(data$conversion, data$morning)
chisq.test(x = data$morning, y = data$conversion)

# H3b: Men tend to convert more than women.
sex.mice.models <- with(data=mice_data, exp = chisq.test(x = sex, y = conversion)) # S.S.
summary(sex.mice.models)

# H4b: Customer journey conversion rates are higher on weekends.
CrossTable(data$weekend, data$conversion)
chisq.test(x = data$weekend, y = data$conversion) # S.S.

# H4a: Customer journey conversion rates are higher in warmer seasons.
CrossTable(data$season, data$conversion)
chisq.test(x = data$season, y = data$conversion) # Seasonality effects on conversion

#or
CrossTable(data$winter, data$conversion)
chisq.test(x = data$conversion, y = data$winter) # S.S. (statistically significant)
CrossTable(data$spring, data$conversion)
chisq.test(x = data$conversion, y = data$spring) # S.S. (statistically significant)
CrossTable(data$summer, data$conversion)
chisq.test(x = data$conversion, y = data$summer) # S.S. (statistically significant)

# H5b: Sunnier days will have higher customer journey conversion rates.
CrossTable(data$rain, data$conversion)
chisq.test(x = data$conversion, y = data$rain) # Not S.S.
ggplot(data, aes(x = jitter(conversion), y = jitter(rain))) +
geom_point() +
labs(x = "Conversion (1=Converted)", y = "Rain (1=Yes)")

# H3c: Past purchase incidence has a positive influence on the customer journey conversion
rate.
CrossTable(data$conversion, data$past_purchase)
chisq.test(x = data$conversion, y = data$past_purchase) # S.S.
ggplot(data, aes(x = jitter(conversion), y = jitter(past_purchase))) +
geom_point() +
labs(x = "Conversion (1=Converted)", y = "Past Purchase Incidence (1=Yes, 2=;)")

# H2a: The lower the average price of products viewed, the higher the customer journey
conversion rate.
summary(glm(data$conversion~data$avg_product_price, data = data, family = "binomial"))

# H2c: The popularity of the products viewed has a positive influence on customer journey
conversion rates.
summary(glm(data$conversion~data$avg_product_popularity, data = data, family = "binomial"))

# H1a: The longer the session time, the lower the CJ conversion rate.
summary(glm(data$conversion~data$session_time, data = data, family = "binomial"))

# H6b: The higher the search interest in competition, the lower the customer journey conversion
rate.
summary(glm(data$conversion~data$Zalando, data = data, family = "binomial"))
summary(glm(data$conversion~data$Amazon, data = data, family = "binomial"))
summary(glm(data$conversion~data$Wehcamp, data = data, family = "binomial"))
summary(glm(data$conversion~data$Bol, data = data, family = "binomial"))

# H5a: A temperature increase increases customer journey conversion rate.
summary(glm(data$conversion~data$temp, data = data, family = "binomial"))

# H5b: Sunnier days will have higher customer journey conversion rates.
summary(glm(data$conversion~data$avg_total_rainfall, data = data, family = "binomial"))

# Visualization of the key insights of our analyses. ----
## H3b: Men tend to convert more than women.
agg_data <- aggregate(conversion ~ sex, data = data, FUN = mean) * 100
ggplot(agg_data, aes(x = conversion, y = as.character(sex), fill = as.character(sex))) +
geom_bar(stat = "identity", position = "identity", width = 0.5) +
scale_fill_brewer(palette = "Dark2", name = "Sex", labels = c("Male", "Female")) +
labs(title = "Conversion by Sex",
x = "Mean Conversion",
y = "Sex") +
theme_minimal() +
scale_x_continuous(expand = c(0.008, 0.0))
21# H4b: Customer journey conversion rates are higher on weekends.
agg_data <- aggregate(conversion ~ weekend, data = data, FUN = mean) * 100
ggplot(agg_data, aes(x = conversion, y = as.character(weekend), fill = as.character(weekend)))
+
geom_bar(stat = "identity", position = "identity", width = 0.5) +
scale_fill_manual(values = brewer.pal(3, "Dark2"), name = "Weekend", labels = c("Weekday",
"Weekend")) +
labs(title = "Conversion on Weekends vs on Weekdays",
x = "Mean Conversion",
y = "Weekend") +
theme_minimal() +
scale_x_continuous(expand = c(0.008, 0.0))

## H4a: Customer journey conversion rates are higher in warmer seasons.
# Aggregate the data.
agg_data <- aggregate(conversion ~ season, data = data, FUN = mean)
agg_data$conversion <- agg_data$conversion * 100

# Create the plot.
p <- ggplot(agg_data, aes(x = season, y = conversion, fill = season)) +
geom_bar(stat = 'identity') +
scale_fill_manual(values = brewer.pal(3, "Dark2")) +
labs(title = 'Conversion by Season', x = 'Season', y = 'Conversion (%)') +
theme_minimal() +
theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
coord_flip() +
scale_y_continuous(expand = c(0, 0), limits = c(0, 15))

# Add text labels on the bars.
p + geom_text(aes(label = sprintf("%.2f%%", conversion)), hjust = 1.2)

# H5a: A temperature increase increases customer journey conversion rate.
ggplot(data, aes(x = as.factor(conversion), y = temp, fill = as.factor(conversion))) +
geom_boxplot(alpha = 0.7) +
scale_fill_brewer(palette = "Dark2", name = "Conversion",
breaks = c("0", "1"), labels = c("Didn't Convert", "Converted")) +
labs(title = 'Relationship Between Conversion and Temperature',
x = 'Conversion',
y = 'Temperature') +
theme_minimal() +
scale_y_continuous(expand = c(0.0005, 0), breaks = seq(0, 40, 5), limits = c(0, 35))

#or

# Creating temperature groups with labels
data$temperature_group <- cut(data$temp, breaks = c(-Inf, 10, 20, 30, Inf), labels = c("< 10ºC",
"11ºC - 20ºC", "21ºC - 30ºC", "31ºC >"))

# Plot the results.
ggplot(data, aes(x = temperature_group, fill = as.factor(conversion))) +
geom_bar(position = "stack", stat = "count", color = "white", width = 0.7) +
scale_fill_manual(values = brewer.pal(3, "Dark2"), name = "Conversion") +
labs(x = "Temperature Group", y = "Count", fill = "Conversion",
title = "Conversion by Temperature Group") +
theme_minimal()
#or
model <- glm(data$conversion~data$temp, data = data, family = "binomial")

# Making predictions using the model
data$predicted_probs <- predict(model, type = "response")

# Creating a scatter plot
ggplot(data, aes(x = temp, y = predicted_probs)) +
geom_point(color = "darkgreen") +
labs(x = "Temperature", y = "Predicted Probability of Conversion",
title = "Scatter Plot of Predicted Probabilities vs Temperature") +
theme_minimal()

# H3c: Past purchase incidence has a positive influence on the customer journey conversion
rate.

# Aggregate per past purchase group.
agg_data <- aggregate(conversion ~ past_purchase, data = data, FUN = mean)
agg_data$conversion <- agg_data$conversion * 100
agg_data$past_purchase <- factor(agg_data$past_purchase, labels = c("Didn't Convert",
"Converted Once", "Converted More than Once"))

# Create the plot.
ggplot(agg_data, aes(x = past_purchase, y = conversion, fill = past_purchase)) +
geom_bar(stat = 'identity') +
scale_fill_manual(values = brewer.pal(3, "Dark2")) +
labs(title = 'Conversion by Past Purchase Incidence (Past 30 days)', x = 'Past Purchase
Incidence', y = 'Conversion (%)') +
theme_minimal() +
theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
scale_y_continuous(labels = scales::percent_format(scale = 1)) +
geom_text(aes(label = sprintf("%.1f%%", conversion)), position = position_stack(vjust = 1.1),
size = 3) +
coord_flip() +
theme(legend.position = "bottom") +
expand_limits(y = c(0, 20)) +
labs(fill = "Past Purchase Incidence")


