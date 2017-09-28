# Homework Assignment 1/DA2
# Study Gruop #29
# Members: Gabor Bokonyi, Gabor Illes
#*************************************
  


# 1. Load the amazon_compare.csv flie
df<- read.csv('amazon_compare.csv')


# 2. Drop observations where price or price online or both are missing.
library(tidyr)
df_amazon <- df %>% drop_na(price, price_online)

# 2. Calculate the 99-percentile values and store it in a variable
price_99 <- quantile(df_amazon$price, 0.99, na.rm = TRUE)
price_online_99 <- quantile(df_amazon$price_online, 0.99, na.rm = TRUE)

# 2. Drop all observation grater than the 99 prcentile of the price or the online price
df_amazon <- subset(df_amazon, df_amazon$price < price_99 & df_amazon$price_online < price_online_99)
df_amazon


# 3. Create a random number generator
id_rand <- set.seed(29)


# 4. Statistics
summary(df_amazon$price)
sd(df_amazon$price)

summary(df_amazon$price_online)
sd(df_amazon$price_online)


# 5. Create variable: variance of the price variable
var_price <- sum((df_amazon$price - mean(df_amazon$price)) ^ 2) / length(df_amazon$price)
var_price

# 5. Create variable: standard deviation of the price variable
std_price <- var_price ^ 0.5
std_price

# 5. Variance with Built-in command
var_price_B <- var(df_amazon$price)
var_price_B

# 5. Standard deviation with Biult-in command
std_price_B <- sd(df_amazon$price)
std_price_B


# 6. Create a dummy (indicator) variable
df_amazon$dummy <- df_amazon$price > df_amazon$price_online
df_amazon$dummy


# 7. Create a variable that is the difference between the price and online price.
df_amazon$diff_price <- df_amazon$price - df_amazon$price_online
df_amazon$diff_price

# 7. Create a dummy variable taking value one if the diff_price is positive
df_amazon$dummy_price_diff <- ifelse (df_amazon$diff_price > 0, df_amazon$dummy_price_diff <- 1, df_amazon$dummy_price_diff <- 0)
df_amazon$dummy_price_diff
sum(df_amazon$dummy_price_diff)


# 8. How often do you observe a positive value for the diff_price?
pos_price_diff <- sum(df_amazon$dummy_price_diff)
pos_price_diff

# 8. How often do you observe no price difference between online and offline prices?
df_amazon$no_price_diff <- ifelse(df_amazon$price == df_amazon$price_online, df_amazon$no_price_diff <-1, df_amazon$no_price_diff <- 0)
df_amazon$no_price_diff
no_price_diff <- sum(df_amazon$no_price_diff)
no_price_diff


# 9. What is the probability of observing a positive price difference if the good category is Electronics?
df_amazon$num_Electronics <- ifelse (df_amazon$category == 'Electronics', df_amazon$num_Electronics <- 1, df_amazon$num_Electronics <- 0)
df_amazon$num_Electronics
sum(df_amazon$num_Electronics)
df_amazon$num_pos_Electronics <- ifelse (df_amazon$category == 'Electronics' & df_amazon$dummy_price_diff ==1, df_amazon$num_pos_Electronics <- 1, df_amazon$num_pos_Electronics <- 0)
sum (df_amazon$num_pos_Electronics)
prob_pos_if_Electronics <- sum (df_amazon$num_pos_Electronics) / sum(df_amazon$num_Electronics)
prob_pos_if_Electronics

# 9. What is the probability of observing a zero price difference for the category Home and Appliances?
df_amazon$Home <- ifelse (df_amazon$category == 'Home and Appliances', df_amazon$Home <- 1, df_amazon$Home <- 0)
df_amazon$Home
sum(df_amazon$Home)
df_amazon$no_diff_Home <- ifelse(df_amazon$category == 'Home and Appliances' & df_amazon$price == df_amazon$price_online, df_amazon$no_diff_Home <- 1, df_amazon$no_diff_Home <- 0)
sum(df_amazon$no_diff_Home)
prob_no_diff_Home <- sum(df_amazon$no_diff_Home) / sum(df_amazon$Home)
prob_no_diff_Home


# 10. Create a scatterplot with price and online price.
plot(df_amazon$price, df_amazon$price_online, main = "Scatter Price/Online price", xlab = 'Price', ylab = 'Online')

# 10. Create a scatterplot with price and amazon price.
plot(df_amazon$price, df_amazon$price_amazon, main = "Scatter Price/Amazon price", xlab = 'Price', ylab = 'Amazon price')


# 11. Boxplot for price difference (Price/Online price) over Electronics 
df_Electronics <- subset(df_amazon, df_amazon$category == 'Electronics')
df_Electronics$price_diff <- (df_Electronics$price - df_Electronics$price_online)
boxplot(df_Electronics$price_diff)

# 11. Histogram for price difference (Price/Online price) over Electronics 
hist(df_Electronics$price_diff)

# 11. Boxplot for price difference (Price/Online price) over Pharmacy and Health 
df_PH <- subset(df_amazon, df_amazon$category == 'Pharmacy and Health')
df_PH$price_diff <- df_PH$price - df_PH$price_online
boxplot(df_PH$price_diff)

# 11. Histogram for price difference (Price/Online price) over Pharmacy and Health 
hist(df_PH$price_diff)


