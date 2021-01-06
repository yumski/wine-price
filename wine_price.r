if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(knitr)) install.packages("knitr")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(randomForest)) install.packages("randomForest")

library(tidyverse)
library(caret)
library(ggplot2)
library(knitr)
library(ggthemes)
library(randomForest)


# Download the file and load into R data
# This data is originally from https://www.kaggle.com/zynicide/wine-reviews
# I downloaded the file and uploaded to github for ease of access.

dl <- tempfile()
download.file("https://raw.githubusercontent.com/yumski/wine-price/master/winemag-data-130k-v2.csv", dl)
dat <- read.csv(dl)

rm(dl)

head(dat)

# Filter out columns that I don't plan to use

new_dat <- dat %>%
  select(country, points, title, price, variety, winery)


# Extract year from title and make into it's own column
pattern <- "\\d{4}"
year <- str_extract(new_dat$title, pattern)

new_dat <- new_dat %>%
  mutate(years = year)

# Filter out NAs from dataset
new_dat <- new_dat %>%
  filter(!is.na(price), !is.na(points), !is.na(country), !is.na(variety), !is.na(years), !is.na(winery))

head(new_dat)

# Divide the data into modeling set and validation set
set.seed(123, sample.kind = "Rounding")

ind <- createDataPartition(new_dat$price, times=1, p=0.1, list=FALSE)
wine_dat <- new_dat[-ind,]

temp_file <- new_dat[ind,]

# Make sure all the features are in both data.
validate_dat <- temp_file %>%
  semi_join(wine_dat, by = "country") %>%
  semi_join(wine_dat, by = "price") %>%
  semi_join(wine_dat, by = "variety") %>%
  semi_join(wine_dat, by = "years") %>%
  semi_join(wine_dat, by = "winery")

rm(temp_file)
dim(wine_dat)

# RMSE Function

RMSE <- function(predicted_price, actual_price){
  sqrt(mean((predicted_price - actual_price)^2))
}

# Dataframe for number of unique entires
data.frame(Country = length(unique(wine_dat$country)),
           Points = length(unique(wine_dat$points)),
           Price = length(unique(wine_dat$price)),
           Title = length(unique(wine_dat$title)),
           Variety = length(unique(wine_dat$variety)),
           Winery = length(unique(wine_dat$winery)),
           Year = length(unique(wine_dat$years)))

# Points range
c(min(wine_dat$points), max(wine_dat$points))

# Price range
c(min(wine_dat$price), max(wine_dat$price))

# Country count graph
wine_dat %>%
  group_by(country) %>%
  summarize(n = n()) %>%
  ggplot(aes(n, country)) +
  geom_point() +
  scale_x_sqrt() +
  xlab("Count (Square Root scale)") +
  ylab("Country") +
  theme_hc() +
  ggtitle("Country Count Distribution",
          subtitle = "Figure 1")


# Price by country
wine_dat %>%
  group_by(country) %>%
  summarize(n = n(), price = price, country = country) %>%
  filter(n > 500) %>%
  ggplot(aes(country, price)) +
  geom_boxplot() +
  scale_y_log10() +
  xlab("Country") +
  ylab("Price (Log 10 Scale)") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme_hc() +
  ggtitle("Price by Country",
          subtitle = "Figure 2")

# Points by country
wine_dat %>%
  group_by(country) %>%
  summarize(n = n(), points = points, country = country) %>%
  filter(n > 500) %>%
  ggplot(aes(country, points)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Points") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme_hc() +
  ggtitle("Points by Country",
          subtitle = "Figure 3")

# Avg Pts vs Price by Country
wine_dat %>%
  group_by(country) %>%
  summarize(n = n(), price = price, country = country, avg_pts = mean(points), avg_p = mean(price)) %>%
  filter(n > 100) %>%
  arrange(desc(n)) %>%
  top_n(100) %>%
  ggplot(aes(avg_pts, avg_p, color = country)) +
  geom_point() +
  theme_hc() +
  xlab("Average Points") +
  ylab("Average Price") +
  ggtitle("Avg Price and Points by Country",
          subtitle = "Figure 4")

# Price standard deviation by Country
wine_dat %>%
  group_by(country) %>%
  summarize(n = n(), stan_dev = sd(price)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than average", "> than average"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation") +
  ylab("Density") +
  theme_hc() +
  ggtitle("Price Standard Deviation by Country",
          subtitle = "Figure 5")

# Country price and points correlation
country_avg <- wine_dat %>%
  group_by(country) %>%
  summarize(avg_pts = mean(points), avg_p = mean(price))

# Correlate table
correlate <- data.frame(Category = "Category",
                        Correlation = "Correlation") 

correlate <- bind_rows(Category = "Country",
                       Correlation = cor(country_avg$avg_pts, country_avg$avg_p))
correlate

# Variety Count Distribution
wine_dat %>%
  group_by(variety) %>%
  summarize(n = n()) %>%
  filter(n > 500) %>%
  ggplot(aes(n, variety)) +
  geom_point() +
  scale_x_sqrt() +
  theme_hc() +
  xlab("Count (Square Root Sclae)") +
  ylab("Variety") +
  ggtitle("Variety Count Distribution",
          subtitle = "Figure 6")

# Variety Price Distribution
wine_dat %>%
  group_by(variety) %>%
  summarize(n = n(), price = price, points = points, variety = variety) %>%
  filter(n > 500) %>%
  ggplot(aes(variety, price)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_hc() +
  xlab("Variety") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  ggtitle("Variety Price Distribution",
          subtitle = "Figure 7")

# Variety Points Distribution
wine_dat %>%
  group_by(variety) %>%
  summarize(n = n(), price = price, points = points, variety = variety) %>%
  filter(n > 500) %>%
  ggplot(aes(variety, points)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_hc() +
  xlab("Variety") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Variety Points Distribution",
          subtitle = "Figure 8")

# Variety Points vs Price
wine_dat %>%
  group_by(variety) %>%
  summarize(n = n(), avg_p = mean(price), avg_pts = mean(points), variety = variety) %>%
  filter(n > 500) %>%
  ggplot(aes(avg_pts, avg_p, color = variety)) +
  geom_point() +
  theme_hc() +
  xlab("Average Points") +
  ylab("Average Price") +
  ggtitle("Avg Points and Price by Variety",
          subtitle = "Figure 9")

# Standard deviation for Variety
wine_dat %>%
  group_by(variety) %>%
  summarize(n = n(), stan_dev = sd(price)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than average", "> than average"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation") +
  ylab("Density") +
  theme_hc() +
  ggtitle("Price Standard Deviation by Variety",
          subtitle = "Figure 10")

# Variety Points Vs Price correlation
variety_avg <- wine_dat %>%
  group_by(variety) %>%
  summarize(avg_pts = mean(points), avg_p = mean(price))

correlate <- bind_rows(correlate,
                       data.frame(Category = "Variety",
                                  Correlation = cor(variety_avg$avg_pts, variety_avg$avg_p)))

correlate

# Years Count Distribution
wine_dat %>%
  group_by(years) %>%
  summarize(n = n(), price = price, points = points, years = years) %>%
  filter(n > 100) %>%
  ggplot(aes(n, years)) +
  geom_point() +
  theme_hc() +
  xlab("Count") +
  ylab("Years") +
  ggtitle("Distribution by Year",
          subtitle = "Figure 11")

# Years Price Distribution
wine_dat %>%
  group_by(years) %>%
  summarize(n = n(), points = points, price = price, years = years) %>%
  filter(n > 500, !is.na(years)) %>%
  ggplot(aes(years, price)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_hc() +   
  xlab("Years") +
  ylab("Price (Log 10 Scale)") +
  ggtitle("Years Price Distribution",
          subtitle = "Figure 12")

# Years Points Distribution
wine_dat %>%
  group_by(years) %>%
  summarize(n = n(), points = points, price = price, years = years) %>%
  filter(n > 500, !is.na(years)) %>%
  ggplot(aes(years, points)) +
  geom_boxplot() +
  theme_hc() + 
  xlab("Years") +
  ylab("Points") +
  ggtitle("Years Points Distribution",
          subtitle = "Figure 13")

# Avg Price to Points by Year
wine_dat %>%
  group_by(years) %>%
  summarize(n = n(), avg_p = mean(price), avg_pts = mean(points), years = years) %>%
  filter(n > 100) %>%
  ggplot(aes(avg_pts, avg_p, color = years)) +
  geom_point() +
  theme_hc() +
  xlab("Average Points") +
  ylab("Average Price") +
  ggtitle("Avg Points and Price by Year",
          subtitle = "Figure 14")

# Standard Deviation by Year
wine_dat %>%
  group_by(years) %>%
  summarize(n = n(), stan_dev = sd(price)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than average", "> than average"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation") +
  ylab("Density") +
  theme_hc() +
  ggtitle("Price Standard Deviation by Years",
          subtitle = "Figure 15")

# Correlate Price to Point by Year
years_avg <- wine_dat %>%
  group_by(years) %>%
  filter(!is.na(years)) %>%
  summarize(avg_pts = mean(points), avg_p = mean(price))

correlate <- bind_rows(correlate,
                       data.frame(
                         Category = "Years",
                         Correlation = cor(years_avg$avg_pts, years_avg$avg_p)
                       ))

correlate

# Winery Count Distribution
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), winery) %>%
  filter(n > 100) %>%
  ggplot(aes(n, winery)) +
  geom_point() +
  xlab("Winery") +
  ylab("Count") +
  theme_hc() +
  ggtitle("Winery Count Distribution",
          subtitle = "Figure 16")

# Price by Winery Distribution
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), winery, price, points) %>%
  filter(n > 100) %>%
  ggplot(aes(winery, price)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Winery") +
  xlab("Price (Log 10 Scale)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_hc() +
  ggtitle("Price by Winery Distribution",
          subtitle = "Figure 17")

# Points by Winery Distribution
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), winery, price, points) %>%
  filter(n > 100) %>%
  ggplot(aes(winery, points)) +
  geom_boxplot() +
  xlab("Winery") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_hc() +
  ggtitle("Points by Winery Distribution",
          subtitle = "Figure 18")

# Avg Points and Price by Winery
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), winery = winery, avg_pts = mean(points), avg_p = mean(price)) %>%
  filter(n > 100) %>%
  ggplot(aes(avg_pts, avg_p, color = winery)) +
  geom_point() +
  theme_hc() +
  xlab("Average Points") +
  ylab("Average Price") +
  ggtitle("Avg Price and Points by Winery",
          subtitle = "Figure 19")

# Standard Deviation by Winery
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), stan_dev = sd(price)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than average", "> than average"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation (Sq root scale)") +
  ylab("Density") +
  scale_x_sqrt() +
  theme_hc() +
  ggtitle("Price Standard Deviation by Winery",
          subtitle = "Figure 20")

# Avg Points and Price by Winery Correlation
winery_avg <- wine_dat %>%
  group_by(winery) %>%
  summarize(winery = winery, avg_pts = mean(points), avg_p = mean(price))

correlate <- bind_rows(correlate, 
                       data.frame(Category = "Winery",
                                  Correlation = cor(winery_avg$avg_pts, winery_avg$avg_p)))
correlate

# Points Count Distribution
wine_dat %>%
  group_by(points) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black", fill = "grey") +
  ylab("Count") +
  theme_hc() +
  ggtitle("Points Count Distribution",
          subtitle = "Figure 21")

wine_dat %>%
  group_by(points) %>%
  summarize(n = n(), points = points) %>%
  ggplot(aes(points, n)) +
  geom_line() +
  scale_y_log10() +
  ylab("Count (Log 10 scale)") +
  xlab("Points") +
  theme_hc() +
  ggtitle("Count by Points",
          subtitle = "Figure 22")

# Standard Deviation by points
wine_dat %>%
  group_by(points) %>%
  summarize(n = n(), stan_dev = sd(price)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than avg", "> than avg"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation (Sq root Scale)") +
  ylab("Density") +
  theme_hc() +
  ggtitle("Standard Deviation of Price - Points Group",
          subtitle = "Figure 23")

# Average Price by points
wine_dat %>%
  group_by(points) %>%
  summarize(avg_p = mean(price), points = points) %>%
  ggplot(aes(points, avg_p)) +
  geom_point() +
  xlab("Points") +
  ylab("Average Price") +
  theme_hc() +
  ggtitle("Average Price by Points",
          subtitle = "Figure 24")

# Correlation avg price to point
pts_avg <- wine_dat %>%
  group_by(points) %>%
  summarize(avg_pts = mean(points), avg_p = mean(price))

correlate <- bind_rows(correlate, 
                       data.frame(Category = "Points",
                                  Correlation = cor(pts_avg$avg_pts, pts_avg$avg_p)))
correlate

# Partition data into train and test set
set.seed(111, sample.kind = "Rounding")
ind <- createDataPartition(wine_dat$price, times = 1, p = 0.1, list = FALSE)

train_dat <- wine_dat[-ind,]
temp <- wine_dat[ind,]

# Make sure the all the criteria are in both data sets
test_data <- temp %>%
  semi_join(train_dat, by = "country") %>%
  semi_join(train_dat, by = "variety") %>%
  semi_join(train_dat, by = "years") %>%
  semi_join(train_dat, by = "winery")

# Baseline
mu <- mean(train_dat$price)


result <- data.frame(Method = "Baseline",
                     RMSE = RMSE(mu, test_data$price))

result

# Country Bias
b_c <- train_dat %>%
  group_by(country) %>%
  summarize(b_c = mean(price - mu))

pred_bc <- test_data %>%
  left_join(b_c, by = "country") %>%
  mutate(pred = mu + b_c) %>%
  .$pred

result <- bind_rows(result, 
                    data.frame(Method = "Country Bias",
                               RMSE = RMSE(pred_bc, test_data$price)))

# Variety bias
b_v <- train_dat %>%
  group_by(variety) %>%
  summarize(b_v = mean(price - mu))

pred_bcv <- test_data %>%
  left_join(b_c, by = "country") %>%
  left_join(b_v, by = "variety") %>%
  mutate(pred = mu + b_c + b_v) %>%
  .$pred

result <- bind_rows(result, 
                    data.frame(Method = "Country + Variety",
                               RMSE = RMSE(pred_bcv, test_data$price)))

# Years bias
b_y <- train_dat %>%
  group_by(years) %>%
  summarize(b_y = mean(price - mu))

pred_bcvy <- test_data %>%
  left_join(b_c, by = "country") %>%
  left_join(b_v, by = "variety") %>%
  left_join(b_y, by = "years") %>%
  mutate(pred = mu + b_c + b_v + b_y) %>%
  .$pred


result <- bind_rows(result, 
                    data.frame(Method = "Country + Variety + Years",
                               RMSE = RMSE(pred_bcvy, test_data$price)))

# Winery Bias
b_w <- train_dat %>%
  group_by(winery) %>%
  summarize(b_w = mean(price - mu))

pred_bcvyw <- test_data %>%
  left_join(b_c, by = "country") %>%
  left_join(b_v, by = "variety") %>%
  left_join(b_y, by = "years") %>%
  left_join(b_w, by = "winery") %>%
  mutate(pred = mu + b_c + b_v + b_y + b_w) %>%
  .$pred

result <- bind_rows(result, 
                    data.frame(Method = "Country + Variety + Years + Winery",
                               RMSE = RMSE(pred_bcvyw, test_data$price)))
# Points bias
b_p <- train_dat %>%
  group_by(points) %>%
  summarize(b_p = mean(price - mu))

pred_bcvywp <- test_data %>%
  left_join(b_c, by = "country") %>%
  left_join(b_v, by = "variety") %>%
  left_join(b_y, by = "years") %>%
  left_join(b_w, by = "winery") %>%
  left_join(b_p, by = "points") %>%
  mutate(pred = mu + b_c + b_v + b_y + b_w + b_p) %>%
  .$pred

result <- bind_rows(result, 
                    data.frame(Method = "Country + Variety + Years + Winery",
                               RMSE = RMSE(pred_bcvywp, test_data$price)))
result

# establish lambda range
lambdas <- seq(0.25, 10, 0.25)

# regularization function
regularization_rmse <- sapply(lambdas, function(l){
  # variety regularization
  bv_r <- train_dat %>%
    group_by(variety) %>%
    summarize(bv_r = sum(price - mu)/(n() + l))
  # years regularization
  by_r <- train_dat %>%
    left_join(bv_r, by = "variety") %>%
    group_by(years) %>%
    summarize(by_r = sum(price - bv_r - mu)/(n() + l))
  # winery regularization
  bw_r <- train_dat %>%
    left_join(bv_r, by = "variety") %>%
    left_join(by_r, by = "years") %>%
    group_by(winery) %>%
    summarize(bw_r = sum(price - bv_r - by_r - mu)/(n() + l))
  # points regularization
  bp_r <- train_dat %>%
    left_join(bv_r, by = "variety") %>%
    left_join(by_r, by = "years") %>%
    left_join(bw_r, by = "winery") %>%
    group_by(points) %>%
    summarize(bp_r = sum(price - bv_r - by_r - bw_r - mu)/(n() + l))
  # predict algorithm
  pred_reg <- test_data %>%
    left_join(bv_r, by = "variety") %>%
    left_join(by_r, by = "years") %>%
    left_join(bw_r, by = "winery") %>%
    left_join(bp_r, by = "points") %>%
    mutate(pred = mu + bv_r + by_r + bw_r + bp_r) %>%
    .$pred
  
  return(RMSE(pred_reg, test_data$price))
})

# Graph Lambdas vs RMSE
data.frame(Lambdas = lambdas, RMSE = regularization_rmse) %>%
  ggplot(aes(Lambdas, RMSE)) +
  geom_point() +
  theme_hc() +
  ggtitle("Lambda vs RMSE Distribution")

lambda <- lambdas[which.min(regularization_rmse)]
lambda

result <- bind_rows(result,
                    data.frame(Method = "Regularization",
                               RMSE = min(regularization_rmse)))
result


# Set column index for model
col_index <- c("points", "winery", "variety", "years", "country")

# set cross validation and RF tuning parameter
control <- trainControl(method = "cv", number = 5)
grid <- data.frame(mtry = c(1,5,10,25,50,100))

# create smaller sample of data
n = 10000

index <- sample(train_dat$price, n)

train_small <- train_dat[index,]

# randomforest training
train_rf <- train(train_small[,col_index], train_small$price,
                method = "rf",
                trControl = control,
                tuneGrid = grid)

ggplot(train_rf) +
  theme_hc() +
  ggtitle("Random Forest Train")


train_rf$bestTune

imp <- varImp(train_rf)
imp

# randomforest fit
rf_fit <- randomForest(train_small[,col_index], train_small$price,
                       minNode = train_rf$bestTune$mtry)

plot(rf_fit)

# predict the test set price
pred_rf <- predict(rf_fit, test_data[,col_index])

result <- bind_rows(result, data.frame(Method = "Random Forest",
                                       RMSE = RMSE(pred_rf, test_data$price)))
result

bv_r <- wine_dat %>%
  group_by(variety) %>%
  summarize(bv_r = sum(price - mu)/(n() + lambda))
# years regularization
by_r <- wine_dat %>%
  left_join(bv_r, by = "variety") %>%
  group_by(years) %>%
  summarize(by_r = sum(price - bv_r - mu)/(n() + lambda))
# winery regularization
bw_r <- wine_dat %>%
  left_join(bv_r, by = "variety") %>%
  left_join(by_r, by = "years") %>%
  group_by(winery) %>%
  summarize(bw_r = sum(price - bv_r - by_r - mu)/(n() + lambda))
# points regularization
bp_r <- wine_dat %>%
  left_join(bv_r, by = "variety") %>%
  left_join(by_r, by = "years") %>%
  left_join(bw_r, by = "winery") %>%
  group_by(points) %>%
  summarize(bp_r = sum(price - bv_r - by_r - bw_r - mu)/(n() + lambda))
# predict algorithm
pred_val <- validate_dat %>%
  left_join(bv_r, by = "variety") %>%
  left_join(by_r, by = "years") %>%
  left_join(bw_r, by = "winery") %>%
  left_join(bp_r, by = "points") %>%
  mutate(pred = mu + bv_r + by_r + bw_r + bp_r) %>%
  .$pred

# Validation results
validate <- data.frame(Method = "Regularization",
                       RMSE = RMSE(pred_val, validate_dat$price))
validate

rf_val <- predict(rf_fit, validate_dat[,col_index])

validate <- bind_rows(Method = "RandomForest",
                      RMSE = RMSE(rf_val, validate_dat$price))
validate