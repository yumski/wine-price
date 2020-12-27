if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(knitr)) install.packages(knitr)

library(tidyverse)
library(caret)
library(ggplot2)
library(knitr)
library(ggthemes)


# Download the file and load into R data
# This data is originally from https://www.kaggle.com/zynicide/wine-reviews
# I downloaded the file and uploaded to github for ease of access.

dl <- tempfile()
download.file("https://raw.githubusercontent.com/yumski/wine-price/master/winemag-data-130k-v2.csv", dl)
dat <- read.csv(dl)

rm(dl)

head(dat)

# Filter out columsn that I don't plan to use

new_dat <- dat %>%
  select(X, country, points, price, title, variety, winery)

# Remove all entries with NA as price
new_dat <- new_dat %>%
  filter(!is.na(price))

# Extract year from title and make into it's own column
pattern <- "\\d{4}"
year <- str_extract(new_dat$title, pattern)

new_dat <- new_dat %>%
  mutate(years = year)

head(new_dat)

# Divide the data into modeling set and validation set
set.seed(123, sample.kind = "Rounding")

ind <- createDataPartition(new_dat$price, times=1, p=0.1, list=FALSE)
wine_dat <- new_dat[-ind,]
validate_dat <- new_dat[ind,]

dim(wine_dat)

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
  ylab("Variety")
  ggtitle("Variety Count Distribution",
          subtitle = "Figure 5")

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
          subtitle = "Figure 6")

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
          subtitle = "Figure 7")

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
          subtitle = "Figure 8")

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
          subtitle = "Figure 9")

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
          subtitle = "Figure 10")

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
          subtitle = "Figure 11")

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
          subtitle = "Figure 12")

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
  theme_hc() +
  xlab("Winery") +
  ylab("Price (Log 10 Scale)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_hc() +
  ggtitle("Winery Count Distribution",
          subtitle = "Figure 13")

# Price by Winery Distribution
wine_dat %>%
  group_by(winery) %>%
  summarize(n = n(), winery, price, points) %>%
  filter(n > 100) %>%
  ggplot(aes(winery, price)) +
  geom_boxplot() +
  scale_y_log10() +
  xlab("Winery") +
  ylab("Price (Log 10 Scale)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_hc() +
  ggtitle("Price by Winery Distribution",
          subtitle = "Figure 14")

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
          subtitle = "Figure 15")

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
          subtitle = "Figure 16")

# Avg Points and Price by Winery Correlation
winery_avg <- wine_dat %>%
  group_by(winery) %>%
  summarize(winery = winery, avg_pts = mean(points), avg_p = mean(price))

correlate <- bind_rows(correlate, 
                       data.frame(Category = "Winery",
                                  Correlation = cor(winery_avg$avg_pts, winery_avg$avg_p)))
correlate
