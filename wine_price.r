if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(knitr)) install.packages(knitr)

library(tidyverse)
library(caret)
library(ggplot2)
library(knitr)


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
  select(X, country, points, price, province, taster_name, title, variety, winery)

# Remove all entries with NA as price
new_dat <- new_dat %>%
  filter(!is.na(price))

# Extract year from title and make into it's own column
pattern <- "\\d{4}"
year <- str_extract(new_dat$title, pattern)

new_dat <- new_dat %>%
  mutate(year = year)

head(new_dat)

# Divide the data into modeling set and validation set
set.seed(123, sample.kind = "Rounding")

ind <- createDataPartition(new_dat$price, times=1, p=0.1, list=FALSE)
wine_dat <- new_dat[-ind,]
validate_dat <- new_dat[ind,]
