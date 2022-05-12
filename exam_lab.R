#install.packages("tm")
library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)

Sys.Date()
Sys.time()
na.omit()
is.na()
df <- read.csv("exam1.csv")
isna_df <- is.na(df)
missing(df)
!is.na(df$first_name)
# cleaning data
df$car_model_year[which(is.na(df$car_model_year))] <- mean(df$car_model_year, na.rm = TRUE)
df$car_model_year[which(is.na(df$car_model_year))] <- getmode(df$car_model_year, na.rm = TRUE)

count
# plot
plot(x = (df$car_color), y = df$car_model_year, line = "h")
hist(x = df$car_model)
boxplot(x = df$car_model)
line(df$gender)
hist(x= count_make)
tidy_df <- df$city %>%
  unnest_tokens(word, text)
letters(df$city)

df_nospace <- tm_map(df, stripWhitespace) #Strips white space
> b<- tm_map(b, tolower) #Changes case to lowercase
whitespace_tokenizer(df)
mode(df)

# what is the most popular car brand
count_make <- table(df$car_make)
pie(count_make)
view(count_make)
# ford is

# what is the most popular model?
count_model <- table(df$car_model)
view(count_model)

# Accent	8
# Corvette	8
# F-Series	8
# Savana 3500	8

# what is the average year of the car makes
mean(df$car_model_year)

# what is the standard deviation of model years

summary(df$car_model_year)
sd(df$car_model_year)

# what is the most popular credit card used?
count_card <- table(df$credit._card_type)
view(count_card)
pie(count_card)

# what is the most popular color
count_color <- table(df$car_color)
view(count_color)

# pie graphs 
pie(count_model)

pie(count_make)

pie(count_card)

pie(count_color)