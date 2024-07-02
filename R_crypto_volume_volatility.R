# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sjPlot)  
library(skimr)  
library(car)

# Loading the data
btc_data <- read.csv("C:/Users/xjock/Desktop/BTC-USD.csv")
eth_data <- read.csv("C:/Users/xjock/Desktop/ETH-USD.csv")

# Skimr
skim_without_charts(btc_data)
skim_without_charts(eth_data)

# Preparing data by calculating volatility and converting volumes to numeric
btc_data <- btc_data %>% 
  mutate(Date = as.Date(Date),
         Volatility = (High - Low) / Low * 100,
         Volume = as.numeric(Volume))

eth_data <- eth_data %>% 
  mutate(Date = as.Date(Date),
         Volatility = (High - Low) / Low * 100,
         Volume = as.numeric(Volume))

btc_data <- btc_data[complete.cases(btc_data), ]
eth_data <- eth_data[complete.cases(eth_data), ]

btc_model_rev <- lm(Volatility ~ Volume, data = btc_data)
eth_model_rev <- lm(Volatility ~ Volume, data = eth_data)

summary(btc_model_rev)
summary(eth_model_rev)

tab_model(btc_model_rev, show.reflvl = FALSE, show.intercept = TRUE, p.style = "numeric_stars", show.p = TRUE)
tab_model(eth_model_rev, show.reflvl = FALSE, show.intercept = TRUE, p.style = "numeric_stars", show.p = TRUE)

plot_model(btc_model_rev, type = "diag") 
plot_model(eth_model_rev, type = "diag")  
plot_model(btc_model_rev, type = "est")   
plot_model(eth_model_rev, type = "est")   

# Generating scatter plots with regression line for analysis
ggplot(btc_data, aes(x = Volume, y = Volatility)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "BTC: Volatility vs. Volume", x = "Volume", y = "Volatility (%)")

ggplot(eth_data, aes(x = Volume, y = Volatility)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "ETH: Volatility vs. Volume", x = "Volume", y = "Volatility (%)")
