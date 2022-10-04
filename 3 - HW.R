# road package -----
library(ggplot2)
library(dplyr)
library(readr)

# read in data ------
plastic <- read.csv("data-plastic.csv")

# Q1.
plastic %>%
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged)))+   
  geom_point()

# Q2.
plastic %>%
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged)))+  #不同軸心
  geom_point(aes(size = population, #更改點點的內容
                 color = region))

# Q3.
plastic %>%
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged)))+  #不同軸心
  geom_point(aes(size = population, #更改點點的內容
                 color = region))+
  labs(x = "per capita GDP(log)",
       y = "Per capital waste",
       color = "region")+  #更改名稱
  guides(size = "none")+
  scale_color_brewer(palette = "Spectral") +
  theme_classic()

# Q4.
plastic %>%
  ggplot(aes(x = log(gdp),
             y = log(per_capita_mismanaged)))+  #不同軸心
  geom_point(aes(size = population, #更改點點的內容
                 colour = region)) +
  geom_text(aes(label = code,
                size = 2, 
                nudge_x = 0.15)) + #更改文字
  geom_smooth(color = "black")+ #增加曲線圖 
  labs(x = "per capita GDP(log)",
       y = "Per capital waste",
       color = "region")+  #labs用於更改圖作標的名稱
  guides(size = "none")+
  scale_color_brewer(palette = "Spectral") + #換不同顏色
  theme_classic()

#為何畫出來不一樣？
  