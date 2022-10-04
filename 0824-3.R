#ggplot第一個值要是dataframe
#geom_?() 可用於畫自己想要的圖
# one row is one observation, one coloum is one variable

# D3-----
library(ggplot2) #plot
library(tidyr) #piovt_wide  #piovt_longer (延展他)
library(dplyr) #read.csv
library(readr) #data warngling
library(forcats) 


# wide or long? ----
# dir() 可用於找在該資料夾的東西
temps <- read_csv("data-temps.csv")
long <- #這個是ggplot可以畫的
  temps %>% 
  pivot_longer(cols = -machine, #把所有的col都延展，除了machine
               names_to = "when", #重新命名新的col
               values_to = "temperature" ) #賦值
               
long %>%
  pivot_wider(ID_cols = machine,
             names_from = "when",
             values_from = "temperature")

# 簡單的圖
long %>%
  ggplot(aes(x=machine , y= temperature))+geom_point()

long %>%
  ggplot(aes(x=machine , y= temperature, colour =when ))+geom_point()

#也可以這樣寫
ggplot(long, aes(x=machine , y= temperature, colour =when))+
  geom_point()

# intorducing the data ----
load("data-sotra.Rdata")

# one variable ----
df.traffic %>% 
  ggplot(aes(x = hourly.volume))+
  geom_histogram(binwidth = 20)
  
df.traffic %>% 
  ggplot(aes(y = weekday.holiday))+
  geom_bar() #可以改變x,y換不同軸心

# what if the counting has already been done?
df.traffic %>% 
  group_by(weekday.holiday) %>%
  summarise(count = n())  #週一到週日數量

# two variable ----
df.traffic %>% 
  group_by(weekday.holiday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday.holiday,
             y = count))+
  geom_col()
  
# reordering the factors
df.traffic %>% 
  group_by(weekday.holiday) %>%
  summarise(count = n()) %>%
  mutate(weekday.holiday = fct_reorder(weekday.holiday, count)) %>% #照順序排列(fct_reorder) #用-（或是.desc = TRUE) 就可以由高到低
  ggplot(aes(x = weekday.holiday,
             y = count))+
  geom_col()

df.traffic %>%
  filter(hourly.volume != 0) %>% #排除是0的值
  ggplot(aes(x = hour, y = hourly.volume)) + geom_point(alpha = 0.05) + #alpha 讓每個點變得模糊一點
  geom_smooth() #畫出一條不同的線

# add text label
df.traffic %>% 
  group_by(weekday.holiday) %>%
  summarise(count = n()) %>%
  mutate(labels = paste(count, "hour")) %>%  #先建立一個欄位for 文字用
  ggplot(aes(x = weekday.holiday,
             y = count,
             ))+
  geom_col()+
  geom_text(aes(label = labels), nudge_y = 120, size= 3 )+  #文字加在圖片裡面 #加逗點，可以新增出不一樣的東西 #nudge_y 把東西往上移 #size可以轉換大小
  ylim(c(0,3000)) #把他的layer層變成在0-3000



# scales and axes

????

#visualizing group ----
#split the plot
df.traffic %>%
  filter(hourly.volume != 0) %>% #排除是0的值
  ggplot(aes(x = hour, y = hourly.volume, colour = weekday.holiday))+   #點點換顏色
  geom_point(alpha = 0.5)+ #alpha 讓每個點變得模糊一點
  geom_smooth() 

df.traffic %>%
  filter(hourly.volume != 0) %>% #排除是0的值
  ggplot(aes(x = hour, y = hourly.volume, colour = weekday.holiday))+   #點點換顏色
  geom_smooth()  #只保留線

df.traffic %>%
  filter(hourly.volume != 0) %>% #排除是0的值
  ggplot(aes(x = hour, y = hourly.volume))+   
  geom_point(alpha = 0.5)+
  geom_smooth() +
  facet_wrap(~weekday.holiday) #facet_wrap可根據該變數做出不同表

#recode 改名稱
#example:
vector <- c("a","a","b","c")
recode(vector, a = "apple")
#come back topic
df.traffic %>%
  filter(hourly.volume != 0) %>% #排除是0的值
  mutate(weekday.holiday = recode(
    weekday.holiday, 
    mandag = "monday")) %>% 
    select(weekday.holiday) %>% 
      distinct()
  #.來代替原本的名稱
  ggplot(aes(x = hour, y = hourly.volume))+   
  geom_point(alpha = 0.5)+
  geom_smooth() +
  facet_wrap(~weekday.holiday) #facet_wrap可根據該變數做出不同表
