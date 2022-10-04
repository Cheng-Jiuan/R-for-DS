#try to learn it ----

# Functions and loops
for(www in 1:10){
  print(www) 
} #創造變數i #in亦即為在裡面有多少 #vector

# 
for (animal in  c("cat","dogs","hamster")){
  print(paste("I like", animal , "!"))
} #paste0 意指為無間隔 #paste 意指為有間隔
 
# loop 可協助做重複的動作
#比較笨的方法
x <- seq(from = 1, to =100,by = 2) #從1-100,每個間隔2
y <- rep(NA, length(x)) #建造一個重複(rep)的NA，跟x一樣長

y[1] <- sum(x[1:1])
y[2] <- sum(x[1:2]) 
y[3] <- sum(x[1:3])

# 用loop
for (i in 1:length(x)){ #length(x)跟他長度一樣
  y[i] <- sum(x[1:i]) 
}

# golden ratio------
library(ggplot2)
# step 1
N <- 10 #看數字裡面做多少個
# step 2
df <- tibble(
  n = 1:N,
  Fibonacci = NA_integer_
) #tibble創建表格 #NA_integer(意指會跟N一樣長) #mutate只能用於dataframe
# step 3
df$Fibonacci[1:2] <-1  #只篩選出前兩個變成1
for (i in 3:N){  #寫迴圈
  df$Fibonacci[i] <- df$Fibonacci[i-2]+df$Fibonacci[i-1] #寫出計算式
} 
#step 4
df %>%
  mutate( ratio = Fibonacci/ lag(Fibonacci,order_by = n))%>% #lag可以用於計算前一格、lead 可以用於後一格,order_by排序
  ggplot(aes(x=n,y=ratio))+
  geom_line()



# start data ----
library(dplyr)
library(lubridate) 
library(readr)
library(ggplot2)

options(digits=3) #改小數點多寡
#paste0(data_folder,stockfile)可以找路徑連結
data_folder <- "stockprices/" #/是重要的,才能找到檔案
stockfile <- dir(data_folder)[1]

stockprices <- 
  tibble(
    date = ymd(), #建日期的col
    close = numeric(), #建多寡的col
    stock = character() #建文字檔
  ) #先寫一個dataframe

#創建data frame的過程
for(stockfile in dir(data_folder)){
  stockprices <- 
    read_csv(paste0(data_folder, stockfile)) %>%
    transmute(
      date = Date,
      close = Close,
      stock = tools::file_path_sans_ext(stockfile))%>%  #把後面的csv刪除了，tools::file_path_sans_ext
    bind_rows(stockprices) #合併兩個row
}




