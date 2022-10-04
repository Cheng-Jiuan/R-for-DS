# load package
install.packages("tweedie")
install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(tweedie)
library(ggplot2)

#Q1. -----
t.test(
  rtweedie(100,mu=10000,phi=100,power=1.9),mu=10000)$p.value #第一個用於次數，mu=平均,phi

#創建fuction
simTweedietest <- function(N){
  t.test(rtweedie(N,mu=10000,phi=100,power=1.9),mu=10000)$p.value
}

#Q2. ---- 
replicate(10,simTweedietest(100))

#用function寫
MTweedietest <- function(M,N,alpha){
  p_value <- replicate(M,simTweedietest(N)) #M次的結果在這裡
  below_alpha <- p_value < alpha
  sum(below_alpha) / M
}

#另一種寫法（合成一種方法）
MTweedietest <- function(M,N,alpha){
  sum(replicate(M,simTweedietest(N))< alpha) / M
}

M <- 10
N <- 100
alpha <- 0.05

#Q3_1.----
df <- tibble(N = c(10, 100, 1000, 5000), M = 100, share_reject = NA)
i <- 1 #賦值

for(i in 1:nrow(df)){
  df$share_reject[i] <-
  MTweedietest(M = df$M[i],alpha = 0.05)
} #M = df$M[i]可以不斷更新 #alpha = 0.05檢定值 #使用loop來做

#Q3_3. ----
df %>%
  ggplot(aes(x = log(N), #log比較簡單可視化
             y = share_reject))+
  geom_line()+
  geom_hline(yintercept = 0.05, color = red)+ #yintercept 畫一條紅色的檢定線
  theme_bw()


