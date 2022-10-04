# Q1 -----
survey %>% #從該資料中使用他裡面的fuction
  group_by(party) %>%
  mutate(n_responders = n()) %>% #把類別變成數值
  filter(n_responders >= 25) %>% #篩選
  summarise(mean_age = mesn(age), 
            sd_age = sd(age)) %>%#只篩選出這兩個
  filter(mean_age == max(mean_age) | min_age == min(mean_age)) #篩出最大跟最小的人 #==用於判斷式、=用於賦值
  arrange(mean_age) #排序

# Q2 -----
levels(survey$party) # levels意指為裡面有幾個因子 
levels(survey$religionsity)
levels(survey$income_delicine)
sapply(survey, levels) # 呈現整理的

# store the non-responses

drop_levels <-
  tail(levels(survey$party, 5)) #tail是只要留最後幾個的意思

drop_levels <-
  survey %>%
  pull(party) %>% #變成向量
  levels() %>%
  tail(5) #與上面的寫法一樣

#
survey %>% 
  filter(party %in% drop_levels) %>%
  summarise(n = n()) #計算有多少個

# Q3 ------
survey %>% 
  filter(!(party %in% drop_levels)) %>% #!可用於相反詞(not)
  filter(!(income_decile %in% drop_levels)) %>% #文字%in%
  group_by(party) %>%
  summarise(mean_religion = mean(as.numeric(religionsity))) %>% #把類別變成數值
  arrange(mean_religion)

# Q4 -------
survey %>% 
  group_by(party) %>%
  mutate(no_respondents = n()) %>%
  filter(no_respondents >75) %>%
  filter(!(income_decile %in% drop_levels)) %>% #把未知的drop掉
  mutate(num_income_decile = as.numeric(income_decile),
         high_icome = num_income_decile >= 9, #創建新的col, >= 9是1, <9 是0
         low_income = num_income_decile <= 2) %>% #創建新的col, <=2是1, >2 是0
  sumarise(ratio = sum(high_incom)/sum(low_income)) %>% 
  arrange(ratio)
