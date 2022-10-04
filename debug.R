library("dplyr")
library("lubridate")
library("readr")
library("ggplot2")

#老師在debug
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

stockplot <- 
  function(data_frame, stockname){
    dataframe %>%
      filter(stock == stockname) %>%
      ggplot(aes(x = date, y = close))+
    geom_line()
  }

