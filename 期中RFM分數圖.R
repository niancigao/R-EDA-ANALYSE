#rm(list=ls(all=T))
pacman::p_load(magrittr, readr, caTools, ggplot2, dplyr, vcd,scales,Hmisc,foreign,lubridate,plotly,gridExtra)


Z = read_csv("data/ta_feng_all_months_merged.csv") %>% 
  data.frame %>% setNames(c(
    "date","cust","age","area","cat","prod","qty","cost","price"))

Z$date = as.Date(Z$date, format="%m/%d/%Y")

age.group = c("<25","25-29","30-34","35-39","40-44",
              "45-49","50-54","55-59","60-64",">65")
Z$age = c(paste0("a",seq(24,69,5)),"a99")[match(Z$age,age.group,11)]

# a99 無年齡資料的(不在範圍裡)

Z$area = paste0("z",Z$area)

# Quantile of Variables
sapply(Z[,7:9], quantile, prob=c(.99, .999, .9995)) #分位數

# Remove Outliers
Z = subset(Z, qty<=24 & cost<=3800 & price<=4000) 
nrow(Z)   # 原817741 後817182

Z$tid = group_indices(Z, date, cust) # same customer same day

X = Z %>% group_by(tid) %>% summarise(
  date = min(date),          # 交易日期  
  cust = min(cust),          # 顧客 ID
  age = min(age),            # 顧客 年齡級別
  area = min(area),          # 顧客 居住區別
  items = n(),               # 交易項目(總)數 (買幾次)
  pieces = sum(qty),         # 產品(總)件數 (買幾樣)
  total = sum(price),        # 交易(總)金額
  gross = sum(price - cost)  # 毛利
) %>% data.frame

# Check Quantile & Remove Outliers
sapply(X[,6:9], quantile, prob=c(.999, .9995, .9999))

# Remove Outliers
X = subset(X, items<=62 & pieces<95 & total<16000) # 119328



d0 = max(X$date) + 1     #現在日期加 1

#以今天加1為基準看同顧客購買紀錄最小天數的距離&最大天數的距離

salesRFM = X %>% mutate(
  days = as.integer(difftime(d0, date, units="days"))
) %>% group_by(cust) %>% summarise(
  Recency = min(days),      # recency   間隔最小天數
  s = max(days),      # seniority   最大間隔天數
  Frequency = n(),            # frquency
  m = mean(total),    # monetary 貨幣 (平均花多少)
  Monetary = sum(total),   # total revenue contribution 總收入貢獻
  raw = sum(gross),   # total gross profit contribution 總毛利貢獻
  age = min(age),     # age group
  area = min(area),   # area code
) %>% data.frame      


###############################################################################
###############################################################################
###############################################################################

#R_S:基於最近一次交易日期計算得分，距離當前日期越近，則得分越高，否則得分越低；
#F_S:基於交易頻率計算得分，交易頻率越高，則得分越高，否則得分越低；
#M_S：基於交易金額得分，交易金額越高，則得分越高，反之得分越低。
#同時為了對每個客戶進行綜合評價，
#也可將以上三個得分進行加權計算
#這裡統一採用100:10:1
#RFM = 100 R_S + 10 F_S + 1*M_S


#計算得分
salesRFM <-mutate(salesRFM,
                   rankR = 6-cut(salesRFM$Recency,
                               breaks=quantile(salesRFM$Recency,
                                               probs=seq(0,1,0.2),
                                               names = FALSE),
                               include.lowest = TRUE,labels=F),   
                  #分數高間隔短 : 1~5分
                  
                   rankF = cut(salesRFM$Frequency ,
                               breaks = c(1,2,3,4,5,85),
#quantile(salesRFM$Frequency,probs=seq(0, 1, 0.2),names = FALSE),# 1 1 2 3 5 85
                               include.lowest = TRUE,labels=F), 
                  #分數高頻率高 : 1~5分

                   rankM = cut(salesRFM$Monetary,
                               breaks = quantile(salesRFM$Monetary,
                                                 probs = seq(0, 1, 0.2),
                                                 names = FALSE),
                               include.lowest = TRUE,labels=F),
                  #分數高買的總額高 : 1~5分

                   rankRMF = 100*rankR + 10*rankF + 1*rankM)
                  #加權(看我們在意的點)

salesRFM <-mutate(salesRFM, 
                  rankR1 = 1-rescale(salesRFM$Recency,to = c(0,1)),  
                  rankF1 = rescale(salesRFM$Frequency,to = c(0,1)),  
                  rankM1 = rescale(salesRFM$Monetary,to = c(0,1)),  
                  rankRMF1 = 0.5*rankR + 0.3*rankF + 0.2*rankM)
                  #把分數正規化，跟上面結果差不多


#客戶分類
salesRFM <- within(salesRFM,
                   {R_S = ifelse(rankR > mean(rankR),2,1)     #分數越高越好
                    F_S = ifelse(rankF > mean(rankF),2,1)  
                    M_S = ifelse(rankM > mean(rankM),2,1)})

salesRFM <- within(salesRFM,{Custom = NA  
Custom[R_S == 2 & F_S == 2 & M_S == 2] = '高價值客戶'  
Custom[R_S == 1 & F_S == 2 & M_S == 2] = '重點保持客戶'  
Custom[R_S == 2 & F_S == 1 & M_S == 2] = '重點發展客戶'    
Custom[R_S == 1 & F_S == 1 & M_S == 2] = '重點挽留客戶'  
Custom[R_S == 2 & F_S == 2 & M_S == 1] = '重點保護客戶'  
Custom[R_S == 1 & F_S == 2 & M_S == 1] = '一般保護客戶'  
Custom[R_S == 2 & F_S == 1 & M_S == 1] = '一般發展客戶'  
Custom[R_S == 1 & F_S == 1 & M_S == 1] = '潛在客戶'
})                                                            #分出客群


#分析結果可視化
ggplot(salesRFM,aes(rankM,fill=Custom))+
  ggtitle("R與F分析圖(分數表)")+
  geom_bar()+                               #在20%區間的人數(2000~0)
  facet_grid(rankF~rankR)+
  xlab("最近一次消費天數(5:top20%~1:80-100%)") + ylab("購買頻率(5:top20%~1:80-100%)")+
  theme_bw()

#RFM熱力圖
heatmap_data <- salesRFM %>% group_by(rankF,rankR) %>% dplyr::summarize(M_mean = mean(Monetary))
ggplot(heatmap_data,aes(rankF,rankR,fill =M_mean ))+
  geom_tile()+
  ggtitle("各組平均收益")+
  xlab("購買頻率(5:top20%~1:80-100%)") + ylab("最近一次消費天數(5:top20%~1:80-100%)")+
  scale_fill_distiller(palette = 'RdYlGn',direction = 1)

#RFM直方圖
p1 <- ggplot(salesRFM,aes(Recency)) + geom_histogram(bins = 40,fill = '#3366CC')+theme_bw()
p2 <- ggplot(salesRFM,aes(Frequency)) + geom_histogram(bins = 40,fill = '#3366CC')+theme_bw()  
p3 <- ggplot(salesRFM,aes(Monetary)) + geom_histogram(bins = 40,fill = '#3366CC')+theme_bw()  
grid.arrange(p1,p2,p3,nrow=1)

#RFM兩兩交叉散點圖
p1 <- ggplot(salesRFM,aes(Monetary,Recency))+geom_point(shape=21,fill='#3366CC',colour='white',size = 2)+theme_bw()
p2 <- ggplot(salesRFM,aes(Monetary,Frequency))+geom_point(shape=21,fill='#3366CC',colour='white',size = 2)+theme_bw()  
p3 <- ggplot(salesRFM,aes(Frequency,Recency))+geom_point(shape=21,fill='#3366CC',colour='white',size = 2)+theme_bw()  
grid.arrange(p1,p2,p3, ncol=1)



#數據結果導出
#write.csv(salesRFM,'salesRFM.csv')




