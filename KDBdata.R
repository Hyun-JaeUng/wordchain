library(ggplot2)
library(dplyr)


KDB <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/KDBbank.csv")
year <- substr(KDB[,3],3,4)
month <- as.numeric(substr(KDB[,3],7,8))
KDB <- cbind(KDB,year)
KDB <- cbind(KDB,month)


# 분기별 filter
KDB2021_1 <- KDB %>% filter(year == 21) %>% filter(month == 1 | month == 2 | month == 3)

KDB2020_1 <- KDB %>% filter(year == 20) %>% filter(month >= 1 & month <= 3)
KDB2020_2 <- KDB %>% filter(year == 20) %>% filter(month >= 4 & month <= 6)
KDB2020_3 <- KDB %>% filter(year == 20) %>% filter(month >= 7 & month <= 9)
KDB2020_4 <- KDB %>% filter(year == 20) %>% filter(month >= 10 & month <= 12)

KDB2019_1 <- KDB %>% filter(year == 19) %>% filter(month >= 1 & month <= 3)
KDB2019_2 <- KDB %>% filter(year == 19) %>% filter(month >= 4 & month <= 6)
KDB2019_3 <- KDB %>% filter(year == 19) %>% filter(month >= 7 & month <= 9)
KDB2019_4 <- KDB %>% filter(year == 19) %>% filter(month >= 10 & month <= 12)

KDB2018_1 <- KDB %>% filter(year == 18) %>% filter(month >= 1 & month <= 3)
KDB2018_2 <- KDB %>% filter(year == 18) %>% filter(month >= 4 & month <= 6)
KDB2018_3 <- KDB %>% filter(year == 18) %>% filter(month >= 7 & month <= 9)
KDB2018_4 <- KDB %>% filter(year == 18) %>% filter(month >= 10 & month <= 12)

# 분기별 테이블
eval_2021_1 <- table(KDB2021_1[,5])

eval_2020_1 <- table(KDB2020_1[,5])
eval_2020_2 <- table(KDB2020_2[,5])
eval_2020_3 <- table(KDB2020_3[,5])
eval_2020_4 <- table(KDB2020_4[,5])

eval_2019_1 <- table(KDB2019_1[,5])
eval_2019_2 <- table(KDB2019_2[,5])
eval_2019_3 <- table(KDB2019_3[,5])
eval_2019_4 <- table(KDB2019_4[,5])

eval_2018_1 <- table(KDB2018_1[,5])
eval_2018_2 <- table(KDB2018_2[,5])
eval_2018_3 <- table(KDB2018_3[,5])
eval_2018_4 <- table(KDB2018_4[,5])

# 코로나 첫 발생 20년 01월 19일 , 20년 02월 기준 데이터 분리
KDB_before_codiv_2020<- KDB %>% filter(year == 20) %>% filter(month >= 2)
KDB_before_codiv_2021<- KDB %>% filter(year == 21) %>% filter(month >= 1)
KDB_before_codiv <- rbind(KDB_before_codiv_2020,KDB_before_codiv_2021)


KDB_after_codiv_2020 <- KDB %>% filter(year == 20) %>% filter(month == 1)
KDB_after_codiv_2019 <- KDB %>% filter(year == 19) 
KDB_after_codiv_2018 <- KDB %>% filter(year == 18)
KDB_after_codiv <- rbind(KDB_after_codiv_2018,KDB_after_codiv_2019,KDB_after_codiv_2020)

# 연도별 평점 그래프
par(mfrow = c(4,1))
barplot(eval_2021, main='2021년 평점', xlab='별 개수', ylab='빈도수') 
barplot(eval_2020, main='2020년 평점', xlab='별 개수', ylab='빈도수')
barplot(eval_2019, main='2019년 평점', xlab='별 개수', ylab='빈도수')
barplot(eval_2018, main='2018년 평점', xlab='별 개수', ylab='빈도수')

# 앱 개선에 대한 평가척도 = 별점 평균
mean(KDB2021[,5])
mean(KDB2020[,5])
mean(KDB2019[,5])
mean(KDB2018[,5])


