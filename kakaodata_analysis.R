library(ggplot2)
library(dplyr)
library(plyr)
library(plotly)

# 데이터 기간 맞추기
data1 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/IBK.csv")
data1 <- data1 %>% filter(substr(data1[,3],3,4) == "21" | substr(data1[,3],3,4) == "20")

data2 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/KDBbank.csv")
data2 <- data2 %>% filter(substr(data2[,3],3,4) == "21" | substr(data2[,3],3,4) == "20")

data3 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/kakao.csv")
data3 <- data3 %>% filter(substr(data3[,3],3,4) == "21" | substr(data3[,3],3,4) == "20")

data4 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/shinhan.csv")
data4 <- data4 %>% filter(substr(data4[,3],3,4) == "21" | substr(data4[,3],3,4) == "20")

data5 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/woori.csv")
data5 <- data5 %>% filter(substr(data5[,3],3,4) == "21" | substr(data5[,3],3,4) == "20")

data6 <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/NHbank.csv")
data6 <- data6 %>% filter(substr(data6[,3],3,4) == "21" | substr(data6[,3],3,4) == "20")


# 단위 기간 별점 평균
mean(data1[,5])
mean(data2[,5])
mean(data3[,5])
mean(data4[,5])
mean(data5[,5])
mean(data6[,5])

# 단위 기간 리뷰수
length(data1[,4])
length(data2[,4])
length(data3[,4])
length(data4[,4])
length(data5[,4])
length(data6[,4])

# 산점도
score <- c(round(mean(data1[,5]),1),
           round(mean(data2[,5]),1),
           round(mean(data3[,5]),1),
           round(mean(data4[,5]),1),
           round(mean(data5[,5]),1),
           round(mean(data6[,5]),1))

count <- c(length(data1[,4]),
           length(data2[,4]),
           length(data3[,4]),
           length(data4[,4]),
           length(data5[,4]),
           length(data6[,4]))


name <- c("기업은행","카카오뱅크","산업은행","신한은행","우리은행","농협은행")
grade <- c(1,2,3,4,5)
reviewCount <- c(2000,3000,4000,5000)

dat <- NULL
dat <- cbind(as.numeric(score),as.numeric(count), name)
dat <- as.data.frame(dat)
class(dat)

par(mfrow=c(1,1))


ggplot(data = dat, aes(x = score, y = count, color= name)) + geom_point(size=5) +
  ggtitle("은행별 앱 평점과 리뷰수", ) + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) -> basicScore_Plot



##########################################################################################################3 


####################### 카카오뱅크 ########################
data <- read.csv("C:/Users/HYM/Desktop/멀티캠퍼스/wordchain/kakao.csv")

tmp1 <- data %>% filter(substr(data[,3],8,8)=="월") 
tmp2 <- data %>% filter(substr(data[,3],8,8)!="월")
tmp1[,3]<- paste(substr(tmp1[,3],1,6),"0",substr(tmp1[,3],7,14),sep='')
data <- rbind(tmp1,tmp2)

tmp3 <- data %>% filter(substr(data[,3],12,12)=="일")
tmp4 <- data %>% filter(substr(data[,3],12,12)!="일")
tmp3[,3] <- paste(substr(tmp3[,3],1,10),"0",substr(tmp3[,3],11,14),sep="")
data <- rbind(tmp3,tmp4)

year <- substr(data[,3],3,4)
month <- as.numeric(substr(data[,3],7,8))
data <- cbind(data,year)
data <- cbind(data,month)
sort(data[,3],decreasing=TRUE)

data <- arrange(data,desc(date))
str(data)
table(data$score)
################# 은행별로 나눠야 할 것 ######################
# 분기별 filter
data2021 <- data %>% filter(year == 21)
data2021_1 <- data %>% filter(year == 21) %>% filter(month == 1 | month == 2 | month == 3)

data2020 <- data %>% filter(year == 20)
data2020_1 <- data %>% filter(year == 20) %>% filter(month >= 1 & month <= 3)
data2020_2 <- data %>% filter(year == 20) %>% filter(month >= 4 & month <= 6)
data2020_3 <- data %>% filter(year == 20) %>% filter(month >= 7 & month <= 9)
data2020_4 <- data %>% filter(year == 20) %>% filter(month >= 10 & month <= 12)

data2019 <- data %>% filter(year == 19)
data2019_1 <- data %>% filter(year == 19) %>% filter(month >= 1 & month <= 3)
data2019_2 <- data %>% filter(year == 19) %>% filter(month >= 4 & month <= 6)
data2019_3 <- data %>% filter(year == 19) %>% filter(month >= 7 & month <= 9)
data2019_4 <- data %>% filter(year == 19) %>% filter(month >= 10 & month <= 12)

data2018 <- data %>% filter(year == 18)
data2018_1 <- data %>% filter(year == 18) %>% filter(month >= 1 & month <= 3)
data2018_2 <- data %>% filter(year == 18) %>% filter(month >= 4 & month <= 6)
data2018_3 <- data %>% filter(year == 18) %>% filter(month >= 7 & month <= 9)
data2018_4 <- data %>% filter(year == 18) %>% filter(month >= 10 & month <= 12)

# 분기별 테이블
score_2021_1 <- table(data2021_1[,5])

score_2020_1 <- table(data2020_1[,5])
score_2020_2 <- table(data2020_2[,5])
score_2020_3 <- table(data2020_3[,5])
score_2020_4 <- table(data2020_4[,5])

score_2019_1 <- table(data2019_1[,5])
score_2019_2 <- table(data2019_2[,5])
score_2019_3 <- table(data2019_3[,5])
score_2019_4 <- table(data2019_4[,5])

score_2018_4 <- table(data2018_4[,5])

# 빈도수 최대값, 빈도수 최소값
count_max <- max(max(score_2021_1),
                 max(score_2020_1),max(score_2020_2),max(score_2020_3),max(score_2020_4),
                 max(score_2019_1),max(score_2019_2),max(score_2019_3),max(score_2019_4),
                 max(score_2018_4)
                 )


# 연도별 평점 그래프
par(mfrow = c(4,1))
barplot(score_2021_1, main='2021년 1분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max)) 

par(mfrow = c(4,1))
barplot(score_2020_1, main='2020년 1분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2020_2, main='2020년 2분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2020_3, main='2020년 3분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2020_4, main='2020년 4분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))

par(mfrow = c(4,1))
barplot(score_2019_1, main='2019년 1분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2019_2, main='2019년 2분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2019_3, main='2019년 3분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2019_4, main='2019년 4분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))

par(mfrow = c(4,1))
#barplot(score_2018_1, main='2018년 1분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
#barplot(score_2018_2, main='2018년 2분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
#barplot(score_2018_3, main='2018년 3분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))
barplot(score_2018_4, main='2018년 4분기 평점', xlab='별 개수', ylab='빈도수', ylim = c(0,count_max))

# 앱 개선에 대한 평가척도 = 별점 평균
mean(data2018[,5])
mean(data2019[,5])
mean(data2020[,5])
mean(data2021[,5])


time <- c(2018,2019,2020,2021)
score <- c(mean(data2018[,5]),mean(data2019[,5]),mean(data2020[,5]),mean(data2021[,5]))
df <- data.frame(time,score)

par(mfrow = c(1,1))
ggplot(data=df, aes(x=time,y=score))+geom_line()+geom_point()+ggtitle("카카오 연도별 별점 평균") + 
        theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) -> kakaoScore_plot

  
ggsave("C:/Users/HYM/Desktop/kakaoScore_plot.jpg", plot = kakaoScore_plot, dpi = 300)

# 코로나 첫 발생 20년 01월 19일 , 20년 02월 기준으로 데이터 분리

data_before_codiv_2020 <- data %>% filter(year == 20) %>% filter(month == 1)
data_before_codiv_2019 <- data %>% filter(year == 19) 
data_before_codiv_2018 <- data %>% filter(year == 18)
data_before_codiv <- rbind(data_before_codiv_2018,data_before_codiv_2019,data_before_codiv_2020)

data_after_codiv_2020<- data %>% filter(year == 20) %>% filter(month >= 2)
data_after_codiv_2021<- data %>% filter(year == 21) %>% filter(month >= 1)
data_after_codiv <- rbind(data_after_codiv_2020,data_after_codiv_2021)

before_codiv_score <- table(data_before_codiv[,5])
after_codiv_score <- table(data_after_codiv[,5])

par(mfrow = c(2,1))

barplot(before_codiv_score, main='카카오 코로나 이전 평점', xlab='별 개수', ylab='빈도수') 
barplot(after_codiv_score, main='카카오 코로나 이후 평점', xlab='별 개수', ylab='빈도수')

# 평균
before_codiv_mean <- mean(data_before_codiv[,5])
after_codiv_mean <- mean(data_after_codiv[,5])

# 1점 리뷰수 대비 5점 리뷰수
before_codiv_score[5]/before_codiv_score[1]
after_codiv_score[5]/after_codiv_score[1]
