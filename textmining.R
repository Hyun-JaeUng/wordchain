library(KoNLP)
library(dplyr)
library(wordcloud)
library(wordcloud2)

# 데이터 불러오기
data <- read.csv('data/kakao.csv')

# 데이터 탐색
str(data) # 행과 열 갯수
table(is.na(data)) # 결측치 확인


# 1. 전체 리뷰 대상
comment_alldata <- data$comment # comment 열만 추출
class(comment_alldata) # character
(allcomment_len <- length(comment_alldata)) # 전체 리뷰 갯수 확인
data %>% group_by(score) %>% count # 평점 별 리뷰 갯수 확인 

etdata <- extractNoun(comment_alldata) # 명사 추출
etdata <- unlist(etdata) # 리스트 해제
etdata <- gsub('[[:punct:]]', "", etdata) # 특수문자 제거
etdata <- Filter(function(x) {nchar(x) >= 2}, etdata) # 2글자 이상 단어만 남겨놓기

etdata2 <- table(etdata) # 추출된 명사 별 갯수 구하기
result <- sort(etdata2, decreasing = T)# 가장 많이 나온 단어 확인

# 막대 그래프
head(result, 20) # 많이 나온 단어 20개 확인 
barplot(head(result, 10), main='전체 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수')

word <- as.data.frame(result) # table 객체를 데이터 프레임으로 변경
wordcloud2(word) # 워드클라우드 그리기



### 2. 5점 리뷰 대상 ###
comment_fivedata <- data %>% filter(score == 5) %>% select(comment, score) # 5점짜리 데이터 필터링
comment_fivedata <- comment_fivedata$comment # comment 열 추출
(fivecomment_len <- length(comment_fivedata)) # 5점 리뷰 갯수 확인

five_etdata <- extractNoun(comment_fivedata) 
five_etdata <- unlist(five_etdata)
five_etdata <- gsub('[[:punct:]]', "", five_etdata)
five_etdata <- Filter(function(x) {nchar(x) >= 2}, five_etdata)

five_etdata2 <- table(five_etdata)
five_result <- sort(five_etdata2, decreasing = T)

# 막대 그래프
head(five_result, 20)
barplot(head(five_result, 10), main='5점 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수')

five_word <- as.data.frame(five_result)
wordcloud2(five_word)



### 3. 1점 리뷰 대상 ###
comment_onedata <- data %>% filter(score == 1) %>% select(comment, score)
comment_onedata <- comment_onedata$comment
(onecomment_len <- length(comment_onedata)) # 1점 리뷰 갯수 확인

one_etdata <- extractNoun(comment_onedata)
one_etdata <- unlist(one_etdata)
one_etdata <- gsub('[[:punct:]]', "", one_etdata)
one_etdata <- Filter(function(x) {nchar(x) >= 2}, one_etdata)

one_etdata2 <- table(one_etdata)
one_result <- sort(one_etdata2, decreasing = T)

# 막대 그래프
head(one_result, 20)
barplot(head(one_result, 10),  main='1점 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수' )

one_word <- as.data.frame(one_result)
wordcloud2(one_word)
