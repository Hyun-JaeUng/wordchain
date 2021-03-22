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
(all_count <- length(comment_alldata)) # 전체 리뷰 갯수 확인

# 평점 별 리뷰 갯수 확인
data %>% group_by(score) %>% count -> score_count  

etdata <- extractNoun(comment_alldata) # 명사 추출
etdata <- unlist(etdata) # 리스트 해제
etdata <- gsub('[[:punct:]]', "", etdata) # 특수문자 제거
etdata <- Filter(function(x) {nchar(x) >= 2}, etdata) # 2글자 이상 단어만 남겨놓기

etdata2 <- table(etdata) # 추출된 명사 별 갯수 구하기

result <- as.data.frame(sort(etdata2, decreasing = T)) # 가장 많이 나온 단어 확인
names(result) <- c('word', 'freq')
(word20_all <- head(result, 20)) # 많이 나온 단어 20개 확인 

# 그래프
all_graph <- sort(etdata2, decreasing = T)
barplot(head(all_graph, 10), main='전체 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수')
wordcloud2(result) # 워드클라우드 그리기



### 2. 5점 리뷰 대상 ###
comment_fivedata <- data %>% filter(score == 5) %>% select(comment, score) # 5점짜리 데이터 필터링
comment_fivedata <- comment_fivedata$comment # comment 열 추출
fivecomment_len <- length(comment_fivedata) # 5점 리뷰 갯수 확인

five_etdata <- extractNoun(comment_fivedata) 
five_etdata <- unlist(five_etdata)
five_etdata <- gsub('[[:punct:]]', "", five_etdata)
five_etdata <- Filter(function(x) {nchar(x) >= 2}, five_etdata)

five_etdata2 <- table(five_etdata)
five_result <- as.data.frame(sort(five_etdata2, decreasing = T))
names(five_result) <- c('five_word', 'five_freq')
(head(five_result, 20) -> word20_five)

# 그래프
five_graph <- sort(five_etdata2, decreasing = T)
barplot(head(five_graph, 10), main='5점 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수')
wordcloud2(five_result)



### 3. 1점 리뷰 대상 ###
comment_onedata <- data %>% filter(score == 1) %>% select(comment, score)
comment_onedata <- comment_onedata$comment
(onecomment_len <- length(comment_onedata)) # 1점 리뷰 갯수 확인

one_etdata <- extractNoun(comment_onedata)
one_etdata <- unlist(one_etdata)
one_etdata <- gsub('[[:punct:]]', "", one_etdata)
one_etdata <- Filter(function(x) {nchar(x) >= 2}, one_etdata)

one_etdata2 <- table(one_etdata)
one_result <- as.data.frame(sort(one_etdata2, decreasing = T))
names(one_result) <- c('one_word', 'one_freq')
(head(one_result, 20) -> word20_one)

# 그래프
one_graph <- sort(one_etdata2, decreasing = T)
barplot(head(one_graph, 10),  main='1점 리뷰 대상 추출된 명사 목록', xlab='명사', ylab='등장 횟수' )
wordcloud2(one_result)


############인사이트 도출 ################
score_count <- as.data.frame(score_count)

# 전체 리뷰 대비 5점 리뷰의 비율
fiveReviewRatio <- round(as.numeric(((score_count %>% filter(score==5) %>% select(n)) / all_count) * 100), 2)
oneReviewRatio <- round(as.numeric(((score_count %>% filter(score==1) %>% select(n)) / all_count) * 100), 2)

##### 전체 df에서 해당 단어의 빈도수를 찾아오기 #####

# 5점 리뷰 대상
all_freq <- c()
for (i in 1:20){
  i <- as.numeric(i)
  searchWord <- as.character(word20_five[i,1])    
  searchNum <- as.numeric(result %>% filter(word==searchWord) %>% select(freq))
  all_freq <- append(all_freq, searchNum)
}

fiveReviewWordList <- cbind(word20_five, all_freq)
fiveReviewWordList <- fiveReviewWordList %>% mutate(wordRatio = round((five_freq/all_freq*100), 2))
fiveReviewRatio_vt <- rep(fiveReviewRatio, 10)
fiveReviewWordList <- cbind(fiveReviewWordList, fiveReviewRatio_vt)
fiveReviewWordList <- fiveReviewWordList %>% mutate(wordIncreseRate = wordRatio - fiveReviewRatio_vt)

View(fiveReviewWordList)

# 1점 리뷰 대상
all_freq <- c()
searchWord <- NULL
searchNum <- NULL

for (i in 1:20){
  searchWord <- as.character(word20_one[i,1])
  searchNum <- as.numeric(result %>% filter(word==searchWord) %>% select(freq))
  all_freq <- append(all_freq, searchNum)
}

oneReviewWordList <- cbind(word20_one, all_freq)
oneReviewWordList <- oneReviewWordList %>% mutate(wordRatio = round((one_freq/all_freq*100), 2))
oneReviewRatio_vt <- rep(oneReviewRatio, 10)
oneReviewWordList <- cbind(oneReviewWordList, oneReviewRatio_vt)
oneReviewWordList <- oneReviewWordList %>% mutate(wordIncreseRate = wordRatio - oneReviewRatio_vt)

View(oneReviewWordList)
