
library(KoNLP)

# 데이터 불러오기
data <- read.csv('data/kakao.csv')
commentdata <- data$comment

# 명사 추출
etdata <- extractNoun(commentdata)
etdata <- unlist(etdata)
etdata <- gsub('[[:punct:]]', "", etdata)
etdata <- Filter(function(x) {nchar(x) >= 2}, etdata)

etdata2 <- table(etdata)
result <- sort(etdata2, decreasing = T)



# 워드클라우드
library(wordcloud)
library(wordcloud2)

word <- as.data.frame(result)
wordcloud2(word)
