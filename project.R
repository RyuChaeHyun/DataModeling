rm(list=ls())

#install.packages("gplots")
library(ggplot2)
library(caret)
library(dplyr)
library(lubridate)
library(gplots)
library(wordcloud)
library(tm)
library(SnowballC)
library(e1071)
library(tidyverse)

kdramalist.df <- read.csv("kdramalist.csv")
View(kdramalist.df)

kdramalist.df$end.airing <- ifelse(kdramalist.df$end.airing == "N/A", NA, 
                                   (ifelse(kdramalist.df$end.airing == "?", 
                                           NA, kdramalist.df$end.airing)))
kdramalist.df <- kdramalist.df[!(is.na(kdramalist.df$end.airing)), ]

kdramalist.df$scored.by <- ifelse(kdramalist.df$scored.by == "N/A", NA, 
                                  kdramalist.df$scored.by)
kdramalist.df <- kdramalist.df[!(is.na(kdramalist.df$scored.by)), ]

kdramalist.df$Genres <- ifelse(kdramalist.df$Genres == "N/A", NA, 
                               kdramalist.df$Genres)
kdramalist.df <- kdramalist.df[!(is.na(kdramalist.df$Genres)), ]

#duration na 처리
kdramalist.df$Duration <- ifelse(kdramalist.df$Duration == "N/A", NA, kdramalist.df$Duration)

View(kdramalist)
#Popularity 제거
kdramalist.df.nonPopularity <- kdramalist.df[,-13]
View(kdramalist.df.nonPopularity)

kdramalist <- kdramalist.df.nonPopularity
kdramalist <- kdramalist[,-c(17, 18, 19)]

#scored.by 50명 이하가 평가한 경우 레코드 삭제


kdramalist$scored.by <- as.numeric(kdramalist$scored.by)
table(kdramalist$scored.by < 50)
kdramalist$scored.by <- ifelse(kdramalist$scored.by<51, NA, kdramalist$scored.by)
table(is.na(kdramalist$scored.by))
kdramalist <- kdramalist[!(is.na(kdramalist$scored.by < 51)), ]

#watchers

kdramalist$Watchers <- gsub(",", "", kdramalist$Watchers)


kdramalist$Genres <- gsub("\\[", "", kdramalist$Genres)
kdramalist$Genres <- gsub("\\]", "", kdramalist$Genres)
kdramalist$Genres <- gsub("\"", "", kdramalist$Genres)
kdramalist$Genres <- gsub("'", "", kdramalist$Genres)


test <- data.frame(name=c(kdramalist[,1]), Genres=c(kdramalist[,2])) 
test1 <- separate_rows(test, Genres, sep = ", ")
table(test1$Genres)


kdramalist$Tags <- gsub("\\[", "", kdramalist$Tags)
kdramalist$Tags <- gsub("\\]", "", kdramalist$Tags)
kdramalist$Tags <- gsub("\"", "", kdramalist$Tags)
kdramalist$Tags <- gsub("'", "", kdramalist$Tags)


test <- data.frame(name=c(kdramalist[,1]), Tags=c(kdramalist$Tags)) 
test1 <- separate_rows(test, Tags, sep = ", ")
View(table(test1$Tags))

#확인용 
sum(kdramalist$Tags== "N/A")
sum(kdramalist$Duration== "N/A")
kdramalist$Duration== "N/A"
sum(kdramalist$Genres== "N/A")
sum(is.na(kdramalist$Duration))
View(kdramalist)
str(kdramalist$end.airing)


kdramalist$start.airing <- as.Date(kdramalist$start.airing, format = "%d-%b-%y")
kdramalist$end.airing <- as.Date(kdramalist$end.airing, format = "%d-%b-%y")


kdramalist[,9] <- as.numeric(kdramalist[,9])
kdramalist[,10] <- as.numeric(kdramalist[,10])
kdramalist[,11] <- as.numeric(kdramalist[,11])
kdramalist[,12] <- as.numeric(kdramalist[,12])
kdramalist[,14] <- as.numeric(kdramalist[,14])
summary(kdramalist)

kdramalist$Duration[is.na(kdramalist$Duration)] <- median(kdramalist$Duration, na.rm = TRUE)

pcs <- prcomp(data.frame(kdramalist$Episodes,kdramalist[,9:12],kdramalist$Watchers),scale. = T)
summary(pcs)
pcs


par(mfcol = c(2,3))
boxplot(kdramalist$Episodes, xlab="Episodes")
boxplot(kdramalist$Duration, xlab="Duration")
boxplot(kdramalist$score, xlab="score")
boxplot(kdramalist$scored.by, xlab="scored.by")
boxplot(kdramalist$Ranked, xlab="Ranked")
boxplot(kdramalist$Watchers, xlab="Watchers")


#2010년과 비교하여 급증한 kdrama watchers 수
kdramalist %>% 
  mutate(years = year(start.airing)) %>% 
  group_by(years) %>% 
  summarise(sum_of_watchers = sum(Watchers)) %>% 
  ggplot(aes(x = years, y = sum_of_watchers)) +
  geom_col(fill = "cornflowerblue")

#typeof
typeof(kdramalist$drama_name)
typeof(kdramalist$Genres)
typeof(kdramalist$Tags)
typeof(kdramalist$Episodes)
typeof(kdramalist$start.airing)
typeof(kdramalist$end.airing)
typeof(kdramalist$Aired.On)
typeof(kdramalist$Original.Network)
typeof(kdramalist$Duration)
typeof(kdramalist$score)
typeof(kdramalist$scored.by)
typeof(kdramalist$Ranked)
typeof(kdramalist$Content.Rating)
typeof(kdramalist$Watchers)
typeof(kdramalist$actors)
typeof(kdramalist$platforms)

#summary
summary(kdramalist$Episodes)
summary(kdramalist$Duration)
summary(kdramalist$score)
summary(kdramalist$scored.by)
summary(kdramalist$Ranked)
summary(kdramalist$Watchers)



heatmap.2(cor(data.frame(kdramalist$Episodes,kdramalist[,9:12],kdramalist$Watchers)), Rowv = FALSE, Colv = FALSE, dendrogram =  "none",
          cellnote = round(cor(data.frame(kdramalist$Episodes,kdramalist[,9:12],kdramalist$Watchers)), 2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10, 10))



kdramalist.highlyRated <- kdramalist 
#평점이 8이상이면 higlyrated 1, 아니면 0
kdramalist.highlyRated$highlyRated <- ifelse(kdramalist.highlyRated$score > 8, 1, 0)


kdramalist.highlyRated$Genres

kdramalist.highlyRated$Genres <- gsub("supernatural", "mystery", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("fantasy", "mystery", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("life", "family", kdramalist.highlyRated$Genres)

kdramalist.highlyRated$Genres <- gsub("school", "youth", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("friendship", "youth", kdramalist.highlyRated$Genres)

kdramalist.highlyRated$Genres <- gsub("crime", "socialProblems", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("law", "socialProblems", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("political", "socialProblems", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("psychological", "socialProblems", kdramalist.highlyRated$Genres)

##
kdramalist.highlyRated$Genres <- gsub("horror", "thriller", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("sitcom", "comedy", kdramalist.highlyRated$Genres)

# 10개 이하 제거 
kdramalist.highlyRated$Genres <- gsub("drama", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("war", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("tokusatsu", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("mature", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("martial arts", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("adventure", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("romance", "", kdramalist.highlyRated$Genres)

kdramalist.highlyRated$Genres <- gsub("sci-fi", "etc", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("business", "etc", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("food", "etc", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("music", "etc", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("military", "etc", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub("sports", "etc", kdramalist.highlyRated$Genres)

kdramalist.highlyRated$Genres <- gsub(" ", "", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub(",,", ",", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres <- gsub(",", " ", kdramalist.highlyRated$Genres)
kdramalist.highlyRated$Genres
View(kdramalist.highlyRated)

tags.df <- data.frame(name=c(kdramalist.highlyRated[,1]), Tags=c(kdramalist.highlyRated$Tags), highlyRated=c(kdramalist.highlyRated$highlyRated)) 
#View(tags.df)
#sparateTags.df <- separate_rows(tags.df, Tags, sep = ", ")


#빈 행 제거
tags.df <- tags.df[!(tags.df$Tags == "" ), ]
#한 문자열로 치환
tags.df$Tags <- paste(tags.df$Tags)
#문자열에서 "," 제거
tags.df$Tags <- gsub(",", "", tags.df$Tags)
#View(tags.df)


#-----------------------문자열 데이터 정리-----------------------
Corpuss<- VCorpus(VectorSource(tags.df$Tags)) #문자열 데이터 정리 
Corpuss<- tm_map(Corpuss,removePunctuation) # 특수문자 제거 
Corpuss<-tm_map(Corpuss, removeNumbers) # 숫자 제거 
Corpuss<-tm_map(Corpuss, content_transformer(tolower)) #알파벳 소문자
typeof(Corpuss)

tdm <- TermDocumentMatrix(Corpuss) #단어를 변수로 만든다
inspect(tdm)

#tag 리스트 확인
rownames(tdm)

#tag 단어의 수 확인
tdm$nrow

#문서의 수(데이터)
tdm$ncol

#행렬로 변환
m2 <- as.matrix(tdm)

#단어의수 많은 거 부터 정렬
freq1 <- sort(rowSums(m2), decreasing = T)
head(freq1, 15)

#특정 횟수 이상 언급된 단어들만 출력
findFreqTerms(tdm, 100)

#워드클라우드

freq10 <- head(freq1, 10)
freq10

wordcloud(names(freq10),freq = freq10, scale = c(5,1),min.freq = 1)

#막대그래프
barplot(freq10,las=2,horiz=T)

#---------------------텍스트 마이닝 -----------------------

#태그 필터링 (상위 10개 태그)
ftag <- tags.df %>% 
  filter(grepl('lead', Tags) | grepl('male', Tags) | grepl('female', Tags)|grepl('relationship', Tags)
         | grepl('love', Tags)| grepl('strong', Tags)| grepl('smart', Tags)| grepl('rich', Tags)
         | grepl('nice', Tags)| grepl('first', Tags))

sparateTags.df <- separate_rows(tags.df, Tags, sep = ", ")
#View(ftag)

# tm_map함수는 텍스트 변환하는 함수
# learning -> learn으로 바뀌어서 나옴
Corpuss<-tm_map(Corpuss,stemDocument)
as.character(Corpuss[[1]])

# 특정 단어가 해당 문서에서 나오면 1, 아니면 0으로 표시 
kdrama_dtm<-DocumentTermMatrix(Corpuss)

inspect(kdrama_dtm[1:30,1:30])

# 150번 이상 나온 태그만 출력
kdrama_freq_words<-findFreqTerms(kdrama_dtm,150)
str(kdrama_freq_words)

kdrama_dtm_freq<-kdrama_dtm[,kdrama_freq_words]

dim(kdrama_dtm_freq)
dim(kdrama_dtm)

# 0과 1로 보이는 것을 문자열로 보이게 하는 과정 
convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  # 요소형을 정의할 때 labels옵션을 넣어주면 화면에서 보이는 
  # 문자열을 지정할 수 있음 
  x<-factor(x,levels=c(0,1),labels=c("0=low","1=high"))
}

#apply, 2 = 열별로 함수(convert_counts) 적용
kdrama_dtm_convert<-apply(kdrama_dtm_freq,2,convert_counts)

#-----------------------Naive bayes-----------------------------

set.seed(100)
#60%의 training data
trainIndex <- sample( row.names(ftag), 0.6*dim(ftag)[1] )
#20%의 valid data
tempIndex <- data.frame(setdiff( row.names(ftag), trainIndex ))
validIndex <- sample(c(1:nrow(tempIndex)), nrow(tempIndex)*0.5)
testIndex <- tempIndex[-validIndex,]

# naive bayes용 훈련,테스트 데이터 만들기
#highlyRated 기준
train_index <- createDataPartition(sparateTags.df$highlyRated,p=0.75,list=FALSE)
kdrama_dtm_train <- kdrama_dtm_convert[trainIndex,]
kdrama_raw_train <- sparateTags.df[trainIndex,] # 답이 담긴 데이터 
kdrama_dtm_valid <- kdrama_dtm_convert[validIndex,]
kdrama_raw_valid <- sparateTags.df[validIndex,] # 답이 담긴 데이터  
kdrama_dtm_test <- kdrama_dtm_convert[testIndex,]
kdrama_raw_test <- sparateTags.df[testIndex,] # 답이 담긴 데이터  

#m = naive bayes
m<-naiveBayes(kdrama_dtm_train, kdrama_raw_train$highlyRated)
#p = predict
validp<-predict(m, kdrama_dtm_valid)
testp<-predict(m,kdrama_dtm_test)


#교차 검증
confusionMatrix(validp,as.factor(kdrama_raw_valid$highlyRated))
confusionMatrix(testp,as.factor(kdrama_raw_test$highlyRated))
table(kdrama_raw_valid$highlyRated,validp)
table(kdrama_raw_test$highlyRated,testp)

#태그가 시청률에 큰 영향을 끼칠 것이라고 예상했지만 valid data로 했을 때 74%, test data로 했을 때 정확도가 75%였다.
#태그가 시청률이 높다, 낮다를 크게 좌우하지는 않지만 시청률에 어느정도 영향을 미친다는 사실을 파악했다.



#logistic regression
kdramalist.lr<-  kdramalist.highlyRated[,c(1, 2, 7, 17, 4, 9)]

kdramalist.lr$action <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'action'), 1, 0)
kdramalist.lr$comedy <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'comedy'), 1, 0)
kdramalist.lr$family <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'family'), 1, 0)
kdramalist.lr$historical <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'historical'), 1, 0)
kdramalist.lr$medical <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'medical'), 1, 0)
kdramalist.lr$mystery <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'mystery'), 1, 0)
kdramalist.lr$melo <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'melo'), 1, 0)
kdramalist.lr$socialProblems <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'socialProblems'), 1, 0)
kdramalist.lr$thriller <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'thriller'), 1, 0)
kdramalist.lr$youth <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'youth'), 1, 0)
kdramalist.lr$etc <- ifelse(str_detect(kdramalist.highlyRated$Genres, 'etc'), 1, 0)



kdramalist.lr$cycle <- ifelse(str_detect(kdramalist.lr$Aired.On, 'monday') & 
                                str_detect(kdramalist.lr$Aired.On, 'tuesday') & 
                                str_detect(kdramalist.lr$Aired.On, 'wednesday') & 
                                str_detect(kdramalist.lr$Aired.On, 'thursday') & 
                                str_detect(kdramalist.lr$Aired.On, 'friday'), 'daily', 
                              ifelse(str_detect(kdramalist.lr$Aired.On, 'friday') & 
                                       str_detect(kdramalist.lr$Aired.On, 'saturday'), 'fri-sat', 
                                     ifelse(str_detect(kdramalist.lr$Aired.On, 'friday'), 'fri-sat', 
                                            ifelse(str_detect(kdramalist.lr$Aired.On, 'saturday') & 
                                                     str_detect(kdramalist.lr$Aired.On, 'sunday'), 'weekend', 
                                                   ifelse(str_detect(kdramalist.lr$Aired.On, 'saturday'), 'weekend',
                                                          ifelse(str_detect(kdramalist.lr$Aired.On, 'sunday'), 'weekend',  
                                                                 ifelse(is.na(kdramalist.lr$Aired.On), 'irregular', 'week')))))))

kdramalist.lr$cycle <-factor(kdramalist.lr$cycle, levels = c('daily','fri-sat', 'weekend','irregular','week'))


View(kdramalist.lr)

#반응 변수 highlyRated ~ 예측 변수 Episodes duration 장르 요일
set.seed(10)

#train, valid data split
train.index <- sample(c(1:nrow(kdramalist.lr)), nrow(kdramalist.lr)*0.6)
train.df <- kdramalist.lr[train.index, ]
temp.df <- kdramalist.lr[-train.index, ]

valid.index <- sample(c(1:nrow(temp.df)), nrow(temp.df)*0.5)
valid.df <- temp.df[valid.index, ]
test.df <- temp.df[-valid.index, ]

#output of the logistic regression model 로지스틱 회귀모델
drama_score <- glm(formula = highlyRated ~ Episodes + Duration + cycle + action+ comedy+ family+ historical+ medical+mystery +melo+socialProblems+thriller + youth + etc,
                   data = train.df,
                   family = binomial()
)
options(scipen = 999)

summary(drama_score)

#prediction for the valid data 

#성능평가


options(scipen = 999)
drama_score.pred = predict(drama_score, newdata = valid.df, type = 'response')
confusionMatrix(as.factor(ifelse(drama_score.pred>0.5 , 1, 0)), as.factor(valid.df$highlyRated))
drama_score.pred = predict(drama_score, newdata = test.df, type = 'response')
confusionMatrix(as.factor(ifelse(drama_score.pred>0.5 , 1, 0)), as.factor(test.df$highlyRated))


#KNN

View(train.df)
library(class)
train.norm.df <- train.df 
valid.norm.df <- valid.df 
test.norm.df <- test.df
norm.values <- preProcess(train.df[,5:13], method=c("center", "scale"))
train.norm.df <- as.data.frame(predict(norm.values, train.df))
valid.norm.df <- as.data.frame(predict(norm.values, valid.df))
test.norm.df <- as.data.frame(predict(norm.values, test.df))
valid.accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))
test.accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))


#성능 평가
for(i in 1:20){
  knn.pred<-class::knn(train = train.norm.df[,5:13],                          
                       test = valid.norm.df[,5:13],                          
                       cl = train.df[,4], k = i)
  
  valid.accuracy.df[i,2] <- sum(knn.pred==valid.df[,4])/length(valid.df[,13])
}
valid.accuracy.df

for(i in 1:20){
  knn.pred<-class::knn(train = train.norm.df[,5:13],                          
                       test = test.norm.df[,5:13],                          
                       cl = train.df[,4], k = i)
  
  test.accuracy.df[i,2] <- sum(knn.pred==test.df[,4])/length(test.df[,13])
}

test.accuracy.df

