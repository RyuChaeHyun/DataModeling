kdramalist.df <- read.csv("project//kdramalist.csv")
View(kdramalist.df) 


#end.airing(종영일)에 존재하는 결측치 제거
kdramalist.df$end.airing <- ifelse(kdramalist.df$end.airing == "N/A", NA, (ifelse(kdramalist.df$end.airing == "?", NA, kdramalist.df$end.airing)))
kdramalist.df <- kdramalist.df[!(is.na(kdramalist.df$end.airing)), ]
table(is.na(kdramalist.df$end.airing))

kdramalist.df$scored.by <- ifelse(kdramalist.df$scored.by == "N/A", NA, kdramalist.df$scored.by)
kdramalist.df <- kdramalist.df[!(is.na(kdramalist.df$scored.by)), ]

sum(kdramalist.df$scored.by == "N/A")

#Popularity 제거
kdramalist.df.nonPopularity <- kdramalist.df[,-13]
View(kdramalist.df.nonPopularity)

#imdb 제거 
sum(kdramalist.df$imdb_description == "N/A")
sum(kdramalist.df$imdb_rating == "N/A")
sum(kdramalist.df$imdb_user_count == "N/A")

kdramalist.df.nonPopularity <- kdramalist.df.nonPopularity[,-c(17,18,19)]


install.packages('tidyverse')
library(tidyverse)

kdramalist.df.nonPopularity$Genres <- gsub("\\[", "", kdramalist.df.nonPopularity$Genres)
kdramalist.df.nonPopularity$Genres <- gsub("\\]", "", kdramalist.df.nonPopularity$Genres)
kdramalist.df.nonPopularity$Genres <- gsub("'", "", kdramalist.df.nonPopularity$Genres)
kdramalist.df.nonPopularity$Genres <- gsub("\"", "", kdramalist.df.nonPopularity$Genres)


test <- data.frame(name=c(kdramalist.df.nonPopularity[,1]), Genres=c(kdramalist.df.nonPopularity[,2])) 
test1 <- separate_rows(test, Genres, sep = ", ")
table(test1$Genres)


kdramalist.df.nonPopularity$Tags <- gsub("\\[", "", kdramalist.df.nonPopularity$Tags)
kdramalist.df.nonPopularity$Tags <- gsub("\\]", "", kdramalist.df.nonPopularity$Tags)
kdramalist.df.nonPopularity$Tags <- gsub("'", "", kdramalist.df.nonPopularity$Tags)
kdramalist.df.nonPopularity$Tags <- gsub("\"", "", kdramalist.df.nonPopularity$Tags)

test <- data.frame(name=c(kdramalist.df.nonPopularity[,1]), Tags=c(kdramalist.df.nonPopularity$Tags)) 
test1 <- separate_rows(test, Tags, sep = ", ")
View(table(test1$Tags))



