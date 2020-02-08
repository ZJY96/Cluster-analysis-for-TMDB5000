#dataProcessing.R
#Data cleaning first step, produce movie1.csv
#04/25/2019
#Mengxiao Li, Jingyi Zhang, Yimei Yang

rm(list = ls())
#read file
setwd("~/Desktop/Project")
Data1 = read.csv(file = "./Raw Data/tmdb_5000_credits.csv")
Data2 = read.csv(file = "./Raw Data/tmdb_5000_movies.csv")

#merging data
colnames(Data1)[which(names(Data1) == "movie_id")] = "id"
data = merge(Data1, Data2, by="id")

#delete useless columns
library(dplyr)
data = select(data, -homepage,-overview,-tagline,-status,-title.x, -original_title)
colnames(data)[which(names(data) == "title.y")] = "title"
sum(is.na(data$runtime))

#change the NA in runtime column to the average length of a film
data$runtime[is.na(data$runtime)] <- mean(data$runtime, na.rm = TRUE)
sum(is.na(data$runtime))

#select numeric columns for clustering
library(mice)
set.seed(1000)
data_cluster = complete(mice(data[,c(4,8,12,13,16,17)]))

#scale the clustering data
data_cluster = scale(data_cluster)

#check if any more NA left
sum(is.na(data))

#output data
setwd("~/Desktop/Project/Processed Data")
write.csv(data, 'movieData.csv')
write.csv(data_cluster, 'clusteringData.csv')

#read data again
movieData=read.csv('movieData.csv',stringsAsFactors = F)

library(jsonlite)
library(dplyr)
library(tidyverse)
#clean and parse json string in columns:genres,cast,crew,production_companies
genres=movieData %>%
  filter(nchar(genres)>2) %>%
  mutate(
    js=lapply(genres,fromJSON)
  ) %>%
  unnest(js) %>%
  select(id,title,keyword=name)
head(genres)
#generate order number for genres
genres3 <- genres
genres3$order <- 0
genres3$order[1] <- 1
for(i in 1:(nrow(genres3)-1)) {
  if(genres3$id[i+1]!=genres3$id[i]){
    genres3$order[i+1] <- 1
  } else {genres3$order[i+1] <- (genres3$order[i])+1}
}
#form dataframe of first 3 genres
genres3 <- genres3 %>% filter(order < 4) %>%
  spread(key=order, value=keyword) %>%
  rename(genre_1="1", genre_2="2", genre_3="3")
head(genres3)

#seperate cast string
cast <- movieData %>%
  filter(nchar(cast)>2) %>%
  mutate(js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  select(id, title,keywords=name,order) 
glimpse(cast)
cast <- cast %>% filter(order %in% c(0, 1, 2)) 
cast$order[1] <- 0
for (i in 1:(nrow(cast)-1)){
  if(cast$id[i+1]!=cast$id[i]){
    cast$order[i+1] <- 0
  } else {cast$order[i+1] <- cast$order[i]+1}
}
#form cast dataframe of first 3 actors
cast <- cast %>% filter(order %in% c(0, 1, 2)) %>%
  spread(key=order, value=keywords) %>%
  rename(actor_1="0", actor_2="1", actor_3="2")
head(cast)

#seperate crew string
crew <- movieData %>%
  filter(nchar(crew)>2) %>%
  mutate(js = lapply(crew, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, keywords=name,job)
head(crew)
#form director dataframe
movies1Director <- crew %>% filter(job=="Director") %>% count(id) %>% filter(n==1)
director= crew %>%
  filter(job=="Director" & id %in%
           movies1Director$id)
head(director)

#adjust production companies strings into dataframe:
production_companies=movieData %>%
  filter(nchar(production_companies)>2) %>%
  mutate(
    js=lapply(production_companies,fromJSON)
  ) %>%
  unnest(js) %>%
  select(id,title,keyword=name)
head(production_companies)

production_companies1 <-production_companies
production_companies1$order <- 0
production_companies1$order[1] <- 1

for(i in 1:(nrow(production_companies1)-1)) {
  if(production_companies1$id[i+1]!=production_companies1$id[i]){
    production_companies1$order[i+1] <- 1
  } else {production_companies1$order[i+1] <- (production_companies1$order[i])+1}
}
#form production companies dataframe
production_companies1 <- production_companies1 %>% filter(order < 3) %>%
  spread(key=order, value=keyword) %>%
  rename(production_companies_1="1", production_companies_2="2")
head(production_companies1)

#select columns in movieData to form movie
movie=movieData[,c('id','title','budget','popularity','revenue','runtime','vote_count','vote_average','release_date')]
#join all the selected string variables into movie1
movie1=inner_join(movie,genres3,by=c('id','title'))
movie1=inner_join(movie1,cast,by=c('id','title'))
movie1=inner_join(movie1,production_companies1,by=c('id','title'))
movie1=inner_join(movie1,director,by=c('id','title'))
#rename and delete useless columns
names(movie1)[names(movie1)=="keywords"] <- "director"
movie1= subset(movie1, select =-c(job) )
str(movie1)
write.csv(movie1, "movie1.csv")
