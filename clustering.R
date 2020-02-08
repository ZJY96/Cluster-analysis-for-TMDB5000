movie1=read.csv('~/Desktop/movie1.csv',stringsAsFactors = F)
movie1 = select(movie1,-X)
#cluster:
#select columns from movie1
head(movie1)
data_cluster=movie1[,3:8]
head(data_cluster)

#scale
data_cluster=scale(data_cluster)
#define similarity measure
set.seed(1000)
d = dist(x = data_cluster,method = 'euclidean') 
clusters = hclust(d = d,method='ward.D2')
plot(clusters)
#goodness of fit
cor(cophenetic(clusters),d)
#output: 0.5807843
#It is interpreted similar to a correlation coefficient. 
#CPC> 0.7 indicates relatively strong fit, 0.3<CPC<0.7 indicates moderate fit.
plot(cut(as.dendrogram(clusters),h=5)$upper)
plot(clusters)
rect.hclust(tree=clusters,k = 3,border='tomato')
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 3,groupLabels = F))
#decide range segments for revenue
plot(movie1$revenue)
hist(movie1$revenue)
#remove revenue=0
movie1<- movie1[!movie1$revenue == 0,]
summary(movie1)

#split into 4
library(sjmisc)
library(tidyr)
library(tibble)
table=split_var(movie1$revenue,n=4)
#1:5-16650000
#2:16650001-53300000
#3:53300001-141500000
#4:141500001-2788000000
#combine movie1 with segments of revenue
movie1=cbind(movie1,table)
colnames(movie1)[colnames(movie1)=="table"] <- "revenue_range"
#label the range  
movie1$revenue_range=factor(movie1$revenue_range,labels = c('5-16650000'
                                                            ,'16650001-53300000'
                                                            ,'53300001-141500000'
                                                            ,'141500001-2788000000'))  
#cluster again
data_cluster=movie1[,c(3,4,6:8)]
#scale:
data_cluster=scale(data_cluster)
head(data_cluster)
#Define Similarity Measure
d = dist(x = data_cluster,method = 'euclidean') 
#clustering method
#hierarchical clustering
clusters = hclust(d = d,method='ward.D2')
#goodness,. It is interpreted similar to a correlation coefficient. CPC> 0.7 indicates relatively strong fit, 0.3<CPC<0.7 indicates moderate fit.
cor(cophenetic(clusters),d)
plot(clusters)
rect.hclust(tree=clusters,k = 3,border='tomato')
plot(clusters)
rect.hclust(tree=clusters,k = 4,border='tomato')
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))

h_segments = cutree(tree = clusters,k=5)
table(h_segments)

library(psych)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(data_cluster,
         h_segments,
         color=T,shade=T,labels=4,lines=0,main='Hierarchical Cluster Plot')

#k-means cluster, cluster into 5 segments
set.seed(1000)
km = kmeans(x = data_cluster,centers = 5,iter.max=10000,nstart=25)
table(km$cluster)
k_segments = km$cluster
movie2=cbind(movie1,k_segments)
#check variables' mean in each segments
library(dplyr)
movie2 %>%
  select(c(budget:vote_average),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()
#compare vote_average in each segments
library(dplyr); library(ggplot2); library(tidyr)
movie2 %>%
  select(vote_average,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,vote_average)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
#compare revenue and budget in each segments
movie2 %>%
  select(c(budget,revenue),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,c(budget,revenue))%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
#compare vote_count in each segments
movie2 %>%
  select(vote_count,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,vote_count)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
#compare popularity and runtime in each segments
movie2 %>%
  select(c(popularity,runtime),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,c(popularity,runtime))%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
#profile clusters
prop.table(table(movie2$k_segments,movie2[,10]),1)
library(ggplot2)
tab = prop.table(table(movie2$k_segments,movie2[,10]),1)
#fill in Foreign
Foreign<-0
tab=cbind(tab[,1:9],Foreign,tab[,10:18])
tab=as.table(tab)
tab1 = data.frame(round(tab,2))
library(RColorBrewer)
#How do these segments differ in the things they value?
ggplot(data=tab1,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

tab = prop.table(table(movie2$k_segments,movie2[,11]),1)
tab2 = data.frame(round(tab,2))
library(RColorBrewer)
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

tab = prop.table(table(movie2$k_segments,movie2[,12]),1)
tab3 = data.frame(round(tab,2))
tab4=inner_join(tab1,tab2,by=c('Var1','Var2'))
tab4=inner_join(tab4,tab3,by=c('Var1','Var2'))
head(tab4)
tab4$combine<-tab4$Freq.x+tab4$Freq.y+tab4$Freq
tab4=tab4[,c(1,2,6)]

ggplot(data=tab4,aes(x=Var2,y=Var1,fill=combine))+
  geom_tile()+
  geom_text(aes(label=combine),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
#compare revenue
prop.table(table(movie2$k_segments,movie2[,19]),1)
library(ggplot2)
tab = prop.table(table(movie2$k_segments,movie2[,19]),1)
tab1 = data.frame(round(tab,2))

library(RColorBrewer)
ggplot(data=tab1,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

write.csv(movie1,'movie1_2.csv')
write.csv(movie2,'movie2.csv')
