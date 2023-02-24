install.packages('readxl')
install.packages('tidyverse')
install.packages('janitor')
install.packages('plotly')
install.packages('plotrix')
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(plotly)
library(plotrix)
library(purrr)

df_customerData <- read_xlsx('Worksheet in Lab Assessment.xlsx')

df_customerData |>
  clean_names() -> df_customerData #no null value in data
is.null(df_customerData)
dim(df_customerData)
str(df_customerData)
names(df_customerData)  
head(df_customerData) 
summary(df_customerData$age) 
sd(df_customerData$age)
ggplot(df_customerData,aes(age))+
           geom_histogram(binwidth = 4,col = '#000000',fill= '#0099F8')+
           stat_bin(binwidth = 4,geom = 'text',color = '#000000', 
           aes(label = ..count..,vjust = -1.1))+
           labs(x= "Age Groups", y = "Frequency",
                title = "Age Frequency Distribution Histogram")+
           theme(axis.text.x = element_text(colour = 'black'),
                 axis.text.y = element_text(colour = 'black'),
                 axis.title = element_text(color ='#000000',
                                            family = "Century Gothic", face = 'plain'),
                 title = element_text(face = "bold.italic",family = "Century Gothic"))

ggplotly(ggplot(df_customerData,aes(gender,age,fill = gender))+
           geom_boxplot()+
  labs(x= "Age Groups", y = "Age",
       title = "Age Frequency Distribution Histogram")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.title = element_text(color ='#000000',
                                  family = "Century Gothic", face = 'plain'),
        title = element_text(face = "plain",family = "Century Gothic"))+
  scale_fill_discrete(name = "Gender"))


ggplotly(ggplot(df_customerData,aes(age,label = ..count..,fill = gender))+
  geom_histogram(binwidth = 4,color = 'white')+
  stat_bin(binwidth = 4,geom = 'text',
           aes(label = ..count..,vjust = 1.5))+
  labs(x= "Age Groups", y = "Frequency",
       title = "Age Frequency Distribution Histogram")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.title = element_text(color ='#000000',
                                  family = "Century Gothic", face = 'plain'),
        title = element_text(face = "plain",family = "Century Gothic"))+
        scale_fill_discrete(name = "Gender"))

a <- table(df_customerData$gender)
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")


ggplot(df_customerData,aes(gender,fill = gender))+
  geom_bar()+
  labs(x= "Gender", y = "Counts",
       title = "No. of Males and Females")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.title = element_text(color ='#000000',
                                  family = "Century Gothic", face = 'plain'),
        title = element_text(face = "plain",family = "Century Gothic"))+
  scale_fill_discrete(name = "Gender")


summary(df_customerData$annual_income_k)
sd(df_customerData$annual_income_k)  
#Histogram for Annual Income
hist(df_customerData$annual_income_k,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


ggplot(df_customerData,aes(annual_income_k,fill = gender))+
geom_density(col = 'black',alpha = 0.2)+
  facet_wrap(~gender)+xlim(0,150)


summary(df_customerData$spending_score_1_100)

ggplot(df_customerData,aes(spending_score_1_100))+
         geom_histogram(binwidth = 10,col = '#000000',fill= '#0099F8')+
  stat_bin(binwidth = 10,geom = 'text',color = '#000000', 
           aes(label = ..count..,vjust = -0.5))+
  labs(x= "Spending Scores", y = "Frequency",
       title = "Spending Scores Distribution Histogram")+theme_bw()


set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(df_customerData[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

k4 <- kmeans(df_customerData[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
df_customerData$clusters <- as.character(k4$cluster)

set.seed(1)
ggplotly(ggplot(df_customerData, aes(x =annual_income_k, y = spending_score_1_100)) + 
  geom_point(stat = "identity", aes(color = as.factor(k4$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")+
  labs(x = 'Annual Income Thousands ($)', y ='Spending score '))
  
pcclust=prcomp(df_customerData[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]



kCols = function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k4$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))



