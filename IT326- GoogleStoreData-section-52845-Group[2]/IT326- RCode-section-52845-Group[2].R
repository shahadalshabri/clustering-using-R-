install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("scales")
install.packages("ggthemes")
install.packages("tidyr")
install.packages("AppliedPredictiveModeling")
install.packages("outliers")
install.packages("factoextra")
install.packages("cluster")
install.packages("ggfortify")
install.packages("fpc")
install.packages("NbClust")
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(ggthemes)
library(tidyr)
library(NbClust)
library(cluster)
library(fpc)
library(outliers)
library(factoextra)
#--------- read -------
help(read.csv)
?read.csv

googleplaystore <- read.csv("C:/Users/mrymh/OneDrive/Desktop/Data Mining Final submission/IT326- Dataset-section-52845-Group[2]/googleplaystore.csv")



#------------ Rating vs.price scatter plot-------

{r echo= FALSE, message=FALSE, warning=FALSE, Bivariate_Plots}
#plot of rating vs. Price
ggplot(aes(x = Rating , y = Price), data = df)+
  geom_jitter(alpha = 0.3, color = 'royalblue1')+
  ylim(0,25)+
  geom_line(stat = 'summary', fun.y = mean)+  ggtitle('Rating vs. Price')


#--------- Plot for top genres vs. category  --------------
ggplot(aes (x = Genres), data = topgenres)+
  geom_bar(aes(fill = Category))+
  coord_flip()+
  ggtitle('Top genres and Category')

#--------- Pie chart for Type ---------------

{r echo= FALSE,message=FALSE, warning=FALSE}
#There are two types paid and unpaid
df_type = subset(df, (Type == 'Free' | Type == 'Paid'))
temp <- df_type%>%
  group_by(Type)%>%
  summarise(n = n())
#pie chart
ggplot(aes(x = '', y = n, fill = Type), data = temp )+
  geom_bar(stat = 'identity')+
  coord_polar('y', start = 0)+
  theme_void()+
  ggtitle('Type')



#--------------preprocessing-------------------------

# ---------Replace missing values--------- 

googleplaystore$Rating[is.nan(googleplaystore$Rating)]<-0.0

#---------------Remove outliers -----------

outrev = outlier(googleplaystore$Rating, logical = TRUE)
sum(outrev)
Find_outlier = which(outrev ==TRUE, arr.ind = TRUE)
googleplaystore= googleplaystore[-Find_outlier,]
#--------------------
outGen = outlier(googleplaystore$Genres, logical = TRUE)
sum(outGen)
Find_outlier = which(outGen ==TRUE, arr.ind = TRUE)
googleplaystore= googleplaystore[-Find_outlier,]
#------------------ 
outCon = outlier(googleplaystore$Content.Rating, logical = TRUE)
sum(outCon)
Find_outlier = which(outCon ==TRUE, arr.ind = TRUE)
googleplaystore= googleplaystore[-Find_outlier,]
#--------------------
outcat = outlier(googleplaystore$Category, logical = TRUE)
sum(outcat)
Find_outlier = which(outcat ==TRUE, arr.ind = TRUE)
googleplaystore= googleplaystore[-Find_outlier,]
#-
outrevi = outlier(googleplaystore$Reviews, logical = TRUE)
sum(outrevi)
Find_outlier = which(outrevi ==TRUE, arr.ind = TRUE)
googleplaystore= googleplaystore[-Find_outlier,]

#----- ENCODING CATEGORICAL DATA------

googleData$Type<- sapply(googleData$Type,as.numeric)


##---------------------Data mining task #---------------------- 
#-------- convert to numeric --------------------------- 


googleplaystore$Category <- sapply(googleplaystore$Category,as.numeric)
googleplaystore$Reviews <- sapply(googleplaystore$Reviews,as.numeric)
googleplaystore$Size <- sapply(googleplaystore$Size,as.numeric)
googleplaystore$Installs <- sapply(googleplaystore$Installs,as.numeric)
googleplaystore$Type <- sapply(googleplaystore$Type,as.numeric)
googleplaystore$Price <- sapply(googleplaystore$Price,as.numeric)
googleplaystore$Content.Rating <- sapply(googleplaystore$Content.Rating,as.numeric)
googleplaystore$Genres <- sapply(googleplaystore$Genres,as.numeric)
googleplaystore$Last.Updated <- sapply(googleplaystore$Last.Updated,as.numeric)
googleplaystore$Current.Ver<- sapply(googleplaystore$Current.Ver,as.numeric)
googleplaystore$Android.Ver<- sapply(googleplaystore$Android.Ver,as.numeric)
googleplaystore$App<- sapply(googleplaystore$App,as.numeric)


#------------------
kmeans2.result <- kmeans(googleplaystore,2)
kmeans2.result 


## visualize clustering k=2
fviz_cluster(kmeans2.result, data = googleplaystore)
set.seed(8953)
kmeans3.result <- kmeans(googleplaystore, 3)
kmeans3.result 


## visualize clustering k=3
fviz_cluster(kmeans3.result, data = googleplaystore)
kmeans4.result <- kmeans(googleplaystore, 4)
kmeans4.result


## visualize clustering k=4
fviz_cluster(kmeans4.result, data = googleplaystore)
kmeans5.result <- kmeans(googleplaystore, 5)
kmeans5.result
## visualize clustering k=5
fviz_cluster(kmeans5.result, data = googleplaystore)
##--------------------------
# group into 4 clusters
pam1.result <- pam(googleplaystore, 2)
plot(pam1.result)


fviz_cluster(pam1.result, data = googleplaystore)


pam2.result <- pam(googleplaystore, 3)
fviz_cluster(pam2.result, data = googleplaystore)

plot(pam2.result)
pam3.result <- pam(googleplaystore, 4)
fviz_cluster(pam3.result, data = googleplaystore)

plot(pam3.result)
pam4.result <- pam(googleplaystore, 5)
fviz_cluster(pam4.result, data = googleplaystore)

plot(pam4.result)

#---Evaluation
##--------------------------
# group into 4 clusters
pam.result <- pam(googleplaystore, 2)
plot(pam.result)
pam.result <- pam(googleplaystore, 3)
plot(pam.result)
pam.result <- pam(googleplaystore, 4)
plot(pam.result)
pam.result <- pam(googleplaystore, 5)
plot(pam.result)
##for all clusters

fviz_nbclust(googleplaystore, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette method")

#extra heririchal clustering

install.packages("pvcluster")

library(pvclust)

dd <- dist(scale(googleplaystore), method = "euclidean")
hc <- hclust(dd, method = "ward.D")
plot(hc)


set.seed(1234)
result <- pvclust(googleplaystore[1:100, 1:10], method.dist="euclidean", 
                  method.hclust="average", nboot=10)
plot(result)
pvrect(result)

dend <- googleplaystore[1:30,-5] %>% scale %>% dist %>% 
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
# plot the dend in usual "base" plotting engine:
plot(dend)

