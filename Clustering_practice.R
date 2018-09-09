Online.Retail <- read.csv("OnlineRetail.csv", stringsAsFactors=FALSE)
order_wise <- na.omit(Online.Retail)
Amount <- order_wise$Quantity * order_wise$UnitPrice
order_wise <- cbind(order_wise,Amount)
order_wise <- order_wise[order(order_wise$CustomerID),]
monetary <- aggregate(Amount~CustomerID, order_wise, sum)
frequency <- order_wise[,c(7,1)]
temp<-table(as.factor(frequency$CustomerID))
temp<-data.frame(temp)
colnames(temp)[1]<-c("CustomerID")
RFM <-merge(monetary,temp,by="CustomerID")

recency <- order_wise[,c(7,5)]
recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%m/%d/%y %H:%M")
maximum<-max(recency$InvoiceDate)
maximum<-maximum+1

maximum$diff <-maximum-recency$InvoiceDate


recency$diff<-maximum$diff
recency<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")
colnames(recency)[1]<- "CustomerID"
colnames(recency)[2]<- "Recency"
RFM <- merge(RFM, recency, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)
box <- boxplot.stats(RFM$Amount)
out <- box$out
RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Freq)
out <- box$out
RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Recency)
out <- box$out
RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1

View(RFM)
summary(RFM)

RFM_Norm <- RFM[,-1]
RFM_Norm$Amount <- scale(RFM_Norm$Amount)
RFM_Norm$Recency <- scale(RFM_Norm$Recency)
RFM_Norm$Freq <- scale(RFM_Norm$Freq)

summary(RFM_Norm)


cricket <- read.csv("Cricket.csv", stringsAsFactors=FALSE)
View(cricket)
c_sample <- cricket[,c(8,10)]
View(c_sample)
c_sample$SR <- scale(c_sample$SR)
c_sample$Ave <- scale(c_sample$Ave)

clus3 <- kmeans(c_sample, centers = 2)
clus3

clus4 <- kmeans(c_sample, centers = 3)
clus4

clus5 <- kmeans(c_sample, centers = 4)
clus5

clus6 <- kmeans(c_sample, centers = 5)
clus6$cluster



clus5 <- kmeans(c_sample, centers = 4)
clus5
c_sample$group4 <- clus5$cluster
c_sample
cricket

name <- c();
for (i in 1:nrow(cricket)){
  name[i] <- cricket$Player[i]
}
c_sample$name <- name

library(dplyr)
km_clusters<- group_by(c_sample, group4)
km_clusters

c_sample$name[]


hc = hclust(cricket, method = ???complete???)
plot(hc,labels=cricket$Player)
rect.hclust(hc, k=4, border="red")

library(ggplot2)
hc <- hclust(dist(cricket), method = "complete")
cricket$cluster <- cutree(hc, k = 4) 
ggplot(cricket, aes(x = SR, y = Ave, 
                    colour = as.factor(cricket$cluster), 
                    label = Player)) + geom_point() + geom_text(size = 3)
