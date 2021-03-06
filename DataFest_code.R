library(readr)
library(dplyr)
data <- read_delim("C:/Users/Alex/Desktop/DataFest/ASADataFest2017 Data/data.txt","\t", escape_double = FALSE)

#Data Cleaning
usa_booked<-data[which((data$is_booking==1) & (data$user_location_country == "UNITED STATES OF AMERICA")),]
usa_booked$search_date<-usa_booked$date_time
usa_booked$search_date<-gsub("[0-9]{2}[:][0-9]{2}[:][0-9]{2}","",usa_booked$search_date)
usa_booked$search_date<-as.Date(usa_booked$search_date)
usa_booked$diff_time<-usa_booked$srch_ci - usa_booked$search_date
usa_booked$stay_time<-usa_booked$srch_co - usa_booked$srch_ci
usa_booked$international <- 1
usa_booked[which(usa_booked$user_location_country == usa_booked$hotel_country),"international"] <- 0
usa_booked$diff_time<-as.numeric(usa_booked$diff_time)
usa_booked$stay_time<-as.numeric(usa_booked$stay_time)
usa_booked$popularity_band<-as.factor(usa_booked$popularity_band)
usa_booked$hist_price_band<-as.factor(usa_booked$hist_price_band)
usa_booked$distance_band<-as.factor(usa_booked$distance_band)
usa_booked<-usa_booked[which((usa_booked$diff_time>=0)&(usa_booked$stay_time>=0)),]
clean<-usa_booked %>% select(is_mobile,is_package,srch_adults_cnt,srch_children_cnt,prop_is_branded,prop_starrating,diff_time,stay_time,international)
clean<-clean[-which(clean$srch_adults_cnt==0 & clean$srch_children_cnt==0),]

summary(clean)
clean2<-as.data.frame(scale(clean))

############Decide Number of Clusters#############

wss <- (nrow(clean2)-1)*sum(apply(clean2,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(clean2, 
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##############PCA###############
pca <- prcomp(clean2,
              center = TRUE,
              scale. = TRUE)
print(pca)
summary(pca)
plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))


#############Clustering Analysis: Kmeans###################
c<-kmeans(clean[,c("diff_time","stay_time","srch_adults_cnt")],3,nstart=20)
c2<-kmeans(clean2[,c("diff_time","stay_time","srch_adults_cnt")],3,nstart=20)
c2
clean2$cluster<-c2$cluster
clean$cluster <- c$cluster


#############Interactive 3D plot############## 
library(plotly)
p <- plot_ly(clean2[1:10000,], x = ~diff_time, y = ~srch_adults_cnt, z = ~stay_time, 
             color = ~cluster, colors = c("lightcoral","lightskyblue","goldenrod1"),
             marker = list(size = 3)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'diff_time'),
                      yaxis = list(title = 'srch_adults_cnt'),
                      zaxis = list(title = 'stay_time')),autosize=F)

p


##########Developing recommendations#############

table1<-clean %>% group_by(cluster) %>% summarise(package=mean(is_package),mobile=mean(is_mobile),international=mean(international),staytime=median(stay_time),difftime=median(diff_time),brand=mean(prop_is_branded),adult=median(srch_adults_cnt))
table1

table2<-clean2 %>% group_by(cluster) %>% summarise(package=mean(is_package),mobile=mean(is_mobile),international=mean(international),staytime=median(stay_time),difftime=median(diff_time),brand=mean(prop_is_branded),adult=median(srch_adults_cnt),children=mean(srch_children_cnt), star=mean(prop_starrating))
table2

#cluster1=international cluster2=business cluster3=family
#cluster1 international: like package, travel internationally, stay the longest time, plan very ahead, do not like brand hotels, like star hotel 
#cluster2 business: use mobile to book, travel domestically, stay the shortest time, do not really plan the travel, stay in brand hotels, and do not stay in high star hotels
#cluster3 family: more adults and more children, stay in relatively cheap hotels and do not like packages