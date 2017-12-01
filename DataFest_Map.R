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

######################Maps##########################
library(leaflet,quietly=TRUE)
library(maps,quietly=TRUE)
library(treemap)

newdat<-usa_booked %>% select(user_location_latitude,user_location_longitude,user_location_region,user_location_city,is_mobile,is_package,srch_adults_cnt,srch_children_cnt,prop_is_branded,prop_starrating,diff_time,stay_time,international)
newdat[, 1:length(newdat)][newdat[, 1:length(newdat)] == 'NULL'] <- NA
newdat$user_location_longitude<-as.numeric(newdat$user_location_longitude)
newdat$user_location_latitude<-as.numeric(newdat$user_location_latitude)
set.seed(1)
c<-kmeans(newdat[,c("diff_time","srch_adults_cnt","stay_time")],3,nstart=20)
newdat$cluster<-c$cluster
newdat$cluster<-as.factor(newdat$cluster)

newdat %>% group_by(user_location_region,user_location_city) %>% summarise(number = n()) %>% arrange(desc(number))

new<-newdat %>% group_by(user_location_region,user_location_city) %>% summarise(number = n()) %>% arrange(desc(number))
treemap(new,index = c('user_location_region','user_location_city'), vSize = 'number')

M_map<-leaflet() %>% 
  addTiles() %>% 
  setView(lng = -118.24368, 
          lat = 34.0522, zoom = 4) %>%  
  addMarkers(lat=newdat$user_location_latitude,lng=newdat$user_location_longitude,
             clusterOptions = markerClusterOptions(),popup=newdat$cluster) 

M_map

##Can we show an map in LA and New York with indications of clusters?
library(ggmap)
NEWYORKMap <- ggmap(get_map(location = "New York", zoom = 10, maptype = "terrain"))
NEWYORKMap + geom_point(aes(x = user_location_longitude, y = user_location_latitude, color = cluster), data = newdat)

LAMap <- ggmap(get_map(location = "Los Angeles", zoom = 10, maptype = "terrain"))
LAMap + geom_point(aes(x = user_location_longitude, y = user_location_latitude, color = cluster), data = newdat)

#what is the distribution of starrating?

#Distribution of Ratings
ratings_and_users <- newdat %>% group_by(prop_starrating) %>% count()

ggplot(data=ratings_and_users, aes(x=cut(prop_starrating,c(0,0.9,1.9,2.9,3.9,5)), y=n,fill=cut(prop_starrating, c(0,0.9,1.9,2.9,3.9,5)))) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "YlOrRd") + labs(title = "Distribution of Ratings by Users", y = "Count of Users", x = "Star Category", fill = "Star Category") + theme(legend.position="none") +
  scale_x_discrete(labels=c("1.5","2.5","3.5","4.5"))

#The distribution of ratings is considerably skewed, there are a lot more 3 and 4 star ratings than 1. we could say that people seem to review things they like. In general, people seem to be more likely to write a review for a positive experience than a negative one.

#where is the state with the maximum mean of starrating?
df_states <- newdat %>% group_by(user_location_region) %>% summarize(avg_stars = mean(prop_starrating), count=n()) %>% arrange(desc(count))
ggplot(data=df_states, aes(user_location_region, avg_stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=1.5, color="white") +labs(y="Average Star", x="State")

#can we provide customized hotel location for each clusters?

#(1) where are all when cluster =1 (with cheap hotels and do not like packages) located?

c1<-newdat%>%group_by(user_location_region)%>%filter(cluster == "1")%>% select(1:14)
c2<-newdat%>%group_by(user_location_region)%>%filter(cluster == "2")%>% select(1:14)
c3<-newdat%>%group_by(user_location_region)%>%filter(cluster == "3")%>% select(1:14)

c1<-c1[c1$is_package==0 & c1$prop_is_branded==0, ]
c2<-c2[c2$is_package==1 & c2$prop_is_branded==0,]
c3<-c3[c3$is_package==0 & c3$prop_is_branded==1,]

t1<-c1%>%select(user_location_region,user_location_city,prop_starrating,is_package,prop_is_branded)%>%group_by(user_location_region,user_location_city)%>%summarise(rate=mean(prop_starrating),package=mean(is_package),brand=mean(prop_is_branded)) %>% arrange(desc(rate))
p1<-head(t1)


#(2)where are all when cluster =2 (like package, do not like brand hotels, like star hotel)located?
t2<-c2%>%select(user_location_region,user_location_city,prop_starrating,is_package,prop_is_branded)%>%group_by(user_location_region,user_location_city)%>%summarise(rate=mean(prop_starrating),package=mean(is_package),brand=mean(prop_is_branded)) %>% arrange(desc(rate))
p2<-head(t2)

#(3)where are all when cluster =3 (brand hotels, and do not stay in high star hotels) located?
t3<-c3%>%select(user_location_region,user_location_city,prop_starrating,is_package,prop_is_branded)%>%group_by(user_location_region,user_location_city)%>%summarise(rate=mean(prop_starrating),package=mean(is_package),brand=mean(prop_is_branded)) %>% arrange(desc(rate))
p3<-head(t3)
##


library(pander)
pander(p1,style='grid',justify= 'center')
pander(p2,style='grid',justify= 'center')
pander(p3,style='grid',justify= 'center')