library(dplyr)
source('avocado_cleanup.R')


#zawężenie dancyh do regionu 'Albany' i typu 'conventional'
av_alb_conv <- avocado %>% subset(region == 'Albany') %>% subset(type == 'conventional')
av_alb_conv$year <- as.factor(av_alb_conv$year)
ggplot(av_alb_conv, aes(x=Total.Volume, y=AveragePrice)) +
  geom_point(color = av_alb_conv$year) + theme_bw()


#klasteryzacja algorytmem k średnich
av_reduced <- data.frame(av_alb_conv$Total.Volume, av_alb_conv$AveragePrice, av_alb_conv$year)
names(av_reduced) <- c('Total.Volume', 'AveragePrice', 'year')

#klasteryzacje wykonujemy dla wszystkich wartości w kolumnach 1 oraz 2, podział na 4 klastry (dane z 4 lat)
clustered <- kmeans(av_reduced[, 1:2], 4)
av_reduced$cluster <- as.factor(clustered$cluster)
#klasteryzacja nie przebiega dobrze
ggplot(av_reduced, aes(x = av_alb_conv$Total.Volume, y = av_alb_conv$AveragePrice, color = cluster, shape=av_alb_conv$year)) + geom_point()



#zawężenie dancyh do regionu 'California'
av_cal <- avocado %>% subset(region == 'California')
av_cal$type <- as.factor(av_cal$type)
ggplot(av_cal, aes(x=Total.Volume, y=AveragePrice)) +
  geom_point(shape = av_cal$type) + theme_bw()


#klasteryzacja algorytmem k średnich
av_cal_reduced <- data.frame(av_cal$Total.Volume, av_cal$AveragePrice, av_cal$type)
names(av_cal_reduced) <- c('Total.Volume', 'AveragePrice', 'type')

#klasteryzacje wykonujemy dla wszystkich wartości w kolumnach 1 oraz 2, podział na 2 klastry (2 typy)
clustered_cal <- kmeans(av_cal_reduced[, 1:2], 2)
av_cal_reduced$cluster <- as.factor(clustered_cal$cluster)
#klasteryzacja przebiega pomyślnie
ggplot(av_cal_reduced, aes(x = av_cal$Total.Volume, y = av_cal$AveragePrice, color = cluster, shape=av_cal$type)) + geom_point()


# klasteryzacja hierarchiczna
av_cal_dist <- dist(av_cal_reduced)
h_clusers <- hclust(av_cal_dist)
plot(h_clusers)

#ograniczamy ilość klastrów do 2
h_cl_reduced <- cutree(h_clusers, 2)
#większe kropki pokazują rzeczywiste dane, mniejsze dane po klastrowaniu. Widać, że klastrowanie nie przebiega pomyślnie.
ggplot(av_cal_reduced, aes(x = av_cal$Total.Volume, y = av_cal$AveragePrice, color=av_cal$type)) + 
  geom_point(size = 7) + geom_point(color = h_cl_reduced)