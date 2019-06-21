library(dplyr)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

source("avocado_cleanup.R")

#pca dla całego zbioru (po usunięciu wartości nienumerycznych)
pca.out = prcomp(avocado_num, scale=TRUE)
summary(pca.out)
ggbiplot(pca.out, scale = 0)
screeplot(pca.out, type = 'lines')

#zawężenie dancyh do regionu 'Albany' i typu 'conventional'
av_alb_conv <- avocado %>% subset(region == 'Albany') %>% subset(type == 'conventional')
av_alb_conv_num <- av_alb_conv %>% select(-one_of(drop.cols))

#pca dla zawężonego zbioru
pca_alb_conv = prcomp(av_alb_conv_num, scale=TRUE)
summary(pca_alb_conv)
ggbiplot(pca_alb_conv, scale = 0)
screeplot(pca_alb_conv, type = 'lines')


#zawężenie zbioru do typu 'organic'
av_org <- avocado %>% subset(type == 'organic')
av_org_num <- av_org %>% select(-one_of(drop.cols))

#pca dla powyższego zbioru
pca_org = prcomp(av_org_num, scale=TRUE)
summary(pca_org)
ggbiplot(pca_org, scale = 0)
screeplot(pca_org, type = 'lines')
