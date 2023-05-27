#install.packages('ggplot2')
#install.packages('ggmap')
#install.packages('grid')
#install.packages('rgeos')
#install.packages('maptools')
#install.packages('rgdal')
#install.packages('raster')

library(ggmap)
library(ggplot2)
library(grid)
library(raster)
library(rgeos)
library(MASS)
library(maptools)
library(dplyr)
library(rgdal)
library(sp)

#emp 코드 맵핑에 필요한 파일 불러오기
data1 <- read.csv("C:/data/preprocessed/data_final.csv")

#필요시 시도별로 subset
data <- subset(data1, SD_NM=="서울")


#읍면동 집값 데이터 표시에 필요한 shape file 불러오기
korea <- shapefile('C:/data/preprocessed/EMD_202101/TL_SCCO_EMD.shp')
korea <- st_read("C:/data/preprocessed/EMD_202101/TL_SCCO_EMD.shp")
korea
korea$EMD_KOR_NM <- iconv(korea$EMD_KOR_NM,
                          from= 'CP949',
                          to='UTF-8',
                          sub=NA,
                          mark=TRUE,
                          toRaw = FALSE)

#시군 구별
data1 <- subset(data1, SD_NM=="서울")

#Shape File ID 통합 (읍면동 맵)
korea <- fortify(korea, region='DONG_CD')
korea1 <- merge(korea, data1, by='id', sort=F)
korea1 <- korea1[order(korea1$order),]
centroid <- aggregate(cbind(long,lat) ~ emd, data=korea1, FUN=mean)
centroid_seoul  <- subset(centroid, emd=="양재동")


# 읍면동 집값 표시
dev.new()
p <-ggplot() + geom_polygon(data=korea1, aes(x=long, y=lat, group=group, fill=price))+
  geom_text(data = centroid, mapping = aes(x=long, y=lat, label=emd, fontface="bold"), family="NanumSquare Bold", size=5)



p + scale_fill_gradient("주택 가격",low='#55dd62', high='#fd482e') +
  theme_bw()+theme(panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())


#최종 위치 (집값 데이터 삭제 - 하얀색 '시'별 지도 바탕에 선택된 '동'만 초록색으로 표시)

p <-ggplot() + geom_polygon(data=korea1, aes(x=long, y=lat, group=group, fill=final))+
  geom_path(data=korea1, aes(x=long, y=lat, group=group), color="light grey")


p + scale_fill_gradient("거주시설 인구수",low='#fafefc', high='#3cb062') +
  theme_bw()+theme(panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   legend.position="none")

