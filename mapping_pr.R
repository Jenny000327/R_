library(ggplot2)
library(data.table)
library(dplyr)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(IRdisplay)

## 행정동 지도 그리기 (map outline data)
h <- rgdal::readOGR('C:/data/preprocessed/map/emd_mapdata.shp')
h <- spTransform(h, CRS('+proj=longlat'))
h_map <- fortify(h)
head(h_map)
write.csv(h_map,"C:/data/preprocessed/map/emd_mapdata.csv")#h_map:위경도값 있음

ggplot() + geom_polygon(data = h_map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black') ## 전국 읍동리 지도그리기
h_map$id <- as.numeric(h_map$id) # id의 class가 character라서 numeric으로 변환
hmap <- h_map[h_map$id <= 423 | h_map$id == 3478,] # id를 QGIS로 확인한 결과 0~423, 3478번이 서울시에 해당

ggplot() + geom_polygon(data = hmap, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black') ## 서울 행정동 지도
ggplot() + geom_polygon(data = hmap, aes(x=long, y=lat, group=group, fill = A))

# 매핑용 행정동 코드 불러오기
list_dong <- read.csv( 'C:/data/preprocessed/data_dong.csv', header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
#list_dong$DONG_CD <- sapply(list_dong$DONG_CD, as.character )
head(list_dong)
cd_data <- df[, c(6,7)]
cd_data$DONG_CD <- as.integer(cd_data$DONG_CD)
head(cd_data)
str(cd_data)

#행정동별_위경도_좌표 불러오기
map_data <- read.csv('C:/data/preprocessed/행정동별_위경도_좌표.csv', header=TRUE, fileEncoding = "CP949", encoding = "UTF-8")
head(map_data)

#행정동 코드랑 위도경도 좌표 합산
data_join <- left_join(map_data, list_dong, by = c('DONG_NM' = 'DONG_NM'))
str(data_join)

#시각화할 데이터 불러오기
hmap <- read.csv('C:/data/preprocessed/emd_mapdata.csv') %>% as_tibble() %>%
hmap_cluster <- read.csv('C:/data/preprocessed/cluster.csv') 
head(hmap_cluster)
hmap_testdata <- read.csv('C:/data/preprocessed/test_data.csv')
head(hmap_testdata)

#시각화할 데이터와 data_join 병합하기

### 클러스터링 시각화
ggplot() + geom_polygon(data = hmap_cluster, aes(x = long, y = lat, group = group, fill = ...),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
##테스트데이터 시각화
ggplot() + geom_polygon(data = hmap_testdata, aes(x = long, y = lat, group = group, fill = ...),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('test data')


