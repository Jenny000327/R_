library(ggplot2)
library(data.table)
library(dplyr)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(IRdisplay)

## 전국 행정동 지도 그리기 (map outline data)
h <- rgdal::readOGR('C:/data/preprocessed/map/emd_mapdata.shp') #shp파일 불러오기
h <- spTransform(h, CRS('+proj=longlat')) #좌표계 설정
h_map <- fortify(h) #데이터프레임으로 변경
head(h_map)
write.csv(h_map,"C:/data/preprocessed/map/emd_mapdata.csv") #h_map: 전국 위경도값 있음

## 전국 행정동 지도그리기
ggplot() + geom_polygon(data = h_map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black')
h_map$id <- as.numeric(h_map$id) # id의 class가 character라서 numeric으로 변환
hmap <- h_map[h_map$id <= 423 | h_map$id == 3478,] # id를 QGIS로 확인한 결과 0~423, 3478번이 서울시에 해당

# 서울시 행정동 지도 그리기
ggplot() + geom_polygon(data = hmap, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black')

#행정동 코드, 위도경도 좌표 불러오기
map_data <- read.csv('C:/data/preprocessed/data_dong_mapping.csv', header=TRUE, fileEncoding = "CP949", encoding = "UTF-8")
map_data_1 <- fortify(map_data, region = 'DONG_CD')
View(map_data_1)

new_merged_data <- inner_join(map_data_1, hmap, by = c("lat" = "lat", "long" = "long"))
View(new_merged_data)
#시각화할 데이터 불러오기
hmap <- read.csv('C:/data/preprocessed/map/emd_mapdata.csv')
hmap_cluster <- read.csv('C:/data/preprocessed/cluster.csv')
#View(hmap_cluster)
cluster1 <- hmap_cluster %>%
  filter(cluster == 1) %>%
  dplyr::select(cluster, DONG_CD)
View(cluster1)
cluster2 <- hmap_cluster %>%
  filter(cluster == 2) %>%
  dplyr::select(cluster, DONG_CD)
View(cluster2)
cluster3 <- hmap_cluster %>%
  filter(cluster == 3) %>%
  dplyr::select(cluster, DONG_CD)
View(cluster3)
cluster4 <- hmap_cluster %>%
  filter(cluster == 4) %>%
  dplyr::select(cluster, DONG_CD)
View(cluster4)

hmap_traindata <- read.csv('C:/data/preprocessed/train_data.csv')
head(hmap_traindata)
traindt <- hmap_traindata %>%
  filter(...==...)%>%
  dplyr::select(...)

hmap_testdata <- read.csv('C:/data/preprocessed/test_data.csv')
head(hmap_testdata)
testdt <- hmap_testdata %>%
  filter(...==...)%>%
  dplyr::select(...)

#시각화할 데이터와 맵데이터 병합하기
merge_result <- merge(map_data_1, cluster1, by ="DONG_CD")
head(merge_result)
merge_result2 <- merge(map_data_1, cluster2, by ="DONG_CD")
head(merge_result2)
merge_result3 <- merge(map_data_1, cluster3, by ="DONG_CD")
head(merge_result3)
merge_result4 <- merge(map_data_1, cluster4, by ="DONG_CD")
head(merge_result4)
merge_result5 <- merge(map_data_1, traindt, by ="DONG_CD")
head(merge_result5)
merge_result6 <- merge(map_data_1, testdt, by ="DONG_CD")
head(merge_result6)

### 클러스터링 시각화
ggplot() + geom_polygon(data = merge_result, aes(x = long, y = lat, fill = cluster),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
ggplot() + geom_polygon(data = merge_result2, aes(x = long, y = lat,  fill = cluster),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
ggplot() + geom_polygon(data = merge_result3, aes(x = long, y = lat,  fill = cluster),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
ggplot() + geom_polygon(data = merge_result4, aes(x = long, y = lat,  fill = cluster),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
### train 데이터 시각화
ggplot() + geom_polygon(data = merge_result5, aes(x = long, y = lat, group = group, fill = ...),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('train data')
### test데이터 시각화
ggplot() + geom_polygon(data = merge_result6, aes(x = long, y = lat, group = group, fill = ...),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('test data')
