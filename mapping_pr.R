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
ggplot() + geom_polygon(data = hmap, aes(x=long, y=lat, group=group, fill = A))

# 매핑용 행정동 코드 불러오기
#list_dong <- read.csv( 'C:/data/preprocessed/data_dong.csv', header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
#list_dong$DONG_CD <- sapply(list_dong$DONG_CD, as.character )
#head(list_dong)
#cd_data <- df[, c(6,7)]
#cd_data$DONG_CD <- as.integer(cd_data$DONG_CD)
#head(cd_data)
#str(cd_data)


#행정동 코드, 위도경도 좌표 불러오기
map_data <- read.csv('C:/data/preprocessed/data_dong_mapping.csv', header=TRUE, fileEncoding = "CP949", encoding = "UTF-8")
map_data_1 <- fortify(map_data)
head(map_data_1)

#시각화할 데이터 불러오기
hmap <- read.csv('C:/data/preprocessed/map/emd_mapdata.csv')
hmap_cluster <- read.csv('C:/data/preprocessed/cluster.csv')
cluster1 <- hmap_cluster %>%
filter(cluster == 1) %>%
dplyr::select(cluster, all_of(1))
head(cluster)

hmap_ttdata <- read.csv('C:/data/preprocessed/total_data.csv')
head(hmap_ttdata)

#시각화할 데이터와 data_join 병합하기
merge_result <- merge(map_data_1, hmap_cluster, by ="DONG_CD")
head(merge_result)
merge_result2 <- merge(hmap, hmap_ttdata, by =' ')

### 클러스터링 시각화
ggplot() + geom_polygon(data = merge_result, aes(x = long, y = lat,  fill = A))
#,
#                       color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
#                                                            space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
##테스트데이터 시각화
ggplot() + geom_polygon(data = merge_result2, aes(x = long, y = lat, group = group, fill = ...),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('test data')


