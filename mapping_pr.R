library(ggplot2)
#install.packages('data.table')
library(data.table)
library(dplyr)
library(ggmap)
library(rgeos)
library(maptools)
library(rgdal)
#install.packages('IRdisplay')
library(IRdisplay)

# 매핑용 행정동 코드 데이터 로드
list_dong <- read.csv( 'C:/data/preprocessed/data_dong.csv', header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
head(list_dong)

list_dong$DONG_CD <- sapply(list_dong$DONG_CD, as.character )
head(list_dong)

## 행정동 지도 그리기 (map outline data)
h <- rgdal::readOGR('C:/data/preprocessed/emd_mapdata.shp')
h <- spTransform(h, CRS('+proj=longlat'))
h_map <- fortify(h)
head(h_map)
write.csv(h_map,"C:/data/preprocessed/emd_mapdata.csv")

ggplot() + geom_polygon(data = h_map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black') ## 전국 읍동리 지도 
h_map$id <- as.numeric(h_map$id) # id의 class가 character라서 numeric으로 변환
hmap <- h_map[h_map$id <= 423 | h_map$id == 3478,] # id를 QGIS로 확인한 결과 0~423, 3478번이 서울시에 해당

ggplot() + geom_polygon(data = hmap, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black') ## 서울 행정동 지도

## map fill data
## hmap_final은 hmap 파일에 행정동을 기준으로 행정동별 노인 인구의 수, 지하철 이용자의 수, 유동인구의 합계를 각각의 열로 병합한것
hmap <- read.csv('C:/data/preprocessed/emd_mapdata.csv') %>% as_tibble() %>%

hmap_cluster <- read.csv('C:/data/preprocessed/cluster.csv') 
head(hmap_cluster)
hmap_testdata <- read.csv('C:/data/preprocessed/test_data.csv')
head(hmap_testdata)

#행정구역별_위경도_좌표
hmap
hmap_cluster %>% colnames()

### 클러스터링 시각화
ggplot() + geom_polygon(data = hmap_cluster, aes(x = long, y = lat, group = group, fill = old),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('클러스터링')
##테스트데이터 시각화
ggplot() + geom_polygon(data = hmap_testdata, aes(x = long, y = lat, group = group, fill = pop_sum),
                        color = 'gray') + scale_fill_gradient(low = "#FFCCFF", high = "#6600CC", 
                                                              space = "Lab", guide = "colourbar") + ggtitle('test data')
##

