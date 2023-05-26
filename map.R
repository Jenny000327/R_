library(ggplot2)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal) # 얘가 안깔림

P <- read.csv("/Users/gayeongkim/Desktop/data/prepared/data_final.csv", header = TRUE) #시각화할 데이터셋
map <- shapefile("/Users/gayeongkim/Desktop/data/LSMD_ADM_SECT_UMD_11.shp") #지리 정보 데이터셋

map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

P_merge <- merge(seoul_map, P, by='id')

ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')
ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))

# 맥 사용자라면 theme_set(theme_bw(base_family='NanumGothic')) 를 먼저 수행하고, 대신 + theme_bw() 코드를 제거하고 수행하세요.

plot <- ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))
plot + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar")
+ theme_bw() + labs(title = "서울시 A 분포")
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 맥 사용자라면 theme_set(theme_bw(base_family='NanumGothic')) 를 먼저 수행하고, 대신 + theme_bw() 코드를 제거하고 수행하세요.

plot <- ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = B))
plot + scale_fill_gradient(low = "#ffffe5", high = "#ffb825", space = "Lab", guide = "colourbar")
+ theme_bw() + labs(title = "서울시 B 분포")
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))



library(tidyverse)
library(sf)
library(mapproj)
library(rgdal)

# 2. sf 패키지 사용: .shp 데이터 불러오기, 파일 변환
#map_korea <- st_read('/Users/gayeongkim/Desktop/emd.shp')
map_korea <- readOGR(dsn = '/Users/gayeongkim/Desktop/emd.shp')
plot(map_korea)

map_korea$CTPRVN_CD <- iconv(map_korea$CTPRVN_CD,
                             from='CP949',
                             to='UTF-8', 
                             sub=NA,
                             mark=TRUE,
                             toRaw=FALSE)

map_korea_shp <-  as(map_korea, 'Spatial')
map_korea_df <- fortify(map_korea_shp)
str(map_korea_df) # df 객체구조
head(map_korea_df) # id를 읍면동구분코드로 바꿔서 돌려보기

# 3. ggplot()+geom_polygon()을 사용한 대한민국 시·도 지도 그리기
map_korea_ggplot <- map_korea_df %>% 
  ggplot(aes(x=long, y=lat, group = group))

# 흰바탕
map_korea_ggplot+
  geom_polygon(fill='white', color='black')+
  coord_quickmap()


# 시·도별로 다른 색상으로 구분한 대한민국 지도
library(nord)
map_korea_ggplot+
  geom_polygon(aes(fill=id), color='black', show.legend=FALSE)+
  scale_fill_nord('lumina')+
  coord_quickmap()+
  theme_minimal()