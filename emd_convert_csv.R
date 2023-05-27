library(ggplot2)
library(dplyr)
library(rgdal)
library(sp)
#완성

from_crs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m")
to_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

shp_emd_grs <- rgdal::readOGR('C:/data/preprocessed/EMD_202101/TL_SCCO_EMD.shp')
summary(shp_emd_grs)
#좌표계 변환
shp_emd <- spTransform(shp_emd_grs, to_crs)
summary(shp_emd)

slotNames(shp_emd)

#shp 속성 확인
shp_emd@data
slotNames(shp_emd@data)
#작업할 데이터 생성
data_emd <- shp_emd@data; str(data_emd)

#기준이 되는  동명 데이터 생성
data_result <- shp_emd@data
str(data_result)
head(data_result)

#shp파일을 data.frame으로 변경( 경도, 위도 값 있음)
shp_result <- fortify(shp_emd)
str(shp_result)
head(shp_result)

#위에서 만든 data_result에서 동명 추출 및 기준id 생성 
Name <- cbind(id=row.names(data_result), emdname=as.character(data_result$EMD_ENG_NM))
str(Name);head(Name)
#list를 data.frame으로 변경 후, id를 factor->char로 변경 
x <- as.data.frame(Name)
x$id <- as.character(x$id)
str(x); head(x)

#마무리
result <- left_join(shp_result, x)
str(result)
head(result)
#csv로 저장
write.csv(result, file="C:/data/preprocessed/EMD_202101/TL_SCCO_EMD_convert.csv", row.names = F)