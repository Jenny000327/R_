# 전처리 코드
setwd("c:/data/raw")

library(tidyverse)
dir()

data <- read.csv("점포정보_2021_상권별_분기별.csv",fileEncoding = "euc-kr") %>% as_tibble()
sub <- read.csv("상권영역_2021.csv") %>% as_tibble()
dong <- read.csv("행정동코드_매핑정보_20200325.csv") %>% as_tibble()
parking <- read.csv("주차장(동별)_2021.csv",skip = 3) %>% as_tibble()
dense <- read.csv("인구밀도_2021.csv",skip = 1) %>% as_tibble()
move <- read.csv("인구이동_2021.csv",skip = 0) %>% as_tibble()


data_rename <- data %>% 
  rename(YEAR_CD = 기준_년_코드,
         QURTER_CD = 기준_분기_코드,
         CMAR_DV_CD = 상권_구분_코드,
         CMAL_DV_NM = 상권_구분_코드_명,
         CMAR_CD = 상권_코드,
         CMAR_NM = 상권_코드_명,
         SV_CD = 서비스_업종_코드,
         SV_NM = 서비스_업종_코드_명,
         MK_NUM = 점포_수,
         SMK_NUM = 유사_업종_점포_수,
         OP_RATE = 개업_율,
         OP_MK_NUM = 개업_점포_수,
         CLS_RATE = 폐업_률,
         CLS_MK_NUM = 폐업_점포_수,
         FRC_MK_NUM =프랜차이즈_점포_수) 

sub_rename <- sub %>% 
  rename(YM_CD = 기준_년월_코드,
         CMAR_DV_CD = 상권_구분_코드,
         CMAL_DV_NM = 상권_구분_코드_명,
         CMAR_CD = 상권_코드,
         CMAR_NM = 상권_코드_명,
         CMAR_X = 엑스좌표_값,
         CMAR_Y = 와이좌표_값,
         SIGUNGU_CD = 시군구_코드,
         DONG_CD = 행정동_코드,
         TYPE_INFO = 형태정보) 

# 행정동 전처리
dong_rename <- dong %>% 
  rename(H_SDNG_CD = 통계청행정동코드,
         DONG_CD = 행자부행정동코드,
         SD_NM = 시도명,
         SIGUNGU_NM = 시군구명,
         DONG_NM = 행정동명)


# 인구밀도 전처리
dense_rename <- dense %>% 
  select(SIGUNGU_NM = 동별.2.,
         DONG_NM = 동별.3.,
         DT = 인구밀도..명...) %>% 
  mutate(DONG_NM = ifelse(DONG_NM %in% c("상일1동","상일2동"),"상일동",DONG_NM)) %>% 
  group_by(SIGUNGU_NM,DONG_NM) %>% summarise(DT = sum(DT)) %>% ungroup()

# 인구이동 전처리
move_rename <- move %>% 
  select(SIGUNGU_NM = 동별.2.,
         DONG_NM = 동별.3.,
         IN = X2021,
         OUT = X2021.1) %>% 
  filter(!(SIGUNGU_NM %in% c("동별(2)","소계"))) %>% 
  mutate(DONG_NM = ifelse(DONG_NM %in% c("상일1동","상일2동"),"상일동",DONG_NM)) %>% 
  mutate(IN = as.integer(IN),
         OUT = as.integer(OUT)) %>% 
  group_by(SIGUNGU_NM,DONG_NM) %>% summarise(IN = sum(IN),
                                             OUT = sum(OUT)) %>% ungroup()

# 주차 전처리
parking_rename <- parking %>% 
  select(SIGUNGU_NM = 동별.2.,
         DONG_NM = 동별.3.,
         PARKING = 개소..개소.) %>% 
  filter(DONG_NM !="소계")

# JOIN ----
data_joined <- data_rename %>%
  left_join(sub_rename, by = c("CMAR_DV_CD","CMAL_DV_NM","CMAR_CD","CMAR_NM")) %>% 
  left_join(dong_rename, by = c("DONG_CD"))

data_joined %>% distinct()

# 동별 데이터 생성: 식음료 대상 필터 추가 ----
target <- c("한식음식점","커피-음료","분식전문점","호프-간이주점",
            "양식음식점","일식음식점","중식음식점","패스트푸드점",
            "치킨전문점")

data_dong <- data_joined %>%
  filter(SV_NM %in% target) %>% 
  group_by(YEAR_CD,YM_CD,SD_NM,SIGUNGU_NM,DONG_CD,DONG_NM) %>%
  summarise(MK_NUM = sum(MK_NUM),
            SMK_NUM = sum(SMK_NUM),
            OP_MK_NUM = sum(OP_MK_NUM),
            CLS_MK_NUM = sum(CLS_MK_NUM),
            FRC_MK_NUM  = sum(FRC_MK_NUM)) %>%
  mutate(OP_RATE = OP_MK_NUM/MK_NUM,
         CLS_RATE = CLS_MK_NUM/MK_NUM) %>% ungroup()

# 추가 데이터 조인
data_dong <- data_dong %>% 
  left_join(dense_rename, by = c("SIGUNGU_NM","DONG_NM")) %>% 
  left_join(move_rename, by = c("SIGUNGU_NM","DONG_NM")) %>% 
  left_join(parking_rename, by = c("SIGUNGU_NM","DONG_NM")) 

data_dong %>% is.na() %>% apply(2,sum)
data_dong %>% filter(is.na(DT))

# # 동/종별 데이터 생성
# data_sv <- data_joined %>%
#   group_by(YEAR_CD,YM_CD,SD_NM,SIGUNGU_NM,DONG_CD,DONG_NM,SV_CD,SV_NM) %>%
#   summarise(MK_NUM = sum(MK_NUM),
#             SMK_NUM = sum(SMK_NUM),
#             OP_MK_NUM = sum(OP_MK_NUM),
#             CLS_MK_NUM = sum(CLS_MK_NUM),
#             FRC_MK_NUM  = sum(FRC_MK_NUM)) %>%
#   mutate(OP_RATE = OP_MK_NUM/MK_NUM,
#          CLS_RATE = CLS_MK_NUM/MK_NUM) %>% ungroup()

write.csv(data_dong,"data_final.csv")

