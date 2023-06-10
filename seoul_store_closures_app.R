# # 라이브러리 불러오기. Library Imports.
#install.packages("corrplot")
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(treemapify)
library(readr)
library(RColorBrewer)
library(tidyr)  # tidyr 패키지
library(gridExtra)
library(shinythemes)
library(corrplot)

# data_final_M 데이터.
data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")
test_data <- read_csv("C:/data/preprocessed/test_data.csv") %>% as_tibble()
train_data <- read_csv("C:/data/preprocessed/train_data.csv") %>% as_tibble()

eval_results_df <- read_csv("C:/data/preprocessed/eval_results_df.csv")
cluster_data <- read_csv("C:/data/preprocessed/cluster.csv")
cluster_centers <- read_csv("C:/data/preprocessed/cluster_centers.csv")
rf1_importance <- read_csv("C:/data/preprocessed/rf1_importance.csv")

# # data_final_M 데이터._가영
#setwd("C:/")
#setwd("/Users/gayeongkim/Desktop/")
# data_final_M <- read_csv("data/preprocessed/data_final.csv")
# test_data <- read_csv("data/preprocessed/test_data.csv") %>% as_tibble()
# train_data <- read_csv("data/preprocessed/train_data.csv") %>% as_tibble()
#
# eval_results_df <- read_csv("data/preprocessed/eval_results_df.csv")
# cluster_data <- read_csv("data/preprocessed/cluster.csv")
# cluster_centers <- read_csv("data/preprocessed/cluster_centers.csv")
# rf1_importance <- read_csv("data/preprocessed/rf1_importance.csv")
#str(eval_results_df)
#View(eval_results_df)

# 변수명을 한국어로 변경
rf1_importance <- mutate(rf1_importance,
                         ...1 = case_when(
                           ...1 == "MK_NUM" ~ "점포수",
                           ...1 == "SMK_NUM" ~ "유사업종점포수",
                           ...1 == "OP_MK_NUM" ~ "개업점포수",
                           ...1 == "FRC_MK_NUM" ~ "프랜차이즈점포수",
                           ...1 == "OP_RATE" ~ "개업률",
                           ...1 == "DT" ~ "인구밀도",
                           ...1 == "IN" ~ "유동인구유입량",
                           ...1 == "OUT" ~ "유동인구유출량",
                           ...1 == "PARKING" ~ "주차장수",
                           ...1 == "cluster" ~ "클러스터",
                           TRUE ~ ...1
                         )
)

# 컬럼명 변경
names(rf1_importance) <- c("Variable", "Importance")

str(rf1_importance)
rf1_importance

#사용자 Ui 부분
ui <- dashboardPage(
  
  dashboardHeader(title = "서울시 동별 폐업률"), #대시보드 해더
  
  
  #사이드 메뉴 부분
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome Page", tabName = "home", icon = icon("home")),
      menuItem("Data Insights", tabName = "EDA", icon = icon("bar-chart")), 
      menuItem("Status Map", tabName = "map", icon = icon("globe")),
      menuItem("Overview by District", tabName = "gu", icon = icon("chart-bar")), 
      menuItem("Neighborhood Details", tabName = "dong", icon = icon("chart-bar")),
      menuItem("Area Group Analysis", tabName = "clustering", icon = icon("sitemap")), #아이콘 이상하면 다른거 추천해죠 (https://fontawesome.com/v5/search)
      menuItem("Closure Rate Predictions", tabName = "predict", icon = icon("line-chart"))
    )
  ),
  
  #바디 부분
  dashboardBody(
    includeCSS("www/custom.css"),
    #includeCSS("Git_R/R_/www/custom.css"),
    
    tabItems(
      
      #Welcome Page 메뉴
      tabItem(tabName = "home",
              fluidRow(
                box(title = "서울시 행정 동별 폐업률 예측", class = "home-box",imageOutput("img1"),width = 11, height = 800),
                box(title = "사회적 가치", class = "home-box",imageOutput("img2"),width = 11, height = 800),
                box(title = "요식업 폐업률 분석", class = "home-box",imageOutput("img3"),width = 11, height = 800),
                box(title = "페르소나", class = "home-box",imageOutput("img4"),width = 11, height = 800),
                box(title = "앱 소개", class = "home-box",imageOutput("img5"),width = 11, height = 800),
                box(title = "요약", class = "home-box",imageOutput("img7"),width = 11, height = 800),
                box(title = "제작자 소개", class = "home-box",imageOutput("img6"),width = 11, height = 800)
                )
      ),
      
      
      #Data Insights 메뉴
      tabItem(tabName = "EDA",
              tabsetPanel(
                tabPanel("Rate Distribution", 
                         fluidRow(
                           box(title = "Open Rate Distribution", class = "custom-box", plotOutput("open_rate_distribution"), width = 6),
                           box(title = "Close Rate Distribution", class = "custom-box", plotOutput("closed_rate_distribution"), width = 6)
                         ),
                         fluidRow(
                           box(title = "Table", class = "custom-box", DTOutput("table_rates"), width = 12)
                         )),
                tabPanel("Correlation Analysis",
                         fluidRow(
                           box(title = "Correlation Scatterplot", class = "custom-box",style = "height: 500px;", selectInput("indicator_dong", "Choose an indicator", choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                                                                                                                                         "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                                                                                                                                         "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                                                                                                                                         "유동인구 유출량" = "OUT", "주차장 수" = "PARKING")), plotOutput("correlation_plot"),width = 6),
                           box(title = "Correlation Matrix", class = "custom-box", plotOutput("correlation_plot02"), style = "height: 500px;",width = 6)
                         ))
              )),
      
      

      # Status Map 메뉴
      tabItem(tabName = "map", 
              fluidRow(
                box(title = "Store Map", class = "custom-box",  #첫번째 박스에 지도 사진을 넣는데
                    tabsetPanel(id = "map_tab", # 그 안에 tabsetpanel을 넣어서 각각 눌러서 볼 수 있도록함.
                                tabPanel("Total", imageOutput("mk_img")),
                                tabPanel("Open", imageOutput("op_img")),
                                tabPanel("Closed", imageOutput("cls_img"))
                    ),
                    height = 800, #이걸로 오류 수정 완료.
                    width = 10
                ),
                box(title = "Table",class = "custom-box", DTOutput("table_map"), width = 10) # 첫번째 박스 아래에 두번째 박스에는 테이블을 넣었어.
              )
      ),
    
      
      #Overview by District 메뉴 [자치구 단위 시각화]
      tabItem(tabName = "gu",
              fluidRow(
                # Indicators box [이 박스 안에는 정보(요인들) SelectInput기능과  "구" 별 tree map이 들어있다.]
                box(title = "District Indicators",class = "custom-box", selectInput("indicator_gu", "원하시는 정보를 선택하세요.",
                                                                           choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                                       "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                                       "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                                       "유동인구 유출량" = "OUT", "주차장 수" = "PARKING"))
                    , plotOutput("gu_treemap"), width = 6),
                # GU 박스[구 selectInput기능과"구" 별 막대 그래프 가 들어있다.]
                box(title = "District Bar Chart",class = "custom-box", selectInput("gu", "원하시는 자치 구를 선택하세요.", choices = unique(data_final_M$SIGUNGU_NM)), plotOutput("bar_gu"), width = 6)),
              fluidRow(
                #구에 속한 동 태이블
                box(title = "District Table",class = "custom-box", DTOutput("table_gu"), width = 12))),
      
      
      
      #Neighborhood Details 메뉴 [행정동 단위 시각화]
      tabItem(tabName = "dong",
              fluidRow(
                box(title = "Neighborhood Indicators", class = "custom-box",  style = "height: 500px;", selectInput("indicator_dong02", "원하시는 정보를 선택하세요.", choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                                                                                                            "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                                                                                                            "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                                                                                                            "유동인구 유출량" = "OUT", "주차장 수" = "PARKING")), plotOutput("dong_treemap"),width = 6),
                box(title = "Neighborhood Bar Chart", class = "custom-box", style = "height: 500px;", plotOutput("bar_dong"), width = 6)),
              fluidRow(
                box(title = "Neighborhood Table", class = "custom-box", DTOutput("table_dong"), width = 12 ))),
      
      
      
      # Area Group Analysis 메뉴[클러스터링]
      tabItem(tabName = "clustering",
              tabsetPanel(id = "clustering_tab",
                          
                          tabPanel("Cluster 1", class = "custom-tab",
                                   fluidRow(
                                     box(title = "Cluster Description",class = "custom-box", verbatimTextOutput("desc_cluster1"), width = 12,
                                         tagList(
                                           h3(tags$span("Cluster 1: ", style = "color: brown;"), "점포 수는 적지만 주차장과 인구 밀도가 높은 도시 중심지역"),
                                           p("이 클러스터는 주차장 수가 많고, 인구 밀도와 유동인구가 매우 높지만, 상대적으로 점포 수가 적은 지역을 표현합니다.", 
                                             br(), # 줄바꿈
                                             "이러한 특성은",tags$span("큰 도시의 중심지역 또는 주거 밀집 지역일 가능성이 높습니다.", style = "color: brown;"),br(), br(), "
이 지역은 많은 사람들이 모이는 곳이지만, 점포 수가 상대적으로 적어 경쟁이 비교적 덜 치열할 수 있습니다.")
                                         )),
                                     box(title = "Cluster1 Map",class = "custom-box", plotOutput("map_cluster1"), width = 8),
                                     box(title = "Cluster Centers",class = "custom-box", plotOutput("centers1"), width = 4),
                                     box(title = "Cluster Table", class = "custom-box",DTOutput("table_cluster1"), width = 12)
                                   )),
                          
                          tabPanel("Cluster 2",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Cluster Description",class = "custom-box", verbatimTextOutput("desc_cluster2"), width = 12,
                                         tagList(
                                           h3(tags$span("Cluster 2: ", style = "color: brown;"), "점포 수는 많지만 인구 밀도와 주차장은 적은 상업적인 도심 지역"),
                                           p(" 이 클러스터는 점포 수가 가장 많지만, 인구 밀도와 유동인구, 주차장 수가 상대적으로 적은 지역을 나타냅니다.",br(),"
이는 ",tags$span("상업 지역이나 상가가 집중되어 있는 도심", style = "color: brown;"), "의 한 지역일 가능성이 있습니다.",br(),br(),"
비록 사람들의 유동이 덜하지만, 다양한 종류의 점포가 많이 위치해 있을 것으로 추측됩니다.")
                                         )),
                                     box(title = "Cluster2 Map",class = "custom-box", plotOutput("map_cluster2"), width = 8),
                                     box(title = "Cluster Centers",class = "custom-box", plotOutput("centers2"), width = 4),
                                     box(title = "Cluster Table", class = "custom-box",DTOutput("table_cluster2"), width = 12)
                                   )),
                          
                          tabPanel("Cluster 3",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Cluster Description",class = "custom-box", verbatimTextOutput("desc_cluster3"), width = 12,
                                         tagList(
                                           h3(tags$span("Cluster 3: ", style = "color: brown;"), "점포 수와 인구 밀도, 주차장이 모두 중간 정도인 도시 외곽 또는 주거 지역"),
                                           p("  이 클러스터는 점포 수가 중간 정도에 위치하며, 인구 밀도와 유동인구가 상대적으로 높은 지역을 표현합니다.",br(),"
                                              이는 ",tags$span("도시의 외곽 지역이나 주거 지역", style = "color: brown;"), "일 수 있으며, 상당히 높은 인구 밀도와 유동인구를 보유하고 있어 점포들에게 고객 유치에 유리할 수 있습니다.")
                                         )),
                                     box(title = "Cluster3 Map",class = "custom-box", plotOutput("map_cluster3"), width = 8),
                                     box(title = "Cluster Centers",class = "custom-box", plotOutput("centers3"), width = 4),
                                     box(title = "Cluster Table", class = "custom-box",DTOutput("table_cluster3"), width = 12)
                                   )),
                          
                          tabPanel("Cluster 4",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Cluster Description",class = "custom-box", verbatimTextOutput("desc_cluster4"), width = 12,
                                         tagList(
                                           h3(tags$span("Cluster 4: ", style = "color: brown;"), "점포 수는 적지만 인구 밀도와 주차장은 높은 주거 지역 또는 도심 주변 지역"),
                                           p(" 이 클러스터는 인구 밀도와 유동인구, 주차장 수가 비교적 높은 반면, 점포 수는 상대적으로 적은 지역을 나타냅니다.",br(),"
이는 ",tags$span("발전된 주거 지역이나 상업적 활동이 상대적으로 덜한 도심 주변의 지역", style = "color: brown;"),"일 가능성이 있습니다.",br(),br(),"
이러한 지역은 점포 수 대비 인구 밀도와 유동인구가 높아, 새로운 점포에게 큰 기회를 제공할 수 있습니다.")
                                         )),
                                     box(title = "Cluster4 Map",class = "custom-box", plotOutput("map_cluster4"), width = 8),
                                     box(title = "Cluster Centers",class = "custom-box", plotOutput("centers4"), width = 4),
                                     box(title = "Cluster Table", class = "custom-box",DTOutput("table_cluster4"), width = 12)
                                   ))
              )
      ),
      
      
  
      
      # Closure Rate Predictions 메뉴 [예측 결과]
      tabItem(tabName = "predict",
              tabsetPanel(id = "predict_tab",
                          tabPanel("Random Forest Plot",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Actual vs Predicted",class = "custom-box", plotOutput("rf_plot"), width = 8),
                                     box(title = "Description Actual vs Predicted",class = "custom-box",  width = 4,
                                         tagList(
                                           p("
Random forest 모델들 중에서 ",br(), tags$span(" RF1 모델이", style = "color: brown;")," 실제값과 예측값 사이에 ",br(),"상대적으로 가장 선형적인 경향을 보였습니다.")
                                         ))
                                   )),
                          tabPanel("Linear Regression Plot",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Actual vs Predicted",class = "custom-box", plotOutput("reg_plot"), width = 8),
                                     box(title = "Description Actual vs Predicted",class = "custom-box",  width = 4,
                                         tagList(
                                           p("
Regression 모델들 중에서 ",br(), tags$span(" RG1 모델이", style = "color: brown;")," 실제값과 예측값 사이에 ",br(),"상대적으로 가장 선형적인 경향을 보였습니다.")
                                         ))
                                   )),
                          tabPanel("Evaluation Metrics",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Evaluation MEtrics for Each Model", class = "custom-box",plotOutput("Evaluation"), width = 12),
                                     box(title = "Description",class = "custom-box",  width = 6,height = 200,
                                         tagList(
                                           p(tags$span("결론적으로, RF1 모델은 다른 모델들보다 더 선형에 가까운 예측을 수행한 것으로 해석할 수 있습니다.", style = "color: brown;"),br(), br(),"RF1 모델은 mse와 rmse 값이 다른 모델들에 비해 가장 작으며,",br(),"  mae와 mape 값도 상대적으로 작습니다.",br(),br()," 따라서 RF1 모델이 실제 값과 예측 값 사이의 오차를 가장 작게 만들어주는 모델입니다.")
                                         )),
                                     box(title = "RF1 모델 vs RG1 모델",class = "custom-box",  width = 6, height = 200,
                                         tagList(
                                           p(tags$span("RG1 모델은 RF1 모델과 비교했을 때 예측 성능이 떨어집니다. ", style = "color: brown;"),br(),br(), "  RG1 모델은 선형성 측면에서 RF1 모델과 유사한 결과를 보여줄 수 있지만, 오차가 크게 나타납니다. ",br(),"  mse와 rmse 값은 RF1 모델과 비슷하지만, mae와 mape 값이 상대적으로 더 큽니다.",br(),"  이는 RG1 모델이 실제 값과 예측 값 사이의 오차가 RF1 모델보다 크다는 것을 의미합니다.",br(),br()," 따라서 RF1 모델이 RG1 모델보다 더 좋은 예측 성능을 가진 것으로 해석할 수 있습니다.")
                                         ))
                                     #box(title = "Evaluation Table", class = "custom-box",plotOutput("Evaluation_Table"), width = 12)
                                   )),
                          tabPanel("Closure Rate Comparison",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Description Train_data",class = "custom-box", verbatimTextOutput("desc_Actual_map"), width = 6,
                                         tagList(
                                           p("모델이", tags$span("학습한 요식업 폐업률", style = "color: brown;"),"의 지역적 분포를 나타냅니다.",
                                            br(),
                                             "테이블은 각 지역의 ", tags$span("실제 폐업률", style = "color: brown;"),"을 보여줍니다.")
                                           )
                                     ),
                                     box(title = "Description Test_data",class = "custom-box", verbatimTextOutput("desc_Predicted_map"), width = 6,
                                         tagList(
                                           p("모델이", tags$span("예측한 요식업 폐업률", style = "color: brown;"),"의 지역적 분포를 나타냅니다." ,
                                             br(),
                                             "테이블은 각 지역의 ", tags$span("실제와 예측 폐업률", style = "color: brown;"),"을 비교해줍니다.")
                                         )
                                     )),
                                   fluidRow(
                                     box(title = "Train_data", plotOutput("Actual_map"),class = "custom-box", width = 6),
                                     box(title = "Test_data", plotOutput("Predicted_map"), class = "custom-box",width = 6)
                                   ),
                                   fluidRow(
                                     box(title = "Train_Table", class = "custom-box",DTOutput("Actual_tabl"), width = 6),
                                     box(title = "Test_Table", class = "custom-box",DTOutput("Predicted_tabl"), width = 6)
                                   )),
                          tabPanel("rf1 Variable Importance", class = "custom-tab",
                                   fluidRow(
                                     box(title = "Barplot of Variable Importance", class = "custom-box", plotOutput("rf1_varimp_bar"), width = 6),
                                     box(title = "Piechart of Variable Importance", class = "custom-box", plotOutput("rf1_varimp_pie"), width = 6)
                                   ))
              )
      )
    )
  )
)



#서버 부분
server <- function(input, output){
  
 
  
#-----------Welcome Page 
  
  
  
  output$img1 <- renderImage({
    list(src = "www/Home_black-1.png", contentType = "image/png", alt = "Total Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  
  output$img2 <- renderImage({
    list(src = "www/Home_black-2.png", contentType = "image/png", alt = "Open Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  
  output$img3 <- renderImage({
    list(src = "www/Home_black-3.png", contentType = "image/png", alt = "Closed Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  output$img4 <- renderImage({
    list(src = "www/Home_black-4.png", contentType = "image/png", alt = "Total Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  
  output$img5 <- renderImage({
    list(src = "www/Home_black-5.png", contentType = "image/png", alt = "Open Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  
  output$img6 <- renderImage({
    list(src = "www/Home_black-6.png", contentType = "image/png", alt = "Closed Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  output$img7 <- renderImage({
    list(src = "www/Home_black-7.png", contentType = "image/png", alt = "Closed Store Count", width = "100%", height = 700)
  }, deleteFile = FALSE)
  
  
  
#------------Data Insights
  
  library(ggplot2)
  
  # Open rates distribution
  output$open_rate_distribution <- renderPlot({
    # 개업률 분포 그리기
    ggplot(data_final_M, aes(x=OP_RATE)) +
      geom_histogram(bins=30, fill='#A2866A') +
      labs(x='개업률', y='Count', title='개업률 분포') +
      theme_minimal()
  })
  
  # Closed rates distribution
  output$closed_rate_distribution <- renderPlot({
    # 폐업률 분포 그리기
    ggplot(data_final_M, aes(x=CLS_RATE)) +
      geom_histogram(bins=30, fill='#A8AAA3') +
      labs(x='폐업률', y='Count', title='폐업률 분포') +
      theme_minimal()
  })
  
  # Rates table
  output$table_rates <- renderDT({
    # 개업률과 폐업률을 포함한 데이터프레임 생성
    rates_data <- data_final_M %>%
      select(SIGUNGU_NM, DONG_NM, OP_RATE, CLS_RATE)%>%
      mutate(CLS_RATE = round(CLS_RATE, 4),OP_RATE = round(OP_RATE, 4))
    
    colnames(rates_data) <- c("시군구 명", "동", "개업률", "폐업률")
    datatable(rates_data, options = list(pageLength = 10))
  })
  
  
  output$correlation_plot <- renderPlot({
    # 사용자가 선택한 변수 이름 가져오기
    selected_indicator <- input$indicator_dong
    
    # 해당 변수가 숫자형인지 확인
    if(is.numeric(data_final_M[[selected_indicator]])){
      # 'CLS_MK_NUM'과 선택한 변수 간의 산점도
      p <- ggplot(data_final_M, aes_string(x = selected_indicator, y = "CLS_MK_NUM")) +
        geom_point() +
        geom_smooth(method = "lm", col = "#FF4500") + # 선형회귀선 추가
        labs(x = selected_indicator, y = "Closure Rate",
             title = paste("Scatterplot of", selected_indicator, "and Closure Rate")) +
        xlim(quantile(data_final_M[[selected_indicator]], 0.01), quantile(data_final_M[[selected_indicator]], 0.99))  # Adjust the x-axis range based on data without outliers
      
      print(p)
    } else {
      print("The selected indicator or the closure rate is not numeric. Please select another indicator.")
    }
  })
  
  
  
  
  output$correlation_plot02 <- renderPlot({
    
    data_numeric <- data_final_M[, c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM",
                                     "FRC_MK_NUM", "OP_RATE", "CLS_RATE", "DT",
                                     "IN", "OUT", "PARKING")]
    
    # 컬럼명 한국어로
    colnames(data_numeric) <- c("점포수", "유사업종점포수", "개업점포수", "폐업점포수",
                                "프랜차이즈 점포수", "개업률", "폐업률", "인구밀도",
                                "유동인구 유입량", "유동인구 유출량", "주차장 수")
    
    # 상관관계 행렬 계산
    cor_matrix <- cor(data_numeric, use = "pairwise.complete.obs")
    
    # 색상 설정
    color_scheme <- colorRampPalette(c("#F6F6F6", "#BC8F8F"))(100)  # 색상 팔레트 설정
    
    # 크기 조절
    plot_size <- 5  # 그래프 크기 조절
    
    # 상관관계 플롯
    p <- ggplot(data = reshape2::melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = color_scheme) +
      labs(x = "", y = "", title = "Correlation Heatmap") +
      theme_bw() +
      theme(plot.title = element_text(size = 20),
            axis.text = element_text(size = 10),
            legend.position = "bottom")
    
    p
  })
  
  
  
  
  
  
  
  
#------------Status Map
  
  # Image rendering
  #지도 표현 메뉴 부분, 지도 이미지.
  output$mk_img <- renderImage({ #이미지를 www에서 불러와서 너비를 100%로 두어 창 크기 바뀔때마다 유동적으로 변하도록 조정
    list(src = "www/mk.png", contentType = "image/png", alt = "Total Store Count", width = "100%") #이미지 크기 조정
  }, deleteFile = FALSE)
  
  output$op_img <- renderImage({
    list(src = "www/op.png", contentType = "image/png", alt = "Open Store Count", width = "100%")
  }, deleteFile = FALSE)
  
  output$cls_img <- renderImage({
    list(src = "www/cls.png", contentType = "image/png", alt = "Closed Store Count", width = "100%")
  }, deleteFile = FALSE)
  
  
  #지도 메뉴에서 사용자가 선택한 지도 탭에 따라 밑에 테이블 정렬 기준을 바뀌도록 하려고 tab이름과 변수명을 스위치 시켰ㅇ.
  current_tab_table <- reactive({
    switch(input$map_tab,
           "Total" = "MK_NUM",
           "Open" = "OP_MK_NUM",
           "Closed" = "CLS_MK_NUM")
  })
  
  # 구 단위로 묶었기 때문에 시군구로 그룹화시키고
  # MK_NUM, CLS_MK_NUM,OP_MK_NUM 열들의 합을 계산해서 만든 시군구로 그룹화된 data_final_M_grouped을 만든다.
  # SIGUNGU_NM에 따라 그룹화하고 합계 계산
  data_final_M_grouped <- data_final_M %>%
    group_by(SIGUNGU_NM) %>%
    summarise(
      MK_NUM = sum(MK_NUM, na.rm = TRUE),
      CLS_MK_NUM = sum(CLS_MK_NUM, na.rm = TRUE),
      OP_MK_NUM = sum(OP_MK_NUM, na.rm = TRUE)
    )
  
  
  # 테이블 
  output$table_map <- DT::renderDataTable({
    data <- data_final_M_grouped %>%
      dplyr::select(SIGUNGU_NM, MK_NUM, CLS_MK_NUM, OP_MK_NUM) %>%
      arrange(-get(current_tab_table()))
    
    colnames(data) <- c("시군구 명", "점포수", "폐업점포수", "개업점포수")
    
    datatable(data,
              options = list(
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              )
    )
  })
  
  
  


  
#------------Overview by District
  
  #indicator 정보들을 테이블에서 한글로 표현하려고.
  indicator_dict <- c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                      "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                      "인구밀도" = "DT", "유동인구 유입량" = "IN",
                      "유동인구 유출량" = "OUT", "주차장 수" = "PARKING")
  
  indicator_dict <- setNames(names(indicator_dict), indicator_dict)
  
  
  #GU related rendering 구와 관련된 코드들(트리맵, 막대그래프, 테이블)
  output$gu_treemap <- renderPlot({
    df <- data_final_M %>%
      group_by(SIGUNGU_NM) %>%
      summarise(total = sum(get(input$indicator_gu))) %>%
      ungroup()
    
    ggplot(df, aes(area = total, label = SIGUNGU_NM, fill = total)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      theme_minimal() +
      labs(title = paste("Treemap of", input$indicator_gu, "by Gu"), fill = input$indicator_gu)
  })
  
  
  # 막대그래프
  output$bar_gu <- renderPlot({
    gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
    ggplot(gu_selected, aes(x = reorder(factor(DONG_NM), -get(input$indicator_gu)), y = get(input$indicator_gu), fill = get(input$indicator_gu))) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Dong", y = input$indicator_gu)
  })
  
  
  # 테이블(사용자가 선택한 구로 필터 걸어서 만든 테이블)
  output$table_gu <- DT::renderDataTable({
    data <- data_final_M %>%
      filter(SIGUNGU_NM == input$gu) %>%
      dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu),CLS_RATE) %>% ###CLS_RATE
      mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
      arrange(-get(input$indicator_gu))
    
    colnames(data) <- c("시군구 명", "동 이름", indicator_dict[input$indicator_gu], "폐업률")
    
    datatable(
      data,
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })

  
  
  
  

#------------Neighborhood Details
  
  # DONG related rendering 동과 관련된 코드들 (트리맵, 막대그래프, 테이블)
  output$dong_treemap <- renderPlot({
    dong_top20 <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarize(Total = sum(get(input$indicator_dong02))) %>%
      top_n(20, Total)
    ggplot(dong_top20, aes(area = Total, fill = Total, label = DONG_NM)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      labs(fill = input$indicator_dong02)
  })
  
  #막대그래프 #모든 동을 다 표현하지는 못함. #상위 20개
  output$bar_dong <- renderPlot({
    dong_top20 <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarize(Total = sum(get(input$indicator_dong02))) %>%
      top_n(20, Total) 
    ggplot(dong_top20, aes(x = reorder(DONG_NM, -Total), y = Total, fill = Total)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Dong", y = input$indicator_dong02)
  })
  
  
  
  # 테이블 (전체 동에 대한 테이블)
  output$table_dong <- DT::renderDataTable({
    data <- data_final_M %>%
      dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_dong02),CLS_RATE) %>%
      mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
      arrange(-get(input$indicator_dong02))
    
    colnames(data) <- c("시군구 명", "동 이름", indicator_dict[input$indicator_dong02], "폐업률")
    
    datatable(
      data,
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  
  
  
  
  
  
#------------Area Group Analysis
  
  # 컬럼 이름 변경 및 필요한 컬럼만 선택
  # 데이터를 읽고, 클러스터를 만들고, 변형.
  cluseter_data_re <- reactive({
    cluster_data %>%
      select(cluster, SIGUNGU_NM, DONG_NM, MK_NUM, OP_MK_NUM, CLS_MK_NUM, SMK_NUM, FRC_MK_NUM, OP_RATE, DT, IN, OUT, PARKING) %>%
      rename(
        "클러스터" = cluster,
        "시군구 명" = SIGUNGU_NM,
        "동 이름" = DONG_NM,
        "점포수" = MK_NUM,
        "개업점포수" = OP_MK_NUM,
        "폐업점포수" = CLS_MK_NUM,
        "유사업종점포수" = SMK_NUM,
        "프랜차이즈 점포수" = FRC_MK_NUM,
        "개업률" = OP_RATE,
        "인구밀도" = DT,
        "유동인구 유입량" = IN,
        "유동인구 유출량" = OUT,
        "주차장 수" = PARKING
      ) %>%
      mutate(개업률 = round(개업률, 3))
  })
  
  cluseter_data_re <- cluseter_data_re
  
  # 클러스터 중심 데이터를 정규화
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  # `...1` 컬럼을 제외
  cluster_centers <- cluster_centers[ , !(names(cluster_centers) %in% "...1")]
  
  cluster_centers_normalized <- as.data.frame(lapply(cluster_centers, normalize))
  #cluster_centers_normalized
  # 데이터 프레임을 긴 형식으로
  cluster_centers_long <- tidyr::pivot_longer(cluster_centers_normalized, everything(), names_to = "Variable", values_to = "Value")
  
  # 클러스터 번호를 추가
  cluster_centers_long$Cluster <- rep(1:nrow(cluster_centers_normalized), each = ncol(cluster_centers_normalized))
  

 
  
  # 클러스터1
  
  #테이블
  test_data_cluster1 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 1,] })
  output$table_cluster1 <- DT::renderDataTable({
    datatable(
      test_data_cluster1(),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  
  # 중심 막대그래프
  output$centers1 <- renderPlot({
    ggplot(cluster_centers_long[cluster_centers_long$Cluster == 1,], aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "#A2866A") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 1")
  })
  #지도
  output$map_cluster1 <- renderPlot({ })
  #Text
  output$desc_cluster1 <- renderText({ })
  
  
  
  # 클러스터2
  
  #테이블
  test_data_cluster2 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 2,] })
  output$table_cluster2 <- DT::renderDataTable({
    datatable(
      test_data_cluster2(),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  # 중심 막대그래프
  output$centers2 <- renderPlot({
    ggplot(cluster_centers_long[cluster_centers_long$Cluster == 2,], aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "#A2866A") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 2")
  })
  #지도
  output$map_cluster2 <- renderPlot({ })
  #Text
  output$desc_cluster2 <- renderText({ })
  
  
  
  
  # 클러스터3
  
  #테이블
  test_data_cluster3 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 3,] })
  output$table_cluster3 <- DT::renderDataTable({
    datatable(
      test_data_cluster3(),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  # 중심 막대그래프
  output$centers3 <- renderPlot({
    ggplot(cluster_centers_long[cluster_centers_long$Cluster == 3,], aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "#A2866A") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 3")
  })
  #지도
  output$map_cluster3 <- renderPlot({ })
  #Text
  output$desc_cluster3 <- renderText({ })
  
  
  
  
  # 클러스터4
  
  #테이블
  test_data_cluster4 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 4,] })
  output$table_cluster4 <- DT::renderDataTable({
    datatable(
      test_data_cluster4(),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  # 중심 막대그래프
  output$centers4 <- renderPlot({
    ggplot(cluster_centers_long[cluster_centers_long$Cluster == 4,], aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "#A2866A") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 4")
  })
  #지도
  output$map_cluster4 <- renderPlot({ })
  #Text
  output$desc_cluster4 <- renderText({  })
  
  

  
  
  
#------------Closure Rate Predictions

  # 컬러 팔레트 설정
  pastel_palette <- c("#C353A5", "#A0522D","#8B4513","#BC8F8F", "red", "saddlebrown")
  
  #D2B48C (Tan)
  #BC8F8F (Rosy Brown)
  #A0522D (Sienna)
  #8B4513 (Saddle Brown)
  
  #랜덤포레스트 모델 산점도 그래프
  output$rf_plot <- renderPlot({
    p_rf1 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf1)) +
      geom_point(color = pastel_palette[1]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF1)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "white", color = "red", size = 2), # 강조를 위해 빨간색 테두리 추가
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_blank(),
            legend.position = "none")
    
    p_rf2 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf2)) +
      geom_point(color = pastel_palette[2]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF2)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rf3 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf3)) +
      geom_point(color = pastel_palette[3]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF3)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rf4 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf4)) +
      geom_point(color = pastel_palette[6]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF4)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(p_rf1, p_rf2, p_rf3, p_rf4, nrow = 2)
  })
  
  
  
  # Regression 산점도 그래프
  output$reg_plot <- renderPlot({
    p_rg1 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg1)) +
      geom_point(color = pastel_palette[2]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG1)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "white", color = "red", size = 2), # 강조를 위해 빨간색 테두리 추가
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_blank(),
            legend.position = "none")
    
    p_rg2 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg2)) +
      geom_point(color = pastel_palette[3]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG2)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rg3 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg3)) +
      geom_point(color = pastel_palette[6]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG3)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rg4 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg4)) +
      geom_point(color = pastel_palette[4]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[5]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG4)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(p_rg1, p_rg2, p_rg3, p_rg4, nrow = 2)
  })
  


  
# 예측 성능 평가 지표
 pastel_palette02 <- c("#AEC6CF", "#ACBCC0", "#AAB3B2", "#A8AAA3", "#A7A195", "#A59886", "#A38F78", "#A2866A")
  
  output$Evaluation <- renderPlot({

    # mse에 대한 그래프
    p_mse <- ggplot(eval_results_df, aes(x = ...1, y = mse, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      #scale_y_continuous(limits = c(min(eval_results_df$mse) * 0.5, max(eval_results_df$mse) * 0.9)) +
      labs(x = "Model", y = "mse", fill = "Model",title = "mse") +
      theme_bw() +
      theme(legend.position = "top",plot.title = element_text(size = 20))
    
    # mae에 대한 그래프
    p_mae <- ggplot(eval_results_df, aes(x = ...1, y = mae, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "mae", fill = "Model",title = "mae") +
      theme_bw() +
      theme(legend.position = "top",plot.title = element_text(size = 20))
    
    # mape에 대한 그래프
    p_mape <- ggplot(eval_results_df, aes(x = ...1, y = mape, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "mape", fill = "Model",title = "mape") +
      theme_bw() +
      theme(legend.position = "top",plot.title = element_text(size = 20))
    
    # rmse에 대한 그래프
    p_rmse <- ggplot(eval_results_df, aes(x = ...1, y = rmse, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "rmse", fill = "Model",title = "rmse") +
      theme_bw() +
      theme(legend.position = "top",plot.title = element_text(size = 20))
    
    grid.arrange(p_mse, p_mae, p_mape, p_rmse, nrow = 2)
  })
  
  output$Evaluation_Table <- DT::renderDataTable({
    datatable(eval_results_df)
  })
  
  
#str(eval_results_df)
  
  # test data, train data 합치기.
  #total_data <- bind_rows(test_data, train_data)
  #write.csv(total_data,"C:/data/preprocessed/total_data.csv")
  
  # Actual table
  output$Actual_tabl <- renderDT({
    datatable(train_data %>%
                mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, CLS_RATE) %>%
                rename("시군구" = SIGUNGU_NM,
                       "동" = DONG_NM,
                       "폐업률" = CLS_RATE) %>%
                arrange(desc("폐업률")),
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  
  # Predicted table
  output$Predicted_tabl <- renderDT({
    datatable(test_data %>%
                mutate(CLS_RATE = round(CLS_RATE, 4), predictions_rf1 = round(predictions_rf1, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, CLS_RATE, predictions_rf1) %>%
                rename("시군구" = SIGUNGU_NM,
                       "동" = DONG_NM,
                       "폐업률" = CLS_RATE,
                       "예측 폐업률" = predictions_rf1) %>%
                arrange(desc("폐업률")),
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  
  #Actual_Predicted_table
  output$Actual_Predicted_tabl<- renderDT({
    datatable(train_data %>%
                mutate(CLS_RATE = round(CLS_RATE, 4), predictions_rf1 = round(predictions_rf1, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, CLS_RATE, predictions_rf1) %>%
                rename("시군구" = SIGUNGU_NM,
                       "동" = DONG_NM,
                       "폐업률" = CLS_RATE,
                       "예측 폐업률" = predictions_rf1) %>%
                arrange(desc("폐업률")),
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  

  rf1_importance <- reactive({
    data <- tibble(
      Variable = c("점포수", "유사업종점포수", "개업점포수", "프랜차이즈점포수", 
                   "개업률", "인구밀도", "유동인구유입량", "유동인구유출량", 
                   "주차장수", "클러스터"),
      Importance = c(0.005885125, 0.006154335, 0.005173152, 0.005736991, 0.017432232, 
                     0.003074676, 0.007259888, 0.002668425, 0.003235863, 0.001132173)
      #Angle = c(60, 80, 110, 130, 140, 30, 60, 80, 170, 0)  # 예시 각도, 실제로는 원하는 각도를 지정해주세요.
    )
    return(data)
  })
  
  # Barplot of Variable Importance
  output$rf1_varimp_bar <- renderPlot({
    data <- rf1_importance()
    ggplot(data, aes(x=reorder(Variable, Importance), y=Importance)) + 
      geom_bar(stat="identity", fill="#A59886") + 
      coord_flip() + 
      theme_minimal() + 
      xlab("중요도") + 
      ylab("변수") +
      ggtitle("변수 중요도 막대그래프")
  })
  
  # color_range02 <- colorRampPalette(c("#aec6cf", "#A2866A"))(10)
  # 
  # # 새로 생성한 팔레트
  # print(color_range02)
  
  color_range02 <- c("#B3C2D1", "#ACC1C6", "#A8BFBB", "#A5BDB0", "#A3BAA5", "#A1B79A", "#9EB590", "#9BB285", "#98AF7B", "#95AC70")
  
  
  output$rf1_varimp_pie <- renderPlot({
    data <- rf1_importance()
    data$angle <- 90 - 360 * (cumsum(data$Importance) - 0.5 * data$Importance)
    ggplot(data, aes(x="", y=Importance, fill=Variable)) +
      geom_bar(width=1, stat="identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = color_range02) + # Use manual color scale
      theme_minimal() + 
      xlab("") + 
      ylab("") +
      ggtitle("변수 중요도 원형차트") +
      theme(legend.title = element_blank()) +
      geom_text(aes(label = Variable, angle = angle), position = position_stack(vjust = 0.5), color = "black")
  })
  
  
  
  
}


#샤이니 앱 실행
shinyApp(ui = ui, server = server)

