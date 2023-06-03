# # 라이브러리 불러오기. Library Imports.
#install.packages("shinythemes")
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(treemapify)
library(readr)
library(RColorBrewer)
library(tidyr)  # tidyr 패키지 추가
library(gridExtra)
library(shinythemes)
#library(reshape2)

#setwd("C:/data/preprocessed/")
setwd("/Users/gayeongkim/Desktop/data/prepared")

# data_final_M 데이터.
data_final_M <- read_csv("data_final.csv")
test_data <- read_csv("test_data.csv") %>% as_tibble()
train_data <- read_csv("train_data.csv") %>% as_tibble()

eval_results_df <- read_csv("eval_results_df.csv")
cluster_data <- read_csv("cluster.csv")
cluster_centers <- read_csv("cluster_centers.csv")

#str(eval_results_df)
#View(eval_results_df)

#사용자 Ui 부분
ui <- dashboardPage(

  dashboardHeader(title = "서울시 동별 폐업률"), #대시보드 해더
                 

  #사이드 메뉴 부분
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("현황 지도로 보기", tabName = "map", icon = icon("globe")),
      menuItem("Total Indicator by Gu", tabName = "gu", icon = icon("chart-bar")), #이거 이름이 바껴서안된거였음.
      menuItem("Total Indicator by Dong", tabName = "dong", icon = icon("chart-bar")),
      menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")), #아이콘 이상하면 다른거 추천해죠 (https://fontawesome.com/v5/search)
      menuItem("Predict 폐업률", tabName = "predict", icon = icon("line-chart"))
    )
  ),

  #바디 부분
  dashboardBody(
    includeCSS("/Users/gayeongkim/Desktop/Git_R/R_/www/custom.css"),

    tabItems(
      #Home 메뉴 (이 프로젝트에 대한 소개 더 추가! 사진도 넣구 글도 추가해주고 이 페이지가 처음 보이게 되는 페이지.)
      tabItem(tabName = "home",
              h2("서울시 동별 폐업률 분석"),
              p("이 프로젝트는 서울시 동별 폐업률을 분석하는 것을 목표로 합니다."),
              tags$br(),
              tags$h4('For whom ? : 예비 자영업자 , 창업 준비자')),
      # 현황 지도로 보기 메뉴 (메뉴 이름 변경?)
      # 현황 지도로 보기 메뉴
      tabItem(tabName = "map", # 이 메뉴 이름은 map
              fluidRow(
                box(title = "Map", class = "custom-box",  #첫번째 박스에 지도 사진을 넣는데
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

      #원형 그래프는 뭔가 이쁜데, 글씨 조절하는걸 못해서 일단 트리맵, 막대그래프, 테이블 로만 표현.
      #정보를 구 단위로 시각화
      tabItem(tabName = "gu",
              fluidRow(
                # Indicators box [이 박스 안에는 정보(요인들) SelectInput기능과  "구" 별 tree map이 들어있다.]
                box(title = "Indicators",class = "custom-box", selectInput("indicator_gu", "Choose an indicator",
                                                      choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                  "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                  "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                  "유동인구 유출량" = "OUT", "주차장 수" = "PARKING"))
                    , plotOutput("gu_treemap"), width = 6),
                # GU 박스[구 selectInput기능과"구" 별 막대 그래프 가 들어있다.]
                box(title = "Gu",class = "custom-box", selectInput("gu", "Choose a Gu", choices = unique(data_final_M$SIGUNGU_NM)), plotOutput("bar_gu"), width = 6)),
              fluidRow(
                #구에 속한 동 태이블
                box(title = "table",class = "custom-box", DTOutput("table_gu"), width = 12))),

      #정보를 동단위로 시각화
      tabItem(tabName = "dong",
              fluidRow(
                #Indicators box [정보(요인들) SelectInput 기능과 "동" 별 tree map 이 들어있다.]
                box(title = "Indicators",class = "custom-box", selectInput("indicator_dong", "Choose an indicator", choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                                                           "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                                                           "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                                                           "유동인구 유출량" = "OUT", "주차장 수" = "PARKING")), plotOutput("dong_treemap"),width = 6),
                #"동" 별 막대그래프 bar
                box(title = "bar",class = "custom-box",plotOutput("bar_dong"),width = 6)),
              fluidRow(
                #"동" 별 테이블
                box(title = "table",class = "custom-box",DTOutput("table_dong"), width = 12 ))),
      
      # 클러스터링에 대한 메뉴
      tabItem(tabName = "clustering",
              tabsetPanel(id = "clustering_tab",
                          tabPanel("Cluster 1", class = "custom-tab",
                                   fluidRow(
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster1"), width = 8),
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster1"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster1"), width = 12),
                                     box(title = "Variable Importance", class = "custom-box", plotOutput("pct_chart_cluster1"), width = 12)
                                   )),
                          tabPanel("Cluster 2",class = "custom-tab",
                                   fluidRow(
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster2"), width = 8),
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster2"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster2"), width = 12)
                                   )),
                          tabPanel("Cluster 3",class = "custom-tab",
                                   fluidRow(
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster3"), width = 8),
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster3"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster3"), width = 12)
                                   )),
                          tabPanel("Cluster 4",class = "custom-tab",
                                   fluidRow(
                                     box(title = "map", class = "custom-box",plotOutput("map_cluster4"), width = 8),
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster4"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster4"), width = 12)
                                   ))
              )
      ),

      # Predict 폐업률 메뉴
      tabItem(tabName = "predict",
              tabsetPanel(id = "predict_tab",
                          tabPanel("RF 산점도 그래프",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Actual vs Predicted",class = "custom-box", plotOutput("rf1_plot"), width = 12)
                                   ),
                                   fluidRow(
                                     box(title = "Actual vs Predicted",class = "custom-box", plotOutput("rf_others_plot"), width = 12)
                                   ),
                                   ),
                          tabPanel("RG 산점도 그래프",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Actual vs Predicted",class = "custom-box", plotOutput("reg_plot"), width = 12)
                                   )),
                          tabPanel("예측 성능 평가 지표",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Evaluation MEtrics for Each Model", class = "custom-box",plotOutput("Evaluation"), width = 12)
                                   )),
                          tabPanel("폐업률 예측 비교",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Actual", plotOutput("Actual_map"),class = "custom-box", width = 6),
                                     box(title = "Predicted", plotOutput("Predicted_map"), class = "custom-box",width = 6)
                                   ),
                                   # fluidRow(
                                   #   box(title = "Table", class = "custom-box",DTOutput("Actual_tabl"), width = 6),
                                   #   box(title = "Table", class = "custom-box",DTOutput("Predicted_tabl"), width = 6)
                                   # )
                                   fluidRow(
                                     box(title = "Table", class = "custom-box",DTOutput("Actual_Predicted_tabl"), width = 12)
                                   )
                                   )
              )
      )
    )
  )
)


#서버 부분
server <- function(input, output){
  
  #지도 부분 
  
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
  
  
#  data_final_M_grouped로 테이블 렌더링 (지도 밑에있는 테이블 table_map)
  # output$table_map <- renderDataTable({
  #   data_final_M_grouped %>%
  #     dplyr::select(SIGUNGU_NM, MK_NUM, CLS_MK_NUM, OP_MK_NUM) %>%
  #     arrange(-get(current_tab_table()))
  # })
  
  output$table_map <- DT::renderDataTable({
    datatable(data_final_M_grouped %>%
                dplyr::select(SIGUNGU_NM, MK_NUM, CLS_MK_NUM, OP_MK_NUM) %>%
                arrange(-get(current_tab_table())), 
              options = list(
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              )
    )
  })
  
  

  
  
  
# -----
  # 
  # #구에 대한 정보 있는 메뉴 부분
  # 
  # # GU related rendering 구와 관련된 코드들(트리맵, 막대그래프, 테이블)
  # output$gu_treemap <- renderPlot({
  #   df <- data_final_M %>%
  #     group_by(SIGUNGU_NM) %>%
  #     summarise(total = sum(get(input$indicator_gu))) %>%
  #     ungroup()
  #   
  #   ggplot(df, aes(area = total, label = SIGUNGU_NM, fill = total)) +
  #     geom_treemap() +
  #     geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
  #     scale_fill_gradient(low = "lightblue", high = "darkblue") +
  #     theme_minimal() +
  #     labs(title = paste("Treemap of", input$indicator_gu, "by Gu"), fill = input$indicator_gu)
  # })
  # 
  # 
  # # 막대그래프
  # output$bar_gu <- renderPlot({
  #   gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
  #   ggplot(gu_selected, aes(x = reorder(factor(DONG_NM), -get(input$indicator_gu)), y = get(input$indicator_gu), fill = get(input$indicator_gu))) +
  #     geom_bar(stat = "identity") +
  #     scale_fill_gradient(low = "lightblue", high = "darkblue") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #     labs(x = "Dong", y = input$indicator_gu)
  # })
  # 
  # # 테이블(사용자가 선택한 구로 필터 걸어서 만든 테이블)
  # output$table_gu <- DT::renderDataTable({
  #   datatable(
  #     data_final_M %>%
  #       filter(SIGUNGU_NM == input$gu) %>%
  #       dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu)) %>%
  #       arrange(-get(input$indicator_gu)), 
  #     options = list(
  #       columnDefs = list(
  #         list(targets = "_all", className = "dt-center")
  #       )
  #     )
  #   )
  # })
  # 
  # 
  # 
  # GU related rendering 구와 관련된 코드들(트리맵, 막대그래프, 테이블)
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
    datatable(
      data_final_M %>%
        filter(SIGUNGU_NM == input$gu) %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu)) %>%
        arrange(-get(input$indicator_gu)), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  
  # 동에 대한 정보 있는 메뉴 부분
  
  # DONG related rendering 동과 관련된 코드들 (트리맵, 막대그래프, 테이블)
  output$dong_treemap <- renderPlot({
    dong_top20 <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarize(Total = sum(get(input$indicator_dong))) %>%
      top_n(20, Total)
    ggplot(dong_top20, aes(area = Total, fill = Total, label = DONG_NM)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      labs(fill = input$indicator_dong)
  })
  
  #막대그래프 #모든 동을 다 표현하지는 못함. #상위 20개 
  # (이부분 스케일응 밑으로 빼는건 어떨까? 오른쪽에 두지 말고 아래쪽으로 빼는건 어떨까?)
  output$bar_dong <- renderPlot({
    dong_top20 <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarize(Total = sum(get(input$indicator_dong))) %>%
      top_n(20, Total) #상위 20개 
    ggplot(dong_top20, aes(x = reorder(DONG_NM, -Total), y = Total, fill = Total)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Dong", y = input$indicator_dong)
  })
  
  # 테이블 (전체 동에 대한 테이블)
  output$table_dong <- DT::renderDataTable({
    datatable(
      data_final_M %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_dong)) %>%
        arrange(-get(input$indicator_dong)), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
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
  
  # # 클러스터 중심비율 계산
  # normalize_cluster_centers <- function(cluster_centers) {
  #   max_values <- apply(cluster_centers, 2, max)
  #   max_values[max_values == 0] <- 1
  #   normalized_centers <- sweep(cluster_centers, 2, max_values, "/")
  #   normalized_centers[normalized_centers < 0] <- 0
  #   return(normalized_centers)
  # }
  # 
  # # 클러스터 중심의 모든 값이 0인 열 제거
  # cluster_centers <- cluster_centers[, apply(cluster_centers, 2, function(x) !all(x == 0))]
  # 
  # # NA 값을 0으로 변경
  # cluster_centers[is.na(cluster_centers)] <- 0
  # 
  
  # 클러스터1
  test_data_cluster1 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 1,] })
  output$table_cluster1 <- DT::renderDataTable({
    datatable(
      test_data_cluster1(), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  output$map_cluster1 <- renderPlot({ })
  output$desc_cluster1 <- renderText({ 
    "클러스터 1: 상업적으로 활발하지만 주차 시설과 거주 인구가 상대적으로 부족한 지역

점포 수(MK_NUM), 유사업종 점포 수(SMK_NUM), 프랜차이즈 점포 수(FRC_MK_NUM)가 매우 높은 반면, 인구 밀도(DT)와 주차장 수(PARKING)는 비교적 낮은 편이다.
클러스터는 상업적으로 매우 활발한 지역이지만 주차 시설이나 거주 인구는 상대적으로 부족한 것으로 보인다.
이런 지역에서는 요식업 폐업률이 경쟁력에 큰 영향을 받을 수 있으며, 주차 시설의 부족도 폐업률에 영향을 줄 수 있다."
  })
  
  # output$pct_chart_cluster1 <- renderPlot({
  #   cluster1 <- normalize_cluster_centers(cluster_centers)[1,]
  #   if (all(cluster1 == 0)) { # 모든 요소가 0인 경우
  #     plot.new() # 빈 plot을 생성
  #     title("No data available for Cluster 1 Variable Importance")
  #   } else {
  #     pie(cluster1, main = "Cluster 1 Variable Importance")
  #   }
  # })
  
  
  # output$pct_chart_cluster1 <- renderPlot({
  #   draw_pie_chart(1)
  # })

  
  
  # 클러스터2
  
  test_data_cluster2 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 2,] })
  output$table_cluster2 <- DT::renderDataTable({
    datatable(
      test_data_cluster2(), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  output$map_cluster2 <- renderPlot({ })
  output$desc_cluster2 <- renderText({ 
    "클러스터 2: 높은 인구 밀도와 유동인구에 비해 상업 활동이 덜 발달한 지역
  
  인구 밀도(DT)가 특히 높고, 유동인구 유입량(IN)과 유출량(OUT)이 높은 편이다.그러나 점포 수와 프랜차이즈 점포 수는 비교적 낮다.
  이런 지역에서는 인구 밀도가 높음에도 불구하고 상업 활동이 비교적 덜 발달하였을 가능성이 있다."
  })
  
  
  
  # 클러스터3
  
  test_data_cluster3 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 3,] })
  output$table_cluster3 <- DT::renderDataTable({
    datatable(
      test_data_cluster3(), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  output$map_cluster3 <- renderPlot({ })
  output$desc_cluster3 <- renderText({ 
    "클러스터 3: 상업적으로 활발하면서도 유동인구에 크게 의존하는 지역
  
  점포 수, 유사업종 점포 수, 프랜차이즈 점포 수가 높고, 인구 밀도는 중간 정도이다.유동인구 유입량과 유출량이 높은 편
  이런 지역은 상업적으로 활발하면서도 인구 밀도가 높지 않은 지역으로, 고객 유입이 주로 유동인구에 의존하는 것으로 보인다. 
  이런 지역에서는 요식업 폐업률이 유동인구의 변화에 영향을 많이 받을 수 있다."
  })
  
  
  
  # 클러스터4
  
  test_data_cluster4 <- reactive({ cluseter_data_re()[cluseter_data_re()$클러스터 == 4,] })
  output$table_cluster4 <- DT::renderDataTable({
    datatable(
      test_data_cluster4(), 
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      )
    )
  })
  output$map_cluster4 <- renderPlot({ })
  output$desc_cluster4 <- renderText({ 
    "클러스터 4: 주거와 상업이 잘 혼합된 높은 인구 밀도와 주차 시설이 충분한 지역
  
  인구 밀도(DT)와 주차장 수(PARKING)가 높은 편이며, 점포 수는 중간 정도.
  이런 지역은 주거 지역과 상업 지역이 잘 혼합된 지역일 수 있다.  요식업 폐업률이 거주 인구와 주차 시설에 크게 영향을 받을 수 있다."
  })
  

  # 컬러 팔레트 설정
  #pastel_palette <- c("#B3CCE6", "#B8A1CF", "#BD7BBA", "#C353A5", "#C72C90", "#CC006C", "#D21E47", "#D63C23", "#DB5900", "#E07700", "#E59500", "#EAB200", "#EFD000", "#F4EE00", "#F9DB00", "#FEC900", "#FEB600", "#FFA300", "#FF9000", "#FF7D00")
  
  
  # 컬러 팔레트 설정
  pastel_palette <- c("#C353A5", "#A0522D","#8B4513","#BC8F8F")
  
  #D2B48C (Tan)
  #BC8F8F (Rosy Brown)
  #A0522D (Sienna)
  #8B4513 (Saddle Brown)
  # RF1 모델 산점도 그래프 (크게)
  output$rf1_plot <- renderPlot({
    ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf1)) +
      geom_point(color = pastel_palette[1]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[2]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF1)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "white", color = "black"),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_blank(),
            legend.position = "none")
  })
  
  
  # RF2~RF4 모델 산점도 그래프
  output$rf_others_plot <- renderPlot({
    p_rf2 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf2)) +
      geom_point(color = pastel_palette[3]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[2]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF2)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rf3 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf3)) +
      geom_point(color = pastel_palette[4]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[2]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF3)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rf4 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rf4)) +
      geom_point(color = pastel_palette[1]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[2]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RF4)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(p_rf2, p_rf3, p_rf4, nrow = 1)
  })
  
  
  
  # Regression 산점도 그래프
  output$reg_plot <- renderPlot({
    p_rg1 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg1)) +
      geom_point(color = pastel_palette[3]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[4]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG1)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rg2 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg2)) +
      geom_point(color = pastel_palette[1]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[4]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG2)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rg3 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg3)) +
      geom_point(color = pastel_palette[2]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[4]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG3)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    p_rg4 <- ggplot(test_data, aes(x = CLS_RATE, y = predictions_rg4)) +
      geom_point(color = pastel_palette[3]) +
      geom_abline(intercept = 0, slope = 1, color = pastel_palette[4]) +
      theme_minimal() +
      ggtitle("Actual vs Predicted (RG4)") +
      labs(x = "Actual", y = "Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(p_rg1, p_rg2, p_rg3, p_rg4, nrow = 2)
  })
  
  
  pastel_palette02 <- c("#aec6cf", "#f5b7b1", "#faf3dd", "#d2a2d4", "#f3d1dc", "#a4e5d9", "#f9cb9c", "#e6bc28")
  
  output$Evaluation <- renderPlot({
    # mse에 대한 그래프 
    p_mse <- ggplot(eval_results_df, aes(x = ...1, y = mse, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "mse", fill = "Model") +
      theme_bw() +
      theme(legend.position = "top")
    
    # mae에 대한 그래프
    p_mae <- ggplot(eval_results_df, aes(x = ...1, y = mae, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "mae", fill = "Model") +
      theme_bw() +
      theme(legend.position = "top")
    
    # mape에 대한 그래프
    p_mape <- ggplot(eval_results_df, aes(x = ...1, y = mape, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "mape", fill = "Model") +
      theme_bw() +
      theme(legend.position = "top")
    
    # rmse에 대한 그래프
    p_rmse <- ggplot(eval_results_df, aes(x = ...1, y = rmse, fill = ...1)) +
      geom_bar(stat = "identity", position = "dodge",
               color = ifelse(eval_results_df$...1 == "rf1", "red", "white"),
               size = ifelse(eval_results_df$...1 == "rf1", 1.5, 0.5)) +  # 빨간색 선의 두께를 수정
      scale_fill_manual(values = pastel_palette02) +
      labs(x = "Model", y = "rmse", fill = "Model") +
      theme_bw() +
      theme(legend.position = "top")
    
    grid.arrange(p_mse, p_mae, p_mape, p_rmse, nrow = 2)
  })
  
  
  # test data, train data 합치기.
  total_data <- bind_rows(test_data, train_data)
  
  # Actual table
  output$Actual_tabl <- renderDT({
    datatable(total_data %>%
                mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, CLS_RATE) %>%
                rename("시군구" = SIGUNGU_NM, 
                       "동" = DONG_NM, 
                       "폐업률" = CLS_RATE) %>%
                arrange(desc(폐업률)), 
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  
  # Predicted table
  output$Predicted_tabl <- renderDT({
    datatable(total_data %>%
                mutate(predictions_rf1 = round(predictions_rf1, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, predictions_rf1) %>%
                rename("시군구" = SIGUNGU_NM, 
                       "동" = DONG_NM, 
                       "예측 폐업률" = predictions_rf1) %>%
                arrange(desc("예측 폐업률")), 
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  
  #Actual_Predicted_table
  output$Actual_Predicted_tabl<- renderDT({
    datatable(total_data %>%
                mutate(CLS_RATE = round(CLS_RATE, 4), predictions_rf1 = round(predictions_rf1, 4)) %>%
                select(SIGUNGU_NM, DONG_NM, CLS_RATE, predictions_rf1) %>%
                rename("시군구" = SIGUNGU_NM, 
                       "동" = DONG_NM,
                       "폐업률" = CLS_RATE,
                       "예측 폐업률" = predictions_rf1) %>%
                arrange(desc("폐업률")), 
              options = list(pageLength = 10, columnDefs = list(list(targets = "_all", className = "dt-center"))))
  })
  

}



#샤이니 앱 실행
shinyApp(ui = ui, server = server)

