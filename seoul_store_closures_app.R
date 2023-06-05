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

# # data_final_M 데이터._가영
#setwd("C:/")
#setwd("Users/gayeongkim/Desktop/")
# data_final_M <- read_csv("data/preprocessed/data_final.csv")
# test_data <- read_csv("data/preprocessed/test_data.csv") %>% as_tibble()
# train_data <- read_csv("data/preprocessed/train_data.csv") %>% as_tibble()
#
# eval_results_df <- read_csv("data/preprocessed/eval_results_df.csv")
# cluster_data <- read_csv("data/preprocessed/cluster.csv")
# cluster_centers <- read_csv("data/preprocessed/cluster_centers.csv")

#str(eval_results_df)
#View(eval_results_df)

#사용자 Ui 부분
ui <- dashboardPage(

  dashboardHeader(title = "서울시 동별 폐업률"), #대시보드 해더


  #사이드 메뉴 부분
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Open & Closed Rates", tabName = "rates", icon = icon("bar-chart")),  # 새로운 메뉴 추가
      menuItem("현황 지도로 보기", tabName = "map", icon = icon("globe")),
      menuItem("Total Indicator by Gu", tabName = "gu", icon = icon("chart-bar")), #이거 이름이 바껴서안된거였음.
      menuItem("Total Indicator by Dong", tabName = "dong", icon = icon("chart-bar")),
      menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")), #아이콘 이상하면 다른거 추천해죠 (https://fontawesome.com/v5/search)
      menuItem("Predict 폐업률", tabName = "predict", icon = icon("line-chart"))
    )
  ),

  #바디 부분
  dashboardBody(
    includeCSS("www/custom.css"),
    #includeCSS("Git_R/R_/www/custom.css"),
#getwd()

    tabItems(
      #Home 메뉴 (이 프로젝트에 대한 소개 더 추가! 사진도 넣구 글도 추가해주고 이 페이지가 처음 보이게 되는 페이지.)
      #Home 메뉴
      tabItem(tabName = "home",
              h2("서울시 동별 폐업률 분석"),
              # 서울시 사진 삽입
              img(src = "www/seul.jpg", width = "100%"),
              # 경기 하락 지표 삽입 및 프로젝트 설명
              fluidRow(
                box(title = "경기 하락 지표", class = "custom-box",style = "height: 300px;", width = 4, "경기 하락 지표 그래프"),
                box(title = "프로젝트의 도움",class = "custom-box",style = "height: 300px;", width = 8, "이 프로젝트를 통해 사회에 어떤 도움을 줄 수 있는지 설명")
              ),
              # 요식업 폐업률 분석 및 그래프
              fluidRow(
                box(title = "요식업 폐업률 분석에 집중 한 이유", class = "custom-box",style = "height: 300px;",width = 8, "요식업 폐업률 분석을 선택한 이유 설명"),
                box(title = "업종별 폐업률 그래프", class = "custom-box",style = "height: 300px;", width = 4, plotOutput("cm_cls"))
              ),
              # 추가 박스
              fluidRow(
                box(title = "추천 사용자", class = "custom-box",style = "height: 200px;", width = 12, "추천 사용자의 프로필"),
                box(title = "contact", class = "custom-box",style = "height: 200px;",width = 12, "우리 프로필 및 역할 분담 내용")
              )
      ),




      tabItem(tabName = "rates",
              fluidRow(
                box(title = "개업률 분포", class = "custom-box", plotOutput("open_rate_distribution"), width = 6),
                box(title = "폐업률 분포", class = "custom-box", plotOutput("closed_rate_distribution"), width = 6)
              ),
              fluidRow(
                box(title = "Table", class = "custom-box", DTOutput("table_rates"), width = 12)
              )
      ),

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
                box(title = "Indicators",class = "custom-box",  style = "height: 500px;", selectInput("indicator_dong", "Choose an indicator", choices = c("점포수" = "MK_NUM", "유사업종점포수" = "SMK_NUM", "개업점포수" = "OP_MK_NUM",
                                                                                                           "폐업점포수" = "CLS_MK_NUM", "프랜차이즈 점포수" = "FRC_MK_NUM",
                                                                                                           "인구밀도" = "DT", "유동인구 유입량" = "IN",
                                                                                                           "유동인구 유출량" = "OUT", "주차장 수" = "PARKING")), plotOutput("dong_treemap"),width = 6),
                #"동" 별 막대그래프 bar
                box(title = "bar",class = "custom-box", style = "height: 500px;", plotOutput("bar_dong"),width = 6)),
              fluidRow(
                #"동" 별 테이블
                box(title = "table",class = "custom-box",DTOutput("table_dong"), width = 12 )),
              fluidRow(
                box(title = "Correlation plot", class = "custom-box", plotOutput("correlation_plot"), width = 6),
                box(title = "Correlation plot", class = "custom-box", plotOutput("correlation_plot02"), width = 6)
              )),

      # 클러스터링에 대한 메뉴
      tabItem(tabName = "clustering",
              tabsetPanel(id = "clustering_tab",
                          tabPanel("Cluster 1", class = "custom-tab",
                                   fluidRow(
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster1"), width = 12),
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster1"), width = 8),
                                     box(title = "cluster_centers",class = "custom-box", plotOutput("centers1"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster1"), width = 12),
                                   )),
                          tabPanel("Cluster 2",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster2"), width = 12),
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster2"), width = 8),
                                     box(title = "cluster_centers",class = "custom-box", plotOutput("centers2"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster2"), width = 12)
                                   )),
                          tabPanel("Cluster 3",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster3"), width = 12),
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster3"), width = 8),
                                     box(title = "cluster_centers",class = "custom-box", plotOutput("centers3"), width = 4),
                                     box(title = "Table", class = "custom-box",DTOutput("table_cluster3"), width = 12)
                                   )),
                          tabPanel("Cluster 4",class = "custom-tab",
                                   fluidRow(
                                     box(title = "Description",class = "custom-box", verbatimTextOutput("desc_cluster4"), width = 12),
                                     box(title = "map",class = "custom-box", plotOutput("map_cluster4"), width = 8),
                                     box(title = "cluster_centers",class = "custom-box", plotOutput("centers4"), width = 4),
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


  # Open rates distribution
  output$open_rate_distribution <- renderPlot({
    # 개업률 분포 그리기
    ggplot(data_final_M, aes(x=OP_RATE)) +
      geom_histogram(bins=30, fill='#aec6cf', color='black') +
      labs(x='개업률', y='Count', title='개업률 분포') +
      theme_minimal()
  })

  # Closed rates distribution
  output$closed_rate_distribution <- renderPlot({
    # 폐업률 분포 그리기
    ggplot(data_final_M, aes(x=CLS_RATE)) +
      geom_histogram(bins=30, fill='#aec6cf', color='black') +
      labs(x='폐업률', y='Count', title='폐업률 분포') +
      theme_minimal()
  })

  # Rates table
  output$table_rates <- renderDT({
    # 개업률과 폐업률을 포함한 데이터프레임 생성
    rates_data <- data_final_M %>%
      select(SIGUNGU_NM, DONG_NM, OP_RATE, CLS_RATE)

    datatable(rates_data, options = list(pageLength = 25))
  })

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
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu),CLS_RATE) %>% ###CLS_RATE
        mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
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
  output$bar_dong <- renderPlot({
    dong_top20 <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarize(Total = sum(get(input$indicator_dong))) %>%
      top_n(20, Total) #상위 20개
    ggplot(dong_top20, aes(x = reorder(DONG_NM, -Total), y = Total, fill = Total)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#aec6cf", high = "#A2866A") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # 이 부분을 추가하였습니다.
      labs(x = "Dong", y = input$indicator_dong)
  })


  output$correlation_plot <- renderPlot({
    # 사용자가 선택한 변수 이름 가져오기
    selected_indicator <- input$indicator_dong

    # 해당 변수가 숫자형인지 확인
    if(is.numeric(data_final_M[[selected_indicator]])){
      # 'CLS_MK_NUM'과 선택한 변수 간의 산점도
      p <- ggplot(data_final_M, aes_string(x = selected_indicator, y = "CLS_MK_NUM")) +
        geom_point() +
        geom_smooth(method = "lm", col = "red") + # 선형회귀선 추가
        labs(x = selected_indicator, y = "Closure Rate",
             title = paste("Scatterplot of", selected_indicator, "and Closure Rate"))
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
    cor_matrix <- cor(data_numeric, use="pairwise.complete.obs")

    # 상관관계 플롯
    corrplot(cor_matrix, method = "circle")
  })




  # 테이블 (전체 동에 대한 테이블)
  output$table_dong <- DT::renderDataTable({
    datatable(
      data_final_M %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_dong),CLS_RATE) %>% ###CLS_RATE
        mutate(CLS_RATE = round(CLS_RATE, 4)) %>%
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

    # 클러스터 1
    output$centers1 <- renderPlot({
      ggplot(cluster_centers_long[cluster_centers_long$Cluster == 1,], aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 1")
    })

    # 클러스터 2
    output$centers2 <- renderPlot({
      ggplot(cluster_centers_long[cluster_centers_long$Cluster == 2,], aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 2")
    })

    # 클러스터 3
    output$centers3 <- renderPlot({
      ggplot(cluster_centers_long[cluster_centers_long$Cluster == 3,], aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 3")
    })

    # 클러스터 4
    output$centers4 <- renderPlot({
      ggplot(cluster_centers_long[cluster_centers_long$Cluster == 4,], aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Variable", y = "Normalized Center Value", title = "Cluster 4")
    })



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
    "Cluster 1: 점포 수는 적지만 주차장과 인구 밀도가 높은 도시 중심지역

     Cluster 1: 이 클러스터는 주차장 수가 많고, 인구 밀도와 유동인구가 매우 높지만, 상대적으로 점포 수가 적은 지역을 표현합니다. 
이러한 특성은 큰 도시의 중심지역 또는 주거 밀집 지역일 가능성이 높습니다. 
이 지역은 많은 사람들이 모이는 곳이지만, 점포 수가 상대적으로 적어 경쟁이 비교적 덜 치열할 수 있습니다."
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
    " Cluster 2: 점포 수는 많지만 인구 밀도와 주차장은 적은 상업적인 도심 지역

이 클러스터는 점포 수가 가장 많지만, 인구 밀도와 유동인구, 주차장 수가 상대적으로 적은 지역을 나타냅니다.
이는 상업 지역이나 상가가 집중되어 있는 도심의 한 지역일 가능성이 있습니다.
비록 사람들의 유동이 덜하지만, 다양한 종류의 점포가 많이 위치해 있을 것으로 추측됩니다."
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
    "Cluster 3: 점포 수와 인구 밀도, 주차장이 모두 중간 정도인 도시 외곽 또는 주거 지역

Cluster 3: 이 클러스터는 점포 수가 중간 정도에 위치하며, 인구 밀도와 유동인구가 상대적으로 높은 지역을 표현합니다.
이는 도시의 외곽 지역이나 주거 지역일 수 있으며, 상당히 높은 인구 밀도와 유동인구를 보유하고 있어 점포들에게 고객 유치에 유리할 수 있습니다."
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
    "Cluster 4: 점포 수는 적지만 인구 밀도와 주차장은 높은 주거 지역 또는 도심 주변 지역

이 클러스터는 인구 밀도와 유동인구, 주차장 수가 비교적 높은 반면, 점포 수는 상대적으로 적은 지역을 나타냅니다.
이는 발전된 주거 지역이나 상업적 활동이 상대적으로 덜한 도심 주변의 지역일 가능성이 있습니다.
이러한 지역은 점포 수 대비 인구 밀도와 유동인구가 높아, 새로운 점포에게 큰 기회를 제공할 수 있습니다."
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

