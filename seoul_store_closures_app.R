# 라이브러리 불러오기. Library Imports.
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(treemapify)
library(readr)
library(RColorBrewer)

# data_final_M 데이터.
data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")


#사용자 Ui 부분 
ui <- dashboardPage(

  dashboardHeader(title = "서울시 동별 폐업률"), #대시보드 해더
  
  #사이드 메뉴 부분 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("현황 지도로 보기", tabName = "map", icon = icon("globe")),
      menuItem("Total Indicator by Gu", tabName = "gu", icon = icon("bar-chart-o")),
      menuItem("Total Indicator by Dong", tabName = "dong", icon = icon("bar-chart-o"))
    )
  ),
 
   #바디 부분
  dashboardBody(  
    includeCSS("www/custom.css"),
    
    tabItems(
      #Home 메뉴 (이 프로젝트에 대한 소개 더 추가! 사진도 넣구 글도 추가해주고 이 페이지가 처음 보이게 되는 페이지.)
      tabItem(tabName = "home",
              h2("서울시 동별 폐업률 분석"),
              p("이 프로젝트는 서울시 동별 폐업률을 분석하는 것을 목표로 합니다.")),
      
      #현황 지도로 보기 메뉴 (메뉴 이름 변경?)  
      #동별 지도 구현이 가능하게 되면 이 부분을 지도로 변경
      tabItem(tabName = "map", #각각의 이미지들은 테블로에서 그려서 따옴. 구별 지도로만 현재 표현.
              fluidRow(
                box(title = "Total Store Count", imageOutput("mk_img"),width = 11)
              ),
              fluidRow(
                box(title = "Open Store Count", imageOutput("op_img"),width = 11)
              ),
              fluidRow(
                box(title = "Closed Store Count", imageOutput("cls_img"),width = 11)
              )),
      
      #원형 그래프는 뭔가 이쁜데, 글씨 조절하는걸 못해서 일단 트리맵, 막대그래프, 테이블 로만 표현.
      #정보를 구 단위로 시각화
      tabItem(tabName = "gu",
              fluidRow(
                #Indicators box , 정보 SelectInput과 "구" 별 tree map
                box(title = "Indicators", selectInput("indicator_gu", "Choose an indicator", choices = c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "DT", "IN", "OUT", "PARKING")), plotOutput("gu_treemap"), width = 6),
                # 구 selectInput과 "구" 별 막대 그래프 bar
                box(title = "Gu", selectInput("gu", "Choose a Gu", choices = unique(data_final_M$SIGUNGU_NM)), plotOutput("bar_gu"), width = 6)),
              fluidRow(
                #태이블
                box(title = "table", DTOutput("table_gu"), width = 12))),
      
      #정보를 동단위로 시각화
      tabItem(tabName = "dong", 
              fluidRow(
                #Indicators box , 정보 SelectInput과 "동" 별 tree map
                box(title = "Indicators", selectInput("indicator_dong", "Choose an indicator", choices = c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "DT", "IN", "OUT", "PARKING")), plotOutput("dong_treemap"),width = 6),
                #"동" 별 막대그래프 bar
                box(title = "bar",plotOutput("bar_dong"),width = 6)),
              fluidRow(
                #"동" 별 테이블 
                box(title = "table",DTOutput("table_dong"), width = 12 )))
      )
  )
)



#서버 부분
server <- function(input, output){

    # Image rendering #지도 표현 메뉴 부분, 지도 이미지.
    output$mk_img <- renderImage({
      list(src = "www/mk.png", contentType = "image/png", alt = "Total Store Count", width = 800, height = 400) #이미지 크기 조정 
    }, deleteFile = FALSE)

    output$op_img <- renderImage({
      list(src = "www/op.png", contentType = "image/png", alt = "Open Store Count", width = 800, height = 400)
    }, deleteFile = FALSE)

    output$cls_img <- renderImage({
      list(src = "www/cls.png", contentType = "image/png", alt = "Closed Store Count", width = 800, height = 400)
    }, deleteFile = FALSE)


    
    # GU related rendering 구와 관련된 코드들(트리맵, 막대그래프, 테이블)
    output$gu_treemap <- renderPlot({
      df <- data_final_M %>%
        group_by(SIGUNGU_NM) %>%
        summarise(total = sum(get(input$indicator_gu))) %>%
        ungroup()

      ggplot(df, aes(area = total, label = SIGUNGU_NM, fill = total)) +
        geom_treemap() +
        geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        labs(title = paste("Treemap of", input$indicator_gu, "by Gu"), fill = input$indicator_gu)
    })

    #컬러풀 색 상 순서X 막대그래프
    # output$bar_gu <- renderPlot({
    #   gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
    #   ggplot(gu_selected, aes(x = factor(DONG_NM), y = get(input$indicator_gu), fill = factor(DONG_NM))) +
    #     geom_bar(stat = "identity") +
    #     scale_fill_discrete() +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     labs(x = "Dong", y = input$indicator_gu)
    # })

    #컬러풀 색 상 순서0 막대그래프
    # output$bar_gu <- renderPlot({
    #   gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
    #   ggplot(gu_selected, aes(x = reorder(factor(DONG_NM), -get(input$indicator_gu)), y = get(input$indicator_gu), fill = factor(DONG_NM))) +
    #     geom_bar(stat = "identity") +
    #     scale_fill_discrete() +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     labs(x = "Dong", y = input$indicator_gu)
    # })

    # 그라데이션 막대그래프
    output$bar_gu <- renderPlot({
      gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
      ggplot(gu_selected, aes(x = reorder(factor(DONG_NM), -get(input$indicator_gu)), y = get(input$indicator_gu), fill = get(input$indicator_gu))) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Dong", y = input$indicator_gu)
    })
    
    #테이블
    output$table_gu <- renderDataTable({
      data_final_M %>%
        filter(SIGUNGU_NM == input$gu) %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu)) %>%
        arrange(-get(input$indicator_gu))
    })



    # DONG related rendering 동과 관련된 코드들 (트리맵, 막대그래프, 테이블)
    output$dong_treemap <- renderPlot({
      dong_top20 <- data_final_M %>%
        group_by(DONG_NM) %>%
        summarize(Total = sum(get(input$indicator_dong))) %>%
        top_n(20, Total)
      ggplot(dong_top20, aes(area = Total, fill = Total, label = DONG_NM)) +
        geom_treemap() +
        geom_treemap_text(fontface = "italic", place = "center", grow = TRUE, color = "white") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(fill = input$indicator_dong)
    })

    #막대그래프 색
    # output$bar_dong <- renderPlot({
    #   dong_top20 <- data_final_M %>%
    #     group_by(DONG_NM) %>%
    #     summarize(Total = sum(get(input$indicator_dong))) %>%
    #     top_n(20, Total)
    #   ggplot(dong_top20, aes(x = reorder(DONG_NM, -Total), y = Total, fill = factor(DONG_NM))) +
    #     geom_bar(stat = "identity") +
    #     scale_fill_discrete() +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     labs(x = "Dong", y = input$indicator_dong)
    # })
    
    #막대그래프 그라데이션
    output$bar_dong <- renderPlot({
      dong_top20 <- data_final_M %>%
        group_by(DONG_NM) %>%
        summarize(Total = sum(get(input$indicator_dong))) %>%
        top_n(20, Total)
      ggplot(dong_top20, aes(x = reorder(DONG_NM, -Total), y = Total, fill = Total)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Dong", y = input$indicator_dong)
    })


    # 테이블
    output$table_dong <- renderDataTable({
      data_final_M %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_dong)) %>%
        arrange(-get(input$indicator_dong))
    })
}

#샤이니 앱 실행
shinyApp(ui = ui, server = server)