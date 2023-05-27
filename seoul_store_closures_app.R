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
      
      # 현황 지도로 보기 메뉴 (메뉴 이름 변경?)
      # 현황 지도로 보기 메뉴
      tabItem(tabName = "map", # 이 메뉴 이름은 map
              fluidRow( 
                box(title = "Map",  #첫번째 박스에 지도 사진을 넣는데
                    tabsetPanel(id = "map_tab", # 그 안에 tabsetpanel을 넣어서 각각 눌러서 볼 수 있도록함.
                                tabPanel("Total", imageOutput("mk_img")), 
                                tabPanel("Open", imageOutput("op_img")),
                                tabPanel("Closed", imageOutput("cls_img"))
                    ), 
                    width = 12# 지도 크기를 화면 크기에 따라 유동적으로 변하게 만들었더니 전체화면으로 하면 밑에 지도가 짤려서 너비를 8로 두어야됨. 
                ),
                box(title = "Table", DTOutput("table_map"), width = 12) # 첫번째 박스 아래에 두번째 박스에는 테이블을 넣었어.
              ) #근데 이부분이 사이즈 조절을 좀 하고 싶은데 안이뻐. 밑에 위에 다 8로 맞추니까 밑에 테이블이 깨지고. 테이블 박스만 10으로 하자니 이상하고.
      ),
      
      #원형 그래프는 뭔가 이쁜데, 글씨 조절하는걸 못해서 일단 트리맵, 막대그래프, 테이블 로만 표현.
      #정보를 구 단위로 시각화
      tabItem(tabName = "gu",
              fluidRow(
                # Indicators box [이 박스 안에는 정보(요인들) SelectInput기능과  "구" 별 tree map이 들어있다.]
                box(title = "Indicators", selectInput("indicator_gu", "Choose an indicator", choices = c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "DT", "IN", "OUT", "PARKING")), plotOutput("gu_treemap"), width = 6),
                # GU 박스[구 selectInput기능과"구" 별 막대 그래프 가 들어있다.]
                box(title = "Gu", selectInput("gu", "Choose a Gu", choices = unique(data_final_M$SIGUNGU_NM)), plotOutput("bar_gu"), width = 6)),
              fluidRow(
                #구에 속한 동 태이블
                box(title = "table", DTOutput("table_gu"), width = 12))),
      
      #정보를 동단위로 시각화
      tabItem(tabName = "dong", 
              fluidRow(
                #Indicators box [정보(요인들) SelectInput 기능과 "동" 별 tree map 이 들어있다.] 
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
    
    
    # data_final_M_grouped로 테이블 렌더링 (지도 밑에있는 테이블 table_map)
    output$table_map <- renderDataTable({
      data_final_M_grouped %>%
        dplyr::select(SIGUNGU_NM, MK_NUM, CLS_MK_NUM, OP_MK_NUM) %>%
        arrange(-get(current_tab_table()))
    })
 
    
 
          
    
#구에 대한 정보 있는 메뉴 부분
    
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


    # 막대그래프
    output$bar_gu <- renderPlot({
      gu_selected <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
      ggplot(gu_selected, aes(x = reorder(factor(DONG_NM), -get(input$indicator_gu)), y = get(input$indicator_gu), fill = get(input$indicator_gu))) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Dong", y = input$indicator_gu)
    })
    
    #테이블(사용자가 선택한 구로 필터 걸어서 만든 테이블)
    output$table_gu <- renderDataTable({
      data_final_M %>%
        filter(SIGUNGU_NM == input$gu) %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_gu)) %>%
        arrange(-get(input$indicator_gu))
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
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
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
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Dong", y = input$indicator_dong)
    })


    # 테이블 (전체 동에대한 테이블)
    output$table_dong <- renderDataTable({
      data_final_M %>%
        dplyr::select(SIGUNGU_NM, DONG_NM, all_of(input$indicator_dong)) %>%
        arrange(-get(input$indicator_dong)) #사용자가 선택한 정보(요인)을 기준으로 내림차순 정렬
    })
}



#샤이니 앱 실행
shinyApp(ui = ui, server = server)

#나는 오늘 잠을 자지 못했다.
#나는 누구인가 여긴 어디인가.


