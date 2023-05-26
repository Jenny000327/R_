#install.packages("treemapify")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("ggmap")
# install.packages("ggmap")
# install.packages("ggplot2")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("maptools")
#install.packages("rgdal")
#install.packages("shinythemes")
#install.packages("shiny.semantic")

#-----------------------------------------------------------

library(ggmap)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(tidyverse)
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(treemapify)
library(raster)
library(readr)
library(magrittr)
#library(shinydashboard)

data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")
View(data_final_M)
data_final_M %>% is.na() %>% apply(2,sum)
data_final_M %>% filter(is.na(DT))
str(data_final_M)




#사용자 Ui 정의
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  titlePanel("Seoul Store Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select an Indicator:",
                  choices = c("Store Count" = "MK_NUM",
                              "Open Store Count" = "OP_MK_NUM",
                              "Closed Store Count" = "CLS_MK_NUM",
                              "Franchise Store Count" = "FRC_MK_NUM",
                              "Inflow" = "IN",
                              "Outflow" = "OUT",
                              "Population Density" = "DT",
                              "Parking Count" = "PARKING")
      )
    ),

    mainPanel(
      #ggmap 추가
      # fluidRow(
      #   column(12,
      #          h2("Total Indicator by Dong"),
      #          plotOutput("map")
      #   )
      # ),
      # tags$br(),

      # 이미지 추가
      fluidRow(
        column(12, img(src = "www/mk.png", alt = "Total Store Count", width = "100%"))
      ),
      fluidRow(
        column(12, img(src = "www/op.png", alt = "Open Store Count", width = "100%"))
      ),
      fluidRow(
        column(12, img(src = "www/cls.png", alt = "Closed Store Count", width = "100%"))
      ),

      fluidRow(
        column(12,
               h2("Total Indicator by Gu"),
               plotOutput("gu_treemap")
        )
      ),
      tags$br(),
      tags$br(),
      selectInput("gu", "Select a Gu:", choices = unique(data_final_M$SIGUNGU_NM)),

      fluidRow(
        column(6,
               h2("Selected Gu's Dong as a Table"),
               DT::dataTableOutput("gu_dong_table")
        ),
        column(6,
               h2("Selected Gu's Dong as a Donut Chart"),
               plotOutput("gu_dong_donutplot")
        )
      ),
      tags$br(),
      tags$br(),
      h2("Total Indicator by Dong"),
      tabsetPanel(
        id = "panel",
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Bar Plot", plotOutput("barplot")),
        tabPanel("Top 20 Dong Treemap", plotOutput("top20_dong_treemap")),
        tabPanel("Top 10 Donut Chart", plotOutput("top10_dong_donutplot"))
      )
    )
  )
)


#server 부분 정의
server <- function(input, output, session) {

  #ggmap 추가
  # output$map <- renderPlot({
  #
  #   location <- c(lon = 127.047, lat = 37.557)
  #   zoom <- 12
  #
  #   map <- ggmap(get_map(location = location, zoom = zoom))
  #
  #   print(map)
  # })

output$gu_treemap <- renderPlot({
  df <- data_final_M %>%
    group_by(SIGUNGU_NM) %>%
    summarise(total = sum(!!sym(input$indicator))) %>%
    ungroup()

  ggplot(df, aes(area = total, label = SIGUNGU_NM, fill = total)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", place = "center", grow = TRUE) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(title = paste("Treemap of", input$indicator, "by Gu"), fill = input$indicator)
})

output$gu_dong_table <- DT::renderDataTable({
  gu_df <- data_final_M %>%
    filter(SIGUNGU_NM == input$gu) %>%
    dplyr::select(DONG_NM, !!sym(input$indicator))

  sorted_data <- gu_df %>%
    arrange(desc(!!sym(input$indicator)))
  DT::datatable(sorted_data)
})

output$gu_dong_donutplot <- renderPlot({
  gu_df <- reactive({
    data_final_M %>%
      filter(SIGUNGU_NM == input$gu) %>%
      dplyr::select(DONG_NM, !!sym(input$indicator))
  })

  df <- gu_df() %>%
    group_by(DONG_NM) %>%
    summarise(total = sum(!!sym(input$indicator))) %>%
    arrange(desc(total))

  ggplot(df, aes(x = "", y = total, fill = DONG_NM)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(label = paste0(DONG_NM, " ", round(total / sum(total) * 100, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    theme(legend.position = "none") +
    labs(title = paste("Donut Chart of Selected Gu's Dong by", input$indicator), fill = input$indicator)
})

output$table <- DT::renderDataTable({
  sorted_data <- data_final_M %>%
    arrange(desc(!!sym(input$indicator))) %>%
    dplyr::select(SIGUNGU_NM, DONG_NM, !!sym(input$indicator))
  DT::datatable(sorted_data)
})


output$barplot <- renderPlot({
  top20 <- data_final_M %>%
    arrange(desc(!!sym(input$indicator))) %>%
    head(20)
  ggplot(top20, aes(x = reorder(DONG_NM,
                                -!!sym(input$indicator)), y = !!sym(input$indicator))) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Dong") +
    ylab(input$indicator)
})


output$top20_dong_treemap <- renderPlot({
  top20 <- data_final_M %>%
    arrange(desc(!!sym(input$indicator))) %>%
    head(20)

  df <- top20 %>%
    group_by(DONG_NM) %>%
    summarise(total = sum(!!sym(input$indicator))) %>%
    arrange(desc(total))

  ggplot(df, aes(area = total, label = DONG_NM, fill = total)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", place = "center", grow = TRUE) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(title = paste("Treemap of Top 20 DONG by", input$indicator), fill = input$indicator)
})

output$top10_dong_donutplot <- renderPlot({
  df <- data_final_M %>%
    group_by(DONG_NM) %>%
    summarise(total = sum(!!sym(input$indicator))) %>%
    arrange(desc(total)) %>%
    head(10)  # top 10 동만 선택

  ggplot(df, aes(x = "", y = total, fill = DONG_NM)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(label = paste0(DONG_NM, " ", round(total / sum(total) * 100, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    theme(legend.position = "none")
})

}

# Run the application
shinyApp(ui = ui, server = server)
