#일단 이거.

#install.packages("treemapify")
library(readr)
data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")
View(data_final_M)
data_final_M %>% is.na() %>% apply(2,sum)
data_final_M %>% filter(is.na(DT))
str(data_final_M)

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(treemapify)

# User Interface
ui <- fluidPage(
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
      ),
      selectInput("gu", "Select a Gu:", choices = unique(data_final_M$SIGUNGU_NM)),
      uiOutput("dong")
    ),
    mainPanel(
      plotOutput("gu_barplot"),
      DT::dataTableOutput("dong_table"),
      tabsetPanel(
        id = "panel",
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Bar Plot", plotOutput("barplot")),
        tabPanel("Donut Chart", plotOutput("donutplot")),
        tabPanel("Top 20 Dong Treemap", plotOutput("top20_dong_treemap"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$dong <- renderUI({
    df <- data_final_M %>% filter(SIGUNGU_NM == input$gu)
    selectInput("dong", "Select a Dong:", choices = unique(df$DONG_NM))
  })
  
  output$gu_barplot <- renderPlot({
    df <- data_final_M %>%
      group_by(SIGUNGU_NM) %>%
      summarise(total = sum(!!sym(input$indicator))) %>%
      ungroup()
    
    ggplot(df, aes(x = reorder(SIGUNGU_NM, -total), y = total)) +
      geom_bar(stat = 'identity', fill = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Gu", y = input$indicator, title = paste("Bar Plot of", input$indicator, "by Gu"))
  })
  
  output$dong_table <- DT::renderDataTable({
    selected_dong <- reactive({
      data_final_M %>%
        filter(SIGUNGU_NM == input$gu, DONG_NM == input$dong) %>%
        select(DONG_NM, !!sym(input$indicator))
    })
    
    DT::datatable(selected_dong())
  })
  
  output$table <- DT::renderDataTable({
    sorted_data <- data_final_M %>%
      arrange(desc(!!sym(input$indicator))) %>%
      select(SIGUNGU_NM, DONG_NM, !!sym(input$indicator)) 
    DT::datatable(sorted_data)
  })
  
  output$barplot <- renderPlot({
    selected_dong <- data_final_M %>%
      filter(SIGUNGU_NM == input$gu, DONG_NM == input$dong)
    
    ggplot(selected_dong, aes(x = "", y = !!sym(input$indicator), fill = DONG_NM)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(x = "", y = "", 
           title = paste(input$indicator, "in", input$dong, "of", input$gu)) +
      geom_text(aes(label = paste0(round(!!sym(input$indicator) / sum(!!sym(input$indicator)) * 100, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      theme(legend.position = "none")
  })
  
  output$donutplot <- renderPlot({
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
}

# Run the application
shinyApp(ui = ui, server = server)

             