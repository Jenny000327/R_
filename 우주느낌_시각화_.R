install.packages("treemapify")
install.packages("ggiraph", dependencies = True, INSTALL_opts = "--no-lock")
install.packages("showtext")
install.packages("magrittr")
install.packages("shiny")
install.packages("ggplot2")
install.packages("DT")
install.packages("dplyr")
library(readr)
data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")
View(data_final_M)
data_final_M %>% is.na() %>% apply(2,sum)
data_final_M %>% filter(is.na(DT))
str(data_final_M)

# Required libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(treemapify)
library(ggiraph)  # for interactive plots
library(showtext)  # for font manipulation
library(magrittr)
# Define the Korean font
font_add_google("Noto Sans KR", "korean")

# Use the font in plots
showtext_auto()

# Data preprocessing
data_final_M <- read_csv("C:/data/preprocessed/data_final.csv")
data_final_M %>% is.na() %>% apply(2,sum)
data_final_M %>% filter(is.na(DT))
str(data_final_M)

ui <- fluidPage(
  titlePanel("Seoul Store Analysis Dashboard"),
  
  tags$head(tags$style(HTML("
                            body {
                              background-color: #000033;
                              color: #cccccc;
                            }
                            "))),
  
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
      fluidRow(
        column(12,
               h2("Total Indicator by Gu"),
               plotOutput("gu_treemap")
        )
      ),
      selectInput("gu", "Select a Gu:", choices = unique(data_final_M$SIGUNGU_NM)),
      
      fluidRow(
        column(12,
               h2("Selected Gu's Dong as a Table"),
               DT::dataTableOutput("gu_dong_table")
        )
      ),
      fluidRow(
        column(12,
               h2("Selected Gu's Dong as a Donut Chart"),
               girafeOutput("gu_dong_donutplot")  # Use girafeOutput instead of plotOutput
        )
      ),
      h2("Total Indicator by Dong"),
      tabsetPanel(
        id = "panel",
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Bar Plot", plotOutput("barplot")),
        tabPanel("Top 20 Dong Treemap", plotOutput("top20_dong_treemap")),
        tabPanel("Top 10 Donut Chart", girafeOutput("top10_dong_donutplot"))  # Use girafeOutput instead of plotOutput
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$gu_treemap <- renderPlot({
    df <- data_final_M %>%
      group_by(SIGUNGU_NM) %>%
      summarise(total = sum(!!sym(input$indicator))) %>%
      ungroup()
    
    ggplot(df, aes(area = total, label = SIGUNGU_NM, fill = total)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "white", place = "center", grow = TRUE, family = "korean") +
      scale_fill_gradient(low = "#000033", high = "steelblue") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = '#000033', colour = '#000033'),
        panel.background = element_rect(fill = '#000033'),
        panel.grid.major = element_line(colour = "#cccccc"),
        panel.grid.minor = element_line(colour = "#cccccc"),
        axis.text = element_text(colour = "#cccccc"),
        axis.title = element_text(colour = "#cccccc"),
        plot.title = element_text(colour = "#cccccc")
      ) +
      labs(title = paste("Treemap of", input$indicator, "by Gu"), fill = input$indicator)
  })
  
  output$gu_dong_table <- DT::renderDataTable({
    gu_df <- data_final_M %>%
      filter(SIGUNGU_NM == input$gu) %>%
      select(DONG_NM, !!sym(input$indicator))
    
    sorted_data <- gu_df %>%
      arrange(desc(!!sym(input$indicator)))
    datatable(sorted_data, options = list(pageLength = 10, 
                                          initComplete = JS('function(settings, json) {',
                                                            '$(".dataTable").css("background-color", "#000033");',
                                                            '$(".dataTable").css("color", "#FFFFFF");',  # Modify the text color to white
                                                            '}')))
  })
  
  output$gu_dong_donutplot <- renderGirafe({
    gu_df <- reactive({
      data_final_M %>%
        filter(SIGUNGU_NM == input$gu) %>%
        select(DONG_NM, !!sym(input$indicator))
    })
    
    df <- gu_df() %>%
      group_by(DONG_NM) %>%
      summarise(total = sum(!!sym(input$indicator))) %>%
      arrange(desc(total))
    
    p <- ggplot(df, aes(x = "", y = total, fill = DONG_NM)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = paste0(DONG_NM, "\n", total)), position = position_stack(vjust = 0.5), color = "white", family = "korean") +  # Add labels to donut plot
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "none") +
      labs(title = paste("Donut Chart of Selected Gu's Dong by", input$indicator), fill = input$indicator)
    
    girafe(code = print(p))  # Create interactive plot with girafe
  })
  
  output$table <- DT::renderDataTable({
    sorted_data <- data_final_M %>%
      arrange(desc(!!sym(input$indicator))) %>%
      select(SIGUNGU_NM, DONG_NM, !!sym(input$indicator)) 
    datatable(sorted_data, options = list(pageLength = 10, 
                                          initComplete = JS('function(settings, json) {',
                                                            '$(".dataTable").css("background-color", "#000033");',
                                                            '$(".dataTable").css("color", "#cccccc");',
                                                            '}')))
  })
  
  output$barplot <- renderPlot({
    top20 <- data_final_M %>%
      arrange(desc(!!sym(input$indicator))) %>%
      head(20)
    ggplot(top20, aes(x = reorder(DONG_NM, -!!sym(input$indicator)), y = !!sym(input$indicator))) +
      geom_bar(stat = 'identity', fill = '#2ca02c') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, color = '#cccccc')) +
      theme(axis.text.y = element_text(color = '#cccccc')) +
      theme(plot.background = element_rect(fill = '#000033')) +
      theme(panel.background = element_rect(fill = '#000033')) +
      theme(panel.border = element_rect(colour = "#cccccc", fill=NA)) +
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
      geom_treemap_text(fontface = "italic", colour = "white", place = "center", grow = TRUE, family = "korean") +
      # geom_treemap_text(fontface = "italic", place = "center", grow = TRUE) +
      scale_fill_gradient(low = "#000033", high = "steelblue") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = '#000033', colour = '#000033'),
        panel.background = element_rect(fill = '#000033'),
        panel.grid.major = element_line(colour = "#cccccc"),
        panel.grid.minor = element_line(colour = "#cccccc"),
        axis.text = element_text(colour = "#cccccc"),
        axis.title = element_text(colour = "#cccccc"),
        plot.title = element_text(colour = "#cccccc")
      ) +
      labs(title = paste("Treemap of Top 20 DONG by", input$indicator), fill = input$indicator)
  })
  
  output$top10_dong_donutplot <- renderGirafe({  # Use renderGirafe instead of renderPlot
    df <- data_final_M %>%
      group_by(DONG_NM) %>%
      summarise(total = sum(!!sym(input$indicator))) %>%
      arrange(desc(total)) %>%
      head(10)  # select top 10 동 only
    
    p <- ggplot(df, aes(x = "", y = total, fill = DONG_NM)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = paste0(DONG_NM, "\n", total)), position = position_stack(vjust = 0.5), color = "white", family = "korean") + 
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "none")
    
    
    girafe(code = print(p))  # Create interactive plot with girafe
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
