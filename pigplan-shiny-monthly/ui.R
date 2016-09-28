library(shiny)
library(shinythemes)
library(plotly)

shinyUI(
  fluidPage(
    
    includeCSS("../public/styles.css"),
    
    theme = shinytheme("readable"),
    
    br(),
    br(),
    HTML(
      "
      <div style='text-align:center;'>
      <h1>
      <span style='white-space: nowrap;'>피그플랜 </span>
      <span style='white-space: nowrap;'>빅데이터 서비스</span>
      </h1>
      <h3>【 월간지표 변화 】</h3>
      </div>
      "
    ),
    br(),
    br(),
    
    wellPanel(
      radioButtons(
        "month_var",
        label = h4("▷ 변수 선택"),
        choices = list(
          "수태율" = "SUTAE",
          "분만율" = "BUNMAN",
          "평균총산" = "CHONGSAN",
          "평균실산" = "SILSAN",
          "평균이유" = "EUDUSU",
          "발정재귀일" = "ZAEGI",
          "7일 내 재귀율" = "ZAEGI7"
        ),
        selected = "SUTAE",
        inline = TRUE
      ),
      style = "text-align:center; padding-left:15%; padding-right:15%; margin-bottom:0; font-size:120%;"
    ),
    
    hr(style = "border-color:black;border-width:2px;"),
    
    wellPanel(h4("월 선택 차트", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),
    
    fluidRow(column(
      6,
      sliderInput(
        "month",
        "▷ 월 선택",
        min = 1,
        max = 12,
        value = 7
      )
    )),
    
    plotlyOutput("plot2a"),
    
    hr(style = 'border-color:black;border-width:2px;'),
    
    wellPanel(h4("연도 선택 차트", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),
    
    fluidRow(column(
      6,
      sliderInput(
        "month_year",
        "▷ 연도 선택",
        min = 2000,
        max = 2015,
        value = 2015
      )
    ), column(
      6
    )),
    
    plotlyOutput("plot2b"),
    
    br()
    
    )
  )
