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
      <h3>【 주요지표 변화 】</h3>
      </div>
      "
    ),
    br(),
    br(),

    wellPanel(
      radioButtons(
        "year_var",
        label = h4("▷ 변수 선택"),
        choices = list(
          "상시모돈" = "상시모돈",
          "모돈평균산차" = "모돈평균산차",
          "모돈평균산차" = "평균도폐산차",
          "PSY" = "PSY",
          "모돈회전율" = "모돈회전율",
          "평균이유두수" = "평균이유두수",
          "평균총산" = "평균총산",
          "평균실산" = "평균실산",
          "평균사산" = "평균사산",
          "평균미라" = "평균미라",
          "임신기간" = "임신기간",
          "포유기간" = "포유기간",
          "비생산일수" = "비생산일수",
          "비생산일_이유_교배" = "비생산일_이유_교배",
          "비생산일_이유_도폐사" = "비생산일_이유_도폐사",
          "비생산일_교배_사고" = "비생산일_교배_사고",
          "비생산일_사고_교배" = "비생산일_사고_교배",
          "비생산일_사고_도폐사" = "비생산일_사고_도폐사",
          "분만율" = "분만율",
          "재귀발정일" = "재귀발정일",
          "재귀율_7일_이내" = "재귀율_7일_이내",
          "최초교배일령" = "최초교배일령"
        ),
        selected = "상시모돈",
        inline = TRUE
      ),
      style = "text-align:center; padding-left:15%; padding-right:15%; margin-bottom:0; font-size:120%;"
    ),

    hr(style = "border-color:black;border-width:2px;"),

    wellPanel(h4("평균", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    plotlyOutput("plot1a"),

    hr(style = 'border-color:black;border-width:2px;'),

    wellPanel(h4("사분위", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    plotlyOutput("plot1_box"),

    hr(style = 'border-color:black;border-width:2px;'),

    wellPanel(h4("지역구분", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    plotlyOutput("plot1b_sg"),
    br(),
    plotlyOutput("plot1b_cb"),
    br(),
    plotlyOutput("plot1b_cn"),
    br(),
    plotlyOutput("plot1b_jb"),
    br(),
    plotlyOutput("plot1b_jn"),
    br(),
    plotlyOutput("plot1b_gb"),
    br(),
    plotlyOutput("plot1b_gn"),
    br(),
    plotlyOutput("plot1b_gw"),
    br(),
    plotlyOutput("plot1b_jj"),

    hr(style = 'border-color:black;border-width:2px;'),

    wellPanel(h4("규모구분", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    plotlyOutput("plot1c_u100"),
    br(),
    plotlyOutput("plot1c_o100"),
    br(),
    plotlyOutput("plot1c_o200"),
    br(),
    plotlyOutput("plot1c_o300"),
    br(),
    plotlyOutput("plot1c_o400"),
    br(),
    plotlyOutput("plot1c_o500"),
    br(),
    plotlyOutput("plot1c_o1000"),

    br()
    )
  )
