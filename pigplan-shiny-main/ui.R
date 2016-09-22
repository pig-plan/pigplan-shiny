library(shiny)
library(shinythemes)
library(plotly)
library(DT)

shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  HTML("<br>
        <div style='text-align:center;'>
          <h1>
            <span style='white-space: nowrap;'>피그플랜 </span>
            <span style='white-space: nowrap;'>빅데이터 서비스</span>
        </div>
        <br><hr>"),
  
  h3("1. 주요지표 변화"),
  
  HTML("<br>"),
  
  fluidRow(
    column(4,
      selectInput(
        "year_var",
        "▷ 변수 선택",
        c(
          "상시모돈",
          "모돈평균산차",
          "평균도폐산차",
          "PSY",
          "모돈회전율",
          "평균이유두수",
          "평균총산",
          "평균실산",
          "평균사산",
          "평균미라",
          "임신기간",
          "포유기간",
          "비생산일수",
          "비생산일_이유_교배",
          "비생산일_이유_도폐사",
          "비생산일_교배_사고",
          "비생산일_사고_교배",
          "비생산일_사고_도폐사",
          "분만율",
          "재귀발정일",
          "재귀율_7일_이내",
          "최초교배일령"
        ),
        selected = "PSY"
      )
    )
  ),
  
  fluidRow(column(
    12,
    tabsetPanel(
      type = "tabs",
      tabPanel("평균", br(), plotlyOutput("plot1a")),
      tabPanel(
        "사분위",
        br(),
        plotlyOutput("plot1_box")
      ),
      tabPanel(
        "지역 구분",
        br(),
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
        plotlyOutput("plot1b_jj")
      ),
      tabPanel(
        "규모 구분",
        br(),
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
        plotlyOutput("plot1c_o1000")
      )
    )
  )),
  
  HTML("<br><hr>"),
  
  h3("2. 월별지표 변화"),
  
  HTML("<br>"),
  
  fluidRow(
    column(
      4,
      selectInput(
        "month_var",
        "▷ 변수 선택",
        list(
          "수태율" = "SUTAE",
          "분만율" = "BUNMAN",
          "평균총산" = "CHONGSAN",
          "평균실산" = "SILSAN",
          "평균이유" = "EUDUSU",
          "발정재귀일" = "ZAEGI",
          "7일 내 재귀율" = "ZAEGI7"
        ),
        selected = "SUTAE"
      )
    ),
    column(
      4,
      sliderInput(
        "month",
        "▷ 월 선택",
        min = 1,
        max = 12,
        value = 7
      )
    ),
    column(
      4,
      sliderInput(
        "month_year",
        "▷ 연도 선택",
        min = 2000,
        max = 2015,
        value = 2010
      )
    )
  ),
  
  fluidRow(

    column(
      12,
      tabsetPanel(
        type = "tabs",
        tabPanel("월 선택 차트", br(), plotlyOutput("plot2a")),
        tabPanel("연도 선택 차트", br(), plotlyOutput("plot2b"))
      )
    )
  ),
  
  HTML("<br><hr>"),
  
  h3("3. 산점도"),
  
  HTML("<br>"),
  
  fluidRow(
    column(
      4,
      selectInput(
        "yvar",
        "▷ Y 축 변수 선택",
        list(
          "연도" = "YEAR",
          "상시모돈" = "상시모돈",
          "모돈평균산차" = "모돈평균산차",
          "평균도폐산차" = "평균도폐산차",
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
          "비생산일(이유-교배)" = "비생산일(이유-교배)",
          "비생산일(이유-도폐사)" = "비생산일(이유-도폐사)",
          "비생산일(교배-사고)" = "비생산일(교배-사고)",
          "비생산일(사고-교배)" = "비생산일(사고-교배)",
          "비생산일(사고-도폐사)" = "비생산일(사고-도폐사)",
          "분만율" = "분만율",
          "재귀발정일" = "재귀발정일",
          "7일 내 재귀율" = "7일 내 재귀율",
          "최초교배일령" = "최초교배일령"
        ),
        selected = "평균이유두수"
      )
    ),
    column(
      4,
      selectInput(
        "xvar",
        "▷ X 축 변수 선택",
        list(
          "연도" = "YEAR",
          "상시모돈" = "상시모돈",
          "모돈평균산차" = "모돈평균산차",
          "평균도폐산차" = "평균도폐산차",
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
        selected = "평균총산"
      )
    )
  ),
  
  fluidRow(
    column(
      4,
      sliderInput(
        "scatter_years",
        "▷ 기간 선택",
        min = 2000,
        max = 2015,
        value = c(2000, 2015)
      )
    ),
    column(
      4,
      selectInput(
        "xy_scale",
        "▷ 규모 선택",
        list(
          "100두 미만" = 180001,
          "100~200두" = 180002,
          "200~300두" = 180003,
          "300~400두" =  180004,
          "400~500두" = 180005,
          "500~1000두" = 180006,
          "1000두 이상" = 180007
        ),
        selected = 180001
      )
    ),
    column(
      4,
      selectInput(
        "xy_region",
        "▷ 지역 선택",
        list(
          "서울경기" = 10001,
          "충청남도" = 10002,
          "충청북도" = 10003,
          "전라남도" = 10004,
          "전라북도" = 10005,
          "경상남도" = 10006,
          "경상북도" = 10007,
          "강원도" = 10008,
          "제주도" = 10009
        ),
        selected = 10001
      )
    )
  ),
  
  fluidRow(
    column(
      12,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "기간별 경향성",
          br(),
          plotlyOutput("plot5a"),
          br(),
          verbatimTextOutput("plot5a_summary")
        ),
        tabPanel(
          "규모 구분",
          br(),
          plotlyOutput("plot5b"),
          br(),
          verbatimTextOutput("plot5b_summary")
        ),
        tabPanel(
          "지역 구분",
          br(),
          plotlyOutput("plot5c"),
          br(),
          verbatimTextOutput("plot5c_summary")
        )
      )
    )
  ),
  
  HTML("<br><hr>"),
  
  h3("4. 원시 데이터"),
  
  HTML("<br>"),
  
  fluidRow(
    
    column(
      12,
      tabsetPanel(
        type = "tabs",
        tabPanel("연도별 종합자료", dataTableOutput("raw_data1")),
        tabPanel("월별 번식지표", dataTableOutput("raw_data2")),
        tabPanel(
          "다운로드",
          br(),
          h5("▣ 연도별 종합자료"),
          HTML('&nbsp;'),
          downloadButton('dn1', '데이터 다운로드'),
          br(),
          hr(),
          h5("▣ 월별 번식지표"),
          HTML('&nbsp;'),
          downloadButton('dn2', '데이터 다운로드'),
          br(),br(),
          HTML('<div style="height:300px;"></div>')
        )
      ) 
    )
    
  )
  
))