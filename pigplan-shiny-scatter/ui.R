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
      <h3>【 산점도 】</h3>
      </div>
      "
    ),
    br(),
    br(),

    wellPanel(
      fluidRow(column(
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
      )),

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
        column(4,
               selectInput(
                 "xy_scale",
                 "▷ 규모 선택",
                 list(
                   "전체" = "전체",
                   "100두 미만" = 180001,
                   "100~200두" = 180002,
                   "200~300두" = 180003,
                   "300~400두" =  180004,
                   "400~500두" = 180005,
                   "500~1000두" = 180006,
                   "1000두 이상" = 180007
                 ),
                 selected = "전체"
               )),
        column(4,
               selectInput(
                 "xy_region",
                 "▷ 지역 선택",
                 list(
                   "전국" = "전국",
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
                 selected = "전국"
               ))
      )
    ),

    br(),
    plotlyOutput("plot_scatter"),
    br(),br(),
    verbatimTextOutput("plot_summary"),
    br()

    )
  )
