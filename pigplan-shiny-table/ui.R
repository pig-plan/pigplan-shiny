library(shiny)
library(shinythemes)
library(DT)

shinyUI(
  fluidPage(
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
      <h3>【 원시 데이터 】</h3>
      </div>
      "
    ),
    br(),
    br(),
    hr(),
    # hr(style = "border-color:black;border-width:2px;"),
    wellPanel(h4("연단위 종합 지표", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    dataTableOutput("raw_data1"),
    br(),
    downloadButton('dn1', '데이터 다운로드'),
    br(),
    br(),
    hr(style = "border-color:black;border-width:2px;"),
    wellPanel(h4("월단위 번식 지표", style = "text-align:center;"),
              style = "background-color:navy; color:white;"),

    dataTableOutput("raw_data2"),
    br(),
    downloadButton('dn2', '데이터 다운로드'),
    br(),
    br()

    )
        )
