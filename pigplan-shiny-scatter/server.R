library(ggplot2)
library(ggthemes)
library(plotly)
library(data.table)
library(shiny)
library(reshape2)
library(psych)

df <- read.csv("../data/df.csv", header = TRUE)

titleForm <- list(size = 14,
                  color = "navy")

scaleMap <- list(
  "전체" = "전체",
  "180001" = "100두 미만",
  "180002" = "100~200두",
  "180003" = "200~300두",
  "180004" = "300~400두",
  "180005" = "400~500두",
  "180006" = "500~1000두",
  "180007" = "1000두 이상"
)
regionMap <- list(
  "전국" = "전국",
  "10001" = "서울경기",
  "10002" = "충청남도",
  "10003" = "충청북도",
  "10004" = "전라남도",
  "10005" = "전라북도",
  "10006" = "경상남도",
  "10007" = "경상북도",
  "10008" = "강원도",
  "10009" = "제주도"
)

shinyServer(function(input, output) {
  scatter_data <- reactive({
    tmp <- data.table(df)
    if (input$xy_scale == "전체" & input$xy_region == "전국") {
      tmp <-
        tmp[tmp$YEAR >= input$scatter_years[1] &
              tmp$YEAR <= input$scatter_years[2],
            c(input$xvar, input$yvar), with = FALSE]
    } else if (input$xy_scale == "전체" & input$xy_region != "전국") {
      tmp <-
        tmp[tmp$YEAR >= input$scatter_years[1] &
              tmp$YEAR <= input$scatter_years[2] &
              tmp$REGION == input$xy_region,
            c(input$xvar, input$yvar), with = FALSE]
    } else if (input$xy_scale != "전체" & input$xy_region == "전국") {
      tmp <-
        tmp[tmp$YEAR >= input$scatter_years[1] &
              tmp$YEAR <= input$scatter_years[2] &
              tmp$SCALE == input$xy_scale,
            c(input$xvar, input$yvar), with = FALSE]
    } else {
      tmp <-
        tmp[tmp$YEAR >= input$scatter_years[1] &
              tmp$YEAR <= input$scatter_years[2] &
              tmp$SCALE == input$xy_scale &
              tmp$REGION == input$xy_region,
            c(input$xvar, input$yvar), with = FALSE]
    }
    colnames(tmp) <- c("X", "Y")

    return(tmp)
  })

  output$plot_scatter <- renderPlotly({
    p <- ggplot(scatter_data(), aes(x = X, y = Y)) +
      geom_point(alpha = 0.3) +
      stat_smooth(
        method = "lm",
        se = FALSE,
        size = 0.5,
        aes(text = "선형회귀"),
        color = "purple"
      ) +
      theme_gdocs()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(
        input$yvar,
        "~",
        input$xvar,
        ",  ",
        input$scatter_years[1],
        "년 ~",
        input$scatter_years[2],
        "년,  ",
        scaleMap[as.character(input$xy_scale)],
        ",  ",
        regionMap[as.character(input$xy_region)]
      ),
      titlefont = titleForm,
      xaxis = list(title = input$xvar),
      yaxis = list(title = input$yvar)
    )
  })

  output$plot_summary <- renderPrint({
    n <- paste(" 자료의 개수 :", nrow(scatter_data()))
    coef <-
      paste(" 기울기 :", round(lm(Y ~ X, data = scatter_data())[[1]][[2]], digits = 2))
    r_square <-
      paste("결정계수 :", round(summary(lm(Y ~ X, data = scatter_data(

      )))[[9]], digits = 2))
    cat(n)
    cat('\n')
    cat(coef)
    cat('      ')
    cat(r_square)
  })

})
