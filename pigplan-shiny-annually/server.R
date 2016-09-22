library(ggplot2)
library(ggthemes)
library(plotly)
library(data.table)
library(shiny)
library(reshape2)


df <- read.csv("../data/df.csv", header = TRUE)

titleForm <- list(
  size = 20,
  color = "navy"
)

shinyServer(function(input, output) {
  year_data <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[, round(mean(get(input$year_var), na.rm = TRUE), digits = 2), by = YEAR]
    colnames(tmp) <- c("year", "mean")
    return(tmp)
  })

  year_data2 <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[, round(mean(get(input$year_var), na.rm = TRUE), digits = 2),
          by = list(YEAR, REGION)]
    colnames(tmp) <- c("year", "region", "value")
    return(tmp)
  })

  year_data3 <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[, round(mean(get(input$year_var), na.rm = TRUE), digits = 2),
          by = list(YEAR, SCALE)]
    colnames(tmp) <- c("year", "scale", "value")
    return(tmp)
  })

  year_data4 <- reactive({
    tmp <- data.table(df)
    tmp <- tmp[, .(YEAR, get(input$year_var))]
    colnames(tmp) <- c("year", "value")
    return(tmp)
  })

  output$plot1a <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.8, aes(text = "평균")) +
      stat_smooth(
        method = "lm",
        se = FALSE,
        size = 0.5,
        linetype = "dashed",
        aes(text = "추세선")
      ) + theme_stata()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(input$year_var, "평균"),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1_box <- renderPlotly({
    p <- ggplot(year_data4(), aes(x = factor(year), y = value)) +
      geom_boxplot() +
      stat_summary(aes(color = "중앙값", group = 1),
                   fun.y = median,
                   geom = "line") +
      stat_summary(aes(color = "평균", group = 1),
                   fun.y = mean,
                   geom = "line") +
      theme_economist()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(input$year_var, ", Boxplot"),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_sg <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10001],
        aes(x = year, y = value, text = "서울경기"),
        size = 0.8,
        color = "limegreen"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("서울경기", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_cb <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10003],
        aes(x = year, y = value, text = "충청북도"),
        size = 0.8,
        color = "hotpink"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("충청북도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_cn <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10002],
        aes(x = year, y = value, text = "충청남도"),
        size = 0.8,
        color = "violet"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("충청남도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_jb <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10005],
        aes(x = year, y = value, text = "전라북도"),
        size = 0.8,
        color = "darkturquoise"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("전라북도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_jn <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10004],
        aes(x = year, y = value, text = "전라남도"),
        size = 0.8,
        color = "lightseagreen"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("전라남도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_gb <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10007],
        aes(x = year, y = value, text = "경상북도"),
        size = 0.8,
        color = "darkolivegreen4"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("경상북도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_gn <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10006],
        aes(x = year, y = value, text = "경상남도"),
        size = 0.8,
        color = "darkgoldenrod"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("경상남도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_gw <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10008],
        aes(x = year, y = value, text = "강원도"),
        size = 0.8,
        color = "salmon"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("강원도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1b_jj <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전국평균")) +
      geom_line(
        data = year_data2()[year_data2()$region == 10009],
        aes(x = year, y = value, text = "제주도"),
        size = 0.8,
        color = "cornflowerblue"
      ) +
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("제주도", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_u100 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180001],
        aes(x = year, y = value, text = "100 두 미만"),
        size = 0.8,
        color = "limegreen"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("100 두 미만 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o100 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180002],
        aes(x = year, y = value, text = "100~200 두"),
        size = 0.8,
        color = "hotpink"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("100~200 두 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o200 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180003],
        aes(x = year, y = value, text = "200~300 두"),
        size = 0.8,
        color = "lightseagreen"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("200~300 두 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o300 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180004],
        aes(x = year, y = value, text = "300~400 두"),
        size = 0.8,
        color = "darkolivegreen4"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("300~400 두 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o400 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180005],
        aes(x = year, y = value, text = "400~500 두"),
        size = 0.8,
        color = "darkgoldenrod"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("400~500 두 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o500 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180006],
        aes(x = year, y = value, text = "500~1,000 두"),
        size = 0.8,
        color = "salmon"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("500~1,000 두 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

  output$plot1c_o1000 <- renderPlotly({
    p <- ggplot(year_data(), aes(x = year, y = mean)) +
      geom_line(size = 0.5, linetype = "dashed", aes(text = "전체평균")) +
      geom_line(
        data = year_data3()[year_data3()$scale == 180007],
        aes(x = year, y = value, text = "1,000 두 이상"),
        size = 0.8,
        color = "cornflowerblue"
      ) +
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("1,000 두 이상 규모", ",  ", input$year_var),
      titlefont = titleForm,
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })

})
