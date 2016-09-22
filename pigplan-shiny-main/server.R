# options(java.parameters = "-Xmx1024m")
# library(RODBC)
library(ggplot2)
library(ggthemes)
library(plotly)
library(data.table)
library(shiny)
library(DT)
library(reshape2)
# library(XLConnect)
library(psych)

# con <- odbcConnect("PIGPLAN", uid = "", pwd = "")
#
# df.t <- sqlQuery(con, "select * from PIGPLAN")
# dfm.t <- sqlQuery(con, "select * from PIGPLAN_MONTH")
#
# odbcClose(con)

df <- read.csv("df.csv", header = TRUE)
dfm <- read.csv("dfm.csv", header = TRUE)
tbl1 <- read.csv("tbl1.csv", header = TRUE)
tbl2 <- read.csv("tbl2.csv", header = TRUE)


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
  
  month_data <- reactive({
    m <-
      list(
        "1" = "M1",
        "2" = "M2",
        "3" = "M3",
        "4" = "M4",
        "5" = "M5",
        "6" = "M6",
        "7" = "M7",
        "8" = "M8",
        "9" = "M9",
        "10" = "M10",
        "11" = "M11",
        "12" = "M12"
      )
    tmp <- data.table(dfm)
    tmp <- tmp[CLASS == input$month_var,
               list(
                 M1 = round(mean(M1, na.rm = TRUE), digits = 2),
                 M2 = round(mean(M2, na.rm = TRUE), digits = 2),
                 M3 = round(mean(M3, na.rm = TRUE), digits = 2),
                 M4 = round(mean(M4, na.rm = TRUE), digits = 2),
                 M5 = round(mean(M5, na.rm = TRUE), digits = 2),
                 M6 = round(mean(M6, na.rm = TRUE), digits = 2),
                 M7 = round(mean(M7, na.rm = TRUE), digits = 2),
                 M8 = round(mean(M8, na.rm = TRUE), digits = 2),
                 M9 = round(mean(M9, na.rm = TRUE), digits = 2),
                 M10 = round(mean(M10, na.rm = TRUE), digits = 2),
                 M11 = round(mean(M11, na.rm = TRUE), digits = 2),
                 M12 = round(mean(M12, na.rm = TRUE), digits = 2)
               ),
               by = YEAR]
    tmp$Mean <-
      tmp[, .(Mean = round(rowMeans(.SD), digits = 2)), by = YEAR][[2]]
    tmp <- tmp[, c("YEAR", m[[input$month]], "Mean"), with = FALSE]
    colnames(tmp)[2] <- "M"
    return(tmp)
  })
  
  month_data2 <- reactive({
    tmp <- data.table(dfm)
    tmp <- tmp[CLASS == input$month_var & YEAR == input$month_year,
               list(
                 "1" = round(mean(M1, na.rm = TRUE), digits = 2),
                 "2" = round(mean(M2, na.rm = TRUE), digits = 2),
                 "3" = round(mean(M3, na.rm = TRUE), digits = 2),
                 "4" = round(mean(M4, na.rm = TRUE), digits = 2),
                 "5" = round(mean(M5, na.rm = TRUE), digits = 2),
                 "6" = round(mean(M6, na.rm = TRUE), digits = 2),
                 "7" = round(mean(M7, na.rm = TRUE), digits = 2),
                 "8" = round(mean(M8, na.rm = TRUE), digits = 2),
                 "9" = round(mean(M9, na.rm = TRUE), digits = 2),
                 "10" = round(mean(M10, na.rm = TRUE), digits = 2),
                 "11" = round(mean(M11, na.rm = TRUE), digits = 2),
                 "12" = round(mean(M12, na.rm = TRUE), digits = 2)
               ),
               by = YEAR]
    tmp <- melt(tmp, id = "YEAR")
    colnames(tmp) <- c("YEAR", "M", "V")
    return(tmp)
  })
  
  month_data3 <- reactive({
    tmp <- data.table(dfm)
    tmp <- tmp[CLASS == input$month_var,
               list(
                 "1" = round(mean(M1, na.rm = TRUE), digits = 2),
                 "2" = round(mean(M2, na.rm = TRUE), digits = 2),
                 "3" = round(mean(M3, na.rm = TRUE), digits = 2),
                 "4" = round(mean(M4, na.rm = TRUE), digits = 2),
                 "5" = round(mean(M5, na.rm = TRUE), digits = 2),
                 "6" = round(mean(M6, na.rm = TRUE), digits = 2),
                 "7" = round(mean(M7, na.rm = TRUE), digits = 2),
                 "8" = round(mean(M8, na.rm = TRUE), digits = 2),
                 "9" = round(mean(M9, na.rm = TRUE), digits = 2),
                 "10" = round(mean(M10, na.rm = TRUE), digits = 2),
                 "11" = round(mean(M11, na.rm = TRUE), digits = 2),
                 "12" = round(mean(M12, na.rm = TRUE), digits = 2)
               ),
               by = YEAR]
    tmp <- data.frame(colMeans(tmp))
    tmp <- tmp[-1,]
    tmp <- data.frame(1:12, tmp)
    colnames(tmp) <- c("M", "V")
    tmp$M <- factor(tmp$M)
    tmp$V <- round(tmp$V, digits = 2)
    return(tmp)
  })
  
  scatter_data1 <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[tmp$YEAR >= input$scatter_years[1] &
            tmp$YEAR <= input$scatter_years[2],
          c(input$xvar, input$yvar), with = FALSE]
    colnames(tmp) <- c("X", "Y")
    
    return(tmp)
  })
  
  scatter_data2 <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[tmp$YEAR >= input$scatter_years[1] &
            tmp$YEAR <= input$scatter_years[2] &
            tmp$SCALE == input$xy_scale,
          c(input$xvar, input$yvar), with = FALSE]
    colnames(tmp) <- c("X", "Y")
    
    return(tmp)
  })
  
  scatter_data3 <- reactive({
    tmp <- data.table(df)
    tmp <-
      tmp[tmp$YEAR >= input$scatter_years[1] &
            tmp$YEAR <= input$scatter_years[2] &
            tmp$REGION == input$xy_region,
          c(input$xvar, input$yvar), with = FALSE]
    colnames(tmp) <- c("X", "Y")
    
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
      ) + theme_economist()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(input$year_var, "평균"),
      titlefont = list(size = 14),
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
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("서울경기", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("충청북도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("충청남도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("전라북도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("전라남도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("경상북도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("경상남도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("강원도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data2()$value), max(year_data2()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("제주도", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("100 두 미만 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("100~200 두 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("200~300 두 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("300~400 두 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("400~500 두 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("500~1,000 두 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
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
      ylim(min(year_data3()$value), max(year_data3()$value)) + theme_hc()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste("1,000 두 이상 규모", ",  ", input$year_var),
      titlefont = list(size = 14),
      xaxis = list(title = "연도"),
      yaxis = list(title = input$year_var)
    )
  })
  
  output$plot2a <- renderPlotly({
    v <- list(
      "SUTAE" = "수태율",
      "BUNMAN" = "분만율",
      "CHONGSAN" = "평균총산",
      "SILSAN" = "평균실산",
      "EUDUSU" = "평균이유",
      "ZAEGI" = "발정재귀일",
      "ZAEGI7" = "7일 내 재귀율"
    )
    p <- ggplot(month_data(), aes(x = YEAR, y = Mean, group = 1)) +
      geom_line(size = 0.8, color = "orange", aes(x = YEAR, y = M, text = paste(input$month, "월"))) +
      geom_line(size = 0.5, aes(text = "1~12월 평균"), linetype = "dashed") +
      theme_wsj()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(v[[input$month_var]], ", ",  input$month, "월"),
      titlefont = list(size = 14),
      xaxis = list(title = "연도"),
      yaxis = list(title = v[[input$month_var]])
    )
  })
  
  output$plot2b <- renderPlotly({
    v <- list(
      "SUTAE" = "수태율",
      "BUNMAN" = "분만율",
      "CHONGSAN" = "평균총산",
      "SILSAN" = "평균실산",
      "EUDUSU" = "평균이유",
      "ZAEGI" = "발정재귀일",
      "ZAEGI7" = "7일 내 재귀율"
    )
    p <- ggplot(month_data2(), aes(x = M, y = V, group = 1)) +
      geom_line(size = 0.8, color = "dodgerblue2", aes(text = paste(input$month_year, "년"))) +
      geom_line(data = month_data3(), size = 0.5, aes(text = "2000~2014년 평균"), linetype = "dashed") + 
      theme_wsj()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(v[[input$month_var]], ", ",  input$month_year, "년"),
      titlefont = list(size = 14),
      xaxis = list(title = "월"),
      yaxis = list(title = v[[input$month_var]])
    )
  })
  
  output$plot5a <- renderPlotly({
    p <- ggplot(scatter_data1(), aes(x = X, y = Y)) +
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", se = FALSE, size = 0.5, aes(text = "선형회귀"), color = "purple") + 
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
        "년"
      ),
      titlefont = list(size = 14),
      xaxis = list(title = input$xvar),
      yaxis = list(title = input$yvar)
    )
  })
  
  output$plot5a_summary <- renderPrint({
    n <- paste("     자료의 개수 : ", nrow(scatter_data1()))
    coef <-
      paste("     기울기 : ", round(lm(Y ~ X, data = scatter_data1())[[1]][[2]], digits = 2))
    r_square <-
      paste("     결정계수 : ", round(summary(lm(Y ~ X, data = scatter_data1()))[[9]], digits = 2))
    cat(n)
    cat('\n')
    cat(coef)
    cat('\n')
    cat(r_square)
  })
  
  output$plot5b <- renderPlotly({
    t <-
      list(
        "180001" = "100두 미만",
        "180002" = "100~200두",
        "180003" = "200~300두",
        "180004" = "300~400두",
        "180005" = "400~500두",
        "180006" = "500~1000두",
        "180007" = "1000두 이상"
      )
    p <- ggplot(scatter_data2(), aes(x = X, y = Y)) +
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", se = FALSE, size = 0.5, aes(text = "선형회귀"), color = "purple") +
      theme_gdocs()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(
        t[[toString(input$xy_scale)]],
        ",  ",
        input$yvar,
        "~",
        input$xvar,
        ",  ",
        input$scatter_years[1],
        "년 ~",
        input$scatter_years[2],
        "년"
      ),
      titlefont = list(size = 14),
      xaxis = list(title = input$xvar),
      yaxis = list(title = input$yvar)
    )
  })
  
  output$plot5b_summary <- renderPrint({
    n <- paste("     자료의 개수 : ", nrow(scatter_data2()))
    coef <-
      paste("     기울기 : ", round(lm(Y ~ X, data = scatter_data2())[[1]][[2]], digits = 2))
    r_square <-
      paste("     결정계수 : ", round(summary(lm(Y ~ X, data = scatter_data2()))[[9]], digits = 2))
    cat(n)
    cat('\n')
    cat(coef)
    cat('\n')
    cat(r_square)
  })
  
  output$plot5c <- renderPlotly({
    t <- list(
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
    p <- ggplot(scatter_data3(), aes(x = X, y = Y)) +
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", se = FALSE, size = 0.5, aes(text = "선형회귀"), color = "purple") +
      theme_gdocs()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(
        t[[toString(input$xy_region)]],
        ",  ",
        input$yvar,
        "~",
        input$xvar,
        ",  ",
        input$scatter_years[1],
        "년 ~",
        input$scatter_years[2],
        "년"
      ),
      titlefont = list(size = 14),
      xaxis = list(title = input$xvar),
      yaxis = list(title = input$yvar)
    )
  })
  
  output$plot5c_summary <- renderPrint({
    n <- paste("     자료의 개수 : ", nrow(scatter_data3()))
    coef <-
      paste("     기울기 : ", round(lm(Y ~ X, data = scatter_data3())[[1]][[2]], digits = 2))
    r_square <-
      paste("     결정계수 : ", round(summary(lm(Y ~ X, data = scatter_data3()))[[9]], digits = 2))
    cat(n)
    cat('\n')
    cat(coef)
    cat('\n')
    cat(r_square)
  })
  
  output$raw_data1 <- renderDataTable({
    datatable(
      tbl1,
      rownames = FALSE,
      filter = 'top',
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$raw_data2 <- renderDataTable({
    datatable(
      tbl2,
      rownames = FALSE,
      filter = 'top',
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$dn1 <- downloadHandler(
    filename = paste("pigplan_report_total.csv"),
    content = function(file) {
      write.csv(tbl1, file)
    }
  )
  
  output$dn2 <- downloadHandler(
    filename = paste("pigplan_by_month.csv"),
    content = function(file) {
      write.csv(tbl2, file)
    }
  )
  
})
