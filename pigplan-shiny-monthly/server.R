library(ggplot2)
library(ggthemes)
library(plotly)
library(data.table)
library(shiny)
library(reshape2)

dfm <- read.csv("../data/dfm.csv", header = TRUE)

titleForm <- list(
  size = 20,
  color = "navy"
)

shinyServer(function(input, output) {
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
      theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(v[[input$month_var]], ", ",  input$month, "월"),
      titlefont = titleForm,
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
      theme_par()
    gg <- ggplotly(p)
    gg %>% layout(
      title = paste(v[[input$month_var]], ", ",  input$month_year, "년"),
      titlefont = titleForm,
      xaxis = list(title = "월"),
      yaxis = list(title = v[[input$month_var]])
    )
  })

})
