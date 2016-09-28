library(ggplot2)
library(ggthemes)
library(shiny)
library(DT)


tbl1 <- read.csv("../data/tbl1.csv", header = TRUE)
tbl2 <- read.csv("../data/tbl2.csv", header = TRUE)


shinyServer(function(input, output) {
  output$raw_data1 <- renderDataTable({
    datatable(
      tbl1,
      rownames = FALSE,
      filter = 'top',
      options = list(pageLength = 10, autoWidth = TRUE),
      style = 'bootstrap',
      class = 'table-bordered table-condensed table-responsive'
    )
  })

  output$raw_data2 <- renderDataTable({
    datatable(
      tbl2,
      rownames = FALSE,
      filter = 'top',
      options = list(pageLength = 10, autoWidth = TRUE),
      colnames = c('연도', '구분', '1월', '2월', '3월', '4월', '5월', '6월', '7월', '8월', '9월', '10월', '11월', '12월'),
      style = 'bootstrap',
      class = 'table-bordered table-condensed table-responsive'
    )
  })

  output$dn1 <- downloadHandler(
    filename = paste("pigplan_yearly.csv"),
    content = function(file) {
      write.csv(tbl1, file)
    }
  )

  output$dn2 <- downloadHandler(
    filename = paste("pigplan_monthly.csv"),
    content = function(file) {
      write.csv(tbl2, file)
    }
  )

})
