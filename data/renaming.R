df.t <- read.csv("PIGPLAN_20160912.csv", header = TRUE)
dfm.t <- read.csv("PIGPLAN_MONTH_20160912.csv", header = TRUE)

df <- df.t[-c(1, 3, 6, 7)]
dfm <- dfm.t[-c(1, 18)]
dfm <- dfm[!(dfm$YEAR %in% c(2016)),]
dfm <- dfm[dfm$GUBUN != "ZAEBAL",]

df$S_MODON <- as.numeric(gsub(",", "", df$S_MODON))

colnames(df) <-
  c(
    "YEAR",
    "REGION",
    "SCALE",
    "PSY",
    "상시모돈",
    "모돈회전율",
    "비생산일수",
    "비생산일_이유_교배",
    "비생산일_이유_도폐사",
    "비생산일_교배_사고",
    "비생산일_사고_교배",
    "비생산일_사고_도폐사",
    "임신기간",
    "분만율",
    "평균총산",
    "평균실산",
    "평균미라",
    "평균사산",
    "평균이유두수",
    "포유기간",
    "재귀발정일",
    "재귀율_7일_이내",
    "평균도폐산차",
    "최초교배일령",
    "모돈평균산차"
  )

colnames(dfm)[2:4] <- c("SCALE", "REGION", "CLASS")


tbl1 <- df.t[-c(1, 3, 4, 5, 6, 7)]

tbl1$S_MODON <- as.numeric(gsub(",", "", tbl1$S_MODON))

colnames(tbl1) <- c(
  "연도",
  "PSY",
  "상시모돈",
  "모돈회전율",
  "비생산일수",
  "비생산일_이유_교배",
  "비생산일_이유_도폐사",
  "비생산일_교배_사고",
  "비생산일_사고_교배",
  "비생산일_사고_도폐사",
  "임신기간",
  "분만율",
  "평균총산",
  "평균실산",
  "평균미라",
  "평균사산",
  "평균이유두수",
  "포유기간",
  "재귀발정일",
  "재귀율_7일_이내",
  "평균도폐산차",
  "최초교배일령",
  "모돈평균산차"
)

tbl1 <- data.frame(
  tbl1$연도,
  tbl1$상시모돈,
  tbl1$모돈평균산차,
  tbl1$평균도폐산차,
  tbl1$PSY,
  tbl1$모돈회전율,
  tbl1$평균이유두수,
  tbl1$평균총산,
  tbl1$평균실산,
  tbl1$평균사산,
  tbl1$평균미라,
  tbl1$임신기간,
  tbl1$포유기간,
  tbl1$비생산일수,
  tbl1$`비생산일_이유_교배`,
  tbl1$`비생산일_이유_도폐사`,
  tbl1$`비생산일_교배_사고`,
  tbl1$`비생산일_사고_교배`,
  tbl1$`비생산일_사고_도폐사`,
  tbl1$분만율,
  tbl1$재귀발정일,
  tbl1$`재귀율_7일_이내`,
  tbl1$최초교배일령
)

colnames(tbl1) <-
  c(
    "연도",
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
  )

tbl2 <- dfm.t[-c(1, 3, 4, 18)]
tbl2 <- tbl2[!(tbl2$YEAR %in% c(2016)),]
tbl2 <- tbl2[tbl2$GUBUN != "ZAEBAL",]

tbl2$GUBUN <- as.character(tbl2$GUBUN)
tbl2$GUBUN[tbl2$GUBUN == "BUNMAN"] <- "분만율"
tbl2$GUBUN[tbl2$GUBUN == "CHONGSAN"] <- "평균총산"
tbl2$GUBUN[tbl2$GUBUN == "SILSAN"] <- "평균실산"
tbl2$GUBUN[tbl2$GUBUN == "EUDUSU"] <- "평균이유"
tbl2$GUBUN[tbl2$GUBUN == "SUTAE"] <- "수태율"
tbl2$GUBUN[tbl2$GUBUN == "ZAEGI"] <- "발정재귀일"
tbl2$GUBUN[tbl2$GUBUN == "ZAEGI7"] <- "7일 내 재귀율"

tbl2$GUBUN <- as.factor(tbl2$GUBUN)

colnames(tbl2) <- c("연도",
                    "구분",
                    "M_1월",
                    "M_2월",
                    "M_3월",
                    "M_4월",
                    "M_5월",
                    "M_6월",
                    "M_7월",
                    "M_8월",
                    "M_9월",
                    "M_10월",
                    "M_11월",
                    "M_12월")

tbl1$연도  <- as.factor(tbl1$연도)
tbl2$연도  <- as.factor(tbl2$연도)


write.csv(df, file="df.csv", row.names = FALSE)
write.csv(dfm, file="dfm.csv", row.names = FALSE)
write.csv(tbl1, file="tbl1.csv", row.names = FALSE)
write.csv(tbl2, file="tbl2.csv", row.names = FALSE)
