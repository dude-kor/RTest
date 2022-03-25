# 변수 초기화
rm(list=ls())

# 해당 디렉토리 설정
wd <- "E:/workspace/2022_01/R"
setwd("E:/workspace/2022_01/R")
getwd()

# 해당 라이브러리 설정
libPath <- "E:/workspace/2021_01/R-4.1.0/library"
.libPaths(libPath)
.libPaths()

# RSelenium 실행
library("RSelenium")

# RSelenium 환경 설정
remDr<-remoteDriver(remoteServerAddr="localhost",port=4445,browserName="chrome")

# RSelenium 창 열기
remDr$open()

# 초기 URL 설정
# https://www.eiass.go.kr 경로는 iframe에 씌워져 있다.
# /main.do 경로를 추가 후 이동하여 iframe을 벗겨준다.
url <- "https://www.eiass.go.kr/main.do"

# 설정된 URL로 이동
# 로딩 시간 대략 10초 정도 소요
# 로딩이 선행 되어야 다른 작업 수행 가능
remDr$navigate(url)

# DOM 구조의 CSS 선택자로 구분
# 소규모 환경영향평가(Small-Scale Environmental Impact Assessment) 버튼 구분
sseia<-remDr$findElements(using ="css selector","#visual_wrap > div > ul > li:nth-child(2) > a")

# 버튼 클릭
sapply(sseia,function(x){x$clickElement()})

# radio 버튼
sKey <- remDr$findElements(using ="css selector","#ADDR")

# radio 버튼 클릭
sapply(sKey,function(x){x$clickElement()})

# 검색창
sVal <- remDr$findElements(using ="css selector","#sVal")

# 검색창에 값 입력
sapply(sVal,function(x){x$sendKeysToElement(list("제주특별자치도"))})

# 검색 버튼
sButton <- remDr$findElements(using ="css selector","#search_frm > div > div > div:nth-child(6) > div.form_box.form_box03 > div > input[type=button]:nth-child(2)")

# 검색 버튼 클릭
sapply(sButton,function(x){x$clickElement()})

# 사업명
title <- remDr$findElements(using ="css selector","#sub_con > div.tbl01_wrap > table > tbody > tr:nth-child(1) > td.title")

# 사업명 클릭
sapply(title,function(x){x$clickElement()})

# 팝업으로 인한 화면 전환
# 열려 있는 윈도우 리스트로 추출
#remDr$getWindowHandles()
#remDr$getCurrentWindowHandle()
windowList <- remDr$getWindowHandles()

# 전환할 윈도우 문자열로 추출
sBuf <- unlist(windowList[2])

# switchToWindow() 함수 오류로 자체 동일 기능 함수 선언
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# 작업할 윈도우로 전환
myswitch(remDr, sBuf)

test <- remDr$findElements(using ="css selector","#sub_con > div.conbody > ul:nth-child(3) > li:nth-child(2) > a")
#sapply(test,function(x){x$clickElement()})

# 현재 작업 중인 창 닫기
#remDr$closeWindow()

# 모든 창 닫기
#remDr$closeall()
