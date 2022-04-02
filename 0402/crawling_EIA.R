# 변수 초기화
rm(list=ls())

# 해당 디렉토리 설정
# 필요시 변수 선언부 교체
# 컴퓨터 디렉토리 : "E:/workspace/2022_01/R"
# 노트북 디렉토리 : "C:/Users/user/RProjects/RTest"

# 디렉토리 변수 선언
wd <- getwd()[1]

# 변수 unlist로 문자열 형식 변환
wd_u <- unlist(wd)

# 디렉토리 설정
setwd(wd_u)

# 해당 라이브러리 설정
# 필요시 변수 선언부 교체
# 컴퓨터 디렉토리 : "E:/workspace/2021_01/R-4.1.0/library"
# 노트북 디렉토리 : "C:/Users/user/Documents/R/win-library/4.1"

# 라이브러리 변수 선언
path <- .libPaths()[1]

# 변수 unlist로 문자열 형식 변환
path_u <- unlist(path)

# 라이브러리 경로 설정
.libPaths(path_u)

# RSelenium 실행
library("RSelenium")

# Selenium 실행
# Selenium 실행 방법은 아래 URL 참조
# https://r-pyomega.tistory.com/7

# RSelenium 환경 설정
# localhost는 본인 환경에 따라 설정
# port는 비어 있는 port 사용 
# CLI에서 'netstat -ano |findstr ####'를 통해 port 사용 여부 확인 가능
# 위 명령어에서 ####에는 사용할 port 번호 입력
# 사용할 브라우저 드라이버가 chrome으로 설정
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
# 환경영향평가(Environmental Impact Assessment) 버튼 구분
sseia<-remDr$findElements(using ="css selector","#visual_wrap > div > ul > li:nth-child(3) > a")

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

# 검색 결과 수 추출
colNum <- remDr$findElements(using ="css selector","#sub_con > div.tbl01_wrap > span")
colNum_c <- sapply(colNum,function(x){x$getElementText()})

# 검색 결과 문자 숫자만 추출
colNum_d <- as.numeric(gsub('\\D',"",colNum_c))

# 올림으로 총 페이지 수 변수 선언
page_total <- ceiling(colNum_d/10)

# 페이지 수가 10개 이상 처리를 위한 변수 선언
page_ten <- ceiling(page_total/10)

# 사업코드, 사업명, 사업등록 유형, 사업구분, 사업위치, 사업규모
bCode = data.frame(matrix(ncol=colNum_d))
bName = data.frame(matrix(ncol=colNum_d))
bType = data.frame(matrix(ncol=colNum_d))
bProp = data.frame(matrix(ncol=colNum_d))
bLoc = data.frame(matrix(ncol=colNum_d))
bArea = data.frame(matrix(ncol=colNum_d))

# data.frame의 초기기 인덱스 선언
index <- 1

# switchToWindow() 함수 오류로 인해 자체 동일 기능 함수 선언
# 중복 선언 회피를 위한 선언 선행
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# 부모창 변수 선언
parentWindow <- remDr$getCurrentWindowHandle()

# 변수 unlist로 문자열 형식 변환
parentWindow_u <- unlist(parentWindow)

for(page in 1:page_ten){
  for(page in 1:10){
    # '처음으로','이전 페이지' 다음 버튼을 위해 +2
    page_css <- paste0("#paging > li:nth-child(",page+2,") > a")
    
    # 페이지 버튼
    page_button <- remDr$findElements(using ="css selector",page_css)
    
    # 버튼 클릭
    sapply(page_button,function(x){x$clickElement()})
    
    for(i in 1:10){
      if(index > colNum_d)
        break
      # 사업명
      cssSelector <- paste0("#sub_con > div.tbl01_wrap > table > tbody > tr:nth-child(",i,") > td.title")
      title <- remDr$findElements(using ="css selector",cssSelector)
      
      # 사업명 클릭
      sapply(title,function(x){x$clickElement()})
      
      # 팝업으로 인한 화면 전환
      # 열려 있는 윈도우 리스트로 추출
      windowList <- remDr$getWindowHandles()
      
      # 자식창 변수 선언
      childWindow <- windowList[2]
      
      # 변수 unlist로 문자열 형식 변환
      childWindow_u <- unlist(childWindow)
      
      # 작업할 윈도우로 전환
      myswitch(remDr, childWindow_u)
      
      # 해당 윈도우 정보 for문으로 반복
      # 1. 전체 정보 크롤링 하는 방법
      # for(i in 1:10){
      #   cssSelector <- paste0("#sub_con > div.conbody > div > table > tbody > tr:nth-child(",i,") > td:nth-child(2)")
      #   header <- remDr$findElements(using ="css selector",cssSelector)
      #   header_c <- sapply(header,function(x){x$getElementText()})
      #   result = header_c
      #   print(result)
      # }
      # 
      # for(i in 1:8){
      #   cssSelector <- paste0("#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(",i,") > td:nth-child(2)")
      #   header <- remDr$findElements(using ="css selector",cssSelector)
      #   header_c <- sapply(header,function(x){x$getElementText()})
      #   result = header_c
      #   print(result)
      # }
      # 
      
      # 2. 해당 정보만 크롤링 하는 방법
      # bList1 <- c(1,2,3)
      # for(i in 1:length(bList1)){
      #     cssSelector <- paste0("#sub_con > div.conbody > div > table > tbody > tr:nth-child(",bList1[i],") > td:nth-child(2)")
      #     header <- remDr$findElements(using ="css selector",cssSelector)
      #     header_c <- sapply(header,function(x){x$getElementText()})
      #     result = header_c
      #     print(result)
      # }
      # bList2 <- c(1,2,3,6)
      # for(i in 1:length(bList2)){
      #   cssSelector <- paste0("#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(",bList2[i],") > td:nth-child(2)")
      #   header <- remDr$findElements(using ="css selector",cssSelector)
      #   header_c <- sapply(header,function(x){x$getElementText()})
      #   result = header_c
      #   print(result)
      # }
      
      # 3. 일부만 크롤링하여 분류하는 방법
      # 화면 전환 딜레이로 첫번째 인덱스를 읽게 해주기 위해
      Sys.sleep(0.5)
      
      # 1) 사업코드 
      # CSS 추출
      #sub_con > div.conbody > div > table > tbody > tr:nth-child(1) > td:nth-child(2)
      code <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(1) > td:nth-child(2)")
      
      # 텍스트 추출
      code_t <- sapply(code,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      code_u <- unlist(code_t)
      
      # data.frame에 삽입
      bCode[index] <- code_u
      
      # 2) 사업명 
      # CSS 추출
      name <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(2) > td:nth-child(2)")
      
      # 텍스트 추출
      name_t <- sapply(name,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      name_u <- unlist(name_t)
      
      # data.frame에 삽입
      bName[index] <- name_u
      
      # 3) 사업등록 유형 
      # CSS 추출
      type <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(3) > td:nth-child(2)")
      
      # 텍스트 추출
      type_t <- sapply(type,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      type_u <- unlist(type_t)
      
      # data.frame에 삽입
      bType[index] <- type_u
      
      # 4) 사업 구분
      # CSS 추출
      prop <- remDr$findElements(using ="css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(1) > td:nth-child(2)")
      
      # 텍스트 추출
      prop_t <- sapply(prop,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      prop_u <- unlist(prop_t)
      
      # data.frame에 삽입
      bProp[index] <- prop_u
      
      # 5) 사업 위치
      # CSS 추출
      loc <- remDr$findElements(using = "css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(2) > td:nth-child(2) > table > tbody > tr > td:nth-child(1)")
      
      # 텍스트 추출
      loc_t <- sapply(loc,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      loc_u <- unlist(loc_t)
      loc_ut <- ""
      
      # 한 개 이상의 소재지가 잡힐 경우 대표 소재지 선택
      # data.frame에 삽입
      #bLoc[index] <- loc_u[1]
      for(t in 1:length(loc_u)){
        if(t == length(loc_u)){
          loc_ut <- paste0(loc_ut,loc_u[t])
        }else{
          loc_ut <- paste0(loc_ut,loc_u[t],"\n")
        }
      }
      loc_u <- loc_ut
      
      bLoc[index] <- loc_u
      # 6) 사업규모
      # CSS 추출
      area <- remDr$findElements(using ="css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(6) > td:nth-child(2)")
      
      # 텍스트 추출
      area_t <- sapply(area,function(x){x$getElementText()})
      
      # 변수 unlist로 문자열 형식 변환
      area_u <- unlist(area_t)
      
      # data.frame에 삽입
      bArea[index] <- area_u
      
      # 부모창 윈도우로 전환
      myswitch(remDr, parentWindow_u)
      
      index <- (index + 1)
    }
  }
  
  # 다음 페이지 버튼
  next_css <- remDr$findElements(using ="css selector","#paging > li:nth-child(13) > a")
  
  # 페이지 버튼
  next_button <- remDr$findElements(using ="css selector",next_css)
  
  # 버튼 클릭
  sapply(next_button,function(x){x$clickElement()})
}

# 모든 창 닫기
remDr$closeall()

# data.frame 열 변환 후 하나로 묶기
csv <- data.frame(t(bCode), t(bName), t(bType), t(bProp), t(bLoc), t(bArea))

# csv 열별 이름 바꾸기
names(csv) = c("사업코드", "사업명", "사업등록유형", "사업구분", "사업위치", "사업규모")

# csv로 내보내기
write.csv(csv, file="소규모환경영향평가.csv")
