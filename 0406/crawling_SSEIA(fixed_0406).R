# 변수 초기화
rm(list=ls())

# 해당 디렉토리 설정
# 필요시 변수 선언부 교체
# 컴퓨터 디렉토리 : "E:/workspace/2022_01/R"
# 노트북 디렉토리 : "C:/Users/user/RProjects/RTest"

# 디렉토리 list 중 첫 번째를 임의 변수 선언
workSpace <- getwd()[1]

# 디렉토리 설정
setwd(workSpace)

# 해당 라이브러리 설정
# 필요시 변수 선언부 교체
# 컴퓨터 디렉토리 : "E:/workspace/2021_01/R-4.1.0/library"
# 노트북 디렉토리 : "C:/Users/user/Documents/R/win-library/4.1"

# 라이브러리 list 중 첫 번째를 임의 변수 선언
path <- .libPaths()[1]

# 라이브러리 경로 설정
.libPaths(path)

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
# RSelenium::rsDriver 혹은 https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf 참조
remDr<-remoteDriver(remoteServerAddr="localhost",port=4445,browserName="chrome")

# RSelenium 창 열기
remDr$open()

# 초기 URL 설정
# 소규모환경영향평가 해당 경로
url <- "https://www.eiass.go.kr/biz/base/info/perList.do?menu=biz&biz_gubn=M"

# 설정된 URL로 이동
# 로딩 시간 대략 1초 정도 소요
# 로딩이 선행 되어야 다른 작업 수행 가능
remDr$navigate(url)

# switchToWindow() 함수 오류로 인해 자체 동일 기능 함수 선언
# 중복 선언 회피를 위한 선언 선행
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# 부모창 변수 선언
# 변수 unlist로 문자열 형식 변환
parentWindow <- unlist(remDr$getCurrentWindowHandle())

# 파일명을 위한 카테고리명 선언
categoryName <- "소규모환경영향평가"

# 검색어 벡터 선언
# 제주도, 서귀포, 제주특별자치도
sVector <- c("제주도","서귀포","제주특별자치도")

# radio 버튼
sKey <- remDr$findElements(using ="css selector","#ADDR")

# radio 버튼 클릭
sapply(sKey,function(x){x$clickElement()})

# 검색어 자동화를 위한 for문 선언
for(sIndex in 1:length(sVector)){
  
  # 검색창
  sVal <- remDr$findElements(using ="css selector","#sVal")
  
  # 검색창 초기화
  sapply(sVal,function(x){x$clearElement()})
  
  # 검색창에 값 입력
  sapply(sVal,function(x){x$sendKeysToElement(list(sVector[sIndex]))})

  # 검색 버튼
  # CSS : "#search_frm > div > div > div:nth-child(6) > div.form_box.form_box03 > div > input[type=button]:nth-child(2)"
  sSearch <- remDr$findElements(using ="css selector",".form_btm > input[type=button]:nth-child(2)")
  
  # 검색 버튼 클릭
  sapply(sSearch,function(x){x$clickElement()})
  
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
  
  # 10개 이상 페이지 처리
  for(page_tenth in 1:page_ten){
   
     # 남은 페이지가 10개 미만일 때 마지막 페이지 처리
    if(page_tenth == page_ten)
    {
      page_index <- page_total %% 10
    }else{
      page_index <- 10
    }
   
     # 웹페이지 내의 페이지 버튼 클릭을 위한 for문 선언
    for(page in 1:page_index){
      
      # '처음으로','이전 페이지' 다음 버튼을 위해 +2
      page_css <- paste0("#paging > li:nth-child(",page+2,") > a")
      
      # 페이지 버튼
      page_button <- remDr$findElements(using ="css selector",page_css)
      
      # 버튼 클릭
      sapply(page_button,function(x){x$clickElement()})
      
      # 마지막 페이지 처리
      if(page_tenth == page_ten && page == page_index)
      {
        index_ten <- colNum_d %% 10
      }else{
        index_ten <- 10
      }
      
      for(i in 1:index_ten){
        # 사업명
        cssSelector <- paste0("#sub_con > div.tbl01_wrap > table > tbody > tr:nth-child(",i,") > td.title")
        title <- remDr$findElements(using ="css selector",cssSelector)
        
        # 사업명 클릭
        sapply(title,function(x){x$clickElement()})
        
        # 팝업으로 인한 화면 전환
        # 열려 있는 윈도우 리스트로 추출
        windowList <- remDr$getWindowHandles()
        
        # 자식창 변수 선언
        # 변수 unlist로 문자열 형식 변환
        childWindow <- unlist(windowList[2])
        
        # 작업할 윈도우로 전환
        myswitch(remDr, childWindow)
        
        # 화면 전환 딜레이로 첫번째 인덱스를 읽게 해주기 위해
        Sys.sleep(0.5)
        
        # 1) 사업코드 
        # CSS 추출
        # CSS : sub_con > div.conbody > div > table > tbody > tr:nth-child(1) > td:nth-child(2)
        code <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(1) > td:nth-child(2)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        code_u <- unlist(sapply(code,function(x){x$getElementText()}))
        
        # data.frame에 삽입
        bCode[index] <- code_u
        
        # 2) 사업명 
        # CSS 추출
        name <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(2) > td:nth-child(2)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        name_u <- unlist(sapply(name,function(x){x$getElementText()}))
        
        # data.frame에 삽입
        bName[index] <- name_u
        
        # 3) 사업등록 유형 
        # CSS 추출
        type <- remDr$findElements(using ="css selector","#sub_con > div.conbody > div > table > tbody > tr:nth-child(3) > td:nth-child(2)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        type_u <- unlist(sapply(type,function(x){x$getElementText()}))
        
        # data.frame에 삽입
        bType[index] <- type_u
        
        # 4) 사업 구분
        # CSS 추출
        prop <- remDr$findElements(using ="css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(1) > td:nth-child(2)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        prop_u <- unlist(sapply(prop,function(x){x$getElementText()}))
        
        # data.frame에 삽입
        bProp[index] <- prop_u
        
        # 5) 사업 위치
        # CSS 추출
        loc <- remDr$findElements(using = "css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(2) > td:nth-child(2) > table > tbody > tr > td:nth-child(1)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        loc_u <- unlist(sapply(loc,function(x){x$getElementText()}))
        loc_ut <- ""
        
        # 한 개 이상의 소재지가 잡힐 경우 대표 소재지 선택
        for(t in 1:length(loc_u)){
          if(t == length(loc_u)){
            loc_ut <- paste0(loc_ut,loc_u[t])
          }else{
            loc_ut <- paste0(loc_ut,loc_u[t],"\n")
          }
        }
        loc_u <- loc_ut
        
        # data.frame에 삽입
        bLoc[index] <- loc_u
        
        # 6) 사업규모
        # CSS 추출
        area <- remDr$findElements(using ="css selector","#sub_con > div.conbody > ul.tab_cont > li.on > table > tbody > tr:nth-child(6) > td:nth-child(2)")
        
        # 텍스트 추출
        # 변수 unlist로 문자열 형식 변환
        area_u <- unlist(sapply(area,function(x){x$getElementText()}))
        
        # data.frame에 삽입
        bArea[index] <- area_u
        
        # 부모창 윈도우로 전환
        myswitch(remDr, parentWindow)
        
        index <- (index + 1)
      }
    }
    
    if(page_tenth > 1 && page_tenth != page_ten){
      # 다음 페이지 버튼
      next_css <- remDr$findElements(using ="css selector","#paging > li:nth-child(13) > a")

      # 페이지 버튼
      next_button <- remDr$findElements(using ="css selector",next_css)
      
      # 버튼 클릭
      sapply(next_button,function(x){x$clickElement()})
    }
  }
  
  # data.frame 열 변환 후 하나로 묶기
  csv <- data.frame(t(bCode), t(bName), t(bType), t(bProp), t(bLoc), t(bArea))

  # csv 열별 이름 바꾸기
  names(csv) = c("사업코드", "사업명", "사업등록유형", "사업구분", "사업위치", "사업규모")
  
  fileName <- paste0(categoryName,"_",sVector[sIndex],".csv")
  
  # csv로 내보내기
  write.csv(csv, file=fileName)
}

# 모든 창 닫기
remDr$closeall()
