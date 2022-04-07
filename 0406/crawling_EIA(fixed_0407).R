# 0406 : URL 통채로 주소 찾기로 변환
# Selenium 실행
# Selenium 실행 방법은 아래 URL 참조
# https://r-pyomega.tistory.com/7
# selenium 디렉토리로 이동 후
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
# 를 차례로 입력
# localhost는 본인 환경에 따라 설정
# port는 비어 있는 port 사용 
# CLI에서 'netstat -ano |findstr ####'를 통해 port 사용 여부 확인 가능
# 위 명령어에서 ####에는 사용할 port 번호 입력
# 사용할 브라우저 드라이버가 chrome으로 설정
# RSelenium::rsDriver 혹은 https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf 참조
# geckodriver 사용 시 RSelenium 오류 https://github.com/ropensci/RSelenium/issues/143 참조

# 변수 초기화
rm(list=ls())

# 오류 해결을 위한 함수 추가
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# 해당 디렉토리 설정
# 디렉토리 list 중 첫 번째를 임의 변수 선언
wd <- getwd()[1]
# 디렉토리 설정
setwd(wd)

# 해당 라이브러리 설정
# 라이브러리 list 중 첫 번째를 임의 변수 선언
path <- .libPaths()[1]
# 라이브러리 경로 설정
.libPaths(path)
# RSelenium 실행
library("RSelenium")

# RSelenium 환경 설정
remDr<-remoteDriver(remoteServerAddr="localhost",port=4445,browserName="chrome")
# RSelenium 창 열기
remDr$open()

# 초기 URL 설정
# 환경영향평가 해당 경로
url <- "https://www.eiass.go.kr/biz/base/info/eiaList.do?menu=biz&biz_gubn=E"

# 검색어 벡터 선언
# 제주도, 서귀포, 제주특별자치도
sVal <- c("제주도","서귀포","제주특별자치도")

# 검색어 수만큼 반복
for(i in 1:length(sVal)){
  # sVal : 검색어, sKey : 검색 형태, pn : 페이지 번호
  # 검색어 입력된 페이지 경로로 이동
  url_ <- paste0(url,"&sKey=ADDR&sVal=",sVal[i])
  remDr$navigate(url_)
  
  # 검색 결과 수 추출
  total <- remDr$findElements(using ="css selector",".tbl01_wrap > span")
  total_t <- sapply(total,function(x){x$getElementText()})
  
  # 검색 결과 숫자만 추출
  total_n <- as.numeric(gsub('\\D',"",total_t))
  
  # 검색 결과가 없을 때 종료
  if(total_n == 0){
    break
  }
  
  # 사업코드, 사업명, 사업등록 유형, 사업구분, 사업위치, 사업규모
  bCode = data.frame(matrix(ncol=total_n))
  bName = data.frame(matrix(ncol=total_n))
  bType = data.frame(matrix(ncol=total_n))
  bProp = data.frame(matrix(ncol=total_n))
  bLoc = data.frame(matrix(ncol=total_n))
  bArea = data.frame(matrix(ncol=total_n))
  
  # 총 페이지 수
  pn_total <- ceiling(total_n/10)
  
  # data.frame에 넣을 인덱스 선언
  index <- 1
  
  # 페이지 수만큼 반복
  for(pn in 1:pn_total){
    # 페이지 경로로 이동
    url_f <- paste0(url_,"&pn=",pn)
    remDr$navigate(url_f)
    
    col_total <- 10
    
    # 마지막 페이지 구분
    if(pn == pn_total){
      col_total <- total_n%%10
    }
    
    # 한 페이지의 컬럼 수만큼 반복
    for(col in 1:col_total){
      # 사업명
      cssSelector <- paste0("div.tbl01_wrap> table > tbody > tr:nth-child(",col,") > td.title")
      title <- remDr$findElements(using ="css selector",cssSelector)
      
      # 사업명 클릭
      sapply(title,function(x){x$clickElement()})
      
      # 팝업으로 인한 화면 전환
      # 열려 있는 윈도우 리스트로 추출
      lWin <- remDr$getWindowHandles()
      # 변수 unlist로 문자열 형식 변환
      lWin_u <- unlist(lWin)
      
      # 자식창 윈도우로 전환
      myswitch(remDr, lWin_u[2])
      # 전환 딜레이 설정
      Sys.sleep(0.1)
      
      # 1) 사업코드 
      # CSS 추출
      code <- remDr$findElements(using ="css selector",".sub_view_wrap> table > tbody > tr:nth-child(1) > td:nth-child(2)")
      
      # 텍스트 추출
      code_t <- sapply(code,function(x){x$getElementText()})
      # 변수 unlist로 문자열 형식 변환
      code_u <- unlist(code_t)
      
      # data.frame에 삽입
      bCode[index] <- code_u
      
      # 2) 사업명 
      # CSS 추출
      name <- remDr$findElements(using ="css selector",".sub_view_wrap> table > tbody > tr:nth-child(2) > td:nth-child(2)")
      
      # 텍스트 추출
      name_t <- sapply(name,function(x){x$getElementText()})
      # 변수 unlist로 문자열 형식 변환
      name_u <- unlist(name_t)
      
      # data.frame에 삽입
      bName[index] <- name_u
      
      # 3) 사업등록 유형 
      # CSS 추출
      type <- remDr$findElements(using ="css selector",".sub_view_wrap> table > tbody > tr:nth-child(3) > td:nth-child(2)")
      
      # 텍스트 추출
      type_t <- sapply(type,function(x){x$getElementText()})
      # 변수 unlist로 문자열 형식 변환
      type_u <- unlist(type_t)
      
      # data.frame에 삽입
      bType[index] <- type_u
      
      # 4) 사업 구분
      # CSS 추출
      prop <- remDr$findElements(using ="css selector","li.on > table > tbody > tr:nth-child(2) > td:nth-child(2)")
      
      # 텍스트 추출
      prop_t <- sapply(prop,function(x){x$getElementText()})
      # 변수 unlist로 문자열 형식 변환
      prop_u <- unlist(prop_t)
      
      # data.frame에 삽입
      bProp[index] <- prop_u
      
      # 5) 사업 위치
      # CSS 추출
      loc <- remDr$findElements(using = "css selector","li.on > table > tbody > tr:nth-child(1) > td:nth-child(2) > table > tbody > tr > td:nth-child(1)")
      
      # 텍스트 추출
      loc_t <- sapply(loc,function(x){x$getElementText()})
      
      # 사업 위치 문자열 변수 선언
      loc_f <- ""
      
      # 한 개 이상의 소재지 구분
      if(length(loc_t) > 1){
        # 추출된 텍스트 수만큼 반복
        for(j in 1:(length(loc_t)-1)){
          # 변수 unlist로 문자열 형식 변환 
          loc_u <- unlist(loc_t[j])
          loc_f <- paste0(loc_f,loc_u,"\n")
        }
      }
      
      # 마지막 텍스트 붙여 넣기
      loc_u <- unlist(loc_t[length(loc_t)])
      loc_f <- paste0(loc_f,loc_u)
      
      # data.frame에 삽입
      bLoc[index] <- loc_f
      
      # 6) 사업규모
      # CSS 추출
      area <- remDr$findElements(using ="css selector","li.on > table > tbody > tr:nth-child(5) > td:nth-child(2) > table > tbody > tr > td")
      # 텍스트 추출
      area_t <- sapply(area,function(x){x$getElementText()})
      # 변수 unlist로 문자열 형식 변환
      area_u <- unlist(area_t)
      
      # data.frame에 삽입
      bArea[index] <- area_u
      
      # index 수 증가
      index <- index + 1
      
      # 부모창으로 변환
      myswitch(remDr, lWin_u[1])
    }
  }
  
  # data.frame 열 변환 후 하나로 묶기
  csv <- data.frame(t(bCode), t(bName), t(bType), t(bProp), t(bLoc), t(bArea))
  
  # csv 열별 이름 바꾸기
  names(csv) = c("사업코드", "사업명", "사업등록유형", "사업구분", "사업위치", "사업규모")
  
  fileName <- paste0("환경영향평가_",sVal[i],".csv")
  
  # csv로 내보내기
  write.csv(csv, file=fileName)
}
# 모든 창 닫기
remDr$closeall()
