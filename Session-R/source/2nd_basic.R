#### 엑셀 패키지 설치 ####
# JAVA_HOME설정
Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home/jre")

# install.packages("rJava")
library(rJava)
library(xlsx)
library(readr)
library(dplyr)
revenue <- read_csv("data/2nd_basic/revenue_2018.csv") # date
revenue$날짜 <- as.Date(revenue$날짜, "%d/%m/%Y")      # 날짜

# 엑셀파일로 내보내기
write.xlsx(revenue, 
           file = "2nd_basic/revenue_2018.xlsx", 
           sheetName = "Sheet1", 
           col.names = TRUE,
           row.names = TRUE, 
           append = FALSE)

# 엑셀파일로 불러오기
revenue2 <- read.xlsx("revenue_2018.xlsx", 
          sheetName = "Sheet1", 
          header = TRUE, 
          colClasses = "character")

#### 데이터 전처리 ####
# 데이터 구조 보기
glimpse(revenue)

# 변수추출
select(revenue, 1, 3, 5) %>% head()
revenue %>% select(1, 3, 5) %>% head()

select(revenue, -날짜) %>% head()
revenue %>% select(-날짜) %>% head()

# 변수 이름의 패턴을 활용한 추출 방법
select(iris, starts_with("Sepal")) %>% head()
iris %>% select(starts_with("Sepal")) %>% head()

select(iris, ends_with("Length")) %>% head()
iris %>% select(ends_with("Length")) %>% head()

select(revenue, contains("_")) %>% head()
revenue %>% select(contains("_")) %>% head()

# 정렬
arrange(iris, desc(Petal.Length)) %>% head()
arrange(iris, Petal.Length) %>% head()

arrange(revenue, desc(날짜)) %>% head()
arrange(revenue, 레몬) %>% head()

# 레코드 Value명 변경
lut <- c("공원" = "서울공원", "해수욕장" = "해운대해수욕장")
glimpse(revenue)
revenue$location <- lut[revenue$위치]

# 변수추가
mutate(revenue, 
       총판매량 = 레몬 + 오렌지, 
       총매출 = 총판매량 * 가격) %>% head()

transmute(revenue, 
       총판매량 = 레몬 + 오렌지, 
       총매출 = 총판매량 * 가격)

# 행추출
glimpse(revenue)
filter(revenue, 레몬 >= 100) # 레몬 판매량이 100개 이상인 경우
filter(revenue, 위치 == "공원") # 공원에서 판매한 경우
filter(iris, Species %in% c("setosa", "versicolor")) # 범주형 value 2개 이상 인 경우

filter(revenue, 레몬 >= 100, 오렌지 >= 100) # AND 
filter(revenue, 레몬 >= 100 & 오렌지 >= 100) # AND 

filter(revenue, 레몬 >= 100 | 오렌지 >= 100) # OR 

# 결측치가 있을 때, 행 추출
df <- data.frame(col1 = c(1:3, NA),
                 col2 = c("this", NA,"is", "text"), 
                 col3 = c(TRUE, FALSE, TRUE, TRUE), 
                 col4 = c(2.5, 4.2, 3.2, NA),
                 stringsAsFactors = FALSE)

filter(df, !is.na(col2)) # NOT

# 기초 통계 요약
summarise(revenue, 
          `레몬 판매량 평균`       = mean(레몬), 
          `오렌지 판매량 표준편차` = sd(오렌지), 
          `레몬 판매량 분산` = var(레몬), 
          `레몬 최대판매량`   = max(레몬), 
          `오렌지 최소판매량` = min(오렌지), 
          `레몬 판매량의 중간값` = median(레몬)
          )

# Aggregate (집계함수) Functions
summarise(revenue, 
          `관측치 갯수` = n(), 
          `판매 위치 갯수` = n_distinct(위치), 
          `공원에서 판매한 날 수` = sum(위치 == "공원"), 
          `해수욕장에서 판매한 날 수` = sum(위치 == "해수욕장"))

# 파이프를 활용한 데이터 요약
revenue %>% 
  mutate(판매량 = 레몬 + 오렌지, 
         일매출액 = 판매량 * 가격) %>% 
  summarise(총매출액 = sum(일매출액), 
            일평균매출액 = mean(일매출액))

#### 실전 테스트 #### 
# 문제 1. 열추가 RealTime & mph 구하기
# RealTime: 실제 비행탑승 시간 추가 (100분)
# mph: 속력 구하기 ((거리 / 실제시간) * 60)

# 문제 2. 필터 적용 mph < 80 이하 & NA 제외

# 문제 3. 요약
# 관측치 행 구하기
# 종착지 갯수 구하기
# 최소거리 구하기
# 최대거리 구하기

library(hflights)
library(dplyr)
dim(hflights) # 행 227,496 열 21
help("hflights")
glimpse(hflights)

hflights %>% 
  mutate(RealTime = ActualElapsedTime + 100, 
         mph = (Distance / RealTime) * 60) %>% 
  filter(!is.na(mph) & mph < 70) %>% 
  summarise(`총 관측치` = n(), 
            `종착지 개수` = n_distinct(Dest), 
            `최소 거리` = min(Distance), 
            `최대 거리` = max(Distance))

#### R을 활용하여 엑셀 보고서 만들기 ####
data("USArrests")
data("mtcars")
data("Titanic")

# 여러 엑셀 데이터 내보내기
write.xlsx(USArrests, 
           file = "myworkbook.xlsx", 
           sheetName = "USA-ARRESTS", 
           append = TRUE)

write.xlsx(mtcars, 
           file = "myworkbook.xlsx", 
           sheetName = "MTCARS", 
           append = TRUE)

write.xlsx(Titanic, 
           file = "myworkbook.xlsx", 
           sheetName = "TITANIC", 
           append = TRUE)

#### 여러 데이터 출력 함수 ####
# file : the path to the output file
# ... : a list of data to write to the workbook
save2Excel <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

save2Excel("myworkbook1.xlsx",
                       mtcars, Titanic, AirPassengers, state.x77)


