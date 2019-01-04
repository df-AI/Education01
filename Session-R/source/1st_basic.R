#### 연습용 데이터 ####

library(dplyr)
glimpse(iris)

#### List 만들기 #### 
# vector 만들기
(my_vector <- 1:10)

# matrix 만들기
(my_matrix <- matrix(1:9, ncol = 3))

# data.frame 만들기
(my_df <- mtcars[1:10, ])

# 위 3개 포함하여 리스트 만들기
my_list <- list(vec = my_vector, mat = my_matrix, df = my_df) # 다형성의 예

# 출력하기
my_list

#### List 출력하기 ####
my_list[["vec"]]
my_list[["vec"]][2]

#### List 추가하기 ####
my_list_add <- c(my_list, number = 100)
my_list_add

#### Data Import ####
# 학습목표
# 1. 기본 명령어를 연습한다. 
# 2. 데이터 읽기와 쓰기를 연습한다. (내부데이터 VS 외부데이터)
# 3. 객체 개념을 이해한다. 

## load package ----
install.packages("readxl"); library(readxl) # 엑셀파일 볼러오기 패키지    
install.packages("dplyr"); library(dplyr)   # 데이터 전처리 패키지
install.packages("rio"); library(rio)       # 데이터 불러오기 방식 간소화
install.packages("DT"); library(DT)         # 빅데이터 불러오기 

## R 기본 문법
getwd()            # 경로확인
data("iris")       # 샘플 내장데이터 불러오기
data(package = .packages(all.available = TRUE))  # 이용가능한 데이터 확인
summary(iris)      # 데이터 요약
names(iris)        # 열이름
rownames(iris)     # 행이름
iris2 <- cbind(Row.names = iris$Species, iris) # 행이름 추가
ls()               # 객체 확인
View(iris)         # 엑셀시트처럼 보기 (*비추천)
datatable(iris)    # 동적시트 보기

#### 데이터 읽기/쓰기 ####
## data export / import
getwd()
dir.create("data")  # 폴더 만들기

