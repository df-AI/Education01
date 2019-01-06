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
setwd("/Users/jihoonjung/Documents/DataFlow/Education01/Session-R/data")
dir.create("data")  # 폴더 만들기

# 저장하기
export(iris, "new.xlsx")
export(iris, "new.csv")

# 데이터 읽기
new1 <- import("new.xlsx")
new2 <- import("new.csv")

## 메모리 정리
rm(iris)                        # 1개 지울 때
rm("group", "i")                # 2개 지울 때
rm(list = ls(pattern = "new"))  # 특정 패턴을 지울 때
rm(list = ls())                 # 등록된 모든 환경 지울 때


#### 기본 그래프 ####
install.packages("mlbench")              # Machine Learning Benchmark Problems
data("Ozone", package = "mlbench")       # 1976년 LA 지역의 오존 오염데이터
plot(Ozone$V8, Ozone$V9); help("Ozone")
glimpse(Ozone)

# 그래프 옵션
plot(Ozone$V8,                          # x축
     Ozone$V9,                          # y축
     xlab = "Sandburg Temperature",     # x축 이름
     ylab = "El Monte Temperature",     # y축 이름
     main = "Ozone",                    # 그래프 주제
     cex = .9,                          # 점의 크기
     pch = "+",                         # 점의 종류
     col = "Blue",                      # 점의 색상
     xlim = c(0, 100),                  # x축 범위
     ylim = c(0, 90))                   # y축 범위

# 이 그래프가 의미하는 바는 무엇인가요?

# 학습목표
# 기술통계량과 시각화의 필요성을 인지한다. 
library(dplyr)
library(rvest)

wikiTable <- read_html("https://en.wikipedia.org/wiki/Anscombe's_quartet") %>% 
  html_nodes(".wikitable") %>% 
  html_table()

mytable <- wikiTable[2] %>% as.data.frame()
glimpse(mytable)
graphTable <- mytable %>% 
  mutate_all(as.double) %>% 
  rename(x1 = I, y1 = I.1, 
         x2 = II, y2 = II.1, 
         x3 = III, y3 = III.1, 
         x4 = IV, y4 = IV.1) %>% 
  filter(x1 != "NA")

# install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
b1 <- ggplot(graphTable, aes(x1, y1)) + geom_point()
b2 <- ggplot(graphTable, aes(x2, y2)) + geom_point()
b3 <- ggplot(graphTable, aes(x3, y3)) + geom_point()
b4 <- ggplot(graphTable, aes(x4, y4)) + geom_point()
gridExtra::grid.arrange(b1, b2, b3, b4, ncol = 2)


#### ggplot2 그래프 #### 
# 변수의 종류에 따른 시각화
# 변수가 문자형 변수 하나일 때
# 빈도수
library(ggplot2)
# install.packages("ggpubr")
library(ggpubr)
ggplot(diamonds, aes(cut)) + 
  geom_bar(fill = "#0073C2FF") + 
  theme_pubclean()

# Pie Charts
library(dplyr)
df <- diamonds %>% 
  group_by(cut) %>% 
  summarise(counts = n()) %>% 
  arrange(desc(cut)) %>% 
  mutate(prop = round(counts * 100 / sum(counts), 1), 
         text.labels = cumsum(prop) - 0.5 * prop)

ggplot(df, aes(x = "", y = prop, fill = cut)) + 
  geom_bar(width = 1, stat = "identity", color = "white") + 
  geom_text(aes(y = text.labels, label = prop), color = "white") + 
  coord_polar("y", start = 0) + 
  ggpubr::fill_palette("jco") + 
  theme_void()

ggplot(df, aes(cut, prop)) + 
  geom_linerange(aes(x = cut, ymin = 0, ymax = prop), 
                 color = "lightgray", size = 3) + 
  geom_point(aes(color = cut), size = 4) + 
  color_palette("jco") + 
  theme_pubclean()

# 변수가 숫자형 변수 하나일 때
# 빈도수
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58))
)
head(wdata, 4)

a <- ggplot(wdata, aes(x = weight)) 
a1 <- a + geom_density() + 
  geom_vline(aes(xintercept = mean(weight)), 
             linetype = "dashed", size = 0.6) 

a2 <- a + geom_density(aes(y = ..count..), fill = "lightgray") + 
  geom_vline(aes(xintercept = mean(weight)), 
             linetype = "dashed", size = 0.6, color = "#FC4E07")
grid.arrange(a1, a2)


# 히스토그램
a1 <- a + 
  geom_histogram(bins = 30, 
                 color = "black", 
                 fill = "gray") + 
  geom_vline(aes(xintercept = mean(weight)), 
             linetype = "dashed", 
             size = 0.6)

# 성별에 따른 변화
a2 <- a + geom_histogram(aes(color = sex), fill = "white", 
                   position = "identity") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) 

grid.arrange(a1, a2)

# 성별에 따른 시각화
a + geom_histogram(aes(y = ..density.., color = sex), 
                   fill = "white",
                   position = "identity")+
  geom_density(aes(color = sex), size = 1) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))

# 두 수량형 변수에 따른 시각화
library(ggplot2)
a1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point()
# 
a2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(size = 1.2, color = "steelblue", shape = 21)

grid.arrange(a1, a2)

# 두 수량형 변수의 그래프를 그룹별로 시각화 할 때
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(color = Species))+               
  geom_smooth(aes(color = Species, fill = Species))+
  facet_wrap(~Species, ncol = 3, nrow = 1)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
