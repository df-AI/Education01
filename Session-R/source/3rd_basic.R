#### 자료의 분류 ####


#### 분포 ####
# 분포: 관측치들의 집합적 양상, 확률분포, (정규분포 T 분포 F 분포 카이제곱 분포)
 
x <- rnorm(100)
hist(x, 
     probability = TRUE, 
     col = gray(.8), lwd = 4, 
     main = "Normal mu = 0, sigma = 1")
curve(dnorm(x), lwd = 4, add = T) # 정규분포

x <- rchisq(100, 5)
hist(x, probability = T, ylim = c(0, 0.2), lwd = 4)
curve(dchisq(x, 5), add = T, lwd = 4) # 카이제곱 분포

# t 분포
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lwd = 4, lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=4, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=4, lty=c(1, 1, 1, 1, 2), col=colors)

# F 분포

# Create the vector x
x <- seq(from = 0, to = 2, length = 200)

# Evaluate the densities
y_1 <- df(x, 1, 1)
y_2 <- df(x, 3, 1)
y_3 <- df(x, 6, 1)
y_4 <- df(x, 3, 3)
y_5 <- df(x, 6, 3)
y_6 <- df(x, 3, 6)
y_7 <- df(x, 6, 6)

# Plot the densities
plot(x, y_1, col = 1, "l", lwd = 4)
lines(x, y_2, col = 2, lwd = 4)
lines(x, y_3, col = 3, lwd = 4)
lines(x, y_4, col = 4, lwd = 4)
lines(x, y_5, col = 5, lwd = 4)
lines(x, y_6, col = 6, lwd = 4)
lines(x, y_7, col = 7, lwd = 4)

# Add the legend
legend("topright", title = "F-distributions",
       c("df = (1,1)", "df = (3,1)", "df = (6,1)", "df = (3,3)", 
         "df = (6,3)", "df = (3,6)", "df = (6,6)"), 
       col = c(1, 2, 3, 4, 5, 6, 7), lty = 1, lwd = 4)

#### Correlation ####
#Title: An example of the correlation of x and y for various distributions of (x,y) pairs
#Tags: Mathematics; Statistics; Correlation
#Author: Denis Boigelot
#Packets needed : mvtnorm (rmvnorm), RSVGTipsDevice (devSVGTips)
#How to use: output()
#
#This is an translated version in R of an Matematica 6 code by Imagecreator.

library(mvtnorm)
library(RSVGTipsDevice)
# install.packages("mvtnorm")
# install.packages("RSVGTipsDevice")

MyPlot <- function(xy, xlim = c(-4, 4), ylim = c(-4, 4), eps = 1e-15) {
  title = round(cor(xy[,1], xy[,2]), 1)
  if (sd(xy[,2]) < eps) title = "" # corr. coeff. is undefined
  plot(xy, main = title, xlab = "", ylab = "",
       col = "darkblue", pch = 16, cex = 0.2,
       xaxt = "n", yaxt = "n", bty = "n",
       xlim = xlim, ylim = ylim)
}

MvNormal <- function(n = 1000, cor = 0.8) {
  for (i in cor) {
    sd = matrix(c(1, i, i, 1), ncol = 2)
    x = rmvnorm(n, c(0, 0), sd)
    MyPlot(x)
  }
}

rotation <- function(t, X) return(X %*% matrix(c(cos(t), sin(t), -sin(t), cos(t)), ncol = 2))

RotNormal <- function(n = 1000, t = pi/2) {
  sd = matrix(c(1, 1, 1, 1), ncol = 2)
  x = rmvnorm(n, c(0, 0), sd)
  for (i in t)
    MyPlot(rotation(i, x))
}

Others <- function(n = 1000) {
  x = runif(n, -1, 1)
  y = 4 * (x^2 - 1/2)^2 + runif(n, -1, 1)/3
  MyPlot(cbind(x,y), xlim = c(-1, 1), ylim = c(-1/3, 1+1/3))
  
  y = runif(n, -1, 1)
  xy = rotation(-pi/8, cbind(x,y))
  lim = sqrt(2+sqrt(2)) / sqrt(2)
  MyPlot(xy, xlim = c(-lim, lim), ylim = c(-lim, lim))
  
  xy = rotation(-pi/8, xy)
  MyPlot(xy, xlim = c(-sqrt(2), sqrt(2)), ylim = c(-sqrt(2), sqrt(2)))
  
  y = 2*x^2 + runif(n, -1, 1)
  MyPlot(cbind(x,y), xlim = c(-1, 1), ylim = c(-1, 3))
  
  y = (x^2 + runif(n, 0, 1/2)) * sample(seq(-1, 1, 2), n, replace = TRUE)
  MyPlot(cbind(x,y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  
  y = cos(x*pi) + rnorm(n, 0, 1/8)
  x = sin(x*pi) + rnorm(n, 0, 1/8)
  MyPlot(cbind(x,y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  
  xy1 = rmvnorm(n/4, c( 3,  3))
  xy2 = rmvnorm(n/4, c(-3,  3))
  xy3 = rmvnorm(n/4, c(-3, -3))
  xy4 = rmvnorm(n/4, c( 3, -3))
  MyPlot(rbind(xy1, xy2, xy3, xy4), xlim = c(-3-4, 3+4), ylim = c(-3-4, 3+4))
}

output <- function() {
  devSVGTips(width = 7, height = 3.2) # remove first and last line for no svg exporting
  par(mfrow = c(3, 7), oma = c(0,0,0,0), mar=c(2,2,2,0))
  MvNormal(800, c(1.0, 0.8, 0.4, 0.0, -0.4, -0.8, -1.0));
  RotNormal(200, c(0, pi/12, pi/6, pi/4, pi/2-pi/6, pi/2-pi/12, pi/2));
  Others(800)
  dev.off() # remove first and last line for no svg exporting
}

# install.packages("corrplot")
library(corrplot)
data("mtcars")
corrplot(cor(mtcars), method = "ellipse")

#### 모수와 통계량 ####
library(readr)
revenue <- read_csv("data/2nd_basic/revenue_2018.csv")

lemon <- revenue$레몬[1:8]

max(lemon)                # 최대값
min(lemon)                # 최소값
mean(lemon)               # 평균
mean(lemon, trim = 0.1)   # 10% 이상치 제거
median(lemon)             # 중앙값
weighted.mean(lemon)      # 가중치 평균

# 평균과 개별관찰값
lemon.m <- mean(lemon)
lemon.dev <- lemon - lemon.m
sum(lemon.dev) 

# 편차 제곱의 평균 구하기
lemon.dev2 <- lemon.dev ^ 2
sum(lemon.dev2)
mean(lemon.dev2)       # 모집단의 분산
var(lemon)             # 표본 분산
sqrt(mean(lemon.dev2)) # 모집단의 표준편차
sd(lemon)              # 표본의 표준편차

# install.packages("rafalib")
library(rafalib)
popvar(lemon)          # 모집단의 분산
popsd(lemon)           # 모집단의 표준편차


#### 이상치 판별 ####
lemon <- c(lemon, 35, 250) # 극단적인 상한, 하한치 입력

(Q <- quantile(lemon)) # 사분위수 출력 후 저장
(ll <- Q[2] - 1.5 * IQR(lemon)) # 하한은 36으로 36보다 작으면 이상치
(ul <- Q[2] + 1.5 * IQR(lemon)) # 상한은 159로 159보다 크면 이상치

lemon[lemon < ll]
lemon[lemon > ul]

boxplot(lemon)

#### 기초통계함수 ####
# 1. 기초 통계 함수를 이해한다. 
# 2. 기초 검정 방법을 이해한다. 

## 함수
# r: 난수 발생
# d: 확률밀도 함수
# p: 누적확률 함수
# q: 확률에 대응하는 데이터 값

mu <- 170  # 평균
sigma <- 6 # 표준편차
ll <- mu - 3 * sigma # 하한
ul <- mu + 3 * sigma # 상한

x <- seq(ll, ul, by = 0.01) # 확률변수 x가 하한~상한까지 0.01씩 증가하는 값
nd <- dnorm(x, mean = mu, sd = sigma) # 확률밀도 함수
plot(x, nd, type = "l", xlab = "x", ylab = "P(X=x)", lwd = 2, col = "red")

pnorm(mu, mean = mu, sd = sigma)   # p(X <= 170)
pnorm(160, mean = mu, sd = sigma)  # p(X <= 150)
pnorm(180, mean = mu, sd = sigma)  # p(X <= 180)

# p(160 <= X <= 180)
pnorm(180, mean = mu, sd = sigma) - pnorm(160, mean = mu, sd = sigma)

qnorm(0.25, mean = mu, sd = sigma) # 사분위 기준 25%에 해당하는 데이터
qnorm(0.5, mean = mu, sd = sigma)  # 사분위 기준 50%에 해당하는 데이터
qnorm(0.75, mean = mu, sd = sigma) # 사분위 기준 75%에 해당하는 데이터

# 정규분포 히스토그램
n_dist <- rnorm(400, mean = mu, sd = sigma) # 표본 400개, 평균, 표준편차
c(mean(n_dist), sd(n_dist))
par(family="AppleGothic")
hist(n_dist, 
     prob = T, 
     main = "N(170, 6^2)으로부터 추출한 표본의 분포(n = 400)", 
     xlab = "", ylab = "", col = "white", border = "black")
lines(x, nd, lty = 2)

## 특징
mu <- 0
sigma <- 1
# 표준정규분포의 데이터 값 구하기
(p0.05 <- qnorm(0.05, mean = mu, sd = sigma))   
# P(Z <= z) = 0.05 z가 -1.645보다 작을 확률은 5%

(p0.025 <- qnorm(0.025, mean = mu, sd = sigma)) 
# P(Z <= z) = 0.025 z가 -1.96보다 작을 확률은 2.5%

options(digits = 4)
pnorm(1.645) - pnorm(-1.645) 
# P(-1.645 <= Z <= 1.645), 정규분포에서 (평균 -1.645 X 표준편차) ~ (평균 + 1.645 X 표준편차) 사이에 들어갈 확률은 0.9
pnorm(1.96) - pnorm(-1.96)
# P(-1.645 <= Z <= 1.645), 정규분포에서 (평균 -1.645 X 표준편차) ~ (평균 + 1.645 X 표준편차) 사이에 들어갈 확률은 0.95

# 예제1) 고객수 정규 분포 가정: 하루 평균 고객수가 200명이고 표준편차가 20명인 커피숍에 230명 넘게 찾아올 확률은?
set.seed(1234)
x <- rnorm(1000000, 200, 20)
plot(density(x))
abline(v = 230, lty = 3)
pnorm(230, 200, 20)
1 - pnorm(230, 200, 20)

# 예제2) 토익점수 정규분포 가정 : 토익점수 평균이 700점, 표준편차가 100점일때 상위 5%에 해당하는 점수는? 
qnorm(0.95, 700, 100)

# 예졔3) 전구수명을 정규분포 가정
# 전구의 평균 수명이 3,000시간, 표준편차가 80시간 일때 임의로 선택한 전구 1개의 수명이 2,948시간에서 3,080시간일 확률은? 
pnorm(3080, 3000, 80) - pnorm(2948, 3000, 80)

#### Z-분포 T-분포 ####
# Z-Score
library(ggplot2)
theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

#### One Sample T-Test ####
set.seed(1234)
# 가상의 데이터 만들기
weight_data <- data.frame(
  ID = paste0(rep("A_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)

# 데이터
head(weight_data, 10)

# 기초 통계량
summary(weight_data$weight)

# 시각화
library(ggpubr)
ggboxplot(weight_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# 질문 1. 데이터의 크기? 10보다 작음
# 질문 2. 데이터의 정규성 확인

# 방법 1. Shapiro-Wilk test:
# 귀무가설: 데이터는 정규성 분포를 이루고 있다. 
# 대립가설: 데이터는 정규성 분포를 이루고 있지 않다. 
shapiro.test(weight_data$weight) # => p-value = 0.12
# 해석? 데이터의 분포가 정규성 분포를 이루는 데이터와 통계적으로 유의하게 다르지 않다. 
# 다시 말하면, 위 데이터는 정규분포를 이루고 있음

# 방법 2. Q-Q 그래프
library("ggpubr")
ggqqplot(my_data$weight, ylab = "Men's weight",
         ggtheme = theme_minimal())

# 만약 정규성이 위배된다면 윌콕슨 검정을 시행한다.

# 질문. 평균 무게가 25g과 다른지 알고 싶음
# 단일표본
(res <- t.test(my_data$weight, mu = 25))

# 위 결과에 대한 해석은? 
# P value가 0.05보다 작다는 뜻은, 평균 무게가 25g보다 p-value 7.95310^{-6}를 가지고 유의하게 다르다

# 평균 무게가 25g보다 작을까?
t.test(my_data$weight, mu = 25,
       alternative = "less")

# 평균 무게가 25g보다 클까?
t.test(my_data$weight, mu = 25,
       alternative = "greater")

#### 독립 표본 ####
# 남녀 무게 데이터
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 77.1, 73.2, 74.1, 89.4, 73.3, 67.3, 67.6, 62.4) 

# Create a data frame
class_weight <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

print(class_weight)

# 그룹별로 요약하기
library(dplyr)
group_by(class_weight, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(class_weight, x = "group", y = "weight", 
          color = "group", palette = c("blue", "red"),
          ylab = "Weight", xlab = "Groups")

# 가정 1. 두 그룹이 독립적인가?
# 가정 2. 두 그룹 모두 정규성을 이루는가?

# 남자 그룹에 대한 정규성 검정
with(class_weight, shapiro.test(weight[group == "Man"]))# p = 0.241

# 여자 그룹에 대한 정규성 검정
with(class_weight, shapiro.test(weight[group == "Woman"])) # p = 0.6

# 만약, 데이터가 정규성을 이루지 않는다고 한다면, 윌콕슨 순위 검정을 시행한다. 

# 가정 3. 두 그룹의 분산값이 동일한가?
(res.ftest <- var.test(weight ~ group, data = my_data))
# P-Value 0.1714

# 해석은? 
# 두 그룹의 분산값이 통계적으로 유의하게 다르지 않음, 다시 말하면, 분산값이 비슷하다는 가정

# 남자 그룹과 여자 그룹에 차이가 존재하는가? 
(res <- t.test(women_weight, men_weight, var.equal = TRUE))
(res <- t.test(weight ~ group, data = class_weight, var.equal = TRUE))

#### 대응 표본 ####
# 실적 데이터
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)

# 광고 후 실적
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

# Create a data frame
revenue_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  revenue = c(before,  after)
)

print(revenue_data)

library("dplyr")
group_by(revenue_data, group) %>%
  summarise(
    count = n(),
    mean = mean(revenue, na.rm = TRUE),
    sd = sd(revenue, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(revenue_data, x = "group", y = "revenue", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "revenue", xlab = "Groups")

# 광고 전 실적 데이터
before <- subset(revenue_data,  group == "before", revenue,
                 drop = TRUE)

# 광고 후 실적 데이터
after <- subset(revenue_data,  group == "after", revenue,
                drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw(base_size = 32)

# 가정 1. 두 샘플이 대응인가?
# 가정 2. 샘플의 크기가 큰가? 크지 않다면, 정규성 검정을 시행한다. 
# 가정 3. 정규성 검정을 실시한다. 

# 차이 계산
d <- with(revenue_data, 
          revenue[group == "before"] - revenue[group == "after"])
# 차이에 대한 정규성 검정
shapiro.test(d) # => p-value = 0.6141

# 검정방법
(res <- t.test(before, after, paired = TRUE))
(res <- t.test(revenue ~ group, data = revenue_data, paired = TRUE))


