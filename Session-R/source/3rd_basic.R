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

#### t검정 #### 


