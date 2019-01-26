#### 1단계: 패키지 설치 ####
# install.packages("googleAnalyticsR", dependencies = TRUE)
library(googleAnalyticsR)

#### 2단계: google Developers API ####
# 1. API https://console.developers.google.com/
# 2. Project 만들기
# 3. API 검색 및 활성화
# 4. google analytics API
# 5. Click Create Credentials
# 6. OAuth Consent Screen 작성
# 7. Click Crete Credentials
# 8. OAuth Client ID 생성

#### 3단계: 클라이언트 ID 옵션 지정) 
# clientID: 500564988864-4vkv69q83h3p7anvuls0hcav5lk5k9gh.apps.googleusercontent.com
# client secret: uCxbDm9DS90k9IlpJW8ylM9g
options(googleAuthR.client_id = "500564988864-4vkv69q83h3p7anvuls0hcav5lk5k9gh.apps.googleusercontent.com")
options(googleAuthR.client_secret = "uCxbDm9DS90k9IlpJW8ylM9g")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")

#### 4단계: 인증 ####
ga_auth()

#### 5단계: account_list ####
ga_id <- 186927038

sample <- google_analytics(viewId     = ga_id, 
                 date_range           = c("2018-12-21", "2019-01-26"), 
                 metrics              = c("sessions", "users"), 
                 dimensions           = "date")
library(DT)
datatable(sample)

#### CausalImpact ####
# install.packages("CausalImpact", dependencies = TRUE)
# 1. library Load
library(CausalImpact)

# 2. 시계열 데이터 생성
set.seed(1)
clicks <- 100 + arima.sim(model = list(ar = 0.999), n = 100) # clicks
revenue <- 1.2 * clicks + rnorm(100)
revenue[71:100] <- revenue[71:100] + 10
time.points <- seq.Date(as.Date("2018-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(revenue, clicks), time.points)
head(data)

# 3. 광고 전후 기간 설정
pre.period <- as.Date(c("2018-01-01", "2018-03-11"))
post.period <- as.Date(c("2018-03-12", "2018-04-10"))

# 4. CausalImpact 모델링 
impact <- CausalImpact(data,         # 시계열 모형 데이터
                       pre.period,   # 광고 전 기간
                       post.period)  # 광고 기간

# 5. 결과 시각화 그래프
plot(impact)

# 첫번째 그래프: 광고 후 실제 매출과, 광고 하지 않았을 경우 예상매출
# 두번째 그래프: 일자별 실제매출 - 예상매출 차이
# 세번째 그래프: 매출 누적 값

# 6. 결과 요약 (영어)
summary(impact, "report")

# 7. Upgrade 모델링
impact <- CausalImpact(data,         # 시계열 모형 데이터
                       pre.period,   # 광고 전 기간
                       post.period, 
                       model.args = list(niter = 1000,          # 
                                         nseasons = 7,          # 7 days
                                         season.duration = 24,  # 1 = 하루, 24 = 시간
                                         prior.level.sd = 0.3)) # Prior standard deviation of the Gaussian random walk of the local level.

plot(impact)


#### 실전 예제 #### 
library(dplyr)
library(readr)
library(CausalImpact)

# 1. 데이터 수집 
web_data <- google_analytics(viewId     = ga_id, 
                           date_range   = c("2018-12-27", "2019-01-26"), 
                           metrics      = c("sessions", "users"), 
                           dimensions   = "date")

revenue_data <- readr::read_csv("data/5th_basic/revenue_2018.csv")
revenue_data <- dplyr::rename(revenue_data, 
                              date         = 날짜, 
                              location     = 위치, 
                              lemon        = 레몬, 
                              orange       = 오렌지, 
                              F_avg_temp   = 화씨_평균온도, 
                              C_avg_temp   = 섭씨_평균온도, 
                              leaflets     = 전단지, 
                              price        = 가격)

time.points <- as.Date(revenue_data$date, format = "%d/%m/%Y")
revenue_data2 <- revenue_data %>% select(-date) %>% 
  mutate(revenue = (lemon + orange) * price, 
         location = ifelse(location == "공원", 1, 0)) %>% # 공원 1, 해수욕장 2
  select(revenue, everything())
web_data2 <- web_data %>% select(-date)

# 2. 데이터 조인
ads_analysis <- zoo(cbind(revenue_data2, web_data2), time.points)
ads_analysis

# 3. 광고 전후 기간 설정
pre.period <- as.Date(c("2018-07-01", "2018-07-21"))
post.period <- as.Date(c("2018-07-22", "2018-07-31"))

# 4. CausalImpact 모델링 
impact <- CausalImpact(ads_analysis,         # 시계열 모형 데이터
                       pre.period,           # 광고 전 기간
                       post.period)          # 광고 기간

# 5. 결과 시각화 그래프
plot(impact)

# 6. 결과 설명
summary(impact, "report")


