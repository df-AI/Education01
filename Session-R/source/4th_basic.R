library(rvest)

# url 객체 저장
url <- "http://www.imdb.com/title/tt1490017/"

# url을 html로 변환
lego_movie <- read_html(url)

# 점수 가져오기
xpath_rating <- lego_movie %>% 
  html_nodes(xpath = "//*[@itemprop='ratingValue']") %>% 
  html_text()

css_rating_1 <- lego_movie %>% 
  html_nodes(css = "strong > span") %>% 
  html_text()

css_rating_2 <- lego_movie %>% 
  html_nodes(css = ".ratingValue strong span") %>% 
  html_text()

# 배우 이름 가져오기
#titleCast > table > tbody > tr:nth-child(2) > td:nth-child(2) > a
css_cast <- lego_movie %>% 
  html_nodes(css = "#titleCast td:nth-child(2) a") %>% 
  html_text(trim = TRUE)

css_cast

# //*[@id="titleCast"]/table/tbody/tr[2]/td[2]/a
attr_cast <- lego_movie %>% 
  html_nodes(css = "#titleCast img") %>% 
  html_attr("title")

attr_cast

