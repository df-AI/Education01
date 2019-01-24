library(rvest)

#### 1. 예제 연습 ####
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

#### 2. 실전 데이터 수집 ####
library(rvest)
library(dplyr)
library(stringr)

# tryCatch 사용
# 신문기사 웹싸이트의 경우 많은 기사들이 업로드 & 내려가기도 함
# tryCatch를 사용하지 않으면 웹 크롤링 시, 크롤릴 도중 데이터 수집이 멈춰질 수 있음

article_num <- seq(148891, 148900, by = 1)
urls <- paste("https://www.lawtimes.co.kr/Legal-Opinion/Legal-Opinion-View?serial=", article_num, sep = "")
# 총 10개의 기사를 모음

# 기사내용
articleList <- lapply(urls, function(url) {
  tryCatch(
    read_html(url) %>% 
      html_nodes(xpath = "//*[@id='contents']/section[1]/section/article") %>% 
      html_text() %>% 
      str_replace_all("[\n\t\r]", ""), 
    error = function(e){print(paste0("no page: ", url))}
  )
})
articles <- ifelse(str_detect(articles, "no page"), "", articles)
articles

#### 텍스트 마이닝 ####
# install.packages("KoNLP")
# install.packages("tm")
library(tm)      # 기본 툴
library(KoNLP)   # 한글처리
library(stringr) # 특수문자 제거
library(devtools)# 
# install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
library(NIADic)

# 1. tm 코퍼스로 텍스트 문서 가져오기
arc_corpus <- VCorpus(VectorSource(as.list(articles)))
lapply(arc_corpus, content)

# 2. 텍스트 전처리
# 문장부호 제거: removePunctuation
arc_corpus <- tm_map(arc_corpus, removePunctuation)

# 숫자 제거: removeNumbers
arc_corpus <- tm_map(arc_corpus, removeNumbers)

# 공백 문자 제거
arc_corpus <- tm_map(arc_corpus, stripWhitespace)
lapply(arc_corpus, content)

# 기타 한글처리
sp_letters <- c("△", "·", "", "‘", "'")
rm_words <- paste(sp_letters, collapse = "|")

arc_corpus2 <- arc_corpus
for(i in seq_along(arc_corpus2)) {
  arc_corpus2[[i]]$content <- stringr::str_replace_all(arc_corpus2[[i]]$content, rm_words, "")
}

#### 3. 명사 추출
arc_corpus3 <- arc_corpus2
for(i in seq_along(arc_corpus3)) {
  nouns <- extractNoun(arc_corpus3[[i]]$content)
  nouns <- nouns[nchar(nouns) > 2]
  arc_corpus3[[i]]$content <- paste(nouns, collapse = " ")
}

#### 4. TermDocumentMatrix 생성
article_tdm <- TermDocumentMatrix(arc_corpus3, 
                                  control = list(tokenize = "scan", 
                                                 wordLenghs = c(2, 10)))

inspect(article_tdm) # 
nTerms(article_tdm)  # 

# 5. 단어 빈도계산
wordFreq <- slam::row_sums(article_tdm)
wordFreq <- sort(wordFreq, decreasing=TRUE)
keywords <- names(wordFreq)
df_word <- data.frame(keywords = keywords, freq = wordFreq, stringsAsFactors = FALSE)

top10 <- df_word %>% 
  head(10)

top10

# 6. 막대 그래프 및 워드클라우드 
library(ggplot2)
theme_set(theme_bw(base_size = 32, base_family = "AppleGothic"))
ggplot(data = top10, aes(x = reorder(keywords, freq), y = freq)) + 
  ylim(0, 20) + 
  geom_col(fill = "purple") + 
  coord_flip() + 
  labs(x = "주요 키워드", y = "빈도수") + 
  geom_text(aes(label = freq), hjust = -0.3, size = 16) # 빈도 표시 

# install.packages("wordcloud")
library(wordcloud)
pal <- brewer.pal(8, "Dark2")
par(family = "AppleGothic")
wordcloud(words = df_word$keywords, 
          freq = df_word$freq, 
          min.freq = 2, 
          max.words = Inf, 
          random.order = F, 
          random.color = T, 
          rot.per = .1, 
          scale = c(6, 0.2), 
          colors = pal)
