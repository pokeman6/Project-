library(rvest)
library(tidyverse)  
library(stringr)   
library(rebus)   
library(lubridate)
library(quantmod)
library(gutenbergr)
library(dplyr)

#Scraping and Cleaning Data

google_stock = getSymbols("GOOGL", auto.assign = F)

url <-'https://www.trustpilot.com/review/www.google.com'

npages<-(1:232)
urllist<-c()
for(i in npages)
{
  urllist[length(urllist)+1]= paste("https://www.trustpilot.com/review/www.google.com?page=",i, sep="")
}

get_review_dates <- function(html){
  date <- html %>% 
    html_nodes('.review-content-header__dates') %>% 
    html_text() %>%
    str_split(',') %>%
    sapply('[',1) %>%
    str_extract('\\d+.*(?=T)')%>%
    unlist()
}

get_reviews <- function(html){
  html %>% 
    html_nodes('.review-content__text') %>%      
    html_text() %>% 
    str_trim() %>%                       
    unlist()                             
}

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.consumer-information__name') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}


get_star_rating <- function(html){

  pattern = 'star-rating-'%R% capture(DIGIT)      
  
  ratings <-  html %>% 
    html_nodes('.star-rating') %>% 
    html_attrs() %>% 
    map(str_match, pattern = pattern) %>%
    map(2) %>%                             
    
    unlist()
  
  # Exclude the first two rating. 
  ratings[3:length(ratings)]               
}

get_data_table <- function(html, company_name){
  dates <- get_review_dates(html)
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  ratings <- get_star_rating(html)
  
  combined_data <- tibble(date = dates,
                          reviewer = reviewer_names,
                          rating = ratings,
                          review = reviews) 
  
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company,date, reviewer, rating, review)
}

get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name){
  urllist %>% 
    map(get_data_from_url, company_name) %>%  
    bind_rows() %>%                           
    write_tsv(str_c(company_name,'.tsv'))     
}

scrape_write_table(url, 'google')
google_tb <- read_tsv('google.tsv')
df<-unique(google_tb)

mean_rating<-df %>% group_by(date) %>% summarise(rate=mean(rating))

#Ploting 
mean(df$rating)
ggplot(df, aes(x=rating)) + 
  geom_histogram(binwidth=1,color="black", fill="yellow")
mean(df$rating)

ggplot(data = mean_rating, aes(date, rate)) + geom_point()

df2<-tail((google_stock[,6]),820)
mean_rating["stock_price"]<-df2
cor(mean_rating$rate, mean_rating$stock_price) 
scatter.smooth(x=mean_rating$rate, y=mean_rating$stock_price, main="Stock ~ Rate")
linearMod <- lm(mean_rating$stock_price ~ mean_rating$rate, data=cars)

tidy_google <- df%>% 
  unnest_tokens(word, review) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)
tidy_google_top15 <- tidy_google[1:15,] %>% with(wordcloud(word, n, max.words = 15))

GO_bigrams <- df %>% 
  unnest_tokens(bigram, review, token = "ngrams", n = 2) 

GO_bigrams_separated <- GO_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

GO_bigrams_separated_filtered <- GO_bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

GO_bigrams_separated_filtered_count <- GO_bigrams_separated_filtered %>% 
  count(word1, word2, sort = TRUE)

GO_bigrams_separated_filtered_count[1:10, ]


list_of_words<-c("search","engine","service","phone","love","company","customer","account","time","email")

search<-df %>%
  filter(str_detect(review, "search"))
search["keyword"]<-"search"

engine<-df %>%
  filter(str_detect(review, "engine"))
engine["keyword"]<-"engine"

service<-df %>%
  filter(str_detect(review, "service"))
service["keyword"]<-"serivice"

phone<-df %>%
  filter(str_detect(review, "phone"))
phone["keyword"]<-"phone"

love<-df %>%
  filter(str_detect(review, "love"))
love["keyword"]<-"love"

df3<-rbind(search,engine,service,phone,love)
df3 %>% 
  select(keyword,rating) %>%
  ggplot(aes(keyword, rating)) + 
  geom_boxplot(color = 'blueviolet',fill='purple') +
  theme(axis.text.x = element_text(angle = 45))



