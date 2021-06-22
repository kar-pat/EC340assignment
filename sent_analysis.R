# Import packages
library(tidyverse)
library(ggplot2)
library(tidytext)
library(text2vec)
library(lubridate)
library(progress)
library(sentimentr)
library(lexicon)
library(quanteda)
library(zoo)
library(randomForest)
library(forecast)
library(knitr)
# Reading data from CSV files

#Preprocessing comment data
comments <- read.csv("C:/Users/karti/R/wsb/comment_data.csv",
                         comment.char="#", na.strings= c("[removed]","[deleted]"))
comments <- as_tibble(comments[, -c(1, 6)])
# remove all columns that do not contain information
comments <- comments %>% 
  drop_na()
comments$body <- char_tolower(comments$body, keep_acronyms = TRUE)
comments$created <- as_date(comments$created)
comments <- comments %>%
  rename(Date = created)
#dont want to remove tickers


#Preprocessing ticker data
ticker_data <- read.csv("C:/Users/karti/R/wsb/data/tickers.csv",
                        comment.char = "#")
tickers_only <- as_tibble(ticker_data[,c(3,7)])


# TASKS
#1 - filtering reddit slang so that it can be detected by dictionary
#Joining strings that consist of slangs
tictoc::tic()
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("to the moon", ignore_case = TRUE),
                                             replacement = "tothemoon")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("paper hands", ignore_case = TRUE),
                             replacement = "paperhands")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("diamond hands",
                                             ignore_case = TRUE),
                             replacement = "diamondhands")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("chicken tendie",
                                             ignore_case = TRUE),
                             replacement = "chickentendie")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("buy high sell low",
                                             ignore_case = TRUE),
                             replacement = "buyhighselllow")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("this is the way",
                                             ignore_case = TRUE),
                             replacement = "thisistheway")
comments$body <- str_replace_all(comments$body[1:4181575],
                             pattern = fixed("apes together strong",
                                             ignore_case = TRUE),
                             replacement = "apestogetherstrong")
tictoc::toc()

wsb_slang= c("tothemoon","diamondhands", "chickentendies","stonks","guh",
              "bagholder","paperhands", "buyhighselllow","apestogetherstrong",
              "thisistheway")

tokenise_comments <- comments %>%
  unnest_tokens(words, body)

issue_words <- tokenise_comments %>% 
  filter(words %in% wsb_slang) %>%
  group_by(Date, words)%>%
  tally() %>% 
  arrange(desc(n))

issue_words <- issue_words %>% 
  bind_tf_idf(words, Date, n)%>%
    group_by(Date)%>%
    arrange(desc(tf_idf), .by_group = TRUE)

issue_words <- issue_words%>%
  group_by(words) %>%
  filter(n>100)%>%
  arrange(desc(tf_idf), .by_group = TRUE)
  
View(issue_words)



#2 updating jockers_rinkers dictionary to get the new wsb sentiment dict
wsb_key <- update_key(hash_sentiment_jockers_rinker,
                      x = data.frame(x=c("tothemoon","diamondhands",
                                         "chickentendies","stonks",
                                         "guh","bagholder","paperhands",
                                         "buyhighselllow","apestogetherstrong",
                                         "thisistheway"), y=c(0.7,0.8,0.5,
                                                              0.9,-1,-0.8,
                                                              -0.9,-0.7,0.4,
                                                              0.6)))
#3 searching the most occuring stocks
tictoc::tic()
tickers_only$count <- sapply(tickers_only$symbol,
                             function(x) sum(grepl(x, comments$body,
                                                   ignore.case = T)))
tictoc::toc()

#TESla
tesla_comments <- comments %>%
  filter(str_detect(body, "TSLA|tesla|tsla|Tesla|TESLA"))
tesla_comments <- tesla_comments %>%
  add_count(Date)

tesla_sent<- with(
  tesla_comments,
  sentiment_by(get_sentences(body), list(Date)))
tesla_sent$Date <- as_date(tesla_sent$Date)
tesla_sent <- tesla_sent %>%
  left_join(distinct(tesla_comments, Date, .keep_all = T)[, c("Date", "n")],
            by = "Date") %>%
  mutate(logof_n = log(n))


tesla_stock<- read_csv("TSLA.csv")
tesla_stock<- tesla_stock %>%
  mutate(percent_change = ((Close - Open)/Open))
tesla_stock<- tesla_stock %>%
  full_join(tesla_sent, by = "Date")%>%
  arrange(Date)
tesla_stock<- tesla_stock[-c(1,129:135,173:176),]
tesla_stock$Close<- na.locf(tesla_stock$Close)
tesla_stock$Low<- na.locf(tesla_stock$Close)
tesla_stock$High<- na.locf(tesla_stock$Close)
tesla_stock$ave_sentiment<- na.approx(tesla_stock$ave_sentiment, rule = 2)
tesla_stock$percent_change<- na.approx(tesla_stock$percent_change, rule = 2)
tesla_stock$logof_n<- na.approx(tesla_stock$logof_n, rule = 2)
tesla_stock$lag1 <- c(NA, tesla_stock$ave_sentiment[seq_along(tesla_stock$ave_sentiment) -1])
tesla_stock$lag2 <- c(NA, NA, tesla_stock$ave_sentiment[seq_along(tesla_stock$ave_sentiment) -c(1,2)])
tesla_stock$lag_Comment <- c(NA, tesla_stock$logof_n[seq_along(tesla_stock$ave_sentiment) -1])

tesla_ts <- ts(tesla_stock)
plot.ts(tesla_stock$Close)

modelts<- lm(percent_change~ave_sentiment+lag1+lag2+logof_n+lag_Comment, data = tesla_train)
model1 <- lm(Close ~ lag_sent+ave_sentiment+logof_n, data = tesla_train)
new_TSLA <- predict(modelts, newdata = tesla_test, interval = "confidence" )
test <- cbind(tesla_test, new_TSLA)
tab_model(modelts)


pacf(tesla_stock$Close)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
rf <- randomForest(Close~lag2+lag_Comment, data = tesla_train[-c(1:2),])
new_TSLA <- predict(rf, newdata = tesla_test)
test <- cbind(tesla_test, new_TSLA)
summary(rf)




## 75% of the sample size
smp_size <- floor(0.75 * nrow(tesla_stock))

## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(tesla_stock)), size = smp_size)
tesla_train<- tesla_stock[c(1:127), ]
tesla_test<- tesla_stock[-c(1:127), ]


model <- lm(Close ~ lag_sent+avg_sentiment+logof_n, data = tesla_train)
new_TSLA <- predict(model, newdata = tesla_test, interval = "confidence" )
test <- cbind(tesla_test, new_TSLA)


ggplot(test, (aes(Date, percent_change))) +
  geom_point()+
  geom_line(aes(y = percent_change, colour = "Test values"))+
  geom_line(aes(y = fit, colour = "Predicted Values"))+
  ggtitle("TSLA", subtitle = "Closing price regressed on Average sentiment, 1st Lag of sentiment and natural log of total comments per day ")



#CCIV
cciv_comments <- comments %>%
  filter(str_detect(body, "cciv|CCIV|Lucid|LUCID"))
cciv_comments <- cciv_comments %>%
  add_count(Date)
cciv_sent<- with(
  cciv_comments,
  sentiment_by(get_sentences(body), list(Date)))
cciv_sent$Date <- as_date(cciv_sent$Date)
cciv_sent <- cciv_sent %>%
  left_join(distinct(cciv_comments, Date, .keep_all = T)[, c("Date", "n")],
            by = "Date") %>%
  mutate(logof_n = log(n))


cciv_stock<- read_csv("CCIV.csv")
cciv_stock<- cciv_stock %>%
  mutate(percent_change = ((Close - Open)/Open))
cciv_stock<- cciv_stock %>%
  full_join(cciv_sent, by = "Date")%>%
  arrange(Date)
cciv_stock<- cciv_stock[-c(1:49,100:106,138),]
cciv_stock$Close<- na.locf(cciv_stock$Close)
cciv_stock$Low<- na.locf(cciv_stock$Close)
cciv_stock$High<- na.locf(cciv_stock$Close)
cciv_stock$ave_sentiment<- na.approx(cciv_stock$ave_sentiment, rule = 2)
cciv_stock$percent_change<- na.approx(cciv_stock$percent_change, rule = 2)
cciv_stock$logof_n<- na.approx(cciv_stock$logof_n, rule = 2)
cciv_stock$lag1 <- c(NA, cciv_stock$ave_sentiment[seq_along(cciv_stock$ave_sentiment) -1])
cciv_stock$lag2 <- c(NA, NA, cciv_stock$ave_sentiment[seq_along(cciv_stock$ave_sentiment) -c(1,2)])
cciv_stock$lag_Comment <- c(NA, cciv_stock$logof_n[seq_along(cciv_stock$ave_sentiment) -1])

cciv_train<- cciv_stock[c(1:50), ]
cciv_test<- cciv_stock[-c(1:50), ]

#regression
cciv_model<- lm(percent_change~lag1+lag_Comment+lag_Comment*lag1, data = cciv_train)
cciv_pred <- predict(cciv_model, newdata = cciv_test)
cciv_pred <- cbind(cciv_test, cciv_pred)
tab_model(cciv_model)

#random forest
rf1 <- randomForest(percent_change~ave_sentiment+lag1+lag2+logof_n+lag_Comment+lag_Comment*lag1, data = cciv_train[-c(1:2),])
cciv_rf<- predict(rf1, newdata = cciv_test)
cciv_pred<- cbind(cciv_pred, cciv_rf)

rmse3<- sqrt(mean((cciv_pred$percent_change - cciv_pred$cciv_rf)^2))
rmse4<- sqrt(mean((cciv_pred$percent_change - cciv_pred$cciv_pred)^2))
cciv_rmse<- data.frame(model=c("ARDL", "Random Forest"), RMSE=c(rmse4, rmse3))
kable(cciv_rmse)

#plot
ggplot(cciv_pred, (aes(Date, percent_change))) +
  geom_point()+
  geom_line(aes(y = percent_change, colour = "Test values"))+
  geom_line(aes(y = cciv_pred, colour = "Predicted Values"))+
  geom_line(aes(y=cciv_rf, colour= "Random Forest Prediction"))+
  ggtitle("CCIV", subtitle = "Closing price regressed on Average sentiment, 1st Lag of sentiment and natural log of total comments per day ")


```


