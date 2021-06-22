df1 <- tickers_only[,1]
df2 <- tickers_only[,2]
df1 <- df1 %>%
  unnest_tokens(words, description)
#only keep those which are longer than 3 characters
df1 <- subset(df1, nchar(words) >= 4)
#add trailing and leading whitespace characters
for (i in 1:nrow(df1)){
  length <- str_length(df1[[1]][[i]])
  df1[[1]][[i]] <- str_pad(df1[[1]][[i]],width = length+2,side="both",pad=" ")
}
df2 <- df2 %>%
  unnest_tokens(words, symbol, to_lower = FALSE)
df <- rbind (df1, df2)
#replacing values
replace_vals <- c("apes together strong", "this is the way", "paper hands",
                  "diamond hands", "chicken tendies" ,"to the moon",
                  "buy high sell low")

data <- data %>%
  mutate(body = stringr::str_replace_all(
    string = body,
    pattern = fixed(replace_vals, ignore_case = TRUE),
    replacement = gsub(" ", "", replace_vals)
  ))

#Here data is just tibble containing all words.
data <- filter(comments, str_detect(comments$body, paste(df$words, collapse = "|")))

tickers_only <- tickers_only %>%
  rowwise() %>%
  mutate(counter = sum(grepl(symbol, comments$body)))

data <- read.csv("C:/Users/karti/R/wsb/data.csv", comment.char="#")
data <- as_tibble(data)
data <- data [, -2]



#TESLA
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
  left_join(tesla_sent, by = "Date")%>%
  drop_na()



tesla_stock$lag_sent <- c(NA, tesla_stock$ave_sentiment[seq_along(tesla_stock$ave_sentiment) -1])


## 75% of the sample size
smp_size <- floor(0.75 * nrow(tesla_stock))

## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(tesla_stock)), size = smp_size)
tesla_train<- tesla_stock[train_ind, ]
tesla_test<- tesla_stock[-train_ind, ]

model <- lm(Close ~ +avg_sentiment+logof_n, data = tesla_train)
new_TSLA <- predict(model, newdata = tesla_test, interval = "confidence" )
test <- cbind(tesla_test, new_TSLA)


ggplot(test, (aes(Date, Close))) +
  geom_point()+
  geom_line(aes(y = Close, colour = "red"))+
  geom_line(aes(y = fit))
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



#GME
gme_comments <- comments %>%
  filter(str_detect(body, "GME|gamestop|Gamestop|gme|Game stop"))
gme_comments <- gme_comments %>%
  add_count(Date)

gme_sent<- with(
  gme_comments,
  sentiment_by(get_sentences(body), list(Date)))
gme_sent$Date <- as_date(gme_sent$Date)
gme_sent <- gme_sent %>%
  left_join(distinct(gme_comments, Date, .keep_all = T)[, c("Date", "n")],
            by = "Date") %>%
  mutate(logof_n = log(n))


gme_stock<- read_csv("GME.csv")
gme_stock<- gme_stock %>%
  mutate(percent_change = ((Close - Open)/Open))
gme_stock<- gme_stock %>%
  left_join(gme_sent, by = "Date")%>%
  drop_na()



gme_stock$lag_sent <- c(NA, gme_stock$ave_sentiment[seq_along(tesla_stock$ave_sentiment) -1])


smp_size <- floor(0.75 * nrow(gme_stock))

## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(gme_stock)), size = smp_size)
gme_train<- gme_stock[train_ind, ]
gme_test<- gme_stock[-train_ind, ]

model <- lm(Close ~ ave_sentiment+logof_n, data = gme_train)
new_GME <- predict(model, newdata = gme_test, interval = "confidence" )
testgme <- cbind(gme_test, new_GME)


ggplot(testgme, (aes(Date, Close))) +
  geom_point()+
  geom_line(aes(y = Close, colour = "red"))+
  geom_line(aes(y = fit))


#SNDL
sndl_comments <- comments %>%
  filter(str_detect(body, "sndl|SNDL"))
sndl_comments <- sndl_comments %>%
  add_count(Date)
sndl_sent<- with(
  sndl_comments,
  sentiment_by(get_sentences(body), list(Date)))
sndl_sent$Date <- as_date(sndl_sent$Date)
sndl_sent <- sndl_sent %>%
  left_join(distinct(sndl_comments, Date, .keep_all = T)[, c("Date", "n")],
            by = "Date") %>%
  mutate(logof_n = log(n))


sndl_stock<- read_csv("SNDL.csv")
sndl_stock<- sndl_stock %>%
  mutate(percent_change = ((Close - Open)/Open))
sndl_stock<- sndl_stock %>%
  left_join(sndl_sent, by = "Date")%>%
  drop_na()

#rf commands edited in 
sndl_stock<- sndl_stock %>%
  full_join(sndl_sent, by = "Date")%>%
  arrange(Date)
sndl_stock<- sndl_stock[-c(1,104:111,145:146),]
sndl_stock$Close<- na.locf(sndl_stock$Close)
sndl_stock$Low<- na.locf(sndl_stock$Close)
sndl_stock$High<- na.locf(sndl_stock$Close)
sndl_stock$ave_sentiment<- na.approx(sndl_stock$ave_sentiment, rule = 2)
sndl_stock$logof_n<- na.approx(sndl_stock$logof_n, rule = 2)
sndl_stock$lag1 <- c(NA, sndl_stock$ave_sentiment[seq_along(sndl_stock$ave_sentiment) -1])
sndl_stock$lag2 <- c(NA, NA, sndl_stock$ave_sentiment[seq_along(sndl_stock$ave_sentiment) -c(1,2)])
sndl_stock$lag_Comment <- c(NA, sndl_stock$logof_n[seq_along(sndl_stock$ave_sentiment) -1])


sndl_train<- sndl_stock[c(1:102), ]
sndl_test<- sndl_stock[-c(1:102), ]
rf <- randomForest(Close~ave_sentiment+lag1+lag2+logof_n+lag_Comment, data = sndl_train[-c(1:2),])
new_sndl <- predict(rf, newdata = sndl_test)
test <- cbind(sndl_test, new_sndl)


sndl_stock$lag_sent <- c(NA, sndl_stock$ave_sentiment[seq_along(sndl_stock$ave_sentiment) -1])


## 75% of the sample size

smp_size <- floor(0.75 * nrow(sndl_stock))

## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(sndl_stock)), size = smp_size)
sndl_train<- sndl_stock[train_ind, ]
sndl_test<- sndl_stock[-train_ind, ]

model <- lm(Close ~ ave_sentiment+lag_sent+logof_n, data = sndl_train)
new_SNDL <- predict(model, newdata = sndl_test, interval = "confidence" )
testsndl <- cbind(sndl_test, new_SNDL)


ggplot(test, (aes(Date, Close))) +
  geom_point()+
  geom_line(aes(y = Close, colour = "Test values"))+
  geom_line(aes(y = new_sndl, colour = "Predicted Values"))+
  ggtitle("SNDL", subtitle = "Closing price regressed on Average sentiment, 1st Lag of sentiment and natural log of total comments per day ")


