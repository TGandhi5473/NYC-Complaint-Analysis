#Project Report - Social and Economic Factors Influencing New Yorkers despair
#Sentiment Analysis

#Read Data

data = read.csv('311_analysis_data.csv')

#Libraries

library(dplyr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(tidyverse)

#Prepare Data for text mining

top_complaints <- data %>%
  count(Complaint.Type, sort = TRUE) %>%
  top_n(25, n)%>%
  pull(Complaint.Type)

data.text <- data %>%
  filter(Complaint.Type %in% top_complaints) %>%
  select(Unique.Key, Complaint.Type, Descriptor, Resolution.Description, Median_income, Duration)%>%
  filter(!is.na(Resolution.Description))

#Explore data

str(data.text)
str_count(string = data.text$Resolution.Description[1], pattern = '\\S+')
summary(nchar(data.text$Resolution.Description))
summary(str_count(string = data.text$Resolution.Description, pattern = '\\S+'))

#Complaint Type Count

complaint_counts <- data.text %>%
  group_by(Complaint.Type) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
complaint_counts

#Distribution of complaint.type

ggplot(data=data.text, aes(x=Complaint.Type)) +
  geom_bar(fill='sienna3') +
  theme_bw() +
  xlab('Complaint Type') +
  coord_flip()

#Example of resolution description

data.text$Resolution.Description[100]

#Frequent words

data.text%>%
  unnest_tokens(input = Resolution.Description, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

#Tokenize

data.text %>%
  select(Unique.Key,Resolution.Description)%>%
  group_by(Unique.Key)%>%
  unnest_tokens(output = word,input=Resolution.Description)%>%
  ungroup()%>%
  group_by(Unique.Key)%>%
  summarize(count = n())

#Binary Sentiment Text

#Bing

as.data.frame(get_sentiments('bing'))[1:50,]
get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()

text.plot <- data.text %>%
  select(Unique.Key,Complaint.Type,Resolution.Description)%>%
  group_by(Unique.Key, Complaint.Type)%>%
  unnest_tokens(output=word,input=Resolution.Description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Complaint.Type,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#Binary Sentiment with Top 25 Complaint Types

ggplot(data = text.plot, aes(x = Complaint.Type, y = proportion, fill = sentiment)) +
  geom_col(position = position_stack(reverse = TRUE)) +  
  theme_bw() +
  coord_flip() +  
  labs(fill = "Sentiment") +
  scale_fill_manual(values = c("negative" = "tomato2", "positive" = "seagreen2"))

#NRC Emotion Lexicon 

top_complaints5 <- data %>%
  count(Complaint.Type, sort = TRUE) %>%
  top_n(5, n)%>%
  pull(Complaint.Type)

data.text5 <- data %>%
  filter(Complaint.Type %in% top_complaints5) %>%
  select(Unique.Key, Complaint.Type, Descriptor, Resolution.Description, Median_income, Duration)%>%
  filter(!is.na(Resolution.Description))

nrc = get_sentiments('nrc')

data.text5%>%
  group_by(Unique.Key)%>%
  unnest_tokens(output = word, input = Resolution.Description)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n), y=n, fill=sentiment))+
  geom_col()+
  guides(fill=F)+
  coord_flip()+
  theme_wsj()

sentiment_data <- data.text5 %>%
  group_by(Unique.Key, Complaint.Type) %>%
  unnest_tokens(output = word, input = Resolution.Description) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  group_by(Unique.Key, Complaint.Type, sentiment) %>%
  count() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  select(Unique.Key, Complaint.Type, positive, negative, anger, trust, fear, anticipation, sadness, surprise, joy) %>%
  ungroup()

#Emotion Lexicon with Top 5 Complaint Types

sentiment_data%>%
  pivot_longer(cols = anger: surprise, names_to = 'sentiment',values_to = 'n')%>%
  group_by(sentiment, Complaint.Type)%>%
  summarize(n = mean(n))%>%
  ggplot(aes(x=Complaint.Type,y=n,fill=Complaint.Type))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+
  coord_flip()+
  theme_bw()+
  scale_fill_grey(start = 0.2, end = 0.8)

#Binary Sentiment and Resolution Time 

duration.plot <- data.text5 %>%
  select(Unique.Key,Duration,Resolution.Description)%>%
  group_by(Unique.Key, Duration)%>%
  unnest_tokens(output=word,input=Resolution.Description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Duration,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

ggplot(data = duration.plot, aes(x = Duration, fill = sentiment)) +
  geom_histogram(binwidth = 1000, position = "identity", alpha = 0.5) +
  theme_bw() +
  labs(fill = "Sentiment", x = "Resolution Time (min)", y = "Count") +
  scale_fill_manual(values = c("positive" = "cyan", "negative" = "red")) +
  facet_wrap(~ sentiment, ncol = 1) +
  xlim(0,20000)

#Correlation with duration

data.text%>%
  group_by(Unique.Key, Duration) %>%
  unnest_tokens(output = word, input = Resolution.Description) %>%
  inner_join(nrc, by = "word") %>%
  group_by(Unique.Key, Duration, sentiment) %>%
  count() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  select(Unique.Key, Duration, positive, negative, trust, anticipation, joy, fear, anger, sadness, surprise) %>%
  ungroup() %>%
  pivot_longer(cols = 3:11, names_to = 'sentiment', values_to = 'n') %>%
  group_by(sentiment) %>%
  summarize(
    `Correlation with rating` = round(cor(n, Duration, use = "complete.obs"), 2),
    p = ifelse(cor.test(n, Duration)$p.value < 0.05, 'p < 0.05', 'not significant'))

