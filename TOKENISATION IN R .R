library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(tm)
library(quanteda)

data(stop_words)

setwd('/Users/user/Desktop/R/Mod B/Project Team')
read_data <- read_document(file="survey.txt")

junk <- data_frame(
  word=c("visit","plan","out","on","which","countries","what","why","how","much","are","spent",
         "spend","spending","usually","by","at","it\'s","i\'m","i\'d","day","don\'t","hussein","tend",
         "that\'s","haven\'t","i\'ll"),
  lexicon=c(rep("SMART",each=27)))


a <- 32 #how many observations to you have
b <- 5 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- read_data[i*b+z-b]
  }}


########################




##### 1 word token #####
my_txt_1 <- my_df$V1
my_txt_2 <- my_df$V2
my_txt_3 <- my_df$V3
my_txt_4 <- my_df$V4
my_txt_5 <- my_df$V5

mydf_1 <- data_frame(line=1:a, text=my_txt_1)
mydf_2 <- data_frame(line=1:a, text=my_txt_2)
mydf_3 <- data_frame(line=1:a, text=my_txt_3)
mydf_4 <- data_frame(line=1:a, text=my_txt_4)
mydf_5 <- data_frame(line=1:a, text=my_txt_5)

Q1_token <- mydf_1 %>%unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q2_token <- mydf_2 %>%unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q3_token <- mydf_3 %>%unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q4_token <- mydf_4 %>%unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q5_token <- mydf_5 %>%unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort=TRUE)

my_txt_1 <- my_df$V1
my_txt_2 <- my_df$V2
my_txt_3 <- my_df$V3
my_txt_4 <- my_df$V4
my_txt_5 <- my_df$V5

mydf_1 <- data_frame(line=1:a, text=my_txt_1)
mydf_2 <- data_frame(line=1:a, text=my_txt_2)
mydf_3 <- data_frame(line=1:a, text=my_txt_3)
mydf_4 <- data_frame(line=1:a, text=my_txt_4)
mydf_5 <- data_frame(line=1:a, text=my_txt_5)

frequency_question <- bind_rows(mutate(Q1_token, question = 'Q1'),
                                mutate(Q2_token, question = 'Q2'),
                                mutate(Q3_token, question = 'Q3'),
                                mutate(Q4_token, question = 'Q4'),
                                mutate(Q5_token, question = 'Q5'))

total_words <- frequency_question %>% group_by(question) %>%summarize(total=sum(n))

book_words <- left_join(frequency_question, total_words)

freq_by_rank <- book_words %>%group_by(question) %>%mutate(rank = row_number(),`term frequency` = n/total)


testing <- book_words %>%bind_tf_idf(word, question, n)

haha<-testing %>%arrange(desc(tf_idf))

haha[order(-haha$tf_idf),] %>%  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+ geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+facet_wrap(~question, ncol=3, scales="free")+coord_flip()



##### Sentiment Graph ###################

Q1_token_sentiment <- mydf_1 %>% unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>% ungroup()
Q2_token_sentiment <- mydf_2 %>% unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>% ungroup()
Q3_token_sentiment <- mydf_3 %>% unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>% ungroup()
Q4_token_sentiment <- mydf_4 %>% unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>% ungroup()
Q5_token_sentiment <- mydf_5 %>% unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>% ungroup()

Q1_token_sentiment %>%  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort=TRUE) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>%   comparison.cloud(colors = c("grey20", "gray80"),max.words=100)
Q2_token_sentiment %>%  inner_join(get_sentiments("nrc")) %>%  count(word, sentiment, sort=TRUE) %>%  
  acast(word ~sentiment, value.var="n", fill=0) %>%  comparison.cloud(colors = c("grey20", "gray80"), max.words=100)
Q3_token_sentiment %>%inner_join(get_sentiments("nrc")) %>%count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%comparison.cloud(colors = c("grey20", "gray80"), max.words=100)
Q4_token_sentiment %>%inner_join(get_sentiments("nrc")) %>%count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%comparison.cloud(colors = c("grey20", "gray80"), max.words=100)
Q5_token_sentiment %>%inner_join(get_sentiments("nrc")) %>%count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%comparison.cloud(colors = c("grey20", "gray80"),max.words=100)

Q1_token_sentiment %>%group_by(sentiment) %>%top_n(10) %>%ungroup() %>% mutate(word=reorder(word, n)) %>%  
  ggplot(aes(word, n, fill=sentiment)) +   geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+ 
  labs(y="Contribution to sentiment", x=NULL)+coord_flip()
Q2_token_sentiment %>%  group_by(sentiment) %>% top_n(10) %>%  ungroup() %>%  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +  geom_col(show.legend = FALSE) +  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+  coord_flip()
Q3_token_sentiment %>% group_by(sentiment) %>%top_n(10) %>%ungroup() %>%mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+ coord_flip()
Q4_token_sentiment %>%  group_by(sentiment) %>%  top_n(10) %>%ungroup() %>%mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +  geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+coord_flip()
Q5_token_sentiment %>%group_by(sentiment) %>%top_n(10) %>%ungroup() %>%mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+ coord_flip()

########## ZIPF's law ################
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=question))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+scale_y_log10()

######## two words token #############

Q1_2token <- mydf_1 %>%unnest_tokens(word,text, token = "ngrams", n=2) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q2_2token <- mydf_2 %>%unnest_tokens(word,text, token = "ngrams", n=2) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q3_2token <- mydf_3 %>%unnest_tokens(word,text, token = "ngrams", n=2) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q4_2token <- mydf_4 %>%unnest_tokens(word,text, token = "ngrams", n=2) %>% anti_join(stop_words) %>% count(word, sort=TRUE)
Q5_2token <- mydf_5 %>%unnest_tokens(word,text, token = "ngrams", n=2) %>% anti_join(stop_words) %>% count(word, sort=TRUE)

Q1_2token['question']<-c('Q1')
Q1_2token_m<-select(Q1_2token, "question", "word", "n")
Q2_2token['question']<-c('Q2')
Q2_2token_m<-select(Q2_2token, "question", "word", "n")
Q3_2token['question']<-c('Q3')
Q3_2token_m<-select(Q3_2token, "question", "word", "n")
Q4_2token['question']<-c('Q4')
Q4_2token_m<-select(Q4_2token, "question", "word", "n")
Q5_2token['question']<-c('Q5')
Q5_2token_m<-select(Q5_2token, "question", "word", "n")

all_2_tokenize<-rbind(Q1_2token_m,Q2_2token_m,Q3_2token_m,Q4_2token_m,Q5_2token_m)

final_tesla_separated <- all_2_tokenize %>%separate(word, c("word1", "word2"), sep = " ")
final_2token_filtered <- final_tesla_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)

final_2token_counts <- final_2token_filtered %>% count(word1, word2, sort = TRUE)
final_2token_united <- final_2token_filtered %>% unite(word, word1, word2, sep = " ")

final_2token_united_total <- final_2token_united %>%group_by(question) %>%summarize(total=sum(n))
final_2token_words <- left_join(final_2token_united, final_2token_united_total)

final_2token_freq_by_rank <-final_2token_words %>%group_by(question) %>% mutate(rank = row_number(),`term frequency` = n / total)
final_2token_freq_by_rank <- final_2token_freq_by_rank %>%  bind_tf_idf(word, question, n)

final_2token_freq_by_rank %>%arrange(desc(tf_idf))

final_2token_freq_by_rank %>%arrange(desc(tf_idf)) %>%mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%top_n(8) %>%ungroup %>%ggplot(aes(word, tf_idf, fill=question))+geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+facet_wrap(~question, ncol=3, scales="free")+coord_flip()

######## dfm ######

transpose <- t(my_df)


t_df <- as.data.frame(transpose)

my_txt_1 <- t_df$V1
my_txt_2 <- t_df$V2
my_txt_3 <- t_df$V3
my_txt_4 <- t_df$V4
my_txt_5 <- t_df$V5
my_txt_6 <- t_df$V6
my_txt_7 <- t_df$V7
my_txt_8 <- t_df$V8
my_txt_9 <- t_df$V9
my_txt_10 <- t_df$V10
my_txt_11 <- t_df$V11
my_txt_12 <- t_df$V12
my_txt_13 <- t_df$V13
my_txt_14 <- t_df$V14
my_txt_15 <- t_df$V15
my_txt_16 <- t_df$V16
my_txt_17 <- t_df$V17
my_txt_18 <- t_df$V18
my_txt_19 <- t_df$V19
my_txt_20 <- t_df$V20
my_txt_21 <- t_df$V21
my_txt_22 <- t_df$V22
my_txt_23 <- t_df$V23
my_txt_24 <- t_df$V24
my_txt_25 <- t_df$V25
my_txt_26 <- t_df$V26
my_txt_27 <- t_df$V27
my_txt_28 <- t_df$V28
my_txt_29 <- t_df$V29
my_txt_30 <- t_df$V30
my_txt_31 <- t_df$V31
my_txt_32 <- t_df$V32

a <- 5

mydf_1 <- data_frame(line=1:a, text=my_txt_1)
mydf_2 <- data_frame(line=1:a, text=my_txt_2)
mydf_3 <- data_frame(line=1:a, text=my_txt_3)
mydf_4 <- data_frame(line=1:a, text=my_txt_4)
mydf_5 <- data_frame(line=1:a, text=my_txt_5)
mydf_6 <- data_frame(line=1:a, text=my_txt_6)
mydf_7 <- data_frame(line=1:a, text=my_txt_7)
mydf_8 <- data_frame(line=1:a, text=my_txt_8)
mydf_9 <- data_frame(line=1:a, text=my_txt_9)
mydf_10 <- data_frame(line=1:a, text=my_txt_10)
mydf_11 <- data_frame(line=1:a, text=my_txt_11)
mydf_12 <- data_frame(line=1:a, text=my_txt_12)
mydf_13 <- data_frame(line=1:a, text=my_txt_13)
mydf_14 <- data_frame(line=1:a, text=my_txt_14)
mydf_15 <- data_frame(line=1:a, text=my_txt_15)
mydf_16 <- data_frame(line=1:a, text=my_txt_16)
mydf_17 <- data_frame(line=1:a, text=my_txt_17)
mydf_18 <- data_frame(line=1:a, text=my_txt_18)
mydf_19 <- data_frame(line=1:a, text=my_txt_19)
mydf_20 <- data_frame(line=1:a, text=my_txt_20)
mydf_21 <- data_frame(line=1:a, text=my_txt_21)
mydf_22 <- data_frame(line=1:a, text=my_txt_22)
mydf_23 <- data_frame(line=1:a, text=my_txt_23)
mydf_24 <- data_frame(line=1:a, text=my_txt_24)
mydf_25 <- data_frame(line=1:a, text=my_txt_25)
mydf_26 <- data_frame(line=1:a, text=my_txt_26)
mydf_27 <- data_frame(line=1:a, text=my_txt_27)
mydf_28 <- data_frame(line=1:a, text=my_txt_28)
mydf_29 <- data_frame(line=1:a, text=my_txt_29)
mydf_30 <- data_frame(line=1:a, text=my_txt_30)
mydf_31 <- data_frame(line=1:a, text=my_txt_31)
mydf_32 <- data_frame(line=1:a, text=my_txt_32)



frequency_person <- bind_rows(mutate(mydf_1, person = 'p01'),
                              mutate(mydf_2, person = 'p02'),
                              mutate(mydf_3, person = 'p03'),
                              mutate(mydf_4, person = 'p04'),
                              mutate(mydf_5, person = 'p05'),
                              mutate(mydf_6, person = 'p06'),
                              mutate(mydf_7, person = 'p07'),
                              mutate(mydf_8, person = 'p08'),
                              mutate(mydf_9, person = 'p09'),
                              mutate(mydf_10, person = 'p10'),
                              mutate(mydf_11, person = 'p11'),
                              mutate(mydf_12, person = 'p12'),
                              mutate(mydf_13, person = 'p13'),
                              mutate(mydf_14, person = 'p14'),
                              mutate(mydf_15, person = 'p15'),
                              mutate(mydf_16, person = 'p16'),
                              mutate(mydf_17, person = 'p17'),
                              mutate(mydf_18, person = 'p18'),
                              mutate(mydf_19, person = 'p19'),
                              mutate(mydf_20, person = 'p20'),
                              mutate(mydf_21, person = 'p21'),
                              mutate(mydf_22, person = 'p22'),
                              mutate(mydf_23, person = 'p23'),
                              mutate(mydf_24, person = 'p24'),
                              mutate(mydf_25, person = 'p25'),
                              mutate(mydf_26, person = 'p26'),
                              mutate(mydf_27, person = 'p27'),
                              mutate(mydf_28, person = 'p28'),
                              mutate(mydf_29, person = 'p29'),
                              mutate(mydf_30, person = 'p30'),
                              mutate(mydf_31, person = 'p31'),
                              mutate(mydf_32, person = 'p32'),
)

survey_dfm <- frequency_person %>%
  group_by(person) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(junk) %>%
  count(word) %>%
  cast_dfm(person, word, n) 




####predict#####

survey_dfm.train<-survey_dfm[1:8,]
survey_dfm.test<-survey_dfm[9:32,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(survey_dfm.train, c(1,1,1,1,0,1,0,1))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, survey_dfm.test)
pred



########LDA######


survey_dtm <- frequency_person %>%
  group_by(person) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(junk) %>%
  count(word) %>%
  cast_dtm(person, word, n) 

survey_dtm


#calling the Latent Dirichlet Allocation algorithm
ap_lda <- LDA(survey_dtm, k=2, control=list(seed=123)) # k = number of topics
ap_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics
library(ggplot2)
library(dplyr)

top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1)) %>% # buat liat top di topic 2
  arrange(desc(log_rate))

beta_spread

my_gamma <- tidy(ap_lda, matrix="gamma") # topic 2 gamma prob nya: = 1 - gamma topic 1


######## clustering #######

basic <- frequency_person %>%
  group_by(person) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(junk) %>%
  count(word)

cluster1 <- basic %>%
  filter(person %in% c("p04","p05","p06","p07","p09","p10","p11","p12","p15","p20","p26","p31"))


cluster2 <- basic %>%
  filter(!person %in% c("p04","p05","p06","p07","p09","p10","p11","p12","p15","p20","p26","p31"))

library(wordcloud2)

wordcloud(words = cluster1$word, freq = cluster1$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Spectral"))

wordcloud(words = cluster2$word, freq = cluster2$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdYlGn"))


