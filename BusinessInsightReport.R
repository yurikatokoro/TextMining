## Setting all packages needed  

library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(scales)
library(wordcloud)
library(reshape2)
library(textdata)
library(igraph)
library(ggraph)


## Importing pdf files

pdf1 <- read_document(file="C:/Users/User/Desktop/DataScience_R/Individual/Colour_Cosmetics_in_Japan.pdf")
pdf2 <- read_document(file="C:/Users/User/Desktop/DataScience_R/Individual/1601.pdf")

## Combine the two pdf files into one 

pdf_combo <- c(pdf1, pdf2)

## Convert pdf_combo into a data frame 

a <- 836 
b <- 1  
pdf_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    pdf_df[i,z]<- pdf_combo[i*b+z-b]
  }
}


## Stop words 

data(stop_words)


## Creating my own stop words and assigning it to "bind"

num <- c(seq.int(0, 6500, 0.1))

num2 <- c("1.0", "2.0", "3.0","4.0", "100.0", "11.0", "5.0", "6.0", "9.0", "8.0", "12.0",
          "23.0", "26.0", "13.0", "21.0", "0.0", "103.0", "130.0", "19.0", "2410.0", "211.0",
          "24.0", "25.0", "28.0", "40.0", "410.0", "7.0", "8620", "110,000", "122.0", "16.0",
          "22.0", "358.0", "42.0", "426.0", "43.0", "45.0", "49.0", "5371", "58.0", "6912",
          "708.0", "73.0", "732.0", "yano.co.jp", "www.yanoresearch.com", "jpy680.9", "jpy920.7", 
          "jpy780.9", "1,000.0", "714.0", "621.0", "376.0", "2136.0", "166.0", "473.0", "425.0",
          "338.0", "353.0", "2417.0", "2139.0", "3523.0", "298.0", "623.0", "171.0", "1131.0", "1005.0",
          "usd", "usd17.3", "usd21.6", "yano")


c <- data_frame(word = c("table", "euromonitor international", "copyright","euromonitor", num2), lexicon = c("custom"))
n <- data_frame(word = num, lexicon = c("number"))

bind <- rbind(c, n)


## Binding my custom stop words and default stop words and assigning it to "junk"

junk <- bind_rows(bind, stop_words)


################
## Tokenizing ##
################

pdf_df1 <- pdf_df$V1

pdf_df2 <- data_frame(line = 1:a, text = pdf_df1)

tidy_df <- pdf_df2 %>%
  unnest_tokens(word, text) %>%
  anti_join(junk) %>%
  count(word, sort = TRUE) %>%
  ungroup()



## Plotting ranking of the most frequent tokens

tidy_df %>%  
  filter(n > 25) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  
  geom_col() +  
  xlab(NULL) +  
  coord_flip()







########################
## Sentiment Analysis ##
########################

## Plotting word cloud with bing (positive vs. negative)

tidy_df %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words = 100,
                   scale = c(1, 1),
                   fixed.asp = TRUE,
                   title.size = 1)


## Plotting contribution to each sentiment with NRC

tidy_df_nrc <- tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


tidy_df_nrc %>%  
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n, fill = sentiment)) +  
  geom_col(show.legend = FALSE) +  
  facet_wrap(~sentiment, scales = "free_y") +  
  labs(y = "Contribution to Sentiment",       x = NULL) + 
  coord_flip()






tidy_df %>%
  inner_join(get_sentiments("afinn")) %>%  
  group_by(word) %>%  
  summarise(sentiment = sum(value)) %>%  
  mutate(method = "AFINN") %>%
  arrange(desc(sentiment)) %>%
  ggplot(aes(word, sentiment)) +  
  geom_col() +  
  xlab(NULL) +  
  coord_flip()







############
## Bigram ##
############


## Tokenization 

pdf_bigram <- pdf_df2 %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

## Separating each pair 

pdf_bi_separated <- pdf_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

## Removing unnecessary words 

pdf_bi_nojunk <- pdf_bi_separated %>%
  filter(!word1 %in% junk$word) %>%
  filter(!word2 %in% junk$word)
  
## Removing NA values 

pdf_bi_filtered <- na.omit(pdf_bi_nojunk)

## Count each word in word1 per word2

pdf_bi_counts <- pdf_bi_filtered %>%
  count(word1, word2, sort = TRUE)

## Reuniting word1 and word2 as bigram 

bigram_united <- pdf_bi_filtered %>%
  unite(bigram, word1, word2, sep=" ")

## Plotting ranking of the most frequent pairs

bigram_united %>%
  top_n(80, bigram) %>%
  ggplot(aes(bigram)) + 
  geom_bar() +  
  coord_flip()




##################
## Bigram Graph ##
##################


pdf_bi_graph <- pdf_bi_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()


ggraph(pdf_bi_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



## Bigram Graph with arrows

set.seed(2016)
arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(pdf_bi_graph, layout = "fr") +  
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,                 
                 arrow = arrow, end_cap = circle(.07, 'inches')) +  
  geom_node_point(color = "lightblue", size = 5) +  
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1) +  
  theme_void()









