##get the emails
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(scales)
library(wordcloud)
library(tidyr)
library(viridis)

setwd("/Users/davidegbert/clinton-emails")

email.csv <- read.csv(file = "./data/Emails.csv", header = TRUE, nrows = 7946, 
                      stringsAsFactors = FALSE)
email <- tbl_df(email.csv)
rm(email.csv)

##ok, now we have a dataframe of the emails
##let's tokenize and do some exploring!
##First up subject of emails

tidy.email.subject <- email %>%
                unnest_tokens(word, MetadataSubject)

data("stop_words")
tidy.email.subject <- tidy.email.subject %>%
                anti_join(stop_words)

tidy.email.subject %>%  
    count(word, sort = TRUE) %>%  
    filter(n > 50) %>%  
    mutate(word = reorder(word, n)) %>%  
    ggplot(aes(word, n)) +  
    geom_bar(stat = "identity") +  
    xlab(NULL) +  
    coord_flip()

tidy.email.subject %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 50))

#A lot of emails with 'sid' in the subject 
#maybe related to longtime Clinton friend Sidney Blumenthal 

## Let's compare words in emails with subject "haiti" and "iran"
haiti.email <- email %>% 
    filter(grepl("haiti", MetadataSubject, ignore.case = TRUE)) %>%
    unnest_tokens(word, ExtractedBodyText) %>%
    anti_join(stop_words)

iran.email <- email %>% 
    filter(grepl("iran", MetadataSubject, ignore.case = TRUE))  %>%
    unnest_tokens(word, ExtractedBodyText) %>%
    anti_join(stop_words)

count(haiti.email, word, sort=TRUE)
count(iran.email, word, sort=TRUE)

my.colors2 <- viridis(5, begin = 0, end = 0.75, option = "A" )
my.colors3 <- viridis(10, begin = 0, end = 0.85, option = "D" )

iran.email %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(word) %>%
    with(wordcloud(word, n, scale=c(75, .5) ,max.words = 50, random.color = TRUE, colors=my.colors2))

haiti.email %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(word) %>%
    with(wordcloud(word, n, scale=c(15, .5) ,max.words = 50, random.color = TRUE, colors=my.colors3))

haiti.percent <- haiti.email %>%  
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(word) %>%  
    transmute(word, haiti = n / sum(n))

iran.percent <- iran.email %>%  
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(word) %>%  
    transmute(word, iran = n / sum(n))

frequency <- iran.percent %>%
                left_join(haiti.percent) %>%
                ungroup()



ggplot(frequency, aes(x = iran, y = haiti, color = abs(iran - haiti))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), 
                         low = "darkslategray4", high = "gray75") +
    theme(legend.position="none") +
    labs(y = "Haiti", x = "Iran")

## Sentiment Analysis of Haiti and Iran emails

haiti.email.sentiment <- email %>% 
    filter(grepl("haiti", MetadataSubject, ignore.case = TRUE)) %>%
    group_by(Id) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, ExtractedBodyText) 

bingPositive <- get_sentiments("bing") %>%
    filter(sentiment == "positive")

haiti.email.sentiment2 <- haiti.email.sentiment %>%
    inner_join(get_sentiments("bing")) %>%
    count(Id, sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive - negative)

haiti.index <- 1:52
df.haiti <- as.data.frame(haiti.email.sentiment2)
df.haiti <- cbind(df.haiti, haiti.index)
haiti.email.sentiment2 <- tbl_df(df.haiti)

ggplot(haiti.email.sentiment2, aes(haiti.index, sentiment)) +
    geom_col(show.legend = TRUE, color = "green") 

iran.email.sentiment <- email %>% 
    filter(grepl("iran", MetadataSubject, ignore.case = TRUE)) %>%
    unnest_tokens(word, ExtractedBodyText) 

bingPositive <- get_sentiments("bing") %>%
    filter(sentiment == "positive")

iran.email.sentiment2 <- iran.email.sentiment %>%
    inner_join(get_sentiments("bing")) %>%
    count(Id, sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive - negative)

iran.index <- 1:30
df.iran <- as.data.frame(iran.email.sentiment2)
df.iran <- cbind(df.iran, iran.index)
iran.email.sentiment2 <- tbl_df(df.iran)

ggplot(iran.email.sentiment2, aes(iran.index, sentiment)) +
    geom_col(show.legend = TRUE, color = "red")

iran.afinn <- iran.email.sentiment %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(Id) %>%
    summarise(sentiment = sum(score)) %>%
    mutate(method = "AFINN")

iran.afinn <- iran.afinn %>% tibble::rownames_to_column(var = "index1")
    
iran.bing.and.nrc <- bind_rows(iran.email.sentiment %>%
                                   inner_join(get_sentiments("bing")) %>%
                                   mutate(method = "Bing"),
                               iran.email.sentiment %>%
                                   inner_join(get_sentiments("nrc") %>%
                                                  filter(sentiment %in% c("positive", 
                                                                          "negative"))) %>%
                                   mutate(method = "NRC")) %>%
                    count(method, index = Id, sentiment) %>%
                        spread(sentiment, n, fill = 0) %>%
                        mutate(sentiment = positive - negative)

iran.bing.and.nrc <- iran.bing.and.nrc %>% tibble::rownames_to_column(var = "index1")
                    
bind_rows(iran.afinn, iran.bing.and.nrc) %>%
    ggplot(aes(index1, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
    
    
    
    
    
