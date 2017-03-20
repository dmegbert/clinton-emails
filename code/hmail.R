##get the emails
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(scales)
library(wordcloud)

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
    ##facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Haiti", x = "Iran")

