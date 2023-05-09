#' intro_text.R
#' 
#' Contributors:
#' 
#' What this file does:
#'  - Introduces students to first founding concepts in text analytics

# --- Libraries --- #
library(readr)      # read files
library(janitor)    # cleanup var names
library(dplyr)      # data manip
library(tibble)     # work with dataframe
library(tidyr)      # data manip
library(ggplot2)    # plotting
library(stringr)    # work with strings
library(tidytext)   # work with text - main functionality
library(textstem)   # stem words
library(tokenizers) # count words
library(reshape2)   # cast from long to wide and vice versa
library(wordcloud)  # plot wordclouds

# --- Load Data --- #
df <- read_csv("data/ebay_reviews.csv") %>% 
  clean_names()


# --- Reducing the Data Size --- #
# what categories are there ? 
df %>% select(category) %>% 
  distinct()

df %>% 
  group_by(category) %>% 
  count(sort=TRUE)      # Most used categories

cameras <-  df %>% 
  filter(category == "Digital Cameras")


# --- Counting Characters and Approx. Word Counts --- #
cameras <- cameras %>%  
  mutate(n_char = nchar(review_content), n_words = count_words(review_content))


cameras %>% 
  ggplot() + 
  geom_histogram(aes(x=n_words)) + 
  theme_bw() 

# everyting 4 5 star positive 1 2 star negative
cameras %>% 
  mutate(negative = if_else(rating <= 3, TRUE, FALSE)) %>% 
  ggplot() + 
  geom_histogram(aes(x=n_words, fill = negative)) + 
  theme_bw() 



# --- Reviews to Tokens --- #
tokens <- 
  cameras %>% 
  select(rating, review_content) %>% 
  rownames_to_column("id") %>% 
  unnest_tokens(word, review_content)


# --- Stop Words --- # 
count_words <- 
  tokens %>% 
  group_by(word) %>% 
  count(sort= TRUE)


#--- Custom Stop Words --- #
# stop_words words that doesnt have meaning default numbers is not included
tokens_no_stop <- 
  tokens %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word,"[[:digit:]]+"))
  
# str_detect(tokens_no_stop$word, "[[:digit:]]+") identifies which have numbers ! which doesnt have

count_words <- tokens_no_stop %>% 
  group_by(word) %>% 
  count(sort=TRUE)

# Content specific stop words

custom_stop_words <- tibble(
  word =c(
    'camera',
    'cameras',
    'review'
    )
  )

tokens_no_stop_custom <- 
  tokens %>% 
  anti_join(stop_words) %>% 
  anti_join(custom_stop_words) %>% 
  filter(!str_detect(word,"[[:digit:]]+"))

count_words <- tokens_no_stop_custom %>% 
  group_by(word) %>% 
  count(sort = TRUE) %>% 
  ungroup()

count_words %>% 
  top_n(40) %>% 
  ggplot() +
  geom_col(aes(n, 
               reorder(word,n)
               ) 
           ) + 
  scale_y_reordered()


# --- Stemming --- #
# cutting off plurals and past tense chopped the end. 

tokens_stem <- tokens_no_stop_custom %>% 
  mutate(word_stem = stem_words(word))

# cleaner approch
tokens_stem <- tokens_no_stop_custom %>% 
  mutate(word_stem = stem_words(word),
         word_lemma = lemmatize_words(word))

count_words <- 
  tokens_stem %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  ungroup()

count_words %>% 
  top_n(40) %>% 
  ggplot() +
  geom_col(aes(n, 
               reorder(word_lemma,n)
  ) 
  ) + 
  scale_y_reordered()

# --- TF-IDF --- #
# scale the importance by reweighting. 
###################################### we will come this next week

cameras_tf_idf <- tokens_stem %>% 
  # data needs to be treated as one group of reviews
  mutate(dummy = 1 ) %>% 
  group_by(dummy,word_lemma) %>% 
  count(sort = TRUE) %>% 
  bind_tf_idf(word_lemma, dummy,n) %>% 
  ungroup()

cameras_tf_idf %>% 
  # top_n(40) %>% 
  ggplot() +
  geom_col(aes(n, 
               reorder(word_lemma,tf_idf)
  ) 
  ) + 
  scale_y_reordered()



# --- Word Clouds --- #
tokens_stem %>% 
  mutate(negative = if_else(rating <= 3, "negative", "positive")) %>% 
  group_by(word_lemma,negative) %>% 
  count(sort = TRUE) %>% 
  acast(word_lemma ~ negative, value.var= "n", fill= 0) %>% 
  comparison.cloud()

























