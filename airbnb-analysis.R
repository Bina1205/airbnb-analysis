## Alnina Akhmatova 
## Cohort: MBAN2
## A1: Airbnb Text Analysis
# Title: Airbnb Text Analysis 

# Step 1: Loading required libraries

#install.packages("cld2")
library(cld2)
library(mongolite)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tidyr)
library(stringr)
library(tm)
library(topicmodels)
library(ggplot2)

# Step 2: Connecting to MongoDB and loading Airbnb data
connection_string <- 'mongodb+srv://bina120502:12052002ka@cluster0.vvzsf.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection = "listingsAndReviews", db = "sample_airbnb", url = connection_string)
airbnb_all <- airbnb_collection$find('{}')

# Step 3: Unnesting host list-column to flat format
airbnb_all <- airbnb_all %>%
  unnest_wider(host)

# Step 4: Unnesting address list-column to flat format
airbnb_all <- airbnb_all %>%
  unnest_wider(address)

# Step 5: Unnesting review_score list-column to flat format
airbnb_all <- airbnb_all %>%
  unnest_wider(review_scores)
print(airbnb_all$review_scores_rating)

# Step 6: Unnesting availability list-column to flat format

airbnb_all <- airbnb_all %>%
  unnest_wider(availability)

# Step 7: Filtering USA-based listings with available descriptions
airbnb_usa_canada_australia <- airbnb_all %>%
  filter(!is.na(description), !is.na(host_location)) %>%
  filter(str_detect(host_location, "United States|USA|Canada|Australia")) %>%
  select(description, price, security_deposit, 
         cleaning_fee, host_listings_count, cancellation_policy, property_type, amenities,
         room_type, host_response_rate, review_scores_rating, number_of_reviews, minimum_nights, maximum_nights, availability_30, availability_60,
         availability_90, availability_365)
#-----------USA----------
airbnb_usa <- airbnb_all %>%
  filter(!is.na(description), !is.na(host_location)) %>%
  filter(str_detect(host_location, "United States|USA")) %>%
  select(description, price, security_deposit, 
         cleaning_fee, host_listings_count, cancellation_policy, property_type, amenities,
         room_type, host_response_rate, review_scores_rating, number_of_reviews, minimum_nights, maximum_nights, availability_30, availability_60,
         availability_90, availability_365)
#-----------Canada----------
airbnb_canada <- airbnb_all %>%
  filter(!is.na(description), !is.na(host_location)) %>%
  filter(str_detect(host_location, "Canada")) %>%
  select(description, price, security_deposit, 
         cleaning_fee, host_listings_count, cancellation_policy, property_type, amenities,
         room_type, host_response_rate, review_scores_rating, number_of_reviews, minimum_nights, maximum_nights, availability_30, availability_60,
         availability_90, availability_365)

#-----------Australia----------

airbnb_australia <- airbnb_all %>%
  filter(!is.na(description), !is.na(host_location)) %>%
  filter(str_detect(host_location, "Australia")) %>%
  select(description, price, security_deposit, 
         cleaning_fee, host_listings_count, cancellation_policy, property_type, amenities,
         room_type, host_response_rate, review_scores_rating, number_of_reviews, minimum_nights, maximum_nights, availability_30, availability_60,
         availability_90, availability_365)
# =====================================Step 8: Mutating and removing description column===================================================

airbnb_usa_canada_australia <- airbnb_usa_canada_australia%>%
  mutate(text = description) %>%
  select(-description)

airbnb_usa <- airbnb_usa %>%
  mutate(text = description) %>%
  select(-description)

airbnb_canada <- airbnb_canada %>%
  mutate(text = description) %>%
  select(-description)


airbnb_australia <- airbnb_australia %>%
  mutate(text = description) %>%
  select(-description)

# =======================Step 9: Tokenizing and analyzing each field separately=================================================
# ---------Canada-USA-Australia---------------
tidy_text_usa_canada_australia <- airbnb_usa_canada_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  # Use correct function
  filter(language == "en")  # Keep only English words

#------------USA--------------

tidy_text_usa <- airbnb_usa %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  # Use correct function
  filter(language == "en")  # Keep only English words

# --------------Canada-----------

tidy_text_canada <- airbnb_canada %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  # Use correct function
  filter(language == "en")  # Keep only English words
# ---------Australia-------------

tidy_text_australia <- airbnb_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  # Use correct function
  filter(language == "en")  # Keep only English words


# =======================Step 10: Frequency analysis of words============================

#--------USA-Canada-Australia---------------
word_freq_usa_canada_australia <- tidy_text_usa_canada_australia %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "NA|[^a-zA-Z]")
  )
# ----------USA-------

word_freq_usa <- tidy_text_usa %>%
  count(word, sort = TRUE) %>%
  filter(n > 50)%>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "NA|[^a-zA-Z]")
  )

# -------Canada--------
word_freq_canada <- tidy_text_canada %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)%>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "NA|[^a-zA-Z]")
  )


# -------Australia--------
word_freq_australia <- tidy_text_australia %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)%>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "NA|[^a-zA-Z]")
  )





# =====================================Visualize word frequency============================================
# ----------USA-Canada-Australia-------------------
ggplot(word_freq_usa_canada_australia, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Word Frequency in Airbnb Descriptions for USA,Canada and Australia",
       x = "Word", y = "Frequency")
# ---------USA------------
ggplot(word_freq_usa, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Word Frequency in Airbnb Descriptions for USA",
       x = "Word", y = "Frequency")
# --------Canada---------
ggplot(word_freq_canada, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Word Frequency in Airbnb Descriptions for Canada",
       x = "Word", y = "Frequency")

# -------Australia-----
ggplot(word_freq_australia, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Word Frequency in Airbnb Descriptions for Australia",
       x = "Word", y = "Frequency")

# =====================Step 11: Analyzing price by property type==========================
price_by_property <- airbnb_usa_canada_australia %>%
  group_by(property_type) %>%
  summarize(mean_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

# Visualize price by property type
ggplot(price_by_property, aes(x = reorder(property_type, mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Average Price by Property Type in the USA, Canada, Australia",
       x = "Property Type", y = "Average Price")

# Step 12: Analyzing review count and its relationship with price
review_vs_price <- airbnb_usa_canada_australia %>%
  filter(!is.na(number_of_reviews), !is.na(price)) %>%
  ggplot(aes(x = number_of_reviews, y = price)) +
  geom_point(aes(color = review_scores_rating), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price vs. Number of Reviews",
       x = "Number of Reviews", y = "Price")
review_vs_price



# ===========================Step 13: N-grams analysis (Bigrams, Trigrams, and 4-grams)======================

# 1. Bigrams

# --------
tidy_text_bigrams <- airbnb_usa_canada_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 50)


# ------------Visualize bigrams: USA, Canada and Australia-------------
ggplot(tidy_text_bigrams, aes(x = reorder(paste(word1, word2), n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Bigram Frequency in Airbnb Descriptions for USA, Canada and Australia",
       x = "Bigram", y = "Frequency")

# ---------USA--------

tidy_text_bigrams_usa <- airbnb_usa %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 50)

# ------------Visualize bigrams: USA-------------
ggplot(tidy_text_bigrams_usa, aes(x = reorder(paste(word1, word2), n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Bigram Frequency in Airbnb Descriptions for USA",
       x = "Bigram", y = "Frequency")


# --------Canada-----------------

tidy_text_bigrams_canada <- airbnb_canada %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 10)




# ------------Visualize bigrams:Canada-------------
tidy_text_usa_canada_australia <- airbnb_usa_canada_australia %>%
  select(description) %>%
  filter(!is.na(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)%>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  # Use correct function
  filter(language == "en")  # Keep only English words

ggplot(tidy_text_bigrams_canada, aes(x = reorder(paste(word1, word2), n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Bigram Frequency in Airbnb Descriptions for Canada",
       x = "Bigram", y = "Frequency")

# --------------------Australia--------------
tidy_text_bigrams_australia <- airbnb_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 15)


# ------------Visualize bigrams: Australia-------------
ggplot(tidy_text_bigrams_australia, aes(x = reorder(paste(word1, word2), n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Bigram Frequency in Airbnb Descriptions for Australia",
       x = "Bigram", y = "Frequency")





# 2.-----------------------Trigrams: USA, Canada and Australia-----------------------
tidy_text_trigrams <- airbnb_usa_canada_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 9)

# ------------- Visualization: Trigrams: USA, Canada and Australia---------------
ggplot(tidy_text_trigrams, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Trigram Frequency in Airbnb Descriptions for USA, Canada and Australia",
       x = "Trigram", y = "Frequency")

# ----------------------Trigrams: USA-----------------------
tidy_text_trigrams_usa <- airbnb_usa %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 9)

# ------------- Visualization: Trigrams: USA---------------
ggplot(tidy_text_trigrams_usa, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Trigram Frequency in Airbnb Descriptions for USA",
       x = "Trigram", y = "Frequency")




# ----------------------Trigrams: Canada-----------------------
tidy_text_trigrams_canada <- airbnb_canada %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 7)

# ------------- Visualization: Trigrams: Canada---------------
ggplot(tidy_text_trigrams_canada, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Trigram Frequency in Airbnb Descriptions for USA",
       x = "Trigram", y = "Frequency")



# ----------------------Trigrams: Australia-----------------------
tidy_text_trigrams_australia <- airbnb_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 8)

# ------------- Visualization: Trigrams: Australia---------------
ggplot(tidy_text_trigrams_australia, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Trigram Frequency in Airbnb Descriptions for USA",
       x = "Trigram", y = "Frequency")





# 3. # ---------------- 4-grams for USA, Canada and Australia -------------------
tidy_text_fourgrams <- airbnb_usa_canada_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, word4, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, word4, sort = TRUE) %>%
  filter(n > 10)

# ------------- Visualization: 4-grams: USA, Canada and Australia ---------------
ggplot(tidy_text_fourgrams, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "4-gram Frequency in Airbnb Descriptions for USA, Canada and Australia",
       x = "4-gram", y = "Frequency")


# 3. # ---------------- 4-grams for USA -------------------
tidy_text_fourgrams_usa <- airbnb_usa %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, word4, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, word4, sort = TRUE) %>%
  filter(n > 7)

# ------------- Visualization: 4-grams: USA ---------------
ggplot(tidy_text_fourgrams_usa, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "4-gram Frequency in Airbnb Descriptions for USA",
       x = "4-gram", y = "Frequency")




# ---------------- 4-grams for Canada-------------------
tidy_text_fourgrams_canada <- airbnb_canada %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, word4, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, word4, sort = TRUE) %>%
  filter(n > 2)

# ------------- Visualization: 4-grams: Canada ---------------
ggplot(tidy_text_fourgrams_canada, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "4-gram Frequency in Airbnb Descriptions for Canada",
       x = "4-gram", y = "Frequency")


# 3. # ---------------- 4-grams for Australia-------------------
tidy_text_fourgrams_australia <- airbnb_australia %>%
  select(text) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  mutate(phrase = paste(word1, word2, word3, word4, sep = " ")) %>%
  mutate(language = cld2::detect_language(phrase, plain_text = TRUE)) %>%
  filter(language == "en") %>%
  count(word1, word2, word3, word4, sort = TRUE) %>%
  filter(n > 5)

# ------------- Visualization: 4-grams: Australia ---------------
ggplot(tidy_text_fourgrams_canada, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "4-gram Frequency in Airbnb Descriptions for Australia",
       x = "4-gram", y = "Frequency")



# ====================== Step 14: Analyzing Cancellation Policy by Property Type ======================
cancellation_analysis <- airbnb_usa_canada_australia %>%
  group_by(property_type, cancellation_policy) %>%
  summarize(mean_price = mean(price, na.rm = TRUE),
            mean_reviews = mean(number_of_reviews, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

# Visualize cancellation policy impact on price by property type
ggplot(cancellation_analysis, aes(x = reorder(property_type, mean_price), y = mean_price, fill = cancellation_policy)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Cancellation Policy Impact on Price by Property Type",
       x = "Property Type", y = "Average Price")


# ============================================ Step 15: TF IDF with price bin =====================================
# Create the price_bin column 
library(dplyr)
airbnb_usa_canada_australia <- airbnb_usa_canada_australia %>%
  mutate(price_bin = ntile(price, 4),
         price_bin = factor(price_bin,
                            labels = c("Low", "Medium-Low", "Medium-High", "High")))

library(tidytuesdayR)
library(dplyr)
library(tidytext)

# create a new data frame and rename the column
workingdf <- airbnb_usa_canada_australia %>%
  select(
    text,
    price_bin,
    
  )

colnames(workingdf)[1] <- "text"
colnames(workingdf)[2] <- "price_bin"

library(dplyr)
library(stringr)
library(tidytext)
library(tidytuesdayR)

#we're grouping by the price_bin this time
workingdf_token <- workingdf %>%
  unnest_tokens(word, text) %>%
  count(price_bin, word, sort=TRUE) %>%
  ungroup()

total_words <- workingdf_token %>%
  group_by(price_bin) %>%
  summarise(total=sum(n))

workingdf_words <- left_join(workingdf_token, total_words)


print(workingdf_words) 

library(ggplot2)
ggplot(workingdf_words, aes(x = n / total, fill = price_bin)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.001) +
  facet_wrap(~price_bin, ncol = 2, scales = "free_y") +
  labs(title = "Distribution of Word Frequencies by Price Bin",
       x = "Proportion of Words",
       y = "Count of Words")



country_words <- workingdf_words %>%
  bind_tf_idf(word, price_bin, n)

country_words 
country_words %>%
  arrange(desc(idf))

country_words %>%
  arrange(desc(tf_idf))

country_words %>%filter (price_bin=="High")%>%
  arrange(desc(tf_idf))



#############
# looking at the graphical apprach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(price_bin) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=price_bin))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~price_bin, ncol=2, scales="free")+
  coord_flip()

# ============================================ Step 16: TF IDF with Score bin =====================================

# Create the score_bin column 
airbnb_usa_canada_australia <- airbnb_usa_canada_australia %>%
  mutate(score_bin = case_when(
    review_scores_rating < 85 ~ "Low Score",
    review_scores_rating > 90 ~ "High Score",
    TRUE ~ "Medium Score"
  )) %>%
  mutate(score_bin = factor(score_bin, levels = c("Low Score", "Medium Score", "High Score")))


library(tidytuesdayR)
library(dplyr)
library(tidytext)

# create a new data frame and rename the column
workingdf1 <- airbnb_usa_canada_australia %>%
  select(
    text,
    score_bin,
    
  )

colnames(workingdf1)[1] <- "text"
colnames(workingdf1)[2] <- "score_bin"



library(dplyr)
library(stringr)
library(tidytext)
#let's look at the data
library(tidytuesdayR)

#we're grouping by the country this time
workingdf_token1 <- workingdf1 %>%
  unnest_tokens(word, text) %>%
  count(score_bin, word, sort=TRUE) %>%
  ungroup()

total_words <- workingdf_token1 %>%
  group_by(score_bin) %>%
  summarise(total=sum(n))

workingdf_words1 <- left_join(workingdf_token1, total_words)


print(workingdf_words1) 

library(ggplot2)
ggplot(workingdf_words1, aes(x = n / total, fill = score_bin)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.001) +
  facet_wrap(~score_bin, ncol = 2, scales = "free_y") +
  labs(title = "Distribution of Word Frequencies by Score Bin",
       x = "Proportion of Words",
       y = "Count of Words")



country_words2 <- workingdf_words1 %>%
  bind_tf_idf(word, score_bin, n)

country_words2 

country_words2 %>%
  arrange(desc(idf))

country_words2 %>%
  arrange(desc(tf_idf))

country_words2 %>%filter (score_bin=="High")%>%
  arrange(desc(tf_idf))



# looking at the graphical apprach:
country_words2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(score_bin) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=score_bin))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~score_bin, ncol=2, scales="free")+
  coord_flip()


# ============================================ Step 17: Correllagram =====================================

airbnb_usa_canada_australia <- airbnb_all %>%
  filter(!is.na(description), !is.na(host_location)) %>%
  filter(str_detect(host_location, "United States|USA|Canada|Australia")) %>%
  select(description, price, security_deposit, 
         cleaning_fee, host_listings_count, cancellation_policy, property_type, amenities,
         room_type, host_response_rate, review_scores_rating, number_of_reviews, minimum_nights, maximum_nights, availability_30, availability_60,
         availability_90, availability_365,country)

tidy_text_usa_canada_australia <- airbnb_usa_canada_australia %>%
  select(description) %>%
  filter(!is.na(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)%>%
  mutate(language = cld2::detect_language(word, plain_text = TRUE)) %>%  
  filter(language == "en")  # Keep only English words

library(tidytuesdayR)
library(dplyr)
library(tidytext)

# create a new data frame and rename the column
workingdf <- airbnb_usa_canada_australia %>%
  select(
    description,
    country,
  )

colnames(workingdf)[1] <- "text"
colnames(workingdf)[2] <- "country"

### creating a tidy format for United States movies
usa <- workingdf %>%
  filter(country== "United States")

tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

### creating a tidy format for Canada
can <- workingdf %>%
  filter(country== "Canada")

tidy_can <- can %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_can)

### creating a tidy format for \ Australia
aus <- workingdf %>%
  filter(country== "Australia")

tidy_aus <- aus %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_aus)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_can, author= "Canada"),
                       mutate(tidy_aus, author="Australia")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, Canada, Australia)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y= `United States`,color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)




#===========================================================================================
# ======================= Shiny DashBoard===================================================    
    
library(shiny)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)
library(shinythemes) 
library(shinycssloaders)   
      
      #============================= Basic Structure =============================================
      ui <- navbarPage(
        title = "Airbnb Text Insights",
        theme = shinytheme("flatly"),
        
        tabPanel("Frequency For Country",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 1"),plotOutput("freq_3_country")),
                     column(6,h3("Plot 2"),plotOutput("freq_usa"))
                   ),

                   fluidPage(
                     column(6,h3("Plot 3"),plotOutput("freq_can")),
                     column(6,h3("Plot 4"),plotOutput("freq_aus"))
                   )
                   )
        ),
        
        tabPanel("Bigram with Country ",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 5"),plotOutput("bigram_3_country")),
                     column(6,h3("Plot 6"),plotOutput("bigram_usa"))
                   ),

                   fluidPage(
                     column(6,h3("Plot 7"),plotOutput("bigram_can")),
                     column(6,h3("Plot 8"),plotOutput("bigram_aus"))
                   )
                   )
        ),
        
         tabPanel("Trigram with Country ",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 9"),plotOutput("trigrams_3_country")),
                     column(6,h3("Plot 10"),plotOutput("trigrams_usa"))
                   ),

                   fluidPage(
                     column(6,h3("Plot 11"),plotOutput("trigrams_can")),
                     column(6,h3("Plot 12"),plotOutput("trigrams_aus"))
                   )
                   )
        ),

        tabPanel("Four-gram with Country ",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 13"),plotOutput("four_gram_3_country")),
                     column(6,h3("Plot 14"),plotOutput("four_gram_usa"))
                   ),

                   fluidPage(
                     column(6,h3("Plot 15"),plotOutput("four_gram_can")),
                     column(6,h3("Plot 16"),plotOutput("four_gram_aus"))
                   )
                   )
        ),


        tabPanel("Numerical data Analysis",
                 wellPanel(
                   fluidRow(
                     column(6, h4("Plot 17"), plotOutput("price_with_property_type")),
                     column(6, h4("Plot 18"), plotOutput("cancelation_with_price_analysis")),
                   ))
        ),


        
        tabPanel("Correlograms with Country",
                 wellPanel(
                   fluidRow(
                     column(6, h4("Plot 19"), plotOutput("country_corregram")),
                   ))
        ),

        tabPanel("Description with Price",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 20"),plotOutput("feq_price_bin")),
                     column(6,h3("Plot 21"),plotOutput("feq_word_price_bin"))
                   )
        )),

        tabPanel("Description with Score",
                 wellPanel(
                   fluidPage(
                     column(6,h3("Plot 22"),plotOutput("feq_score_bin")),
                     column(6,h3("Plot 23"),plotOutput("feq_word_score_bin"))
                   ))
        )
      )
        



      
      
      #============================= Plots =============================================
      server <- function(input, output) {
        
        #============================= PLOT 1 =============================================
        output$freq_3_country <- renderPlot({
          ggplot(word_freq_usa_canada_australia, aes(x = reorder(word, n), y = n)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Word Frequency in Airbnb Descriptions for USA,Canada and Australia",
                x = "Word", y = "Frequency")
         
          
        })
        
        
        #============================= PLOT 2 =============================================
        
        output$freq_usa <- renderPlot({
          # ---------USA------------
            ggplot(word_freq_usa, aes(x = reorder(word, n), y = n)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              coord_flip() +
              labs(title = "Word Frequency in Airbnb Descriptions for USA",
                x = "Word", y = "Frequency")
          
        })
        
        
        #============================= PLOT 3 =============================================
        
        output$ freq_can<- renderPlot({
          # --------Canada---------
          ggplot(word_freq_canada, aes(x = reorder(word, n), y = n)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Word Frequency in Airbnb Descriptions for Canada",
              x = "Word", y = "Frequency")
         
        })
        
        
        #============================= PLOT 4 =============================================
        output$freq_aus <- renderPlot({
         # -------Australia-----
          ggplot(word_freq_australia, aes(x = reorder(word, n), y = n)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Word Frequency in Airbnb Descriptions for Australia",
              x = "Word", y = "Frequency")
        })
        
        #============================= PLOT 5 =============================================
        
        output$price_with_property_type <- renderPlot({
          ggplot(price_by_property, aes(x = reorder(property_type, mean_price), y = mean_price)) +
            geom_bar(stat = "identity", fill = "blue") +
            coord_flip() +
            labs(title = "Average Price by Property Type in the USA, Canada, Australia",
                x = "Property Type", y = "Average Price")
        })
        
        #============================= PLOT 6 =============================================
        
        output$bigram_3_country <- renderPlot({
          ggplot(tidy_text_bigrams, aes(x = reorder(paste(word1, word2), n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Bigram Frequency in Airbnb Descriptions for USA, Canada and Australia",
       x = "Bigram", y = "Frequency")

        })
        
        #============================= PLOT 7 =============================================
        
        output$bigram_usa<- renderPlot({
          ggplot(tidy_text_bigrams_usa, aes(x = reorder(paste(word1, word2), n), y = n)) +
            geom_bar(stat = "identity", fill = "salmon") +
            coord_flip() +
            labs(title = "Bigram Frequency in Airbnb Descriptions for USA",
                x = "Bigram", y = "Frequency")
          
        })

        #============================= PLOT 8 =============================================
        
        output$bigram_can<- renderPlot({
          ggplot(tidy_text_bigrams_canada, aes(x = reorder(paste(word1, word2), n), y = n)) +
            geom_bar(stat = "identity", fill = "salmon") +
            coord_flip() +
            labs(title = "Bigram Frequency in Airbnb Descriptions for Canada",
                x = "Bigram", y = "Frequency")
        })

        #============================= PLOT 9 =============================================
        
        output$bigram_aus<- renderPlot({
          ggplot(tidy_text_bigrams_australia, aes(x = reorder(paste(word1, word2), n), y = n)) +
            geom_bar(stat = "identity", fill = "salmon") +
            coord_flip() +
            labs(title = "Bigram Frequency in Airbnb Descriptions for Australia",
                x = "Bigram", y = "Frequency")


          
        })

        #============================= PLOT 10 =============================================
        
        output$trigrams_3_country <- renderPlot({
          ggplot(tidy_text_trigrams, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            coord_flip() +
            labs(title = "Trigram Frequency in Airbnb Descriptions for USA, Canada and Australia",
                x = "Trigram", y = "Frequency")
          
        })

        #============================= PLOT 11 =============================================
        
        output$trigrams_usa <- renderPlot({
          ggplot(tidy_text_trigrams_usa, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            coord_flip() +
            labs(title = "Trigram Frequency in Airbnb Descriptions for USA",
                x = "Trigram", y = "Frequency")

          
        })

        #============================= PLOT 12 =============================================
        
        output$trigrams_can <- renderPlot({
          ggplot(tidy_text_trigrams_canada, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            coord_flip() +
            labs(title = "Trigram Frequency in Airbnb Descriptions for Canada",
                x = "Trigram", y = "Frequency")

          
        })

        #============================= PLOT 13 =============================================
        
        output$trigrams_aus <- renderPlot({
          ggplot(tidy_text_trigrams_australia, aes(x = reorder(paste(word1, word2, word3), n), y = n)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            coord_flip() +
            labs(title = "Trigram Frequency in Airbnb Descriptions for Australia",
                x = "Trigram", y = "Frequency")

          
        })

        #============================= PLOT 14 =============================================
        
        output$four_gram_3_country <- renderPlot({
          ggplot(tidy_text_fourgrams, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
            geom_bar(stat = "identity", fill = "lightgreen") +
            coord_flip() +
            labs(title = "4-gram Frequency in Airbnb Descriptions for USA, Canada and Australia",
                x = "4-gram", y = "Frequency")

          
        })

        #============================= PLOT 15 =============================================
        
        output$four_gram_usa <- renderPlot({
          ggplot(tidy_text_fourgrams_usa, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
            geom_bar(stat = "identity", fill = "lightgreen") +
            coord_flip() +
            labs(title = "4-gram Frequency in Airbnb Descriptions for USA",
                x = "4-gram", y = "Frequency")

          
        })

        #============================= PLOT 16 =============================================
        
        output$four_gram_can <- renderPlot({
          ggplot(tidy_text_fourgrams_canada, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
            geom_bar(stat = "identity", fill = "lightgreen") +
            coord_flip() +
            labs(title = "4-gram Frequency in Airbnb Descriptions for Canada",
                x = "4-gram", y = "Frequency")

          
        })

        #============================= PLOT 17 =============================================
        
        output$four_gram_aus <- renderPlot({
          ggplot(tidy_text_fourgrams_australia, aes(x = reorder(paste(word1, word2, word3, word4), n), y = n)) +
            geom_bar(stat = "identity", fill = "lightgreen") +
            coord_flip() +
            labs(title = "4-gram Frequency in Airbnb Descriptions for Australia",
                x = "4-gram", y = "Frequency")

        })

        #============================= PLOT 18 =============================================
        
        output$cancelation_with_price_analysis <- renderPlot({
          ggplot(cancellation_analysis, aes(x = reorder(property_type, mean_price), y = mean_price, fill = cancellation_policy)) +
            geom_bar(stat = "identity", position = "dodge") +
            coord_flip() +
            labs(title = "Cancellation Policy Impact on Price by Property Type",
                x = "Property Type", y = "Average Price")
        })


        #============================= PLOT 19 =============================================
        
        output$feq_price_bin <- renderPlot({
          ggplot(workingdf_words, aes(x = n / total, fill = price_bin)) +
            geom_histogram(show.legend = FALSE, bins = 30) +
            xlim(NA, 0.001) +
            facet_wrap(~price_bin, ncol = 2, scales = "free_y") +
            labs(title = "Distribution of Word Frequencies by Price Bin",
                x = "Proportion of Words",
                y = "Count of Words")
          
        })

        #============================= PLOT 20 =============================================
        
        output$feq_word_price_bin <- renderPlot({
          country_words %>%
            arrange(desc(tf_idf)) %>%
            mutate(word=factor(word, levels=rev(unique(word)))) %>%
            group_by(price_bin) %>%
            top_n(15) %>%
            ungroup %>%
            ggplot(aes(word, tf_idf, fill=price_bin))+
            geom_col(show.legend=FALSE)+
            labs(x=NULL, y="tf-idf")+
            facet_wrap(~price_bin, ncol=2, scales="free")+
            coord_flip()
          
        })

        #============================= PLOT 21 =============================================
        
        output$feq_score_bin <- renderPlot({
          ggplot(workingdf_words1, aes(x = n / total, fill = score_bin)) +
            geom_histogram(show.legend = FALSE, bins = 30) +
            xlim(NA, 0.001) +
            facet_wrap(~score_bin, ncol = 2, scales = "free_y") +
            labs(title = "Distribution of Word Frequencies by Score Bin",
                x = "Proportion of Words",
                y = "Count of Words")
        })

        #============================= PLOT 22 =============================================
        
        output$feq_word_score_bin <- renderPlot({
          country_words2 %>%
            arrange(desc(tf_idf)) %>%
            mutate(word=factor(word, levels=rev(unique(word)))) %>%
            group_by(score_bin) %>%
            top_n(15) %>%
            ungroup %>%
            ggplot(aes(word, tf_idf, fill=score_bin))+
            geom_col(show.legend=FALSE)+
            labs(x=NULL, y="tf-idf")+
            facet_wrap(~score_bin, ncol=2, scales="free")+
            coord_flip()
        })

        #============================= PLOT 23 =============================================
        
        output$country_corregram <- renderPlot({
          library(scales)
          ggplot(frequency, aes(x=proportion, y= `United States`,color = abs(`United States`- proportion)))+
            geom_abline(color="grey40", lty=2)+
            geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
            geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
            scale_x_log10(labels = percent_format())+
            scale_y_log10(labels= percent_format())+
            scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
            facet_wrap(~author, ncol=2)+
            theme(legend.position = "none")+
            labs(y= "United States", x=NULL)
                    
        })

        
      }
      
      shinyApp(ui = ui, server = server)
      
  


