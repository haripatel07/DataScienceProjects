# Load necessary libraries
library(tidytext)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(tibble)

# Function to load text from a file
load_text_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("The provided file path does not exist!")
  }
  tibble(text = readLines(file_path, warn = FALSE))
}

# Function to clean the text data
clean_text_data <- function(text_data) {
  text_data %>%
    mutate(line_number = row_number()) %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_remove_all(word, "[[:punct:]]")) %>%
    anti_join(stop_words, by = "word") %>%
    filter(nchar(word) > 1)
}

# Function to perform sentiment analysis
perform_sentiment_analysis <- function(tidy_data) {
  sentiment_lexicon <- get_sentiments("bing")
  tidy_data %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(line_number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative)
}

# Function to generate a word cloud
generate_wordcloud <- function(tidy_data) {
  sentiment_lexicon <- get_sentiments("bing")
  
  # Join tidy data with sentiment lexicon, count sentiment words, and reshape data
  sentiment_data <- tidy_data %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0)
  
  # Generate a comparison word cloud for positive and negative words
  comparison.cloud(sentiment_data, 
                   colors = c("red", "darkgreen"), 
                   max.words = 100)
}

# Function to plot sentiment trends
plot_sentiment_trends <- function(sentiment_scores) {
  ggplot(sentiment_scores, aes(line_number, sentiment_score)) +
    geom_col(fill = "steelblue") +
    labs(title = "Sentiment Trends", x = "Line Number", y = "Sentiment Score") +
    theme_minimal()
}

# Function to plot frequent sentiment words
plot_frequent_sentiment_words <- function(tidy_data) {
  sentiment_data <- tidy_data %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    filter(n > 0)
  
  ggplot(sentiment_data, aes(reorder(word, n), n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(title = "Frequent Sentiment Words", x = "Word", y = "Frequency") +
    theme_minimal()
}

# Main function to analyze the file
analyze_text_file <- function(file_path) {
  # Load, clean, and analyze the text data
  text_data <- load_text_data(file_path)
  cleaned_data <- clean_text_data(text_data)
  sentiment_scores <- perform_sentiment_analysis(cleaned_data)
  
  # Plot results
  print(plot_sentiment_trends(sentiment_scores))
  print(plot_frequent_sentiment_words(cleaned_data))
  print(generate_wordcloud(cleaned_data))
}

# Example usage
file_path <- "Enter/File/Path.txt"
analyze_text_file(file_path)
