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
  text_data <- readLines(file_path, warn = FALSE)  # Read text file
  tibble(text = text_data)  # Convert to tibble
}

# Function to clean the text data
clean_text_data <- function(text_data) {
  tidy_data <- text_data %>%
    mutate(line_number = row_number()) %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_remove_all(word, "[[:punct:]]")) %>%
    anti_join(stop_words, by = "word") %>%
    filter(nchar(word) > 1)
  return(tidy_data)
}

# Function to perform sentiment analysis
perform_sentiment_analysis <- function(tidy_data) {
  sentiment_lexicon <- get_sentiments("bing")
  
  sentiment_scores <- tidy_data %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(line_number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative)
  
  return(sentiment_scores)
}

# Function to generate a word cloud
generate_wordcloud <- function(tidy_data) {
  tidy_data %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, max.words = 100, colors = "darkgreen"))
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
  tidy_data %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    filter(n > 20) %>%
    ggplot(aes(reorder(word, n), n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(title = "Frequent Sentiment Words", x = "Word", y = "Frequency") +
    theme_minimal()
}

# Main function to analyze the file
analyze_text_file <- function(file_path) {
  text_data <- load_text_data(file_path)
  cleaned_data <- clean_text_data(text_data)
  sentiment_scores <- perform_sentiment_analysis(cleaned_data)
  
  # Plot results
  plot_sentiment_trends(sentiment_scores)
  plot_frequent_sentiment_words(cleaned_data)
  generate_wordcloud(cleaned_data)
}

# Example usage
file_path <- "path/to/your/textfile.txt"  # Update with your file path
analyze_text_file(file_path)
