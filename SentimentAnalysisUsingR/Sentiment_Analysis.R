# Load necessary libraries
library(tidytext)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

# Function to load text from a file
load_text_data <- function(file_path) {
  text_data <- readLines(file_path, warn = FALSE)  # Read text file
  data_frame(text = text_data)  # Convert to data frame
}

# Function to clean the text data
clean_text_data <- function(text_data) {
  tidy_data <- text_data %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_remove_all(word, "[[:punct:]]")) %>%
    filter(!word %in% stop_words$word) %>%
    filter(nchar(word) > 1)
  return(tidy_data)
}

# Function to perform sentiment analysis
perform_sentiment_analysis <- function(tidy_data) {
  # Load sentiment lexicon
  bing <- get_sentiments("bing")
  
  # Compute sentiment scores
  sentiment_scores <- tidy_data %>%
    inner_join(bing) %>%
    count(linenumber, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
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
  ggplot(sentiment_scores, aes(linenumber, sentiment)) +
    geom_bar(stat = "identity") +
    labs(title = "Sentiment Trends", x = "Line Number", y = "Sentiment Score")
}

# Function to plot frequent sentiment words
plot_frequent_sentiment_words <- function(tidy_data) {
  tidy_data %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    filter(n > 20) %>%
    ggplot(aes(reorder(word, n), n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(title = "Frequent Sentiment Words", y = "Frequency")
}

# Main function to analyze the file
analyze_file <- function(file_path) {
  # Load and clean the data
  text_data <- load_text_data(file_path)
  tidy_data <- clean_text_data(text_data)
  
  # Perform sentiment analysis
  sentiment_scores <- perform_sentiment_analysis(tidy_data)
  
  # Plot results
  plot_sentiment_trends(sentiment_scores)
  plot_frequent_sentiment_words(tidy_data)
  generate_wordcloud(tidy_data)
}

# Example usage
file_path <- "path/to/your/textfile.txt"  # Change this to your file path
analyze_file(file_path)
