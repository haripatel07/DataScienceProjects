# Sentiment Analysis using R

## Overview
This project performs sentiment analysis on a text file using R. It utilizes the `tidytext` package to process and analyze textual data, identifying positive and negative words.

## Features
- Reads a text file and tokenizes words.
- Applies sentiment analysis using the Bing lexicon.
- Visualizes sentiment trends using `ggplot2`.
- Generates a word cloud for sentiment distribution.

## Installation
### Prerequisites
Ensure you have R installed along with the necessary libraries:
```r
install.packages(c("tidytext", "stringr", "dplyr", "tidyr", "ggplot2", "reshape2", "wordcloud"))
```

## Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/haripatel07/DataScienceProjects.git
   cd DataScienceProjects/SentimentAnalysis
   ```
2. Place your text file in the project directory and update `file_path` in `sentiment_analysis.R`.
3. Run the script in RStudio or using:
   ```r
   source("sentiment_analysis.R")
   ```

## Example Output
- **Bar chart of sentiment trends**
- **Word cloud of frequently used words**

## Contribution
Feel free to fork, modify, and improve this project!

## License
This project is open-source and available under the MIT License.


