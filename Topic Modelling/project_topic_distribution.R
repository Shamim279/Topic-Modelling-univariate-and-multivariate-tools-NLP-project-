
library(tm)
library(topicmodels)
library(ggplot2)

file_path <- "D://Educational/AIUB sem 9/MID/Data Science/Project2.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

corpus <- Corpus(VectorSource(data$tokens))
print(corpus)

dtm <- DocumentTermMatrix(corpus)
print(dtm)


row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ] 

dtm_tfidf <- weightTfIdf(dtm)


print(dtm_tfidf)

k <- 5  

lda_model <- LDA(dtm, k = k, control = list(seed = 123))

topic_term_weights <- posterior(lda_model)$terms




print("Topics with token weights:")
for (i in 1:k) {
  cat(sprintf("\nTopic %d:\n", i))

  token_weights <- sort(topic_term_weights[i, ], decreasing = TRUE)
  top_tokens <- head(token_weights, 10)
  
  for (token in names(top_tokens)) {
    cat(sprintf("%s (%.4f)\n", token, top_tokens[token]))
  }
}



topic_word_counts <- colSums(posterior(lda_model)$topics)

topic_df <- data.frame(
  Topic = paste("Topic", 1:k),
  WordCount = topic_word_counts
)


ggplot(topic_df, aes(x = Topic, y = WordCount, fill = Topic)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Words in Each Topic",
    x = "Topics",
    y = "Word Count"
  ) +
  theme_minimal()




for (i in 1:k) {
  token_weights <- sort(topic_term_weights[i, ], decreasing = TRUE)
  top_tokens <- head(token_weights, 10)
  

  topic_df <- data.frame(
    Term = names(top_tokens),
    Frequency = top_tokens
  )
  

  print(
    ggplot(topic_df, aes(x = reorder(Term, -Frequency), y = Frequency, fill = Term)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste("Top Words in Topic", i),
        x = "Words",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}
