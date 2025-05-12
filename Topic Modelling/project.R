library(rvest)
library(tm)
library(tokenizers)
library(hunspell)
library(textstem)
library(textclean)

url <- "https://en.wikipedia.org/wiki/American_International_University-Bangladesh"
webpage <- read_html(url)
text_data <- html_text(html_nodes(webpage, "p"))


lower_case_data <- tolower(text_data)
print(lower_case_data)

no_punc_data <- gsub("[[:punct:]]", "", lower_case_data)
print(no_punc_data)

no_digit_data <- gsub("\\d+", "", no_punc_data)

no_space_data <- stripWhitespace(no_digit_data)

no_contraction_text <- replace_contraction(no_space_data)


emoji_pattern <- "[\U0001F600-\U0001F64F]"
emoticon_pattern <- "[:;]-?[)D(|P]" 
text_no_emojis <- sapply(no_contraction_text, function(text) {
  text <- gsub(emoji_pattern, "", text, perl = TRUE)
  text <- gsub(emoticon_pattern, "", text, perl = TRUE)
  return(text)
})


text_no_stopwords <- removeWords(text_no_emojis, stopwords("en"))

remove_misspelled_words <- function(text) {
  words <- unlist(tokenize_words(text))
  is_correct <- hunspell_check(words)
  corrected_words <- words[is_correct]
  return(paste(corrected_words, collapse = " "))
}

spell_checked_text <- sapply(text_no_stopwords, remove_misspelled_words)

tokens <- tokenize_words(spell_checked_text)

clean_tokens <- unlist(tokens)

clean_tokens <- lemmatize_words(clean_tokens)

clean_tokens <- clean_tokens[nchar(clean_tokens) > 1]

tokens_df <- data.frame(tokens = clean_tokens, stringsAsFactors = FALSE)


write.csv(tokens_df, "D://Educational/AIUB sem 9/MID/Data Science/Project2.csv", row.names = FALSE)

