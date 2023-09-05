library(tidyverse)
library(readr)
library(dplyr)
library(foreach)

words_raw <- read_csv("unigram_freq.csv")
words_clean <- na.omit(words_raw)
df_2Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==2))
df_3Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==3))
df_4Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==4))
df_5Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==5))
df_6Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==6))
df_8Letter_words <- data.frame(subset(words_clean,nchar(words_clean$word, keepNA=FALSE)==8))


rdl_revealed_letters = "[^eo]"

createRegex = function(input_conditions){
  tmp_word = c(rep("",length(input_conditions)))
  # tmp_unique = paste(c(
  #   "[^",
  #   paste(unique(input_conditions[nzchar(input_conditions)]),collapse = ''),
  #   "]"
  #   ),collapse = '')
  tmp_unique = rdl_revealed_letters
  foreach(i = 1:length(input_conditions)) %do% {
    tmp_word[i] <- ifelse(input_conditions[i]!="",
                          paste(c(
                            "[",
                            input_conditions[i],
                            "]"),collapse = ''),
                          tmp_unique)
  }
  return <- paste(tmp_word,collapse = '')
}

createDFPos = function(input_condition){
  return <-data.frame(subset(words_clean,grepl(createRegex(input_condition),word) & nchar(word, keepNA=FALSE)==length(input_condition)))

}

#rdl_1_rgx = "[o][^oe][^oe][e][^oe][^oe]"


#Riddle Words conditions
rdl_sen1word1_con = c("o","","","e","","") #6 letters, 1 character = O, 4 character = e
rdl_sen1word2_con = c("","","o","","","")
rdl_sen1word3_con = c("","e")
rdl_sen1word4_con = c("","","","","","")
rdl_sen1word5_con = c("","","","")
rdl_sen1word6_con = c("","")
rdl_sen1word7_con = c("","e","","")

df_sen1word1 <- createDFPos(rdl_sen1word1_con)
df_sen1word2 <- createDFPos(rdl_sen1word2_con)
df_sen1word3 <- createDFPos(rdl_sen1word3_con)
df_sen1word4 <- createDFPos(rdl_sen1word4_con)
df_sen1word5 <- createDFPos(rdl_sen1word5_con)
df_sen1word6 <- createDFPos(rdl_sen1word6_con)
df_sen1word7 <- createDFPos(rdl_sen1word7_con)
