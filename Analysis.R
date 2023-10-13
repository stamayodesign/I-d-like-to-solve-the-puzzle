library(tidyverse)
library(readr)
library(dplyr)
library(foreach)
library(stringr)

words_raw <- read_csv("unigram_freq.csv")
words_clean <- na.omit(words_raw)

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

rdl_sen2word1_con = c("o","")
rdl_sen2word2_con = c("","o")
rdl_sen2word3_con = c("","","","","")
rdl_sen2word4_con = c("","","","")

rdl_sen3word1_con = c("","","","o","","","","e")
rdl_sen3word2_con = c("","e","")
rdl_sen3word3_con = c("","")
rdl_sen3word4_con = c("","o")
rdl_sen3word5_con = c("","","","o","","","","e")
rdl_sen3word6_con = c("","","","","o","")
rdl_sen3word7_con = c("","")
rdl_sen3word8_con = c("","o","")

df_sen1word1 <- createDFPos(rdl_sen1word1_con)
df_sen1word2 <- createDFPos(rdl_sen1word2_con)
df_sen1word3 <- createDFPos(rdl_sen1word3_con)
df_sen1word4 <- createDFPos(rdl_sen1word4_con)
df_sen1word5 <- createDFPos(rdl_sen1word5_con)
df_sen1word6 <- createDFPos(rdl_sen1word6_con)
df_sen1word7 <- createDFPos(rdl_sen1word7_con)

df_sen2word1 <- createDFPos(rdl_sen2word1_con)
df_sen2word2 <- createDFPos(rdl_sen2word2_con)
df_sen2word3 <- createDFPos(rdl_sen2word3_con)
df_sen2word4 <- createDFPos(rdl_sen2word4_con)

df_sen3word1 <- createDFPos(rdl_sen3word1_con)
df_sen3word2 <- createDFPos(rdl_sen3word2_con)
df_sen3word3 <- createDFPos(rdl_sen3word3_con)
df_sen3word4 <- createDFPos(rdl_sen3word4_con)
df_sen3word5 <- createDFPos(rdl_sen3word5_con)
df_sen3word6 <- createDFPos(rdl_sen3word6_con)
df_sen3word7 <- createDFPos(rdl_sen3word7_con)
df_sen3word8 <- createDFPos(rdl_sen3word8_con)

df_sec1_top20 <- data.frame(first = df_sen1word1$word[1:20],
                            second = df_sen1word2$word[1:20],
                            third = df_sen1word3$word[1:20],
                            fourth = df_sen1word4$word[1:20],
                            fifth = df_sen1word5$word[1:20],
                            sixth = df_sen1word6$word[1:20],
                            seventh = df_sen1word7$word[1:20]
                            )


df_sec2_top20 <- data.frame(first = df_sen2word1$word[1:20],
                            second = df_sen2word2$word[1:20],
                            third = df_sen2word3$word[1:20],
                            fourth = df_sen2word4$word[1:20]
                            )



df_sec3_top20 <- data.frame(first = df_sen3word1$word[1:20],
                            second = df_sen3word2$word[1:20],
                            third = df_sen3word3$word[1:20],
                            fourth = df_sen3word4$word[1:20],
                            fifth = df_sen3word5$word[1:20],
                            sixth = df_sen3word6$word[1:20],
                            seventh = df_sen3word7$word[1:20],
                            eighth = df_sen3word8$word[1:20]
)

#Using word clouds
#templated from https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)

set.seed(1234) # for reproducibility 

#par(mfrow=c(1,1))

wordcloud(words = df_sen1word1$word, freq = df_sen1word1$count, min.freq = 34664,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df_sen1word2$word, freq = df_sen1word2$count, min.freq = 10000,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df_sen1word3$word, freq = df_sen1word3$count, min.freq = 566617666,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df_sen1word4$word, freq = df_sen1word4$count, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale=c(4,0.25) )

wordcloud(words = df_sen1word5$word, freq = df_sen1word5$count, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale=c(5,0.25) )

wordcloud(words = df_sen1word6$word, freq = df_sen1word6$count, min.freq = 576436203,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale=c(5,0.25) )

wordcloud(words = df_sen1word7$word, freq = df_sen1word7$count, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale=c(5,0.25) )

numA = 0
numB = 0
numC = 0
numD = 0
numE = 0
numF = 0
numG = 0
numH = 0
numI = 0
numJ = 0
numK = 0
numL = 0
numM = 0
numN = 0
numO = 0
numP = 0
numQ = 0
numR = 0
numS = 0
numT = 0
numU = 0
numV = 0
numW = 0
numX = 0
numY = 0
numZ = 0

lettersC <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
base_freqC <- replicate(26, 0)

determineLetterWeight <- function (inp_letter, inp_word, inp_weight){
  tmp_count <- str_count(inp_word,inp_letter)
  return <- tmp_count * inp_weight
}

df_sen1_word1_freqC <- base_freqC

determineSenWord <- function(inp_dfword,inp_wordfreqC){
  for (row in 1:nrow(inp_dfword)){
    for (i in 1:26) {
      tmp_freqC <- determineLetterWeight(lettersC[i],inp_dfword$word[row],inp_dfword$count[row])
      inc(inp_wordfreqC[i]) <- tmp_freqC
    }
  }
}


test1 <- determineLetterWeight("f",df_sen1word1$word[1],df_sen1word1$count[1])

df_sen1word1alphabet <-data.frame("word" = df_sen1word1$word,
                                  "count" = df_sen1word1$count,
                                  "a" = determineLetterWeight("a",df_sen1word1$word,df_sen1word1$count),
                                  "b" = determineLetterWeight("b",df_sen1word1$word,df_sen1word1$count),
                                  "c" = determineLetterWeight("c",df_sen1word1$word,df_sen1word1$count),
                                  "d" = determineLetterWeight("d",df_sen1word1$word,df_sen1word1$count),
                                  "e" = determineLetterWeight("e",df_sen1word1$word,df_sen1word1$count),
                                  "f" = determineLetterWeight("f",df_sen1word1$word,df_sen1word1$count),
                                  "g" = determineLetterWeight("g",df_sen1word1$word,df_sen1word1$count),
                                  "h" = determineLetterWeight("h",df_sen1word1$word,df_sen1word1$count),
                                  "i" = determineLetterWeight("i",df_sen1word1$word,df_sen1word1$count),
                                  "j" = determineLetterWeight("j",df_sen1word1$word,df_sen1word1$count),
                                  "k" = determineLetterWeight("k",df_sen1word1$word,df_sen1word1$count),
                                  "l" = determineLetterWeight("l",df_sen1word1$word,df_sen1word1$count),
                                  "m" = determineLetterWeight("m",df_sen1word1$word,df_sen1word1$count),
                                  "n" = determineLetterWeight("n",df_sen1word1$word,df_sen1word1$count),
                                  "o" = determineLetterWeight("o",df_sen1word1$word,df_sen1word1$count),
                                  "p" = determineLetterWeight("p",df_sen1word1$word,df_sen1word1$count),
                                  "q" = determineLetterWeight("q",df_sen1word1$word,df_sen1word1$count),
                                  "r" = determineLetterWeight("r",df_sen1word1$word,df_sen1word1$count),
                                  "s" = determineLetterWeight("s",df_sen1word1$word,df_sen1word1$count),
                                  "t" = determineLetterWeight("t",df_sen1word1$word,df_sen1word1$count),
                                  "u" = determineLetterWeight("y",df_sen1word1$word,df_sen1word1$count),
                                  "v" = determineLetterWeight("v",df_sen1word1$word,df_sen1word1$count),
                                  "w" = determineLetterWeight("w",df_sen1word1$word,df_sen1word1$count),
                                  "x" = determineLetterWeight("x",df_sen1word1$word,df_sen1word1$count),
                                  "y" = determineLetterWeight("y",df_sen1word1$word,df_sen1word1$count),
                                  "z" = determineLetterWeight("z",df_sen1word1$word,df_sen1word1$count)
                                  )
