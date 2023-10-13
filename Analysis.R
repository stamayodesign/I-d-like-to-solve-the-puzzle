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


createAlphabet_df <- function(input_dataframe){
  return <- data.frame("word" = input_dataframe$word,
                       "count" = input_dataframe$count,
                       "a" = determineLetterWeight("a",input_dataframe$word,input_dataframe$count),
                       "b" = determineLetterWeight("b",input_dataframe$word,input_dataframe$count),
                       "c" = determineLetterWeight("c",input_dataframe$word,input_dataframe$count),
                       "d" = determineLetterWeight("d",input_dataframe$word,input_dataframe$count),
                       "e" = determineLetterWeight("e",input_dataframe$word,input_dataframe$count),
                       "f" = determineLetterWeight("f",input_dataframe$word,input_dataframe$count),
                       "g" = determineLetterWeight("g",input_dataframe$word,input_dataframe$count),
                       "h" = determineLetterWeight("h",input_dataframe$word,input_dataframe$count),
                       "i" = determineLetterWeight("i",input_dataframe$word,input_dataframe$count),
                       "j" = determineLetterWeight("j",input_dataframe$word,input_dataframe$count),
                       "k" = determineLetterWeight("k",input_dataframe$word,input_dataframe$count),
                       "l" = determineLetterWeight("l",input_dataframe$word,input_dataframe$count),
                       "m" = determineLetterWeight("m",input_dataframe$word,input_dataframe$count),
                       "n" = determineLetterWeight("n",input_dataframe$word,input_dataframe$count),
                       "o" = determineLetterWeight("o",input_dataframe$word,input_dataframe$count),
                       "p" = determineLetterWeight("p",input_dataframe$word,input_dataframe$count),
                       "q" = determineLetterWeight("q",input_dataframe$word,input_dataframe$count),
                       "r" = determineLetterWeight("r",input_dataframe$word,input_dataframe$count),
                       "s" = determineLetterWeight("s",input_dataframe$word,input_dataframe$count),
                       "t" = determineLetterWeight("t",input_dataframe$word,input_dataframe$count),
                       "u" = determineLetterWeight("y",input_dataframe$word,input_dataframe$count),
                       "v" = determineLetterWeight("v",input_dataframe$word,input_dataframe$count),
                       "w" = determineLetterWeight("w",input_dataframe$word,input_dataframe$count),
                       "x" = determineLetterWeight("x",input_dataframe$word,input_dataframe$count),
                       "y" = determineLetterWeight("y",input_dataframe$word,input_dataframe$count),
                       "z" = determineLetterWeight("z",input_dataframe$word,input_dataframe$count)
  )
  
}

df_sen1word1alphabet <- createAlphabet_df(df_sen1word1)
df_sen1word2alphabet <- createAlphabet_df(df_sen1word2)
df_sen1word3alphabet <- createAlphabet_df(df_sen1word3)
df_sen1word4alphabet <- createAlphabet_df(df_sen1word4)
df_sen1word5alphabet <- createAlphabet_df(df_sen1word5)
df_sen1word6alphabet <- createAlphabet_df(df_sen1word6)
df_sen1word7alphabet <- createAlphabet_df(df_sen1word7)

df_sen2word1alphabet <- createAlphabet_df(df_sen2word1)
df_sen2word2alphabet <- createAlphabet_df(df_sen2word2)
df_sen2word3alphabet <- createAlphabet_df(df_sen2word3)
df_sen2word4alphabet <- createAlphabet_df(df_sen2word4)

df_sen3word1alphabet <- createAlphabet_df(df_sen3word1)
df_sen3word2alphabet <- createAlphabet_df(df_sen3word2)
df_sen3word3alphabet <- createAlphabet_df(df_sen3word3)
df_sen3word4alphabet <- createAlphabet_df(df_sen3word4)
df_sen3word5alphabet <- createAlphabet_df(df_sen3word5)
df_sen3word6alphabet <- createAlphabet_df(df_sen3word6)
df_sen3word7alphabet <- createAlphabet_df(df_sen3word7)
df_sen3word8alphabet <- createAlphabet_df(df_sen3word8)
                             
sumAlphabet_df <- function(input_dataframe){
  return <- data.frame(
    "a" = sum(input_dataframe[3]),
    "b" = sum(input_dataframe[4]),
    "c" = sum(input_dataframe[5]),
    "d" = sum(input_dataframe[6]),
    "e" = sum(input_dataframe[7]),
    "f" = sum(input_dataframe[8]),
    "g" = sum(input_dataframe[9]),
    "h" = sum(input_dataframe[10]),
    "i" = sum(input_dataframe[11]),
    "j" = sum(input_dataframe[12]),
    "k" = sum(input_dataframe[13]),
    "l" = sum(input_dataframe[14]),
    "m" = sum(input_dataframe[15]),
    "n" = sum(input_dataframe[16]),
    "o" = sum(input_dataframe[17]),
    "p" = sum(input_dataframe[18]),
    "q" = sum(input_dataframe[19]),
    "r" = sum(input_dataframe[20]),
    "s" = sum(input_dataframe[21]),
    "t" = sum(input_dataframe[22]),
    "u" = sum(input_dataframe[23]),
    "v" = sum(input_dataframe[24]),
    "w" = sum(input_dataframe[25]),
    "x" = sum(input_dataframe[26]),
    "y" = sum(input_dataframe[27]),
    "z" = sum(input_dataframe[28])
  )
}

df_sen1word1alphabetCount <- sumAlphabet_df(df_sen1word1alphabet)
df_sen1word2alphabetCount <- sumAlphabet_df(df_sen1word2alphabet)
df_sen1word3alphabetCount <- sumAlphabet_df(df_sen1word3alphabet)
df_sen1word4alphabetCount <- sumAlphabet_df(df_sen1word4alphabet)
df_sen1word5alphabetCount <- sumAlphabet_df(df_sen1word5alphabet)
df_sen1word6alphabetCount <- sumAlphabet_df(df_sen1word6alphabet)
df_sen1word7alphabetCount <- sumAlphabet_df(df_sen1word7alphabet)

df_sen2word1alphabetCount <- sumAlphabet_df(df_sen2word1alphabet)
df_sen2word2alphabetCount <- sumAlphabet_df(df_sen2word2alphabet)
df_sen2word3alphabetCount <- sumAlphabet_df(df_sen2word3alphabet)
df_sen2word4alphabetCount <- sumAlphabet_df(df_sen2word4alphabet)

df_sen3word1alphabetCount <- sumAlphabet_df(df_sen3word1alphabet)
df_sen3word2alphabetCount <- sumAlphabet_df(df_sen3word2alphabet)
df_sen3word3alphabetCount <- sumAlphabet_df(df_sen3word3alphabet)
df_sen3word4alphabetCount <- sumAlphabet_df(df_sen3word4alphabet)
df_sen3word5alphabetCount <- sumAlphabet_df(df_sen3word5alphabet)
df_sen3word6alphabetCount <- sumAlphabet_df(df_sen3word6alphabet)
df_sen3word7alphabetCount <- sumAlphabet_df(df_sen3word7alphabet)
df_sen3word8alphabetCount <- sumAlphabet_df(df_sen3word8alphabet)

df_wordAlphabetCountFull <- df_sen1word1alphabetCount +
                                df_sen1word2alphabetCount +
                                df_sen1word3alphabetCount +
                                df_sen1word4alphabetCount +
                                df_sen1word5alphabetCount +
                                df_sen1word6alphabetCount +
                                df_sen1word7alphabetCount +
                                df_sen2word1alphabetCount +
                                df_sen2word2alphabetCount +
                                df_sen2word3alphabetCount +
                                df_sen2word4alphabetCount +
                                df_sen3word1alphabetCount +
                                df_sen3word2alphabetCount +
                                df_sen3word3alphabetCount +
                                df_sen3word4alphabetCount +
                                df_sen3word5alphabetCount +
                                df_sen3word6alphabetCount +
                                df_sen3word7alphabetCount +
                                df_sen3word8alphabetCount
                              
df_wordAlphabetCountFull_Transpose = data.frame(letter = lettersC,
                                                count = t(df_wordAlphabetCountFull))

df_wordAlphabetCountFull_Transpose[which.max(df_wordAlphabetCountFull_Transpose$count),]
