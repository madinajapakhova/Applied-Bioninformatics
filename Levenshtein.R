rm(list=ls())
setwd("C:/Users/49176/Desktop/Summer_2022/Bioinformatics") 

lev <- function(a,b){
  # initialise dynamic programming matrix 
  d = matrix(rep(0, (nchar(a)+1)*(nchar(b)+1)),nrow = nchar(a)+1, ncol = nchar(b)+1)
  
  # initialise first column 
  for (i in 1:(nchar(a)+1)){
    d[i,1] = i-1
  }
  
  # initialise first row
  for (j in 1:(nchar(b)+1)){
    d[1,j] = j-1
  }
  
  # fill in rest of the matrix 
  for (i in 2:(nchar(a)+1)){
    for (j in 2:(nchar(b)+1)){
      # if letters are the same, no edit operation is used, 
      # otherwise it is one (replace operation) 
      if (substr(a,i-1,i-1) == substr(b,j-1, j-1)){
        s = 0
      } else {
        s = 1
      }
      
      # Get the minimum of the scores
      d[i,j] = min(d[i-1,j]+1, d[i,j-1]+1, d[i-1,j-1] + s)
      }}
  return(d[nchar(a)+1,nchar(b)+1])
} 

a <- "Christmas"
d <- "Fantasy"

str1 <- "browncats"
str2 <- "yellowkats"
f<-lev(str1, str2)

#install.packages("stringdist")
library(stringdist)
#calculate Levenshtein distance between Christmas and Fantasy
stringdist("Christmas", "Fantasy", method = "lv")

# Pubmed 
library(stringr)
library(magrittr)
pubmed <- read.delim("Pubmed.Forename.txt", sep = " ") 
  
pubmed$first_name <- str_extract(pubmed$Fore, '[A-Za-z]+') 

# to fasten things a bit, subset to just 1000 cases (orifinal data ~400 000 observations)
pubmed <- pubmed[1:1000,] %>% 
  na.omit(pubmed)
  
similarity_threshold <- 2
similar_names <- data.frame(similar_name=character(),stringsAsFactors=FALSE) 

similar <- function(author){
  
  for(i in 1:nrow(pubmed)){
    word <- pubmed[i,3]
    if(lev(author, word) <= similarity_threshold & lev(author, word) != 0){
      similar_names[i,1] <- word 
      similar_names <- na.omit(similar_names)
    }
  }
  return(similar_names)
  
}
a <-similar(author = "Peter")
#install.packages("readr")
library(readr)
write_csv(a, "Peter_similar_names_pubmed.csv")









