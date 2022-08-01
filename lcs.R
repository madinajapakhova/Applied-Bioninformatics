rm(list=ls())
setwd("C:/Users/49176/Desktop/Summer_2022/Bioinformatics") 

# Longest common subsequence

# from lev to lcs: 1) first row, first column in d initialization: all zeros 
#                  2) reverted scoring scheme: +1 for matches, 0 for anything else
#                  3) max instead of min of the three cells

lcs <- function(a,b){
  # initialise dynamic programming matrix 
  d = matrix(rep(0, (nchar(a)+1)*(nchar(b)+1)),nrow = nchar(a)+1, ncol = nchar(b)+1)
  
  # initialise first column: 0. Empty string is a substring of any string, lcs = 0
  for (i in 1:(nchar(a)+1)){
    d[i,1] = 0
  }
  
  # initialise first row: 0. Empty string is a substring of any string, lcs = 0
  for (j in 1:(nchar(b)+1)){
    d[1,j] = 0
  }
  
  # fill in rest of the matrix 
  for (i in 2:(nchar(a)+1)){
    for (j in 2:(nchar(b)+1)){
      # reward a match, zero for anything else (mismatch, insertion, deletion = 0)
      if (substr(a,i-1,i-1) == substr(b,j-1, j-1)){
        s = 1
      } else {
        s = 0
      }
      
      # Get the maximum of the scores
      d[i,j] = max(d[i-1,j], d[i,j-1], d[i-1,j-1] + s)
    }}
  return(d[nchar(a)+1,nchar(b)+1])
} 


str1 <- "world"
str2 <- "leader"
lcs(str1, str2)


