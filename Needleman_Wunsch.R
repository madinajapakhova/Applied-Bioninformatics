rm(list=ls())
setwd("C:/Users/49176/Desktop/Summer_2022/Bioinformatics") 

# Global alignment: Needleman-Wunsch algorithm (NW)

# from LCS to NW: 1) first row, first column in d initialization: accumulate (negative) gap penalties
#                 2) adapt scoring scheme

nw <- function(a,b, s_m, s_r, s_g){
  # initialise dynamic programming matrix 
  d = matrix(rep(0, (nchar(a)+1)*(nchar(b)+1)),nrow = nchar(a)+1, ncol = nchar(b)+1)
  
  # accumulate gap penalties in the 1st query
  for (i in 1:(nchar(a)+1)){
    d[i,1] = (i-1)*s_g
  }
  
  # accumulate gap penalties in the 2nd query
  for (j in 1:(nchar(b)+1)){
    d[1,j] = (j-1)*s_g
  }
  
  # fill in rest of the matrix 
  for (i in 2:(nchar(a)+1)){
    for (j in 2:(nchar(b)+1)){
      if (substr(a,i-1,i-1) == substr(b,j-1, j-1)){
        s = s_m
      } else {
        s = s_r
      }
      
      # Get the maximum of the scores
      d[i,j] = max(d[i-1,j] + s_g, d[i,j-1] + s_g, d[i-1,j-1] + s)
    }}
  return(d[nchar(a)+1,nchar(b)+1])
} 


str1 <- "sweet"
str2 <- "sweat"
nw(str1, str2, 1, -1, -2)




