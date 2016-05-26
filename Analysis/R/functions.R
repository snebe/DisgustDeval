#Create vectors with participant details
#These take the form:

#IDcode <- c("path to data file.log", counterbalancing version, "IDcode", participant number)

##Example from another similar task
# iPIT401 <- c("R/data/iPIT401.log", "A", "iPIT401", 1)
# iPIT402 <- c("R/data/iPIT402.log", "B", "iPIT402", 2)
# iPIT403 <- c("R/data/iPIT403.log", "A", "iPIT403", 3)
# iPIT404 <- c("R/data/iPIT404.log", "B", "iPIT404", 4)


ID001 <- c("R/data/combined/ID001.log", "A", "ID001", 1)
ID002 <- c("R/data/combined/ID002.log", "B", "ID002", 2)

# #ID codes for analysis of the instrumental training data
# ID001_i <- c("R/data/instru/ID001_instru.log", "A", "ID001", 1)
# ID002_i <- c("R/data/instru/ID002_instru.log", "A", "ID002", 2)
# 
# 
# 
# #ID vectors for analysis of the devaluation test data
# #Devaluation Version in these vectors
# #Deval version 1: O1 devalued
# #Deval version 2: O2 devalued
# ID001_d <- c("R/data/deval/ID001_deval.log", "1", "ID001", 1)
# ID002_d <- c("R/data/deval/ID002_deval.log", "1", "ID002", 2)


#calculates CS end time (6s after CSonset)
endTime <- function(x){
  x + 6
}

#calculates preCS start time (6s before CSonset)
preTime <- function(x){
  x - 6
}

#find times of text strings
findTime <- function(x){
  data$time[data$text == x]
}

#find position of an element in a vector
findPos <- function(x){
  which(data$text == x)
}

#count responses
#this counts between TWO values
countResp <- function(x, y){
  c(sum(y > x[1] & y < x[2]))
}



#assigns an empty vector to a variable
#y = length of the empty vector
createVector <- function(x, y){
  x <- numeric(length=length(y))
}


#assigns an empty matrix to a variable
#y = number of rows of the empty matrix
#z = number of columns of the empty matrix
createDF <- function(x,y,z){
  x <- matrix(nrow = length(y), ncol = z)
}


addID <- function(id, x, cols){
  df <- data.frame(id, x)
  colnames(df) <- cols
  df
}
