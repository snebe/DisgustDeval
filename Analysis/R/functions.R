#Create vectors with participant details
#These take the form:

#IDcode <- c("path to data file.log", counterbalancing version, "IDcode", participant number)

##Example from another similar task
# iPIT401 <- c("R/data/iPIT401.log", "A", "iPIT401", 1)
# iPIT402 <- c("R/data/iPIT402.log", "B", "iPIT402", 2)
# iPIT403 <- c("R/data/iPIT403.log", "A", "iPIT403", 3)
# iPIT404 <- c("R/data/iPIT404.log", "B", "iPIT404", 4)


# ID001 <- c("R/data/combined/ID001.log", "A", "ID001", 1)
# ID002 <- c("R/data/combined/ID002.log", "B", "ID002", 2)

ID001 <- c("R/data/combined/p001_1.log", "C", "ID001_1", 1)
ID002 <- c("R/data/combined/p002_1.log", "C", "ID002_1", 2)
ID003 <- c("R/data/combined/p003_1.log", "D", "ID003_1", 3)
ID004 <- c("R/data/combined/p004_1.log", "A", "ID004_1", 4)
ID005 <- c("R/data/combined/p005_1.log", "C", "ID005_1", 5)
ID006 <- c("R/data/combined/p006_1.log", "C", "ID006_1", 6)
ID007 <- c("R/data/combined/p007_1.log", "D", "ID007_1", 7)
ID008 <- c("R/data/combined/p008_1.log", "A", "ID008_1", 8)
ID009 <- c("R/data/combined/p009_1.log", "B", "ID009_1", 9)
ID010 <- c("R/data/combined/p010_1.log", "A", "ID010_1", 10)
ID011 <- c("R/data/combined/p011_1.log", "D", "ID011_1", 11)

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


## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

