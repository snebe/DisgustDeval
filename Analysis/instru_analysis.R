##This script extracts and summarises Instrumental training data from the Disgust Devaluation Task. 
##Before you run this you need:
#The log files for each participant in the 'R/data/instru' folder
#Participant log files all named '(whateveryouridsis)_instru.log'

#-if you try to run this without the appropriate packages installed it wont work
#-you can use install.packages() to download these
#-this may take a bit of fiddling if your uni's firewall doesn't like you doing this
#You will need to create individual participant ID vectors in the 'R/functions.R' script 

library(ggplot2)
library(reshape2)
library(stringr)
source("R/functions.R") #this loads the functions that will be needed for analysis


#Create a list of all of the participant info vectors
ID <- list(ID001_i, ID002_i)

#Create the empty vectors that will be filled with the data extracted from each participant. 

participant <- character(length = length(ID))  # creates a vector that will be filled with participant IDs
instru_measures <- c("r1total", "r2total", "o1won", "o2won", "totalWon", "o1rating", "o2rating", "o3rating", "hunger")
emptymeasures <- vector("list", length(instru_measures)) #create a list for each training measure
names(emptymeasures) <- instru_measures #give each item in the list names

emptymeasures <- lapply(emptymeasures, createVector, y=ID) #for each measure, create an empty vector the length of the # of participants

#Loop through the data extraction for each particpant

for(i in ID){
  data <- read.delim(i[[1]], header = FALSE) #uses the path in the participant ID vector to read the log file
  version <- i[[2]] #looks for the version in the second item of the participant vector
  
  #give the data column headings
  colnames(data) <- c("time", "type", "text")
  
  #find the time that each left and right response was made
  #this subsets the time column for each row where a left or right keypress is made
  #IF YOU CHANGE KEYS FOR LEFT AND RIGHT YOU WILL NEED TO CHANGE THIS 
  
  i.r1times <- findTime("Keypress: t") #left is always R1
  i.r2times <- findTime("Keypress: v") #right is always R2
  
  #Count the number of responses on each action
  i.r1total <- length(i.r1times)
  i.r2total <- length(i.r2times)
  
  #find the times that each left and right outcome was made
  i.o1times <- findTime("win A")
  i.o2times <- findTime("win B")
  
  #Count the number of snacks won
  i.o1won <- length(i.o1times)
  i.o2won <- length(i.o2times)
  
  #Total number of snacks won
  i.totalWon <- i.o1won + i.o2won
  
  ##Extract the ratings for each of the three Snacks. 
  #string for the outcome rating
  #searches the text logs of data for any rows that containg the rating scale for that snack
  #this spits out three rows
  #we only care about the first row which contains the numerical rating [1,x]
  #we only are about the third column in that row, which contains the text log [1,3]
  #gsub extracts the numerical rating that participants give
  #Rating between 1-7
  # 1: Most Unpleasant
  # 7: Most Pleasant
  
  i.hunger <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "", 
         data[grep("RatingScale Hunger_rating:", data$text),][1,3]) #string for the outcome rating
  )
  
  ttRating <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "", 
         data[grep("RatingScale teddy_rating:", data$text),][1,3]) #string for the outcome rating
  )
  
  mmRating <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "", 
         data[grep("RatingScale MM_rating:", data$text),][1,3]) #string for the outcome rating
  )
  
  bbqRating  <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "", 
         data[grep("RatingScale BBQ_rating:", data$text),][1,3]) #string for the outcome rating
  )
  
  ##Counterbalancing for version used
  #O1 <- outcome earned by R1 response
  #O2 <- outcome earned by R2 response
  #O3 <- outcome not associated with any instrumental responses
  if(version == "A"){
    i.o1rating <- mmRating
    i.o2rating <- bbqRating
    i.o3rating <- ttRating
  } else if(version == "B"){
    i.o1rating <- bbqRating
    i.o2rating <- mmRating
    i.o3rating <- ttRating
  } else if(version == "C"){
    i.o1rating <- ttRating
    i.o2rating <- bbqRating
    i.o3rating <- mmRating
  } else if(version == "D"){
    i.o1rating <- bbqRating
    i.o2rating <- ttRating
    i.o3rating <- mmRating
  } else if(version == "E"){
    i.o1rating <- mmRating
    i.o2rating <- ttRating
    i.o3rating <- bbqRating
  } else if(version == "F"){
    i.o1rating <- ttRating
    i.o2rating <- mmRating
    i.o3rating <- bbqRating
  } else {
    "Invalid Version Selected"
  }
  
  #Insert individual participant values into the empty vectors created
  #for each cue type, inset the value of the participant into the row that corresponses with their participant number
  participant[as.numeric(i[4])] <- i[[3]] # insert participant ID code into vector
  for(j in instru_measures){
    emptymeasures[[j]][[as.numeric(i[4])]] <- get(paste0("i.", j))
  }
  
}

#Create a data frame of the group data from instrumental training
instru.df <- as.data.frame(emptymeasures) #dataframe from list
wide.instru.df <- data.frame(participant, emptymeasures) #group data frame with participant IDs

#Set directory for output
dir.output <- 'R/output/data/instru'

#Export group data
write.csv(wide.instru.df, file = file.path(dir.output, "group_instruData.csv"), row.names = FALSE)

