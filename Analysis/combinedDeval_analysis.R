## This script extracts and summarises Instrumental and Devaluation data from the Disgust Deval task
## Before you run this script you will need:
##    - the log files for each participant in the 'R/data/combined' folder
##    - participant files all named '(whateveyouridis).log'
##
##
##
## If you try and run this script without the appropriate packages installed, it won't work
##    - you can use install.packages() to download these
##    - this may take a little bit of fiddling depending on if your university's firewall isn't happy with it. 
##
##
## You will need to create individual participant ID vectors in the 'R/functions.R' script.

library(ggplot2)
library(reshape2)
library(stringr)
source("R/functions.R") #this loads the functions that will be needed for analysis

#Create a list of all of the participant info vectors
ID <- list(ID001, ID002)

#Create the empty vectors that will be filled with the data extracted from each participant. 

participant <- character(length = length(ID))  # creates a vector that will be filled with participant IDs
instru_measures <- c("instru_r1", "instru_r2", "o1won", "o2won", "totalWon", "o1rating", "o2rating", "preHunger")
emptyInstrumeasures <- vector("list", length(instru_measures)) #create a list for each training measure
names(emptyInstrumeasures) <- instru_measures #give each item in the list names

emptyInstrumeasures <- lapply(emptyInstrumeasures, createVector, y=ID) #for each measure, create an empty vector the length of the # of participants

deval_measures <- c("devalRs", "nondevalRs","pre_devalRating", "pre_nondevalRating", "post_devalRating", "post_nondevalRating","preHunger", "postHunger")

emptyDevalmeasures <- vector("list", length(deval_measures))
names(emptyDevalmeasures) <- deval_measures

emptyDevalmeasures <- lapply(emptyDevalmeasures, createVector, y=ID)

#Loop through the data extraction for each participant

for(i in ID){
  data <- read.delim(i[[1]], header = FALSE) #uses the path in the participant ID vector to read in the data file
  version <- i[[2]] #looks for the version in the second item of the participant vector
  
  #give the data column headings
  colnames(data) <- c("time", "type", "text")
  
  #Find the start and end times for the instrumental training and devaluation stages
  
  instruStart <- findTime("start instru")
  instruEnd <- findTime("end instru")
  
  instruTimes <- c(instruStart, instruEnd)
  
  
  #Find the time that each left and right response was made
  #This subsets the time column for eahc row where a left or right keypress is made
  #IF YOU CHANGE THE KEYS FOR LEFT AND RIGHT YOU WILL NEED TO CHANGE THI
  
  i.r1times <- findTime("Keypress: c") #left is always R1
  i.r2times <- findTime("Keypress: m") #right is always R2
  
  #Find the times that each left and right outcome was made
  i.o1times <- findTime("win A")
  i.o2times <- findTime("win B")
  
  #Count the number of responses made during Instrumental training
  i.instru_r1 <- countResp(instruTimes, i.r1times)
  i.instru_r2 <- countResp(instruTimes, i.r2times)
  
  #Count the number of snacks won
  i.o1won <- length(i.o1times)
  i.o2won <- length(i.o2times)
  
  #Calculate the total number of snacks won
  i.totalWon <- i.o1won + i.o2won
  
  ## Extract the ratings for each of the two Snacks
  ##    - Finds the string that includes the outcome rating
  ##    - Searches the text logs of data for any rows that contain the rating scale for that snack
  ##    - This spits out three rows
  ##    - We only care about the first row wich contains the numerical rating [1,x]
  ##    - We only care about the third column in that row, which contains the text log [1,3]
  ##    - gsub extracts the numerical rating that participants give
  ## 
  ## Ratings between 1-7
  ##  - 1: Most Unpleasant
  ##  - 7: Most Pleasant
  
  i.preHunger <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale Hunger_rating:", data$text),][2,3]) #string for the outcome rating
  )
  
  i.pre_mmRating <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale MM_rating:", data$text),][2,3]) #string for the outcome rating
  )
  
  i.pre_bbqRating <- as.numeric( #convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale BBQ_rating:", data$text),][2,3]) #string for the outcome rating
  )
  
  ## Counterbalancing for the version used
  ##    - O1: Outcome earned by R1
  ##    - O2: Outcome earned by R2
  
  if(version == "A" | version == "B"){
    i.o1rating <- i.pre_mmRating
    i.o2rating <- i.pre_bbqRating
  } else if (version == "C" | version == "D"){
    i.o1rating <- i.pre_bbqRating
    i.o2rating <- i.pre_mmRating
  } else {
    "Invalid Version Selected"
  }
  
  
  ## DEVALUATION DATA
  ## Find the start and end times for the Outcome Devaluation test
  
  devalStart <- findTime("start deval")
  devalEnd <- findTime("end deval")
  
  devalTimes <- c(devalStart, devalEnd)
  
  ## Count the number of left and right responses that are made during the devaluation test
  
  i.deval_r1 <- countResp(devalTimes, i.r1times)
  i.deval_r2 <- countResp(devalTimes, i.r2times)
  
  ## Counterbalancing for the outcome devalued in each different version
  ## Versions:
  ##    - A: O1 Devalued
  ##    - B: O2 Devalued
  ##    - C: O1 Devalued
  ##    - D: O2 Devalued
  
  
  if(version == "A" | version == "C"){
    i.devalRs <- i.deval_r1
    i.nondevalRs <- i.deval_r2
  } else if(version =="B" | version == "D"){
    i.devalRs <- i.deval_r2
    i.nondevalRs <- i.deval_r1
  } else{
    "Invalid version Selected"
  }
  
  ## Pleasantness ratings post-devaluation
  ## Ratings between 1-7
  ##  - 1: Most Unpleasant
  ##  - 7: Most Pleasant
  
  i.postHunger <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale Hunger_rating_2:", data$text),][2,3]) #string for the outcome rating
  )
  
  i.post_mmRating <- as.numeric( # convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale MM_rating_2:", data$text),][2,3]) #string for the outcome rating
  )
  
  i.post_bbqRating <- as.numeric( #convert string to numeric
    gsub("[^0-9]", #any matching numbers within the string
         "",
         data[grep("RatingScale BBQ_rating_2:", data$text),][2,3]) #string for the outcome rating
  )
  
  ## Counterbalance outcome ratings for Devalued/Nondevalued Outcomes
  ## Version:
  ##  - A: MMs Devaled
  ##  - B: BBQ Devalued
  ##  - C: BQQ Devalued
  ##  - D: MMs Devalued
  
  if(version == "A" | version == "B"){
    i.pre_devalRating <- i.pre_mmRating
    i.pre_nondevalRating <- i.pre_bbqRating
      
    i.post_devalRating <- i.post_mmRating
    i.post_nondevalRating <- i.post_bbqRating
  } else if (version == "B" | version == "D"){
    i.pre_devalRating <- i.pre_bbqRating
    i.pre_nondevalRating <- i.pre_mmRating
    
    i.post_devalRating <- i.post_bbqRating
    i.post_nondevalRating <- i.post_mmRating
  } else{
    "Invalid version selected"
  }
  
  ## Insert individual participant values into the empty vectors created
  ## For each cue type, inset the value of the participant into the row that corresponds with their participant number
  
  participant[as.numeric(i[4])] <- i[[3]] # insert participant ID code into vector
  for(j in instru_measures){
    emptyInstrumeasures[[j]][[as.numeric(i[4])]] <- get(paste0("i.", j))
  }
  for(k in deval_measures){
    emptyDevalmeasures[[k]][[as.numeric(i[[4]])]] <- get(paste0("i.", k))
  }
}

#Create a data frame of the group data from instrumental training
instru.df <- as.data.frame(emptyInstrumeasures) # dataframe from list
wide.instru.df <- data.frame(participant, emptyInstrumeasures) #group dataframe with participant ID

#Create a data frame of the group data from the devaluation test
deval.df <- as.data.frame(emptyDevalmeasures)
wide.deval.df <- data.frame(participant, emptyDevalmeasures)

# Set directory for output
dir.output <- 'R/output/data'

# Export group data
write.csv(wide.instru.df, file = file.path(dir.output, "group_instruData.csv"), row.names = FALSE)
write.csv(wide.deval.df, file = file.path(dir,output, "group_devalData.csv"), row.names = FALSE)

