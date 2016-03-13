##This script extracts and summarises the data from the devaluation test of the Disgust Devaluation task. 
##Before you run this you need:
#The log files for each participant in the 'R/data/deval' folder
#Participant log files all named '(whateveryouridsis)_deval.log'

#-if you try to run this without the appropriate packages installed it wont work
#-you can use install.packages() to download these
#-this may take a bit of fiddling if your uni's firewall doesn't like you doing this
#You will need to create individual participant ID vectors in the 'R/functions.R' script 

source("R/functions.R") #this loads the functions that will be needed for analysis

ID <- list(ID001_d, ID002_d)

#create the empty vectors that will be filled by data extracted from participant data

participant <- character(length = length(ID))  # creates a vector that will be filled with participant IDs
devalmeasures <- c("devalRs", "nondevalRs")
emptymeasures <- vector("list", length(devalmeasures)) #create a list for each training measure
names(emptymeasures) <- devalmeasures #give each item in the list names

emptymeasures <- lapply(emptymeasures, createVector, y=ID) #for each measure, create an empty vector the length of the # of participants


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
  
  #Counterbalancing for the deval version used
  if(version == "1"){
    i.devalRs <- i.r1total
    i.nondevalRs <- i.r2total
  } else if(version == "2"){
    i.devalRs <- i.r2total
    i.nondevalRs <- i.r1total
  } else {
    "Invalid Version Selected"
  }
  
  #Insert individual participant values into the empty vectors created
  participant[as.numeric(i[4])] <- i[[3]] # insert participant ID code into vector
  for(j in devalmeasures){
    emptymeasures[[j]][[as.numeric(i[4])]] <- get(paste0("i.", j))
  }
}

deval.df <- as.data.frame(emptymeasures) #dataframe from list
wide.deval.df <- data.frame(participant, emptymeasures) #group data frame with participant IDs


#Set directory for output
dir.output <- 'R/output/data/deval'

#Export group data
write.csv(wide.deval.df, file = file.path(dir.output, "group_devalData.csv"), row.names = FALSE)

