loadData<-function(seed = 2024){
  # Checker dependencies og evt. installere disse
  dependencies <- c("xts","sp","zoo","CASdatasets","splitTools")
  md<-c()
  for(dep in dependencies){
    if(!require(eval(dep),character.only = T)){
      md<-append(md,dep)
    }
  }
  if(length(md)>0){
    cat("The following dependencies have not been met:\n")
    cat(paste(md),"\n")
    answer<-tolower(readline(paste("Do you wish to install them? [y/N] : ")))
    if(answer == "y"){
      install.packages(md[md != "CASdatasets"])
      if("CASdatasets" %in% md){
        install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
      }
    }
    library("splitTools","CASdatasets")
  }
  
  # Henter datasættene som beskrevet, men i funktionens enviroment istedet for det globale
  data(freMPL1,envir = environment())
  
  data(freMPL2,envir = environment())
  
  data(freMPL3,envir = environment())
  
  data(freMPL4,envir = environment())
  
  freMPL3 <- subset( freMPL3 , select = -DeducType )  
  
  freMPL4 <- subset( freMPL4 , select = -DeducType )  
  
  freMPL <- rbind(freMPL1,freMPL2,freMPL3,freMPL4)
  
  # For at mlr3 skal kunne omdanne freMPL eller train til task skal dates laves om til POSIXct
  freMPL$RecordBeg<-as.POSIXct(freMPL$RecordBeg)
  freMPL$RecordEnd<-as.POSIXct(freMPL$RecordEnd)
  
  set.seed(seed) 
  
  # De relevante objekter skubbes ud til det globale enviroment
  freMPL <<- freMPL
  
  # vigtigt at bruge splitTools::partition da mlr3 overskriver partition, men kun acceptere class task
  ind <<- splitTools::partition(freMPL$ClaimInd, p = c(train = 0.8, test = 0.2)) #### train and test have the same claim frequency
  train <<- freMPL[ind$train, ] 
  test <<- freMPL[ind$test, ]
  
  # Tømmer alt fra det funktionens enviroment, ie. de store dataset, som allerede er gemt i det globale enviroment
  rm(list = ls())
}

