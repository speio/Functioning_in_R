#Genetics Lab calculations, data, graphs, etc. 
#Jan 25th 2014

#Commonly used packages
library(MASS)
library(ggplot2)
library(gdata)
library(car)
library(lmtest)
library(stats)
library(foreign)
library(vcd)
library(extrafont)
library(directlabels)
library(graphics)


#I think I used this for importing nice fonts, I'll figure it out later. 
font_import()

#####
# E.coli Growth Curve
#####

##Complemented mutant
#"OD of overnight culture 1:20 dilution is 0.315 32 ml was added into the fermenter"
od.cm <- c(0.095, 0.075, 0.108, 0.12, 0.162, 0.217, 0.315, 0.394, 0.439, 0.468, 0.501)
time.cm <- c(0,15,30,45,60,75,90,100,207,110,113)

##Mutant
#REALLY FUCKING COOL, THEY ARE ALL OF DIFFERENT LENGTH
#OD of overnight culture 1:20 dilution is 0.296. 33.8 ml was added into the fermenter
od.ihf <- c(0.091, .089, .103, .143, .198, .238, .318, .463, .516)
time.ihf <- c(0,15,30,45,60,75,90,100,103)

##Wild Type
#OD of overnight culture 1:20 dilution is 0.444. 22.5 ml was added into the fermenter
od.wt <- c(.096, .102, .118, .145, .155, .217, .364, .39, .491, .543)
time.wt <- c(0,15,30,40,45,60,75,80,90,91)

growth.list <- list(od.wt,time.wt, od.ihf,time.ihf, od.cm,time.cm)
name.list <- list("OD WT", "Time WT", "OD ihf Mutant", "Time ihf Mutant", "OD CM", "Time CM")

df.make(growth.list, name.list)




####### Begin: Making a function to write unequel lists to a dataframe #######

###All Helper Functions###

##Finds the longest sublist and outputs its length
biggestsublist <- function(list,version) {
  if(version == 'length'){
    lengths <- c()
    #how many sublists to iterate over
    for(i in 1:length(list)) {
      lengths[i] <- length(unlist(list[i])) 
      #(error check) cat("lengths inside of for loop: ", lengths)
    }  
    max.index <- which.max(na.omit(lengths))   
    #plugging in the max index into the inputted list and printing length
    return(length(list[[max.index]]))
  }  
  if(version == 'index'){
      lengths <- c()
    #how many sublists to iterate over
    for(i in 1:length(list)) {
      lengths[i] <- length(unlist(list[i])) 
      #(error check) cat("lengths inside of for loop: ", lengths)
    }  
    max.index <- which.max(na.omit(lengths))   
    #plugging in the max index into the inputted list and printing length
    return(max.index)
  }  
}
  #Testing
  biggestsublist(growth.list, 'length')
  growth.list
  lengthlister <- function(list){
    lengthlist <-c()
    for(i in 1:length(list)){
      lengthlist[i] <- length(list[[i]])
    } 
    return(lengthlist)
  } #prints vector of all lengths of sublists
  lengthlister(growth.list)

##Finds length of the sublist with the least elements (returns 1 when all sublists are of same length)
smallestsublist <- function(list, version) {
  if(version == 'length'){  
    lengths <- c()
    #how many sublists to iterate over
    for(i in 1:length(list)) {
      lengths[i] <- length(list[[i]]) 
      #(erro check)cat("lengths inside of for loop: ", lengths)
    }  
    #(error check)cat("lengths outside of for loop: ", lengths)    
    min.index <- which.min(na.omit(lengths))
    min.length <- length(list[[min.index]])
    return(min.length)
  }
  
  if(version == 'index'){
    lengths <- c()
    #how many sublists to iterate over
    for(i in 1:length(list)) {
      lengths[i] <- length(list[[i]]) 
      #(error check)cat("lengths inside of for loop: ", lengths)
    }  
    #(error check)cat("lengths outside of for loop: ", lengths)    
    min.index <- which.min(na.omit(lengths))
    return(min.index)    
  }      
}    
  #testing 
  smallestsublist(growth.list, 'index')
  growth.list
  lengthlister <- function(list){
    lengthlist <-c()
    for(i in 1:length(list)){
      lengthlist[i] <- length(list[[i]])
    } 
    return(lengthlist)
  } #prints vector of all lengths of sublists
  lengthlister(growth.list)

##Creates the actual data frame after all checks passed
#(small issues with nested lists as input needs unlisting as can see in test)
make.df <- function(datalist, colnamelist){
  
  dataframe <- data.frame(matrix( #Creating empty dataframe
    ncol=length(datalist), 
    nrow=biggestsublist(datalist,'length')))
  
  for(i in 1:length(datalist)){   
    dataframe[,i] <- datalist[i]
  }
  colnames(dataframe) <- colnamelist
  #print(sprintf("A dataframe named...\n %s \n ...has been created ", dfname)) #restate name of new dataframe
  
  return(dataframe)
} 
  #Testing make.df function:
  name.list = list("ODCM", "TimeCM")
  small.growth <- list(unlist(growth.list[3]), unlist(growth.list[2]))
  small.growth  
  testframe <- make.df(small.growth, name.list)
  rm(testframe)

##"Balances" out sublist lengths in a list by appending NAs
balance.list <- function(list){
  balanced.list <- list
  while( !balanced.bool(balanced.list)){
    small.index <- smallestsublist(balanced.list, 'index') #find index of shortest sublist
    #big.length <- biggestsublist(balanced.list, 'length') #find length of longest sublist
    #small.length <- smallestsublist(balanced.list, 'length') #finds length of shortest sublist
    
    balanced.list[[small.index]] <- append(balanced.list[[small.index]], NA) #appending NAs to shortest sublist
    
  }
  return(balanced.list)
}
  #testing balancing function
  balanced.bool(balance.list(growth.list)) 
  balanced.bool(growth.list)



##Helper function to return logical value if sublists of a list are
#'balanced' or not, hence, all have same length
#not actually sure how well this works
balanced.bool <- function(list){
  lengths <- c()
  bool <- c()
  for(i in 1:length(list)){
    lengths[i] <- length(unlist(list[i]))
  }
  for(i in 1:length(list)){
    bool[i] <- sum(lengths) == lengths[i]*length(lengths)
    #error check print(bool)
  }
  if(FALSE %in% bool){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
  #testing balanced.bool function 
  balanced.bool(growth.list)
  bal.list <- list(c(1,2,3),c(2,3,4,5))    
  unbal.list <- list(c(1,2,3,5),c(1,2,3,4), c(1:10), c(1:25))
  


####Final Function####
#function to make a data frame out of lists
#takes data form a list and creates a data frame with second
#argument being another list of names for each column
df.make <- function(datalist,colnamelist){
  length.data <- length(datalist)
  length.names <- length(colnamelist)
  if(length.data == length.names){  
    #if datalist is unbalanced enter (based on balanced bool helper being false)
    if( ! balanced.bool(datalist)) {
      balanced.list <- balance.list(datalist) #function that balances sublist lengths with NAs
      make.df(balanced.list, colnamelist) #calls make.df to make the dataframe
    }  
   
    else{
        make.df(datalist,colnamelist)    
    }
  }
  
  else{
    sprintf("Data list and Name list must be of equal length. Currently: Data list length = %i , Names length = %i"
          , length.data, length.names )
  }
  
}

  #Testing df.make
  df.make(growth.list, name.list)#working(?)


