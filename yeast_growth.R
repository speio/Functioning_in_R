#utz's growth curve



library(reshape)
library(ggplot2)

growth <- file.choose()

df <- as.data.frame(read.table(growth))

ymk211.30 <- subset(df, select = ymk211.30.1:ymk211.30.3)
ymk211.18 <- subset(df, select = ymk211.18.1:ymk211.18.3)
ymk118.30 <- subset(df, select = ymk118.30.1:ymk118.30.3)
ymk118.18 <- subset(df, select = ymk118.18.1:ymk118.18.3)


deviations <- list()

#creating list of standard deviations
#How to access the list
deviations[[1]] <- apply(ymk118.18, 1, sd)
deviations[[2]] <- apply(ymk118.30, 1, sd)
deviations[[3]] <- apply(ymk211.18, 1, sd)
deviations[[4]] <- apply(ymk211.30, 1, sd)
#how to access in current form
deviations[[c(1,1)]]

#list of average growths between triplicates
avg.growth <- list()
avg.growth[[1]] <- rowMeans(ymk118.18)
avg.growth[[2]] <- rowMeans(ymk118.30)
avg.growth[[3]] <- rowMeans(ymk211.18)
avg.growth[[4]] <- rowMeans(ymk211.30)
avg.growth

#Creating a new data frame of the averages
df.avg<-as.data.frame(avg.growth)
x <- c("ymk118.18","ymk118.30", "ymk211.18", "ymk211.30")
colnames(df.avg) <- x
df.avg$Time <- df$time.1.8.  

df.avg

df.deviations <- as.data.frame(deviations)
colnames(df.deviations) <- x
df.deviation
df.deviations$Time <- df$time.1.8.

#some additional setup for the plot
df.avg.melt <- melt(df.avg ,  id = 'Time', variable_name = 'Sample')
df.dvs <- melt(df.deviations, id = 'Time', variable_name = 'Sample') 
df.avg.melt$sd <- df.dvs[3]
colnames(df.avg.melt) <- c('Time', 'Sample', 'OD', 'sd')
#dataframe of all data with the standard error
df.avg.melt

#how I would normally add the standard error to a plot
error <- aes(ymin = OD - sd, ymax = OD + sd)
#final plot
growth.plot <- ggplot(df.avg.melt, aes(Time,OD), stat = 'identity') +
  geom_line(aes(col = Sample)) +
    geom_point(aes(col = Sample)) +
      #doesn't work with this active idk why
      #geom_errorbar(error, col = 'tan', width = .2, size = 0.8, position = position_dodge(.9)) + 
        labs(x = 'Time (min)', y = 'OD' )

print(growth.plot)


#dvs.1 <- df.deviations$ymk118.18
#df.1 <- list(y = df.avg$ymk118.18, x = df.avg$Time, sd = dvs.1)
#df.1 <- as.data.frame(df.1)

##AN EXMAPLE WITH ERROR BARS (individually would have to add each plot layer by layer)
#plot1 <- ggplot(df.1, aes(x=x,y=y), stat = 'identity') +
#  geom_errorbar(error1, size = 0.8, width = 20, col = 'grey')+        
#  geom_line() + geom_point()


#print(plot1)







####       SCRAP     ####
rowss <- function(df, row_num){
  row <- c()
  for(i in 1:length(df)){
    row[i] <- df[[c(i,row_num)]]
  }
  return(row)
}
sd(rowss(ymk118.18,2))


deviations <- function(df){
  for(i in 1:length(df[[1]])){
    print(sd(rowss(df, i)))
  }
}
deviations(ymk118.18)
