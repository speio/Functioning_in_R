#utz's growth curve



library(reshape)

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
df.deviations$Time <- df$time.1.8.

#some additional setup for the plot
df.avg <- melt(df.avg ,  id = 'Time', variable_name = 'Sample')
df.dvs <- melt(df.deviations, id = 'Time', variable_name = 'Sample') 
df.avg$sd <- df.dvs[3]
colnames(df.avg) <- c('Time', 'Sample', 'OD', 'sd')
df.avg

error <- aes( ymin= OD - sd, ymax= OD + sd)

growth.plot <- ggplot(df.avg, aes(Time,OD), stat = 'identity') +
  geom_line(aes(col = Sample)) +
    geom_point(aes(col = Sample)) +
      geom_errorbar(error, col = 'tan', width = .2, size = 0.8, position = position_dodge(.9)) + 
        labs(x = 'Time (min)', y = 'OD' )

print(growth.plot)

test <- plotly(username="g.villafano", key="nbny7yk7g3")

pl <- test$ggplotly(growth.plot)









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

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
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
