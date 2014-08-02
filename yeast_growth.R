#utz's growth curve



library(reshape)

growth <- file.choose()

df <- as.data.frame(read.table(growth))
df

ymk211.30 <- subset(df, select = ymk211.30.1:ymk211.30.3)
ymk211.18 <- subset(df, select = ymk211.18.1:ymk211.18.3)
ymk118.30 <- subset(df, select = ymk118.30.1:ymk118.30.3)
ymk118.18 <- subset(df, select = ymk118.18.1:ymk118.18.3)

x <- c("ymk118.18","ymk118.30", "ymk211.18", "ymk211.30")

deviations <- list()
deviations[[4]] <- apply(ymk211.30, 1, sd)
#creating list of standard deviations
#How to access the list
deviations[[c(1,1)]]

avg.growth <- list()
avg.growth[[1]] <- rowMeans(ymk118.18)
avg.growth[[2]] <- rowMeans(ymk118.30)
avg.growth[[3]] <- rowMeans(ymk211.18)
avg.growth[[4]] <- rowMeans(ymk211.30)
avg.growth

df.avg<-as.data.frame(avg.growth)

colnames(df.avg) <- x

df.avg$Time <- df$time  
df.avg
df

#some additional setup for the plot
df.avg <- melt(df.avg ,  id = 'Time', variable_name = 'Sample')
dvs <- melt(deviations)[1]
error <- aes(ymin=df.avg-dvs, ymax=df.avg+dvs)

# plot on same grid, each series colored differently -- 
# good if the series have same scale
growth.plot <- ggplot(df.avg, aes(Time,value)) +
  geom_line(aes(col = Sample)) +
  geom_point(aes(col = Sample)) +
  geom_errorbar(error, col = 'tan', width = .2, size = 0.8, position = position_dodge(.9)) + 
  #THIS LOOKS RIDICULOUS stat_smooth(aes(colour = series, fill = series)) +
    labs(x = 'Time', y = 'OD' )

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

