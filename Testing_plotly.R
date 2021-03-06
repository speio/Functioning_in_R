##demoing plotly

library(plotly)
library(datasets)






seed <- as.double(1)
RANDU <- function() {
  seed <<- ((2^16 + 3) * seed) %% (2^31)
  seed/(2^31)
}
randoms <- function(length){ 
  u.u <-c()
  for(i in 1:length) {
    u.u[i] <- RANDU()    
  }
  print(round(u.u,3))
}

Data_Frame <- list(x = 1:100, y = randoms(100))
Data_Frame <- as.data.frame(Data_Frame)

test.plot <- ggplot(data = Data_Frame, aes(x=x, y=y))  +
  geom_bar(aes(x=x, y=y), fill = 'black', stat = "identity", alpha = 0.8)+ #You could replace this with 
  #geom_point if you want dots on your line for the time points instead of the bars. then don’ use stat=identity
  geom_smooth( col = 'steel blue', fill = 'tan', size = 4 , method = 'loess') +  #This plots the loess smoothed line (with Error)
  
  labs(x="Time (seconds)", y="%  Attention on any one Subject", title = expression(paste('My Attention Span'))) #Labels of course, with the syntax to use math symbols/greek 

print(test.plot)

#applying the above ggplot to plotly...?
set_credentials_file(username="username", api_key="key")
test2 <- plotly(username="username", key="key")
#plot <- p$plotly(data = Data_Frame, x = x,  y = Data_Frame$y, type = 'line')

plot2 <- test2$ggplotly(test.plot)

#gives the error
#> plot2 <- test2$ggplotly(test.plot)
#Uh oh, an error occured on the server. 
#Error in browseURL(resp$url) : 'url' must be a non-empty character string
#In addition: Warning message:
#  In layer2traces(L, df, misc, trace.list$plot) :
#  Conversion not implemented for geom_smooth (basic geom_smooth), ignoring. Please open an issue with your example code at https://github.com/ropensci/plotly/issues
















