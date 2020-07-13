library(dbplyr)
#tidyverse includes ggplot2
library(tidyverse)

#first, import spreadsheet with scores and name it 'scores'
  #option to name is in the lower left hand corner of dialog

#create new data object with spreadsheet, titled 'originaldata'
#better to fiddle and modify (or accidentally mess up) 'originadata' 
#than it is to mess up imported object 'scores'
originaldata <- scores

x <- originaldata$Midterm
result.mean <- mean(x)
result.mean

#create another data object removing rows containing 'pending' or 'not submitted'
  #necessary to have a column with numberic values only (rather than character)

#first, create data object omitting 'not submitted in midterm column
numberdata1 <- originaldata[!grepl('not submitted',scores$Midterm),]

#calculate mean Midterm score by first turning character to numeric values
numberdata1$Midterm <- as.numeric(as.character(numberdata1$Midterm))

#x now refers to the midterm column of numberdata1
x <- numberdata1$Midterm
result.mean <- mean(x)
result.mean
# printed result.mean should read '[1] 73.56506'

result.median <- median(x)
result.median
#printed result.median should read '[1] 75'

#create data object omitting 'not submitted' and 'pending' in Essay1 column
numberdata2 <- originaldata[!grepl('not submitted|pending',originaldata$Essay1),]

#calculate mean Essay1 score by first turning character to numeric values
numberdata2$Essay1 <- as.numeric(as.character(numberdata2$Essay1))

#y now refers to the Essay1 column of numberdata2
y <- numberdata2$Essay1
result.mean <- mean(y)
result.mean
# printed result.mean should read '[1] 70.36364'

result.median <- median(y)
result.median
#printed result.median should read '[1] 72'

#MIDTERM: MEAN 73.56506, MEDIAN 75; ESSAY1 MEAN 70.36364, MEDIAN 72

#Now, we create a new data object ('numberdata') that includes rows ONLY with scores in BOTH columns
  #So, we create a new data object using last numberdata1 that omits 'not submitted'
    #or 'pending' in Essay1 column
numberdata <- numberdata1[!grepl('not submitted|pending',numberdata1$Essay1),]
  #i.e. data includes ONLY students who received scores for BOTH assignments

#now, we can plot the scores with midterm score on the x axis and essay1 score on the y
ggplot(numberdata, aes(Midterm, Essay1, colour = mean)) +
  scale_color_gradient(low="yellow",high="brown")+
  ggtitle('Essay1 Score Compared to Midterm Score')+ geom_point()+geom_smooth()

#first, turn all values from character to numeric 
numberdata$Midterm <- as.numeric(as.character(numberdata$Midterm))
numberdata$Essay1 <- as.numeric(as.character(numberdata$Essay1))
#now, we can create a new column with the mean score (midterm and essay)
numberdata$mean <- rowMeans(subset(numberdata, select = c(Midterm, Essay1)), na.rm = TRUE)

#for a histogram plotting out course 
meanscore <- numberdata$mean
hist(meanscore)
#for a histogram with count of each category
mh <- hist(meanscore,ylim=c(0,20))
text(mh$mids,mh$counts,labels=mh$counts, adj=c(0.5, -0.5))


