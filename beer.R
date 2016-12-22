library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

beer <- read.csv("YOUR/PATH/TO/Beer_Raw_Ratings.csv")
beer$Date <- as.Date(beer$Date, format = "%m/%d/%y")

bbi <- element_text(face="bold.italic", color="black")

Mean<-ddply(beer, .(Beer), summarize, mean=mean(Overall))
Mean_Bayes<-ddply(beer, .(Beer), summarize, mean=mean(Weighted))

table_mean<- Mean[Mean$mean>3.87,]
table_mean <- table_mean[order(-table_mean$mean),]
table_mean$mean<-round(table_mean$mean,2)
row.names(table_mean)<-c()
names(table_mean)<-c("Beer", "Conventional Mean")
grid.table(table_mean)

table_mean_Bayes<- Mean_Bayes[Mean_Bayes$mean>3.76,]
table_mean_Bayes <- table_mean_Bayes[order(-table_mean_Bayes$mean),]
row.names(table_mean_Bayes)<-c()
names(table_mean_Bayes)<-c("Beer", "Bayesian Average")
grid.table(table_mean_Bayes)

black <- subset(beer, Beer=="Brooklyn Black Ops")
black_decile_means <- rep(0, 10)

for (i in 1:10){
  end <-i*93
  start <-end-92
  black_decile_means[i]<-mean(black$Overall[start:end])
}

black_10 <-as.data.frame(cbind(1:10, black_decile_means))

black_graph <- ggplot(data=black_10, aes(x=V1, y=black_decile_means)) + geom_point(size=2)+
  scale_x_continuous(limits=c(0,11),
                     breaks=1:10, 
                     labels=c("First 10% \nof votes\n(Dec 2007 - Jan 2009)", 
                              rep("", 8),
                              "Last 10% \nof votes\n(Dec 2014 - May 2016)")) +
  labs (y="Average Score", x="", title="Brooklyn Black Ops") +
  theme(title=bbi, axis.text=element_text(size=12))

stout <- subset(beer, Beer=="Brooklyn Black Chocolate Stout")
stout_decile_means <- rep(0, 10)

for (i in 1:10){
  end <-i*336
  start <-end-335
  stout_decile_means[i]<-mean(stout$Overall[start:end])
}

stout_10 <-as.data.frame(cbind(1:10, stout_decile_means))

stout_graph <- ggplot(data=stout_10, aes(x=V1, y=stout_decile_means)) + geom_point(size=2)+
  scale_x_continuous(limits=c(0,11), breaks=1:10, 
                     labels=c("First 10% \nof votes\n(Dec 2007 - June 2010)", 
                              rep("", 8),
                              "Last 10% \nof votes\n(May 2014 - June 2016)")) +
  labs (y="Average Score", x="", title="Brooklyn Black Chocolate Stout") +
  theme(title=bbi, axis.text=element_text(size=12))
