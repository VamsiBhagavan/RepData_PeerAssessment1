
assignment = read.csv("activity.csv",header=TRUE)
missingvalues = is.na(assignment)
clean = subset(assignment,missingvalues == 'FALSE',select = steps:interval )

SumDate <- aggregate(clean$steps,by = list(clean$date),FUN = sum)
mean(SumDate$x)
median(SumDate$x)
hist(SumDate$x,col ="green")

meanInterval = aggregate(clean$steps,by = list(clean$interval),FUN = mean)
qplot(meanInterval$Group.1,meanInterval$x,geom="line",xlab="interval",ylab="avg steps")
test = assignment
for (i in 1:nrow(test)){
  t1<- test[i,]
  if(is.na(t1$steps) == TRUE){
    t2 = subset(meanInterval,meanInterval$Group.1==t1$interval)
  test$steps[i]=t2$x
}
}
SumDate_Imputed <- aggregate(test$steps,by = list(test$date),FUN = sum)
hist(SumDate_Imputed$x,col="gray")

test$date <- as.Date(test$date, format = "%Y-%m-%d")
test$weekdays <- as.character(test$weekdays)
test$weekdays <- weekdays(test$date)

for (i in 1:nrow(test)){
 
  if(test$weekdays[i] == "Sunday" || test$weekdays[i] == "Saturday" ){
 test$weekend[i] = 1
  }
  else{
    test$weekend[i] = 100
  }
}

join <- aggregate(test$steps,by = list(test$interval,test$weekend), FUN = mean)
colnames(join)=c("interval","Weekday","Avgsteps")
xyplot(Avgsteps ~ interval| Weekday, data=join, layout =c(1,2),type='l')
