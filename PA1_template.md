# PROJECT

This assignment creates a report that answers the following questions:
* What is mean total number of steps taken per day?
* What is the average daily activity pattern?
* Imputing missing values
* Are there differences in activity patterns between weekdays and weekends?

To do the project we need to do these tasks:
* Load the data (i.e. read.csv())
* Process/transform the data (if necessary) to answer the questions above.
* Make the Plots
* Make a report in a R markdown document that can be processed by knitr and be transformed into an HTML file.

## Loadign and Preprocessing the data

**Downloading the file**
```{r directory,message=FALSE}
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
directory<-("raw_data.zip")
download.file(fileUrl,directory)
unzip(directory)
library(plyr)
library(dplyr)
```

**Loading the data**
```{r data }
data<-read.table("activity.csv",header=TRUE, sep=",")
```

The data contains NA values; for the question 1 and 2 let's going to use only the complete data.
```{r complete_data }
complete_data <- data[complete.cases(data), ]
```

## QUESTION 1: What is the mean total of steps taken per day?

**Processing the data**
```{r steps_taken}
steps_taken<-complete_data%>%
	group_by(date)%>%
	summarise(steps=sum(steps))
```

**Making the plot** 
```{r hist,echo=TRUE,results="hold"}
hist<-hist(steps_taken$steps,breaks=20,col="grey",main="Total Number of steps taken each 
day",xlab="steps taken")
```

![plot of chunk hist-1](https://github.com/sofiyag87/ReproducibleData/blob/master/PA1_template_files/figure-html/hist-1.png)


**Report**
```{r original_data, results="hold"}
original_data<-summary(steps_taken$steps)
original_data
```
```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

The mean of steps taken each day is 10766


## QUESTION 2: What is average daily activity pattern?

**Processing the data**
```{r steps_interval}
steps_interval<-complete_data%>%
	group_by(interval)%>%
	summarise(steps=mean(steps))
```

**Making the plot** 
```{r plot,echo=TRUE}
plot<-plot(steps_interval$interval,steps_interval$steps,type="l",col="red",
main="Average Daily Pattern",xlab="Intervals",ylab="Number of Steps")
```
![plot of chunk plot1](https://github.com/sofiyag87/ReproducibleData/blob/master/PA1_template_files/figure-html/plot-1.png)



**Report**
```{r max_number}
max_number<-steps_interval[which.max(steps_interval$steps),]
max_number
```
```
## # A tibble: 1 x 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

The maximun average number of steps is 206 and it is located in the interval 835.


## QUESTION 3: Imputting Missing Values

**Processing the data**
```{r missing_values}
missing_values<-sum(is.na(data$steps))
```
The total number of missing values is 2304, which reprents the 13% of the data.

As we have the average steps for each interval, let's assign the mean values to the values-steps for the interval that has a missing value in the original dataset.

```{r new_data,results="hold"}
new_data<-data
NAs<-is.na(new_data$steps)
mean_interval<-tapply(complete_data$steps,complete_data$interval,mean)
new_data$steps[NAs]<-mean_interval[as.character(new_data$interval[NAs])]
```
```{r steps_taken_new}
steps_taken_new<-new_data%>%
	group_by(date)%>%
	summarise(steps=sum(steps))
```

**Making the plot**  
```{r hist2,echo=TRUE,results="hold"}
hist2<-hist(steps_taken_new$steps,breaks=20,col="grey",main="Total Number of steps taken each 
day",xlab="steps taken")
```
![plot of chunk hist2](https://github.com/sofiyag87/ReproducibleData/blob/master/PA1_template_files/figure-html/hist2-1.png)
**Report**
```{r new_data_report, results="hold"}
new_data_report<-summary(steps_taken_new$steps)
new_data_report
```
```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

The mean of steps taken each day is 10766

**Impact of imputing missing values**
```{r impact, echo=TRUE}
par(mfrow=c(1,2))
hist<-hist(steps_taken$steps,breaks=15,col="grey",main="Total Number of steps taken each \n day Original Dataset
",xlab="steps taken",ylim=c(0,25))
hist2<-hist(steps_taken_new$steps,breaks=15,col="grey",main="Total Number of steps taken each \n day New Dataset",xlab="steps taken",ylim=c(0,25))
```
![plot of chunk hist3](https://github.com/sofiyag87/ReproducibleData/blob/master/PA1_template_files/figure-html/impact-1.png)
```{r report}
impact_report<-rbind(original_data,new_data_report)
impact_report
```
```
##                 Min. 1st Qu.   Median     Mean 3rd Qu.  Max.
## original_data     41    8841 10765.00 10766.19   13294 21194
## new_data_report   41    9819 10766.19 10766.19   12811 21194
```

With the introduction of the missing values, the new mean is 10766 and the new median is 10766 . Comparing the means between the original dataset and the new dataset, this value has not changed, and the median has changed to the same value of the mean. 
This is due to the filling of missing values with mean values; which means that al least the 13% (missing values of original data) are the same as the mean, making the data closer to this value.

Imputing missing values in the original dataset had an impact in the frecuencies. The frecuency of steps taken increased in the mean in the center of the histogram.

## QUESTION 4: Are there differences in activity patterns between weekdays and weekends?

**Processing the data**

First we need to change the class of the data from "factor" to "date" and then set the date as a weekday or a weekend. To to this let's create a new variable called new_data_type.
As my system is in spanish we use "sábado" and "domingo" as "saturday" and "sunday". 
```{r date, echo=TRUE }
date<-as.Date(as.character(new_data$date))
new_data$weekdays<-weekdays(date)
new_data$type<-as.factor(ifelse(new_data$weekdays %in% c("sábado", "domingo"), 
"weekend", "weekday"))

steps_taken_weekdays<-new_data%>%
	group_by(type,interval)%>%
	summarise(steps=mean(steps))
```

**Making the plot** 

To make the plot let's use the lattice system, which allows to make differents plots in the same panel by conditioning the independent/dependent variables to the factor type.
```{r echo=TRUE }
library(lattice)
with(steps_taken_weekdays, 
      xyplot(steps ~ interval | type, 
      type = "l",      
      main = "Activity Pattern",
      xlab = "Intervals",
      ylab = "Number of Steps"))
```
![plot of chunk date](https://github.com/sofiyag87/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/date-1.png)
From the figure we can see that the person who owns this dataset walks the similar average steps on weekdays and weekends.