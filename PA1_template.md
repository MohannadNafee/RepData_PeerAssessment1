#Reproducible Research: Peer Assessment 1

##reading in the dataset and/or processing the data


```r
require(knitr)
knit2html("PA1_template.Rmd")
```

```
## Warning in readLines(con): incomplete final line found on
## 'PA1_template.Rmd'
```

```
## 
## 
## processing file: PA1_template.Rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |....                                                             |   6%
##   ordinary text without R code
## 
##   |                                                                         |........                                                         |  12%
## label: unnamed-chunk-9
```

```
##   |                                                                         |............                                                     |  19%
##   ordinary text without R code
## 
##   |                                                                         |................                                                 |  25%
## label: unnamed-chunk-10
```

```
##   |                                                                         |....................                                             |  31%
##   ordinary text without R code
## 
##   |                                                                         |........................                                         |  38%
## label: unnamed-chunk-11
##   |                                                                         |............................                                     |  44%
##   ordinary text without R code
## 
##   |                                                                         |................................                                 |  50%
## label: unnamed-chunk-12
```

```
##   |                                                                         |.....................................                            |  56%
##   ordinary text without R code
## 
##   |                                                                         |.........................................                        |  62%
## label: unnamed-chunk-13
##   |                                                                         |.............................................                    |  69%
##   ordinary text without R code
## 
##   |                                                                         |.................................................                |  75%
## label: unnamed-chunk-14
##   |                                                                         |.....................................................            |  81%
##   ordinary text without R code
## 
##   |                                                                         |.........................................................        |  88%
## label: unnamed-chunk-15
```

```
##   |                                                                         |.............................................................    |  94%
##   ordinary text without R code
## 
##   |                                                                         |.................................................................| 100%
## label: unnamed-chunk-16
```

```
## output file: PA1_template.md
```

```r
filePath<- getwd()
if(!file.exists("./PA1")){dir.create("./PA1")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
download.file(fileUrl, destfile = "./Activity_monitoring_data.zip")

unzip(zipfile ="./Activity_monitoring_data.zip", exdir="./PA1")

variable.class<-c('numeric','character','numeric')
activity <- read.csv('./PA1/activity.csv',header=TRUE,na.strings='NA', colClasses = variable.class)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

##Histogram of the total number of steps taken each day


```r
require(ggplot2)

totalSteps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)

qplot(totalSteps, binwidth= 1000, xlab="total number of steps taken each day", ylab="Frequecy", main="Fig1")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

##Mean and median number of steps taken each day


```r
mean(totalSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalSteps, na.rm=TRUE)
```

```
## [1] 10395
```

##Time series plot of the average number of steps taken


```r
averages <- aggregate(steps ~ interval, data= activity, FUN = mean, na.rm=TRUE)

qplot(interval,steps , data= averages ,  geom= "line", xlab="5-Minute Interval", ylab="Average Number of Steps Taken", main="Fig2")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

##The 5-minute interval that, on average, contains the maximum number of steps


```r
averages[which.max(averages$steps),1]
```

```
## [1] 835
```

##Code to describe and show a strategy for imputing missing data


```r
missing<- is.na(activity$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
complete<-activity
complete$steps<- ifelse(is.na(complete$steps),averages$steps[match(complete$interval,averages$interval)],complete$steps)
```

##Histogram of the total number of steps taken each day after missing values are imputed


```r
fullstepsperday<- aggregate(complete$steps, list(complete$date), FUN=sum)
qplot(fullstepsperday$x, binwidth= 5000, xlab="Steps Per Day", ylab="Frequecy", main="Fig3")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

activity$wDay <- factor((weekdays(activity$date) %in% weekdays),levels=c(FALSE, TRUE), labels=c('weekend','weekday'))

averages2 <- aggregate(steps ~ interval + wDay , data= activity, FUN = mean, na.rm=TRUE) 

ggplot(averages2, aes(x=interval, y=steps, color = wDay)) + geom_line() + facet_wrap(~wDay, ncol = 1, nrow=2)+ ggtitle("Fig4")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
