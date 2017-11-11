# Reproducible Research: Peer Assessment 1

## Required libraries

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

## Loading and preprocessing the data

```r
## csv file must be in working directory
rawdf <- read.csv('activity.csv', stringsAsFactors =F)
```

## What is mean total number of steps taken per day?
### Preparing data

```r
### locate rows with NA values for steps
na_rows <- is.na(rawdf$steps)

### keep usable data
df <- rawdf[!na_rows,]

### convert string date field "date" to R date
df$day <- ymd(df$date)
```

### Histogram 

```r
#### number of steps per day
steps_d <- aggregate(steps~day, data=df, sum)
g <- ggplot(data=steps_d, aes(steps_d$steps)) 
g <- g + geom_histogram(binwidth=500)
g + labs(title = "Histogram of the total number of steps per day"
         ,x="Steps per day", y="Count")
```

![](RepData_PeerAssessment1_files/figure-html/plot1-1.png)<!-- -->

### mean and median for number of steps per day

```r
summary1<-summary(steps_d$steps)
summary1
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?
### Time series plot of the average number of steps taken

```r
#### aggregate steps on day and compute mean
steps_dm <- aggregate(steps~day, data=df, mean)
g <- ggplot(data=steps_dm, aes(x=day, y=steps)) 
g <- g + geom_line(size=1)
g + labs(title = "Time series plot of the average number of steps taken"
         ,x="day", y="Average count")
```

![](RepData_PeerAssessment1_files/figure-html/plot2-1.png)<!-- -->

### 5 minute interval containing the maximum number of steps

```r
### aggregate steps on interval and compute mean
steps_im <- aggregate(steps~interval, data=df, mean)

### index at which max value for steps is found
i_max <- match(max(steps_im$steps),steps_im$steps )
```


```r
### interval containing the maximum number of steps
steps_im$interval[i_max]
```

```
## [1] 835
```

## Imputing missing values
### total number of missing values in the dataset

```r
#### rows with NA values were identified earlier with na_rows is.na(rawdf$steps)
#### there are
length(na_rows)
```

```
## [1] 17568
```

```r
#### missing values
```
### Strategy for imputing missing data

```r
#### missing steps values for intervals are replaced by 
#### interval average values calculated earlier

#### subset for rows with na values
df_na <- rawdf[na_rows,]

#### join (merge) data frames df_na and steps_im calculated earlier
names(df_na)
```

```
## [1] "steps"    "date"     "interval"
```

```r
names(steps_im)
```

```
## [1] "interval" "steps"
```

```r
df_joined <- merge(df_na[,c(2,3)],steps_im, by="interval" )
names(df_joined)
```

```
## [1] "interval" "date"     "steps"
```

```r
#### convert string date field "date" to R date
df_joined$day <- ymd(df_joined$date)

#### as names(df) order is different from names(df_joined)
cols<- intersect( names(df), names(df_joined) )

#### rows with average values are appended
df <- rbind(df[, cols], df_joined[,cols])
```

### Histogram of the total number of steps per day after missing values are imputed

```r
#### aggregate steps on day
steps_d_filled <- aggregate(steps~day, data=df, sum)
g <- ggplot(data=steps_d_filled, aes(steps_d_filled$steps)) 
g <- g + geom_histogram(binwidth=500)
g + labs(title = "Histogram of the total number of steps per day, with imputed values"
         ,x="Steps per day", y="Average count")
```

![](RepData_PeerAssessment1_files/figure-html/plot3-1.png)<!-- -->

### mean and median for number of steps per day including imputed valuues

```r
summary2<-summary(steps_d_filled$steps)
summary2
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

### Do mean and median values differ from the estimates from the first part of the assignment?

```r
#### without imputed values
summary1
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
#### with imputed values
summary2
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
#### mean remains the same, median differs
#### we notice an increase of 1 for the nedian
```

## Are there differences in activity patterns between weekdays and weekends?
### preparation

```r
#### computes numeric day of week (Sunday is 1)
df$wday <- wday(df$day)

#### create a mapping day to type of day
day2dtype <- data.frame( wday=sort(unique(df$wday))
                        ,dtype=c( "WEEKEND","WEEKDAY","WEEKDAY","WEEKDAY"
                                ,"WEEKDAY","WEEKDAY","WEEKEND"))
day2dtype
```

```
##   wday   dtype
## 1    1 WEEKEND
## 2    2 WEEKDAY
## 3    3 WEEKDAY
## 4    4 WEEKDAY
## 5    5 WEEKDAY
## 6    6 WEEKDAY
## 7    7 WEEKEND
```

```r
#### join df with mapping
names(df)
```

```
## [1] "steps"    "date"     "interval" "day"      "wday"
```

```r
names(day2dtype)
```

```
## [1] "wday"  "dtype"
```

```r
df_merged <- merge(df,day2dtype, by="wday")
names(df_merged)
```

```
## [1] "wday"     "steps"    "date"     "interval" "day"      "dtype"
```

### Time series plots of the average number of steps taken per interval

```r
#### average number of steps per 5 minutes interval and day type
steps_idm <- aggregate(steps~interval+dtype, data=df_merged, mean)
g <- ggplot(steps_idm, aes(x=interval, y=steps)) 
g <- g + geom_line(size=1) + facet_wrap(~dtype, ncol=1)
g + labs(title = "Average number of steps taken per 5 minutes interval across week days and weekends"
         ,x="Interval", y="Average count")
```

![](RepData_PeerAssessment1_files/figure-html/plot4-1.png)<!-- -->

### Activity patterns between weekdays and weekends differ
### On weekdays activity peaks in the morning
### On weekends activity spreads out more evenly during morning and afternoon.
