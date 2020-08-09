---
title: "PA1_template"
author: "David Vanegas"
date: "8/8/2020"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<br>

# REPRODUCIBLE RESEARCH: ASSESMENT 1

This is the project for the week 2 of the Reproducible Research course

Specific elements of the project are detailed below:

*****

<br>

## INTRODUCTION

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

<br>

## DATA

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as <span style="color:red">NA</span>)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

******

<br>

### ***FIRST PART: Loading and preprocessing the data***

```{r Library, warning = FALSE, message = FALSE}
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
```

Read link and download file, unzip the archive and finally, load data

```{r}
setwd("C:/Users/david/Desktop/Sync/Data Esp/Course5 - Reproducible Research")
Archive_Zip <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(Archive_Zip, destfile = "data.zip")
unzip("data.zip")
activity <- read.csv("activity.csv")
```
<br>

Now, verify structure dataframe
```{r}
dim(activity)
names(activity)
head(activity)
```

******

<br>


### ***SECOND PART: What is mean total number of steps taken per day?***

Create a new dataframe and add necessary data

*Note: Ignore the missing values*

```{r warning = FALSE, message = FALSE}
act.day <- group_by(activity, date)
act.day <- summarize(act.day, sum_steps = sum(steps, na.rm = TRUE))
```

<br>

Verify structure dataframe
```{r}
dim(act.day)
max(act.day$sum_steps)
```

<br>

Calculate and report the mean and median total number of steps taken per day
```{r question1}
central_values <- data.frame(measurement = c("Mean", "Median"),
                             value = c(mean(act.day$sum_steps), median(act.day$sum_steps)))
meandt <-  round(central_values[1, 2], 0)
mediandt <-  round(central_values[2, 2], 0)
print(paste("The MEAN value from total number of steps taken per day is [", 
            meandt, "] and their MEDIAN value is [", mediandt, "]"))
```

<br>

Now, using the filled data set, let's make a histogram of the total number of steps taken each day

<br>

_Note: First calculate appropriate parameters for the plot_

Use the formula $1 + 3.322logn$, to find the number of the intervals of the histogram and their width, where $n$ is equal to nrow from data.

```{r}
log_value <- log10(nrow(act.day)) 
num_bin <- round(1 + 3.322*log_value)
bin_width <- round((max(act.day$sum_steps)) / num_bin)
print(paste("The number of bins are [", num_bin, "], therefore the bin width value is [", bin_width, "]"))
```

<br>
And, make the plot
```{r}
ggplot(act.day, aes(sum_steps)) + geom_histogram(aes(sum_steps), binwidth = 3028, color = "grey40", fill = "aquamarine3", boundary = 0)+
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))+
        theme(plot.title = element_text(margin = margin(0, 0, 0, 20)))+
        scale_x_continuous("Steps", breaks = seq(0, 21194,along.with = 0: 7),
                  labels = scales::number_format(accuracy = 1.0, decimal.mark = "."))+
        scale_y_continuous("Frecuency")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12))+
        theme(axis.text.y = element_text(size = 12))+
        theme(axis.title.x = element_text(size = 12, face = "bold"))+
        theme(axis.title.x = element_text(margin = margin(20, 0, 0, 0)))+
        theme(axis.title.y = element_text(size = 12, face = "bold"))+
        theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0)))+
        ggtitle(expression(textstyle(atop("Histogram of Daily Steps"))))+
        theme(panel.background = element_blank())+
        theme(panel.border = element_blank(),panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(), 
                          axis.line = element_line(colour = "black"))+
        stat_bin(binwidth = 3028, geom="text", aes(label=..count..), 
                 vjust = - .3, hjust = .1, size = 4, boundary = 0)+
        geom_vline(data = central_values, aes(xintercept = value, 
                                              color = measurement), size = 2) +
        scale_color_manual(values = c("red", "orange"), name = NULL)
        
![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```

<br>

```
- What is mean total number of steps taken per day?

Answer:

[`r meandt`] is mean total number of steps taken per day

```

*****

<br>

### ***THIRD PART: What is the average daily activity pattern?***

To create a time series plot of the average number of steps taken, first create a new dataframe and add data necessary

```{r}
act.int <- aggregate(x = list(meansteps = activity$steps), 
                        by = list(interval = activity$interval), 
                                        FUN = mean, na.rm = TRUE)


```

<br>

Calculate mean of the steps average
```{r}
mean_int <- round(mean(act.int$meansteps), 0)
```

<br>

Then, plot the relation between 5-minute interval and the average number of steps taken averaged across all days

```{r}
ggplot(act.int, aes(x = interval, y = meansteps)) + 
        geom_line(color = "grey10")+
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))+
        theme(plot.title = element_text(margin = margin(0, 0, 0, 20)))+
        scale_x_continuous("Time intervals (5 - minute)", breaks = seq(0, 3000 , by = 200))+
        scale_y_continuous("Mean steps")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12))+
        theme(axis.text.y = element_text(size = 12))+
        theme(axis.title.x = element_text(size = 12, face = "bold"))+
        theme(axis.title.x = element_text(margin = margin(20, 0, 0, 0)))+
        theme(axis.title.y = element_text(size = 12, face = "bold"))+
        theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0)))+
        ggtitle(expression(textstyle(atop("Average number of steps taken"))))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank())
```

<br>

And finally, calculate the 5-minute interval that on average contains the maximum number of steps

```{r}
maxsteps <- act.int[which(act.int$meansteps == max(act.int$meansteps)),]
print(paste("5-minute interval that on average contains the maximum number of steps is [", maxsteps [, 1], "]"))
```

<br>

```
- What is the average daily activity pattern?

Answer:

The average daily activity pattern is [`r mean_int`] steps given per day

```

*****

<br>

### ***FOURTH PART:  Imputing missing values***

First, calculate and report number of days where there are missing values (coded as NAs)
 
```{r}
print(paste("The total number of days with missing values are [", sum(is.na(activity$steps)), "]"))
```

<br>

Copy in a new dataset to the original dataset but with the missing data filled in. After, make a  strategy for filling in all of the missing values in the dataset using mean values
```{r}
act.full <- activity  

for (i in 1: nrow(activity)){
        if(is.na(activity$steps[i])){
                act.full$steps[i] <- 
                        act.int$meansteps[act.full$interval[i] == act.int$interval]
        }
}
```

<br>

Make a new dataframe and add data necessary
```{r warning = FALSE, message = FALSE}
act.fullday <- group_by(act.full, date)
act.fullday <- summarize(act.fullday, sumsteps = sum(steps))
```

<br>

Calculate and report the mean and median total number of steps taken per day
```{r}
central_values2 <- data.frame(measurement = c("Mean", "Median"),
                             value = c(mean(act.fullday$sumsteps),
                                       median(act.fullday$sumsteps)))
central_values2
print(paste("The MEAN value from total number of steps taken per day is [", 
            round(central_values2[1, 2], 2), "] and their MEDIAN value is [",
            round(central_values2[2, 2], 2), "]"))
```

<br>

Plot histogram with previously created data frame
```{r}
ggplot(act.fullday, aes(sumsteps))+ geom_histogram(aes(sumsteps), binwidth = 3028,
                        color = "grey40", fill = "aquamarine3", boundary = 0)+
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))+
        theme(plot.title = element_text(margin = margin(0, 0, 0, 20)))+
        scale_x_continuous("Steps", breaks = seq(0, 21194,along.with = 0: 7),
                        labels = scales::number_format(accuracy = 1.0, decimal.mark = '.'))+
        scale_y_continuous("Frecuency")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12))+
        theme(axis.text.y = element_text(size = 12))+
        theme(axis.title.x = element_text(size = 12, face = "bold"))+
        theme(axis.title.x = element_text(margin = margin(20, 0, 0, 0)))+
        theme(axis.title.y = element_text(size = 12, face = "bold"))+
        theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0)))+
        ggtitle(expression(textstyle(atop("Histogram of Daily Steps"))))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank())+
        stat_bin(binwidth = 3028, geom="text", aes(label=..count..), 
                 vjust = - .3, hjust = .9, size = 4, boundary = 0)+
        geom_vline(data = central_values2, aes(xintercept = value, 
                                              color = measurement), size = 2) +
        scale_color_manual(values = c("red", "orange"), name = NULL)
```

<br>

```
- Do these values differ from the estimates from the first part of the assignment? 

Answer: 

      Yes, the mean and the median values are now the same after replacing missing values after replacing missing values with the mean values for the each interval. 


- What is the impact of imputing missing data on the estimates of the total daily number of steps?

Answer:

      The impact is very evident, if imputing missing data it can cause the homogenization of the data; which generates that the reality of the phenomenon is not visualized correctly
```

*****

<br>

### ***FIFTH PART: Are there differences in activity patterns between weekdays and weekends?***

<br>

Transform date variable to the date class using weekdays() function to generate the day of the week of each date 
```{r}
act.week <- activity
act.week$week_day <- weekdays(as.Date(act.week$date))
act.week$week_end <-as.factor(act.week$week_day == "Saturday"|
                                      act.week$week_day == "Sunday")
levels(act.week$week_end) <- c("Weekday", "Weekend")
```

<br>

Create a new dataframe and add data necessary
```{r warning = FALSE, message = FALSE}
act_week <- group_by(act.week, week_end, interval)
act_week <- summarize(act_week, meansteps = mean(steps, na.rm = TRUE))
```

<br>

Make a panel plot containing Average number of steps between Weekdays and Weekend

```{r}
ggplot(act_week, aes(interval, meansteps, color = week_end)) + geom_line()+
        facet_grid(week_end ~.)+
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(plot.title = element_text(margin = margin(0, 0, 0, 20)))+
        scale_x_continuous("Time intervals", breaks = seq(0, 3000 , by = 200))+
        scale_y_continuous("Mean steps")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12))+
        theme(axis.text.y = element_text(size = 12))+
        theme(axis.title.x = element_text(size = 12, face = "bold"))+
        theme(axis.title.x = element_text(margin = margin(20, 0, 0, 0)))+
        theme(axis.title.y = element_text(size = 12, face = "bold"))+
        theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0)))+
        ggtitle(expression(textstyle(atop("Average number of steps between Weekdays and Weekend"))))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank())+
        theme(strip.background = element_rect(fill = c("aquamarine3")), 
              strip.text.y = element_text(size = 12, face = "bold"))
```


<br>

```
- Are there differences in activity patterns between weekdays and weekends?

Answer:

    Principally, there are differences in intervals greater than 800, in which it is observed that the average steps per time interval is higher on weekends than on weekdays

```

<br>
