---
title: "Activity Data Analysis"
author: "Jake Ackman"
date: "10/12/2019"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---




```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'))
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

act <- read.csv("data/activity.csv")
```

## Activity Data

Read in the data above to act dataframe. Then below we:

-Calculate the average steps per day

-Chart a histogram of steps per day

-Calculate the mean and median steps

-Chart the mean steps per time interval

-Identify that 835 is the five minute time interval with the highest average


```r
steps_per_day <- act %>% group_by(date) %>% dplyr::summarise(steps_per_day = sum(steps, na.rm = TRUE))

steps_per_day
```

```
## # A tibble: 61 x 2
##    date       steps_per_day
##    <fct>              <int>
##  1 2012-10-01             0
##  2 2012-10-02           126
##  3 2012-10-03         11352
##  4 2012-10-04         12116
##  5 2012-10-05         13294
##  6 2012-10-06         15420
##  7 2012-10-07         11015
##  8 2012-10-08             0
##  9 2012-10-09         12811
## 10 2012-10-10          9900
## # ... with 51 more rows
```

```r
ggplot(steps_per_day, aes(steps_per_day)) + geom_histogram() + ggtitle("Steps Per Day Histogram")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_Template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
act %>% filter(!is.na(steps)) %>% dplyr::group_by(date) %>% dplyr::summarise(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm =TRUE))
```

```
## # A tibble: 53 x 3
##    date       mean_steps median_steps
##    <fct>           <dbl>        <dbl>
##  1 2012-10-02      0.438            0
##  2 2012-10-03     39.4              0
##  3 2012-10-04     42.1              0
##  4 2012-10-05     46.2              0
##  5 2012-10-06     53.5              0
##  6 2012-10-07     38.2              0
##  7 2012-10-09     44.5              0
##  8 2012-10-10     34.4              0
##  9 2012-10-11     35.8              0
## 10 2012-10-12     60.4              0
## # ... with 43 more rows
```

```r
act %>% filter(!is.na(steps)) %>% dplyr::group_by(interval) %>% dplyr::summarise(avg = mean(steps, na.rm = TRUE)) %>% ggplot(.,aes(interval, avg)) + geom_line() + ggtitle("Mean steps per Time Interval")
```

![](PA1_Template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

## Impute NAs

Get rid of those dang NAs! First show the total number of NAs. Then create a new variable so that the mean replaces any NAs. Add a histogram of the imputed mean, and then show a table of the 


```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
act_imputed <- act %>% mutate(impute = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

act_imputed %>% dplyr::group_by(date) %>% dplyr::summarise(mean_imputed = mean(impute)) %>% ggplot(.,aes(mean_imputed)) + geom_histogram() + ggtitle("Mean Steps Per Day From Dataset with Imputed Mean")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
act_imputed %>% dplyr::group_by(date) %>% dplyr::summarise(avg = mean(impute, na.rm = TRUE), median = median(impute, na.rm = TRUE))
```

```
## # A tibble: 61 x 3
##    date          avg median
##    <fct>       <dbl>  <dbl>
##  1 2012-10-01 37.4     37.4
##  2 2012-10-02  0.438    0  
##  3 2012-10-03 39.4      0  
##  4 2012-10-04 42.1      0  
##  5 2012-10-05 46.2      0  
##  6 2012-10-06 53.5      0  
##  7 2012-10-07 38.2      0  
##  8 2012-10-08 37.4     37.4
##  9 2012-10-09 44.5      0  
## 10 2012-10-10 34.4      0  
## # ... with 51 more rows
```


## Weekday Analysis

Created a factor variable called weekday to show if it's a weekday or the weekend. Graphed the imputed mean per time interval for Weekdays vs. Weekends.


```r
act_weekday <- act_imputed %>% dplyr::mutate(day = weekdays(ymd(date)))

act_weekday <- act_weekday %>% mutate(weekday = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

act_weekday$weekday <- factor(act_weekday$weekday, levels = c("Weekday", "Weekend"))

ggplot(act_weekday, aes(interval, impute)) + geom_line(color = "blue") + facet_grid(weekday~., scales = "free") + theme_classic()
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

