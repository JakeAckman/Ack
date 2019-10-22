---
title: "Activity Data Analysis"
author: "Jake Ackman"
date: "10/12/2019"
output: html_document
---

```{r setup, include=FALSE, echo = FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(haven)
library(foreign)
library(plyr)
library(tidyverse)
library(plotly)
library(knitr)
library(formattable)
library(data.table)
library(lubridate)
```

```{r}
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

```{r }
steps_per_day <- act %>% group_by(date) %>% dplyr::summarise(steps_per_day = sum(steps, na.rm = TRUE))

steps_per_day

ggplot(steps_per_day, aes(steps_per_day)) + geom_histogram() + ggtitle("Steps Per Day Histogram")

act %>% filter(!is.na(steps)) %>% dplyr::group_by(date) %>% dplyr::summarise(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm =TRUE))

act %>% filter(!is.na(steps)) %>% dplyr::group_by(interval) %>% dplyr::summarise(avg = mean(steps, na.rm = TRUE)) %>% ggplot(.,aes(interval, avg)) + geom_line() + ggtitle("Mean steps per Time Interval")

```

## Impute NAs

Get rid of those dang NAs! First show the total number of NAs. Then create a new variable so that the mean replaces any NAs. Add a histogram of the imputed mean, and then show a table of the 

```{r echo = TRUE}

sum(is.na(act$steps))

act_imputed <- act %>% mutate(impute = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

act_imputed %>% dplyr::group_by(date) %>% dplyr::summarise(mean_imputed = mean(impute)) %>% ggplot(.,aes(mean_imputed)) + geom_histogram() + ggtitle("Mean Steps Per Day From Dataset with Imputed Mean")

act_imputed %>% dplyr::group_by(date) %>% dplyr::summarise(avg = mean(impute, na.rm = TRUE), median = median(impute, na.rm = TRUE)) %>% kable()

```


## Weekday Analysis

Created a factor variable called weekday to show if it's a weekday or the weekend. Graphed the imputed mean per time interval for Weekdays vs. Weekends.

```{r echo = TRUE}

act_weekday <- act_imputed %>% dplyr::mutate(day = weekdays(ymd(date)))

act_weekday <- act_weekday %>% mutate(weekday = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

act_weekday$weekday <- factor(act_weekday$weekday, levels = c("Weekday", "Weekend"))

ggplot(act_weekday, aes(interval, impute)) + geom_line(color = "blue") + facet_grid(weekday~., scales = "free") + theme_classic()


```

