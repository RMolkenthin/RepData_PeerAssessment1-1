## data load and processing
## assuming that the working dir is set to repo
## the existing activity.zip is not used to be sure to have the newest downloaded

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fzip <- "repdata%2Fdata%2Factivity.zip"
fname <- "activity.csv"

if(!(file.exists(fname))) {
    download.file(fileUrl, destfile = fzip, method = "auto")
    unzip(fzip)
}

## read data into data frame
df_act <- read.csv(fname, header = TRUE, sep = ",", 
                   stringsAsFactors = FALSE, na.strings = "NA")

## Date coercion
df_act$steps <- as.integer(df_act$steps)
df_act$interval <- as.integer(df_act$interval)
df_act$date <- as.Date(df_act$date, format = "%Y-%m-%d")

## Data frame for mean, median and total steps per day
library(dplyr)
library(ggplot2)

df01 <- df_act %>% group_by(date) %>% summarize(totalSteps = sum(steps))

## Histogram
ggplot(df01, aes(totalSteps)) + 
    geom_histogram(binwidth = 500, fill = "indianred") +
    labs(title = "Histogram of total steps per day between 01-10-2012 and 30-11-2012", 
         x ="Total steps per day", y = "Frequency") + 
    scale_x_continuous(breaks = seq(0, 25000, 2000)) +
    scale_y_continuous(breaks = seq(0, 10, 1))

## Table on Mean and Median
c("Mean" = mean(df01$totalSteps, na.rm = TRUE),
        "Median" = median(df01$totalSteps, na.rm = TRUE))

## Time series: Intervals averaged accross the days
df02 <- df_act %>% group_by(interval) %>% 
    summarize(avgSteps = round(mean(steps, na.rm = TRUE), 2))
ggplot(df02, aes(x = interval, y = avgSteps)) +
    geom_line(color = "chocolate", size = 1) +
    labs(title = "Average steps per interval between 01-10-2012 and 30-11-2017",
         y = "Number of Steps", x = "Interval") +
    scale_x_continuous(breaks = seq(0, max(df02$interval), 200)) + 
    scale_y_continuous(breaks = seq(0, 500, 20))

## Maximum of average steps accross the intervals
df02[df02$avgSteps == max(df02$avgSteps),]

## Total number of rows with NA in steps
sum(is.na(df_act$steps))

## Replacing NAs with interval's mean
df_act_nna <- left_join(df_act, df02, by = "interval")
df_act_nna$steps[is.na(df_act_nna$steps)] <- as.numeric(df_act_nna$avgSteps)
df_act_nna$avgSteps <- NULL

df03 <- df_act_nna %>% group_by(date) %>% summarize(totalSteps = sum(steps))

##histogram with replaces values
ggplot(df03, aes(totalSteps)) + 
    geom_histogram(binwidth = 500, fill = "aquamarine") +
    labs(title = "Histogram of total steps per day between 01-10-2012 and 30-11-2012", 
         x ="Total steps per day", y = "Frequency") + 
    scale_x_continuous(breaks = seq(0, 25000, 2000)) +
    scale_y_continuous(breaks = seq(0, 12, 1))

## Table on Mean and Median with replaced values
c("Mean" = mean(df03$totalSteps, na.rm = TRUE),
        "Median" = median(df03$totalSteps, na.rm = TRUE))

## Comparison between weekdays and weekends
library(lubridate)
## using wday -> 1, 7 means Sunday resp. Saturday
df_act_nna <- cbind(df_act_nna, "day" = ifelse(wday(df_act_nna$date) %in% c(1, 7), 0, 1))
df_act_nna$day <- factor(df_act_nna$day, levels = c(0, 1), labels = c("weekend", "weekday"))

## panel plot
## summarizing by weekday or weekend and interval
df04 <- df_act_nna %>% group_by(day, interval) %>% summarize(avgStepsInt = mean(steps))

ggplot(df04, aes(x = interval, y = avgStepsInt, color = day)) +
    geom_line(size = 1) +
    facet_grid(day ~.) +
    labs(title = "Average steps per interval on weekdays and weekend", 
         x = "Interval", y = "Average steps") +
    theme(legend.position = "none")


