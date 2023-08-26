How Does a Bike-Share Navigate Speedy Success?
================
Matteo Casetta
2023-08-20

## Introduction

In this case study, you will perform many real-world tasks of a junior
data analyst. You will work for a fictional company, Cyclistic, and meet
different characters and team members. In order to answer the key
business questions, you will follow the steps of the data analysis
process: **ask**, **prepare**, **process**, **analyze**, **share**, and
**act**. Along the way, the **Case Study Roadmap** tables — including
guiding questions and key tasks — will help you stay on the right path.

### Scenario

You are a junior data analyst working in the marketing analyst team at
Cyclistic, a bike-share company in Chicago. The director of marketing
believes the company’s future success depends on maximizing the number
of annual memberships. Therefore, your team wants to understand how
casual riders and annual members use Cyclistic bikes differently. From
these insights, your team will design a new marketing strategy to
convert casual riders into annual members. But first, Cyclistic
executives must approve your recommendations, so they must be backed up
with compelling data insights and professional data visualizations.

### Characters and teams

- **Cyclistic**: A bike-share program that features more than 5.800
  bicycles and 600 docking stations. Cyclistic sets itself apart by also
  offering reclining bikes, hand tricycles, and cargo bikes, making
  bike-share more inclusive to people with disabilities and riders who
  can’t use a standard two-wheeled bike. The majority of riders opt for
  traditional bikes; about 8% of riders use the assistive options.
  Cyclistic users are more likely to ride for leisure, but about 30% use
  them to commute to work each day.
- **Lily Moreno**: The director of marketing and your manager. Moreno is
  responsible for the development of campaigns and initiatives to
  promote the bike-share program. These may include email, social media,
  and other channels.
- **Cyclistic marketing analytics team**: A team of data analysts who
  are responsible for collecting, analyzing, and reporting data that
  helps guide Cyclistic marketing strategy. You joined this team six
  months ago and have been busy learning about Cyclistic’s mission and
  business goals — as well as how you, as a junior data analyst, can
  help Cyclistic achieve them.
- **Cyclistic executive team**: The notoriously detail-oriented
  executive team will decide whether to approve the recommended
  marketing program.

### About the company

In 2016, Cyclistic launched a successful bike-share offering. Since
then, the program has grown to a fleet of 5.824 bicycles that are
geotracked and locked into a network of 692 stations across Chicago. The
bikes can be unlocked from one station and returned to any other station
in the system anytime.

Until now, Cyclistic’s marketing strategy relied on building general
awareness and appealing to broad consumer segments. One approach that
helped make these things possible was the flexibility of its pricing
plans: single-ride passes, full-day passes, and annual memberships.
Customers who purchase single-ride or full-day passes are referred to as
casual riders. Customers who purchase annual memberships are Cyclistic
members.

Cyclistic’s finance analysts have concluded that annual members are much
more profitable than casual riders. Although the pricing flexibility
helps Cyclistic attract more customers, Moreno believes that maximizing
the number of annual members will be key to future growth. Rather than
creating a marketing campaign that targets all-new customers, Moreno
believes there is a very good chance to convert casual riders into
members. She notes that casual riders are already aware of the Cyclistic
program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at
converting casual riders into annual members. In order to do that,
however, the marketing analyst team needs to better understand how
annual members and casual riders differ, why casual riders would buy a
membership, and how digital media could affect their marketing tactics.
Moreno and her team are interested in analyzing the Cyclistic historical
bike trip data to identify trends.

## Step 1: Ask

Three questions will guide the future marketing program:

1.  How do annual members and casual riders use Cyclistic bikes
    differently?
2.  Why would casual riders buy Cyclistic annual memberships?
3.  How can Cyclistic use digital media to influence casual riders to
    become members?

## Step 2: Prepare

In order to analyze and identify trends I will use Cyclistic’s
historical trip data that can be found in the following link:
*<https://divvy-tripdata.s3.amazonaws.com/index.html>*. The data has
already been processed to remove trips that are taken by staff as they
service and inspect the system. All the data is stored in Comma
Separated Files (.csv): as the number of data to analyze is very large
the tools I decided to use is R. I’ll start by loading the main
libraries I need for the analysis.

``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
```

Now I can load the monthly data from the .csv files into 12 data frames.

``` r
aug_2022 <- read_csv("202208-divvy-tripdata.csv")
```

    ## Rows: 785932 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sep_2022 <- read_csv("202209-divvy-tripdata.csv")
```

    ## Rows: 701339 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
oct_2022 <- read_csv("202210-divvy-tripdata.csv")
```

    ## Rows: 558685 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nov_2022 <- read_csv("202211-divvy-tripdata.csv")
```

    ## Rows: 337735 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dec_2022 <- read_csv("202212-divvy-tripdata.csv")
```

    ## Rows: 181806 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
jan_2023 <- read_csv("202301-divvy-tripdata.csv")
```

    ## Rows: 190301 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
feb_2023 <- read_csv("202302-divvy-tripdata.csv")
```

    ## Rows: 190445 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
mar_2023 <- read_csv("202303-divvy-tripdata.csv")
```

    ## Rows: 258678 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
apr_2023 <- read_csv("202304-divvy-tripdata.csv")
```

    ## Rows: 426590 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
may_2023 <- read_csv("202305-divvy-tripdata.csv")
```

    ## Rows: 604827 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
jun_2023 <- read_csv("202306-divvy-tripdata.csv")
```

    ## Rows: 719618 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
jul_2023 <- read_csv("202307-divvy-tripdata.csv")
```

    ## Rows: 767650 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The output shows that the column names and their data types match
perfectly, so, I’m proceeding with the merge.

``` r
all_trips <- bind_rows(aug_2022, sep_2022, oct_2022, nov_2022, dec_2022, jan_2023, 
                       feb_2023, mar_2023, apr_2023, may_2023, jun_2023, jul_2023)

dim(all_trips)
```

    ## [1] 5723606      13

The output shows we have a data set that contains a total of 13 columns
and 5.723.606 rows.

## Step 3: Process

At the moment the data can only be aggregated at the ride-level, which
is too granular. I want to add some additional columns of data, such as
day, month and hour, that provide additional opportunities to aggregate
the data. After that I will add a calculated field for the rides’
length.

I’ll start with adding columns that list the date, month, day, and hour
of each ride.

``` r
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$date, "%b")
all_trips$day_of_week <- format(all_trips$date, "%a")
all_trips$hour <- format(all_trips$started_at, format = "%H")
```

Then I’m adding a ride length calculation to all_trips (in minutes).

``` r
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, 
                                  units = "mins")
```

Inspecting the data frame.

    ## Rows: 5,723,606
    ## Columns: 18
    ## $ ride_id            <chr> "550CF7EFEAE0C618", "DAD198F405F9C5F5", "E6F2BC47B6…
    ## $ rideable_type      <chr> "electric_bike", "electric_bike", "electric_bike", …
    ## $ started_at         <dttm> 2022-08-07 21:34:15, 2022-08-08 14:39:21, 2022-08-…
    ## $ ended_at           <dttm> 2022-08-07 21:41:46, 2022-08-08 14:53:23, 2022-08-…
    ## $ start_station_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ start_station_id   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ end_station_name   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ end_station_id     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ start_lat          <dbl> 41.93, 41.89, 41.97, 41.94, 41.85, 41.79, 41.89, 41…
    ## $ start_lng          <dbl> -87.69, -87.64, -87.69, -87.65, -87.65, -87.72, -87…
    ## $ end_lat            <dbl> 41.94, 41.92, 41.97, 41.97, 41.84, 41.82, 41.89, 41…
    ## $ end_lng            <dbl> -87.72, -87.64, -87.66, -87.69, -87.66, -87.69, -87…
    ## $ member_casual      <chr> "casual", "casual", "casual", "casual", "casual", "…
    ## $ date               <date> 2022-08-07, 2022-08-08, 2022-08-08, 2022-08-08, 20…
    ## $ month              <chr> "Aug", "Aug", "Aug", "Aug", "Aug", "Aug", "Aug", "A…
    ## $ day_of_week        <chr> "Sun", "Mon", "Mon", "Mon", "Sun", "Mon", "Mon", "S…
    ## $ hour               <chr> "21", "14", "15", "02", "20", "13", "14", "20", "21…
    ## $ ride_length        <drtn> 7.516667 mins, 14.033333 mins, 10.733333 mins, 15.…

The output shows “ride_length” column is formatted as duration type: I
need to convert it to numeric so I can run calculations on the data.

``` r
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
```

Now let’s see a summary of the data frame.

    ##    ride_id          rideable_type        started_at                    
    ##  Length:5723606     Length:5723606     Min.   :2022-08-01 00:00:00.00  
    ##  Class :character   Class :character   1st Qu.:2022-09-28 13:56:43.50  
    ##  Mode  :character   Mode  :character   Median :2023-02-16 13:53:51.50  
    ##                                        Mean   :2023-02-01 23:55:22.17  
    ##                                        3rd Qu.:2023-06-03 07:41:37.00  
    ##                                        Max.   :2023-07-31 23:59:56.00  
    ##                                                                        
    ##     ended_at                      start_station_name start_station_id  
    ##  Min.   :2022-08-01 00:05:00.00   Length:5723606     Length:5723606    
    ##  1st Qu.:2022-09-28 14:12:20.25   Class :character   Class :character  
    ##  Median :2023-02-16 14:04:56.50   Mode  :character   Mode  :character  
    ##  Mean   :2023-02-02 00:13:43.58                                        
    ##  3rd Qu.:2023-06-03 08:00:15.00                                        
    ##  Max.   :2023-08-12 04:53:41.00                                        
    ##                                                                        
    ##  end_station_name   end_station_id       start_lat       start_lng     
    ##  Length:5723606     Length:5723606     Min.   :41.64   Min.   :-87.92  
    ##  Class :character   Class :character   1st Qu.:41.88   1st Qu.:-87.66  
    ##  Mode  :character   Mode  :character   Median :41.90   Median :-87.64  
    ##                                        Mean   :41.90   Mean   :-87.65  
    ##                                        3rd Qu.:41.93   3rd Qu.:-87.63  
    ##                                        Max.   :42.07   Max.   :-87.52  
    ##                                                                        
    ##     end_lat         end_lng       member_casual           date           
    ##  Min.   : 0.00   Min.   :-88.16   Length:5723606     Min.   :2022-08-01  
    ##  1st Qu.:41.88   1st Qu.:-87.66   Class :character   1st Qu.:2022-09-28  
    ##  Median :41.90   Median :-87.64   Mode  :character   Median :2023-02-16  
    ##  Mean   :41.90   Mean   :-87.65                      Mean   :2023-02-01  
    ##  3rd Qu.:41.93   3rd Qu.:-87.63                      3rd Qu.:2023-06-03  
    ##  Max.   :42.18   Max.   :  0.00                      Max.   :2023-07-31  
    ##  NA's   :6102    NA's   :6102                                            
    ##     month           day_of_week            hour            ride_length       
    ##  Length:5723606     Length:5723606     Length:5723606     Min.   :-10353.35  
    ##  Class :character   Class :character   Class :character   1st Qu.:     5.45  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :     9.60  
    ##                                                           Mean   :    18.36  
    ##                                                           3rd Qu.:    17.08  
    ##                                                           Max.   : 51461.40  
    ## 

The summary of the data frame shows that I have negative values for the
ride length column: giving the fact that any trips that is below 60
seconds in length is potentially a false starts or users trying to
re-dock a bike to ensure it was secure, I will remove all the rows
containing ride length values less than 1 minute and save the result in
a new data frame.

``` r
all_trips_v2 <- all_trips[!(all_trips$ride_length < 1), ]
```

## Step 4: Analyze

For the analysis step I’m going to perform the following calculations:

- User type percentage over total rides.

``` r
users_percentage <- all_trips_v2 %>%
  group_by(member_casual) %>%
  summarize(percentage = n() / nrow(.))

head(users_percentage)
```

    ## # A tibble: 2 × 2
    ##   member_casual percentage
    ##   <chr>              <dbl>
    ## 1 casual             0.379
    ## 2 member             0.621

- Average duration and number of rides by user type and day of week.

``` r
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Mon", "Tue", 
                                    "Wed", "Thu", "Fri", "Sat", "Sun"))

ride_n_length_by_day_and_user <- all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarize(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  )

head(ride_n_length_by_day_and_user)
```

    ## # A tibble: 6 × 4
    ## # Groups:   member_casual [1]
    ##   member_casual day_of_week number_of_rides average_duration
    ##   <chr>         <ord>                 <int>            <dbl>
    ## 1 casual        Mon                  251438             28.0
    ## 2 casual        Tue                  250428             25.8
    ## 3 casual        Wed                  255856             24.7
    ## 4 casual        Thu                  281852             24.5
    ## 5 casual        Fri                  326577             28.1
    ## 6 casual        Sat                  424172             33.4

- Average duration and number of rides by user type and month.

``` r
all_trips_v2$month <- ordered(all_trips_v2$month, levels = c("Jan", "Feb", "Mar", "Apr",
                              "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ride_n_length_by_month_and_user <- all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarize(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  )

head(ride_n_length_by_month_and_user)
```

    ## # A tibble: 6 × 4
    ## # Groups:   member_casual [1]
    ##   member_casual month number_of_rides average_duration
    ##   <chr>         <ord>           <int>            <dbl>
    ## 1 casual        Jan             38837             23.6
    ## 2 casual        Feb             41818             23.8
    ## 3 casual        Mar             60279             22.1
    ## 4 casual        Apr            142521             28.6
    ## 5 casual        May            227375             29.4
    ## 6 casual        Jun            293046             30.2

- Average duration and number of rides by user type and hour.

``` r
ride_n_by_hour <- all_trips_v2 %>%
  group_by(member_casual, hour) %>%
  summarize(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  )

head(ride_n_by_hour)
```

    ## # A tibble: 6 × 4
    ## # Groups:   member_casual [1]
    ##   member_casual hour  number_of_rides average_duration
    ##   <chr>         <chr>           <int>            <dbl>
    ## 1 casual        00              39848             33.5
    ## 2 casual        01              25880             34.8
    ## 3 casual        02              15636             41.7
    ## 4 casual        03               8892             44.2
    ## 5 casual        04               6291             41.6
    ## 6 casual        05              11824             24.5

## Step 5: Share

Now it’s time to draw the conclusion after the Analysis process. As
shown in the pie chart here below casual riders represent the 37.95% of
the total users: this means that members are the main source of income,
but there’s a good chance to convert some of the casual riders into
members.

<img src="cyclistic_case_study_files/figure-gfm/plot user percentage-1.png" width="75%" style="display: block; margin: auto;" />

The above observation is reinforced by plotting the average rides
duration per day and rider type: casual riders’ average ride duration,
in fact, exceeds member rides’ length every day of the week. Also, we
can see a slightly increase during the weekend for casual members.

<img src="cyclistic_case_study_files/figure-gfm/plot duration by day and user-1.png" width="75%" style="display: block; margin: auto;" />

Looking at the number of rides per day we can see how the two members
data are slightly inversely proportional: casual members tends to take
more rides through the week-end, while member prefer to use the service
during working days, meaning that many member probably use Cyclistic
service mainly to commute to work.

<img src="cyclistic_case_study_files/figure-gfm/plot number of rides by day and user-1.png" width="75%" style="display: block; margin: auto;" />

If we look at the number of rides per month and rider type there’s a
clear usage increase by both type of riders during spring and summer
seasons. So there is a clear seasonability in the service usage.

<img src="cyclistic_case_study_files/figure-gfm/plot number of rides by month and user-1.png" width="75%" style="display: block; margin: auto;" />

Finally, if we plot the number of rides per hour and rider type we can
see how the two type of users have similar habitudes: number of rides
start to increase at 5 AM, reaching the first peak at 8 AM, then we have
a second peak at 5 PM. This is more evident for member users, confirming
that their main bike usage is to commute to work.

<img src="cyclistic_case_study_files/figure-gfm/plot number of rides by hour and user-1.png" width="75%" style="display: block; margin: auto;" />

## Step 6: Act

My personal recommendations for the marketing campaign are:

- As we have seen a seasonability in the analyzed data we should launch
  casual riders email campaign in early spring and run through summer
  season and schedule different offerings and promotions throughout the
  campaign (for example discounts for becoming member before busy
  seasons).
- Create new Cyclistic member packages for entertainment and week-end
  activities.
- Raise awareness about benefits of using Cyclistic service for morning
  and evening commutes.
