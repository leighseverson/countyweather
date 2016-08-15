Working with online data in R
===
author: Rachel Severson
date: June 3, 2016


===

Reading in a data table from online

http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt

![census](census.png)

===

With `read.csv()`, you can read in a file in table format and save it as a `data.frame`:


```r
url <- paste0("http://www2.census.gov/geo/docs/",
              "reference/codes/files/",
              "national_county.txt")
  county_names <- read.csv(url, header = FALSE, colClasses = "character")
head(county_names)
```

Reading in 'secure' websites
===

(image of https thing in browser)
htts://



Web scraping
===

Working with APIs
===

- 'Application Program Interface'
- provides building blocks for developing a program

- NOAA's API (NCDC CDO)

^ for example. That one requires a key, but not every API does (for example, working
with hourly data does not).

===

![logo](ropensci_LOGO.png)

ROpenSci develops "open scource R packages that provide programmatic access to a variety of scientific data."
- `rnoaa` package

===

`countyweather` package

- raw data
- daily, hourly, stream data

Use examples from vignette
Make screenshots of everything - don't rely on code/internet working

Focus on what Dr. Anderson was so jazzed about - final code to produce files for
many counties can take a while, but once you have those you can loop through and
produce exploratory plots, for example, no problem

GitHub
===

- version control
- collaboration

Look up more online about github
screenshot of logo or what front page looks like
Talk about process of editing a script, pushing to github, someone else adding
changes, pulling those changes directly into R

Problems
===

- Big Data
- We don't have control over the software these funcitons rely on
- Sometimes unclear how to best 'clean' this data

Talk about specific examples - not sure how to average over multiple weather stations
Fow now we're leaning towards just doing a weighted mean (look up what this actually
means statistically, how does it apply to this data - use a specific example)
If anyone has any insight on that we'd love to hear about it LOL

THIS PRESENTATION IS FOR DR. BELL'S 12 LUNCH MEETING ON WEDNESDAY

- plan for subway vs. taxi tomorrow morning, plan on departure time
- Dr. Kinney 10 AM, potentially his student(s) at 10:30 AM, Dr. Petkova at 2 PM
- Train to New Haven v early Tuesday morning - look in to what needs to happen for
that
- book a hotel in New Haven for Tuesday night (?) - def staying in New Haven,
just need to figure out what I'm up to that night
- last meeting on Wendesday is 1:15 pm w/ Dr. Bell, then back to NY - this could
be a good night for ~a play~
