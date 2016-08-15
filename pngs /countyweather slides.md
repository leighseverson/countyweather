---
title: ""
author: Rachel Severson
date: June 3, 2016
output: beamer_presentation
---

# Reading data from online into R

![census](census.png)

---

# `read.csv()`


```r
url <- paste0("http://www2.census.gov/geo/docs/reference/",
                "codes/files/national_county.txt")
  county_names <- read.csv(url, header = FALSE, colClasses = "character")
head(county_names)
```

```
  V1 V2  V3             V4 V5
1 AL 01 001 Autauga County H1
2 AL 01 003 Baldwin County H1
3 AL 01 005 Barbour County H1
4 AL 01 007    Bibb County H1
5 AL 01 009  Blount County H1
6 AL 01 011 Bullock County H1
```

---




```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-3](countyweather slides-figure/unnamed-chunk-3-1.png)
